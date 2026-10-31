#' 发表级横向森林图（landscape 版式）
#'
#' 按 forest-plot skill 的 landscape 版式输出单页矢量 PDF：类别（如滞后窗口、暴露窗口）沿 x 轴从左到右排列，
#' 估计值与带端帽的 95% CI 纵向绘制，多个面板自上而下堆叠并共用 y 轴刻度；类别标签仅出现在最下方面板。
#' 线宽、字体、校验规则与 \code{plot_forest} 相同（结构线 0.60 pt，CI 与端帽 0.86 pt）。
#'
#' @section 输入:
#' \itemize{
#'   \item 具名列表（每个元素为一个面板，自上而下排列）：元素为单个 myepi 结果时，其非参照行依次作为类别；
#'     元素为 myepi 结果的列表时，每个结果须恰有一个非参照行，类别标签取列表名称（或 \code{category_labels}）。
#'   \item 数据框：用 \code{panel_col} 与 \code{panels}（具名正则向量，名称为面板标题）划分面板，
#'     类别标签取 \code{category_col} 或 \code{category_labels}（二选一），与 skill 的 CLI 等价。
#' }
#'
#' @param x 具名列表或数据框（见“输入”）
#' @param file 输出 PDF 路径（以 .pdf 结尾）
#' @param panels 仅数据框输入：具名字符向量，名称为面板标题，值为匹配 \code{panel_col} 的正则表达式
#' @param panel_col 仅数据框输入：用于匹配面板的列
#' @param category_col 仅数据框输入：类别标签列
#' @param category_labels 类别标签（按顺序），可替代 \code{category_col} 或列表名称
#' @param estimate,lower,upper 仅数据框输入：估计值与 CI 列名
#' @param unmatched 仅数据框输入：不匹配任何面板的行，"error"（默认）报错或 "drop" 剔除并报告
#' @param effect_label y 轴标题，如 "HR"
#' @param null_value 无效值：比值类为 1，差值类为 0
#' @param scale "linear"（默认）或 "log"
#' @param y_min,y_max,y_ticks y 轴范围与刻度（NULL 为自动）
#' @param tick_digits 刻度小数位（NULL 为自动）
#' @param width_mm,height_mm 成图尺寸（mm），默认宽 190（最小 140），高度自动
#' @param lancet_style 是否使用 Lancet 排版，默认 FALSE
#' @param font_family 字体，默认 "Helvetica"；未安装时报错（不允许字体替换）
#' @param overwrite 是否允许替换已存在的输出文件，默认 FALSE
#' @return 不可见地返回 \code{myepi_forest} 对象（\code{summary}、\code{file}）；用 \code{print()} 查看
#' @export
plot_forest_landscape <- function(x, file, panels = NULL, panel_col = NULL, category_col = NULL, category_labels = NULL,
                                  estimate = "HR", lower = "HR_lower", upper = "HR_upper",
                                  unmatched = c("error", "drop"), effect_label = "HR", null_value = 1,
                                  scale = c("linear", "log"), y_min = NULL, y_max = NULL, y_ticks = NULL,
                                  tick_digits = NULL, width_mm = 190, height_mm = NULL, lancet_style = FALSE,
                                  font_family = "Helvetica", overwrite = FALSE) {
  unmatched <- match.arg(unmatched)
  scale <- match.arg(scale)
  null_value <- .fp_check_number(null_value, "null_value", allow_null = FALSE)
  tick_digits <- .fp_check_digits(tick_digits, "tick_digits")
  y_ticks <- .fp_check_ticks(y_ticks, "y_ticks")
  width_mm <- .fp_check_number(width_mm, "width_mm", allow_null = FALSE)
  if (width_mm < 140) stop("横向森林图宽度至少为 140 mm", call. = FALSE)
  if (!is.character(font_family) || length(font_family) != 1L || !nzchar(font_family)) stop("font_family 必须为单个字体名", call. = FALSE)
  if (!is.null(category_labels)) {
    if (!is.character(category_labels) || any(!nzchar(category_labels)) || anyDuplicated(category_labels)) {
      stop("category_labels 必须为唯一且非空的字符向量", call. = FALSE)
    }
  }

  input <- .fp_landscape_input(x, panels, panel_col, category_col, category_labels, estimate, lower, upper)
  settings <- list(
    panel_column = input$panel_column, panels = input$panels, unmatched = unmatched,
    category_column = input$category_column, category_labels = input$category_labels,
    estimate_column = input$estimate, lower_column = input$lower, upper_column = input$upper,
    effect_label = effect_label, null_value = null_value, log_scale = scale == "log",
    width_mm = width_mm, height_mm = .fp_check_number(height_mm, "height_mm"),
    lancet_style = isTRUE(lancet_style)
  )

  .fp_with_utf8({
    .fp_reset(font_family)
    .fp_validate_font(font_family)
    prepared <- .fp_prepare_landscape(input$data, settings)
    ap <- .fp_plan_with_axis(
      prepared$data$lower, prepared$data$upper, null_value,
      .fp_check_number(y_min, "y_min"), .fp_check_number(y_max, "y_max"), y_ticks,
      settings$log_scale, tick_digits, settings$lancet_style,
      function(axis, tick_labels) .fp_plan_landscape(prepared, settings, axis, tick_labels)
    )
    rendered <- .fp_render(
      file, overwrite, font_family,
      function(path) .fp_draw_landscape(path, prepared, settings, ap$axis, ap$tick_labels, ap$plan),
      c(.FP_STRUCTURE_LWD, .FP_LANDSCAPE_STYLE$ci_lwd), settings$lancet_style
    )
  })

  if (length(prepared$dropped) > 0L) {
    warning("已剔除不匹配任何面板的行（行号:标识）: ", paste(prepared$dropped, collapse = ", "), call. = FALSE)
  }
  if (any(rendered$geometry$minimum_ci < 4)) {
    warning("有面板的最短 CI 不足 4 mm；请检查成图，必要时加高面板或收紧坐标轴", call. = FALSE)
  }
  plan <- ap$plan
  axis <- ap$axis
  titles <- vapply(settings$panels, `[[`, character(1), "title")
  summary <- list(
    status = "ok",
    output = rendered$output,
    rows = paste0("source=", prepared$source_rows, ", dropped=", length(prepared$dropped),
                  ", plotted=", nrow(prepared$data), "; panels=", length(titles),
                  " (top to bottom: ", paste(titles, collapse = ", "), "); categories=", length(prepared$labels)),
    category_order = paste(prepared$labels, collapse = " | "),
    size_mm = sprintf("%.1f x %.1f; plot_height_mm: %.1f; plot_left_mm: %.1f", plan$width, plan$height, plan$plot_height, plan$plot_left),
    axis = paste0("scale=", if (axis$log) "log" else "linear", ", limits=", signif(axis$min, 6), " to ",
                  signif(axis$max, 6), ", ticks=", paste(axis$ticks, collapse = ","), ", null=", null_value),
    shortest_ci_mm = paste(sprintf("%.1f", rendered$geometry$minimum_ci), collapse = ", "),
    line_weights_pt = sprintf("structure=%.2f, ci_and_caps=%.2f", .FP_STRUCTURE_LWD * 0.75, .FP_LANDSCAPE_STYLE$ci_lwd * 0.75),
    strokes_in_pdf = .fp_strokes_text(rendered$strokes),
    axis_baseline = "drawn from the first to the last boundary tick (by construction)",
    fonts = if (anyNA(rendered$fonts)) "not verified (poppler unavailable)" else paste0(paste(rendered$fonts, collapse = ", "), " (all embedded; no other fonts)"),
    text_check = if (is.na(rendered$token_count)) "not verified (poppler unavailable)" else paste0(rendered$token_count, " drawn tokens reproduced by pdftotext"),
    lancet_style = settings$lancet_style
  )
  invisible(structure(list(file = rendered$output, summary = summary), class = "myepi_forest"))
}

.FP_LANDSCAPE_STYLE <- list(
  body = 7.25, panel = 7.8, lineheight = 0.9, title_zone = 11, title_offset = 5,
  min_plot = 35, panel_gap = 5, bottom_zone = 13, right = 4, min_left = 17,
  axis_title_x = 5.2, tick_length = 1.5, tick_label_gap = 2.2, cap = 2.4,
  square = 1.45, ci_lwd = 1.15,
  label_top = 3.7, min_tick_gap = 0.6
)

# myepi 结果 → 带面板/类别列的数据框，随后与数据框输入走同一套校验
.fp_landscape_input <- function(x, panels, panel_col, category_col, category_labels, estimate, lower, upper) {
  if (is.data.frame(x)) {
    if (is.null(panel_col) || !is.character(panels) || is.null(names(panels)) || any(!nzchar(names(panels)))) {
      stop("数据框输入需提供 panel_col 与具名正则向量 panels（名称为面板标题）", call. = FALSE)
    }
    if (anyDuplicated(names(panels))) stop("面板标题必须唯一", call. = FALSE)
    if (is.null(category_col) == is.null(category_labels)) stop("category_col 与 category_labels 必须且只能提供一个", call. = FALSE)
    return(list(
      data = as.data.frame(x, stringsAsFactors = FALSE), panel_column = panel_col,
      panels = lapply(seq_along(panels), function(i) list(title = names(panels)[[i]], pattern = unname(panels[[i]]))),
      category_column = category_col, category_labels = category_labels,
      estimate = estimate, lower = lower, upper = upper
    ))
  }
  if (!is.list(x) || is.null(names(x)) || any(!nzchar(names(x))) || .fp_is_result(x)) {
    stop("x 必须为具名列表（每个元素为一个面板）或数据框", call. = FALSE)
  }
  if (anyDuplicated(names(x))) stop("面板名称必须唯一", call. = FALSE)
  if (!is.null(panels) || !is.null(panel_col) || !is.null(category_col)) {
    stop("myepi 结果输入时面板与类别由列表结构确定，不使用 panels / panel_col / category_col", call. = FALSE)
  }
  rows <- lapply(names(x), function(title) {
    element <- x[[title]]
    if (.fp_is_result(element)) {
      r <- element$result
      keep <- if ("Reference" %in% names(r)) !r$Reference %in% TRUE else rep(TRUE, nrow(r))
      r <- r[keep, , drop = FALSE]
      labels <- if (all(is.na(r$Interval)) || length(unique(r$varname)) == nrow(r)) r$varname else r$Interval
      if (!is.null(element$breaks)) labels <- r$Interval
      return(data.frame(.panel = title, .category = labels, HR = r$HR, HR_lower = r$HR_lower, HR_upper = r$HR_upper,
                        stringsAsFactors = FALSE))
    }
    if (!is.list(element) || !all(vapply(element, .fp_is_result, logical(1)))) {
      stop("面板 '", title, "' 必须为 myepi 结果对象或其列表", call. = FALSE)
    }
    if (is.null(category_labels) && (is.null(names(element)) || any(!nzchar(names(element))))) {
      stop("面板 '", title, "' 的结果列表需要名称作为类别标签（或提供 category_labels）", call. = FALSE)
    }
    do.call(rbind, lapply(seq_along(element), function(i) {
      r <- element[[i]]$result
      keep <- if ("Reference" %in% names(r)) !r$Reference %in% TRUE else rep(TRUE, nrow(r))
      if (sum(keep) != 1L) {
        stop("面板 '", title, "' 第 ", i, " 个结果有 ", sum(keep), " 个非参照行；每个结果须恰有一行（否则请使用数据框接口）",
             call. = FALSE)
      }
      r <- r[keep, , drop = FALSE]
      data.frame(.panel = title, .category = if (is.null(names(element))) NA_character_ else names(element)[[i]],
                 HR = r$HR, HR_lower = r$HR_lower, HR_upper = r$HR_upper, stringsAsFactors = FALSE)
    }))
  })
  data <- do.call(rbind, rows)
  list(
    data = data, panel_column = ".panel",
    panels = lapply(names(x), function(title) list(title = title, pattern = paste0("^\\Q", title, "\\E$"))),
    category_column = if (is.null(category_labels)) ".category" else NULL, category_labels = category_labels,
    estimate = "HR", lower = "HR_lower", upper = "HR_upper"
  )
}

# ---- 数据准备、版面规划与绘制（移植自 skill 的 prepare/plan/draw_landscape）----

.fp_prepare_landscape <- function(data, settings) {
  required <- unique(c(settings$panel_column, settings$estimate_column, settings$lower_column,
                       settings$upper_column, settings$category_column))
  missing_columns <- setdiff(required, names(data))
  if (length(missing_columns) > 0L) stop("缺少列: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  if (nrow(data) == 0L) stop("输入没有数据行", call. = FALSE)
  panel_ids <- as.character(data[[settings$panel_column]])
  if (anyNA(panel_ids)) stop("面板列含有缺失值", call. = FALSE)

  matches <- vapply(settings$panels, function(panel) {
    tryCatch(grepl(panel$pattern, panel_ids, perl = TRUE), error = function(error) {
      stop("面板 '", panel$title, "' 的正则表达式无效: ", conditionMessage(error), call. = FALSE)
    })
  }, logical(nrow(data)))
  if (is.null(dim(matches))) matches <- matrix(matches, ncol = 1L)
  match_counts <- rowSums(matches)
  ambiguous <- which(match_counts > 1L)
  if (length(ambiguous) > 0L) stop("以下数据行匹配了多个面板: ", paste(ambiguous, collapse = ", "), call. = FALSE)
  unmatched <- which(match_counts == 0L)
  if (length(unmatched) > 0L && settings$unmatched == "error") {
    stop("以下数据行未匹配任何面板: ", paste(unmatched, collapse = ", "),
         "；请修正面板正则、筛选输入，或设置 unmatched = \"drop\"", call. = FALSE)
  }
  keep <- which(match_counts == 1L)

  values <- vapply(c(settings$estimate_column, settings$lower_column, settings$upper_column), function(column) {
    .fp_numeric_column(data, column, keep)
  }, numeric(nrow(data)))
  values <- matrix(values, ncol = 3L)
  invalid <- intersect(which(values[, 2] > values[, 1] | values[, 1] > values[, 3]), keep)
  if (length(invalid) > 0L) stop("以下数据行的 CI 未包含估计值: ", paste(invalid, collapse = ", "), call. = FALSE)

  positions <- lapply(seq_along(settings$panels), function(i) intersect(which(matches[, i]), keep))
  counts <- lengths(positions)
  if (any(counts < 1L) || length(unique(counts)) != 1L) {
    stop("每个面板的类别数必须相同且为正；实际为 ",
         paste(sprintf("%s=%d", vapply(settings$panels, `[[`, character(1), "title"), counts), collapse = ", "), call. = FALSE)
  }
  category_count <- counts[[1]]

  if (!is.null(settings$category_labels)) {
    if (length(settings$category_labels) != category_count) {
      stop("category_labels 需包含 ", category_count, " 个标签以匹配每个面板", call. = FALSE)
    }
    labels <- .fp_normalise_text(settings$category_labels)
  } else {
    labels <- .fp_normalise_text(data[[settings$category_column]][positions[[1]]])
    if (any(!nzchar(labels)) || anyDuplicated(labels)) stop("同一面板内的类别标签必须唯一且非空", call. = FALSE)
    for (rows in positions) {
      if (!identical(.fp_normalise_text(data[[settings$category_column]][rows]), labels)) {
        stop("各面板的类别标签及其顺序必须一致", call. = FALSE)
      }
    }
  }

  prepared <- do.call(rbind, lapply(seq_along(positions), function(i) {
    rows <- positions[[i]]
    data.frame(panel_index = i, category_index = seq_len(category_count), estimate = values[rows, 1],
               lower = values[rows, 2], upper = values[rows, 3], source_row = rows)
  }))
  rownames(prepared) <- NULL
  list(
    data = prepared, labels = labels, source_rows = nrow(data),
    dropped = if (length(unmatched)) paste0(unmatched, ":", panel_ids[unmatched]) else character()
  )
}

.fp_plan_landscape <- function(prepared, settings, axis, tick_labels) {
  style <- .FP_LANDSCAPE_STYLE
  panel_count <- length(settings$panels)
  category_count <- length(prepared$labels)
  text_height <- style$body * .FP_PT_TO_MM

  tick_widths <- .fp_text_width_mm(tick_labels, style$body)
  plot_left <- max(style$min_left, style$axis_title_x + text_height / 2 + 1 + max(tick_widths) + style$tick_label_gap)
  plot_right <- settings$width_mm - style$right
  boundary_x <- seq(plot_left, plot_right, length.out = category_count + 1L)
  interval <- diff(boundary_x)[[1]]
  if (interval < 8) stop("类别间隔仅 ", sprintf("%.1f", interval), " mm；请加宽 width_mm", call. = FALSE)
  labels <- vapply(.fp_visible_text(prepared$labels, settings$lancet_style), .fp_wrap_to_width, character(1),
                   width_mm = interval - 2, fontsize = style$body, USE.NAMES = FALSE)
  bottom_zone <- style$bottom_zone + (max(.fp_line_count(labels)) - 1L) * .fp_line_advance_mm(style$body, style$lineheight)

  minimum_height <- panel_count * (style$title_zone + style$min_plot) + (panel_count - 1L) * style$panel_gap + bottom_zone
  height <- if (is.null(settings$height_mm)) minimum_height else settings$height_mm
  if (height < minimum_height - 0.01) stop("height_mm 过小；最小值: ", sprintf("%.1f mm", minimum_height), call. = FALSE)
  plot_height <- (height - panel_count * style$title_zone - (panel_count - 1L) * style$panel_gap - bottom_zone) / panel_count

  tick_y <- .fp_make_scale(axis, 0, plot_height)(axis$ticks)
  if (any(diff(tick_y) < text_height + style$min_tick_gap)) {
    stop(.fp_tick_overlap_error("相邻 y 轴刻度标签重叠或过近；请减少 y_ticks 或加高面板"))
  }
  effect_label <- .fp_visible_text(settings$effect_label, settings$lancet_style)
  if (.fp_text_width_mm(effect_label, style$body) > plot_height) {
    stop("effect_label 长于面板高度；请缩短或增大 height_mm", call. = FALSE)
  }

  list(
    width = settings$width_mm, height = height, plot_left = plot_left, plot_right = plot_right,
    boundary_x = boundary_x, plot_height = plot_height, labels = labels, effect_label = effect_label,
    panel_titles = .fp_visible_text(vapply(settings$panels, `[[`, character(1), "title"), settings$lancet_style)
  )
}

.fp_draw_landscape <- function(path, prepared, settings, axis, tick_labels, plan) {
  style <- .FP_LANDSCAPE_STYLE
  grDevices::cairo_pdf(path, width = plan$width / 25.4, height = plan$height / 25.4,
                       family = .fp_state$font, onefile = TRUE, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  line_gp <- function(lwd, lty = "solid") grid::gpar(col = .FP_INK, lwd = lwd, lty = lty, lineend = "butt")

  boundary_x <- plan$boundary_x
  centre_x <- utils::head(boundary_x, -1L) + diff(boundary_x) / 2
  panel_count <- length(settings$panels)
  minimum_ci <- numeric(panel_count)

  for (p in seq_len(panel_count)) {
    panel_top <- plan$height - (p - 1L) * (style$title_zone + plan$plot_height + style$panel_gap)
    plot_top <- panel_top - style$title_zone
    plot_bottom <- plot_top - plan$plot_height
    map_y <- .fp_make_scale(axis, plot_bottom, plan$plot_height)
    rows <- prepared$data[prepared$data$panel_index == p, , drop = FALSE]
    rows <- rows[order(rows$category_index), , drop = FALSE]

    .fp_text_mm(plan$panel_titles[[p]], mean(c(plan$plot_left, plan$plot_right)), panel_top - style$title_offset,
                fontsize = style$panel, fontface = "bold")
    grid::grid.lines(x = grid::unit(c(plan$plot_left, plan$plot_right), "mm"),
                     y = grid::unit(rep(map_y(settings$null_value), 2), "mm"), gp = line_gp(.FP_STRUCTURE_LWD, "dashed"))

    for (i in seq_len(nrow(rows))) {
      x <- centre_x[[i]]
      ends <- map_y(c(rows$lower[[i]], rows$upper[[i]]))
      grid::grid.lines(x = grid::unit(rep(x, 2), "mm"), y = grid::unit(ends, "mm"), gp = line_gp(style$ci_lwd))
      for (cap_y in ends) {
        grid::grid.lines(x = grid::unit(x + c(-1, 1) * style$cap / 2, "mm"), y = grid::unit(rep(cap_y, 2), "mm"),
                         gp = line_gp(style$ci_lwd))
      }
      grid::grid.points(x = grid::unit(x, "mm"), y = grid::unit(map_y(rows$estimate[[i]]), "mm"), pch = 15,
                        size = grid::unit(style$square, "mm"), gp = grid::gpar(col = .FP_INK, fill = .FP_INK))
    }
    minimum_ci[[p]] <- min(map_y(rows$upper) - map_y(rows$lower))

    grid::grid.lines(x = grid::unit(rep(plan$plot_left, 2), "mm"), y = grid::unit(c(plot_bottom, plot_top), "mm"),
                     gp = line_gp(.FP_STRUCTURE_LWD))
    grid::grid.lines(x = grid::unit(c(boundary_x[[1]], boundary_x[[length(boundary_x)]]), "mm"),
                     y = grid::unit(rep(plot_bottom, 2), "mm"), gp = line_gp(.FP_STRUCTURE_LWD))
    for (t in seq_along(axis$ticks)) {
      tick_y <- map_y(axis$ticks[[t]])
      grid::grid.lines(x = grid::unit(c(plan$plot_left, plan$plot_left - style$tick_length), "mm"),
                       y = grid::unit(rep(tick_y, 2), "mm"), gp = line_gp(.FP_STRUCTURE_LWD))
      .fp_text_mm(tick_labels[[t]], plan$plot_left - style$tick_label_gap, tick_y, just = c("right", "centre"),
                  fontsize = style$body)
    }
    for (bx in boundary_x) {
      grid::grid.lines(x = grid::unit(rep(bx, 2), "mm"), y = grid::unit(c(plot_bottom, plot_bottom - style$tick_length), "mm"),
                       gp = line_gp(.FP_STRUCTURE_LWD))
    }
    if (p == panel_count) {
      for (i in seq_along(plan$labels)) {
        .fp_text_mm(plan$labels[[i]], centre_x[[i]], plot_bottom - style$label_top, just = c("centre", "top"),
                    fontsize = style$body, lineheight = style$lineheight)
      }
    }
    .fp_text_mm(plan$effect_label, style$axis_title_x, (plot_bottom + plot_top) / 2, fontsize = style$body, rot = 90)
  }
  list(minimum_ci = minimum_ci)
}
