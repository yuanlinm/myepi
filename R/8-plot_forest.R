#' 发表级森林图（table 版式）
#'
#' 按 forest-plot skill 的 table 版式输出单页矢量 PDF：合并的亚组/类别标签列（亚组为加粗标题行，类别缩进），
#' 每个面板依次为水平 CI、"估计值 (95% CI)" 列与可选的 P 列；按最终印刷尺寸排版，字体嵌入，单色 #222222。
#' 所有结构线 0.60 pt、CI 线 0.86 pt。布局、文字截断、刻度表示、CI 截断等问题均在绘制前检查，
#' 任一检查失败都不会生成文件；poppler 可用时额外校验嵌入字体、文本提取与线宽。
#'
#' @section 输入:
#' \itemize{
#'   \item \code{cox_run_sub} 结果：每个结果为一个亚组块（标题为分组变量，类别为各水平），
#'     \code{P_interaction} 显示在标题行；分类暴露时显示各非参照水平。
#'   \item \code{cox_run_q} 结果：一个块，类别为各分位组，参照组显示 "1.00 (Ref)"；有趋势检验时显示 P for trend。
#'   \item \code{cox_run}、\code{logit_run}、\code{clogit_run}、\code{lm_run} 结果：每个关注变量一个块；数值变量折叠为一行。
#'     效应量（HR、OR、beta）、轴标题与无效值自动识别。
#'   \item 上述结果组成的无名列表：按顺序堆叠为同一面板中的多个块；具名列表：每个元素为一个面板（1–3 个），
#'     各面板的行必须一一对应。
#'   \item 数据框：按 \code{subgroup_col}、\code{category_col} 与 \code{panels} 的列映射作图（与 skill 的 CLI 等价）。
#' }
#'
#' @param x myepi 结果对象、其列表，或数据框（见“输入”）
#' @param file 输出 PDF 路径（以 .pdf 结尾）
#' @param panels 仅数据框输入：面板定义列表（1–3 个），每个元素为 list(title, estimate, lower, upper, p, color)
#'   或 list(title, ci_text, p, color)；ci_text 为 "1.07 (1.03-1.12)" 形式的组合列；p 可省略。
#'   默认单面板 list(title = "", estimate = "HR", lower = "HR_lower", upper = "HR_upper")
#' @param subgroup_col,category_col 仅数据框输入：亚组（加粗标题行）列与类别（缩进行）列
#' @param count_cols 可选计数列（字符向量；具名时以名称作为列标题），如 \code{c(Events = "N_event", Total = "N_total")}；
#'   myepi 结果可用 N_event、N_total、N_case、N_control、N
#' @param ref_col 仅数据框输入：逻辑列，标记参照行（显示 "无效值 (Ref)"，如 "1.00 (Ref)"，不画 CI）
#' @param labels 可选具名字符向量，将亚组/类别的原始标签（名称）精确替换为显示标签（值），
#'   如 \code{c(sex_f = "Sex", age_group = "Age")}
#' @param exclude 按精确标签剔除显示行："亚组|类别"，或 "亚组|*" 剔除整个亚组（使用替换后的显示标签）
#' @param effect_axis_label 效应量名称（轴标题与列标题）；NULL 时按输入自动确定：HR 为 "Hazard ratio"，
#'   OR（\code{logit_run}、\code{clogit_run}）为 "Odds ratio"，beta（\code{lm_run}）为 "Beta"；数据框输入默认 "Hazard ratio"
#' @param effect_abbreviation 效应量缩写（用于 footer_note = "AUTO"）；NULL 时按输入自动确定
#' @param null_value 无效值；NULL 时按输入自动确定：比值类（HR、OR）为 1，beta 为 0
#' @param scale "linear"（默认）或 "log"
#' @param x_min,x_max,ticks 坐标轴范围与刻度（NULL 为自动）
#' @param tick_digits 刻度小数位（NULL 为自动）
#' @param digits 估计值与 CI 的小数位，默认 2
#' @param p_digits 数值 P 值的小数位，默认 3（小于 0.001 显示为 "<0.001"）；文本 P 值按原文显示
#' @param ci_sep CI 分隔符："auto"（存在负值时用 " to "）、"dash" 或 "to"
#' @param p_kind P 列含义："auto"（myepi 结果按来源确定；数据框按列名推断）、"interaction"、"heterogeneity"、"trend" 或 "row"
#' @param p_heading 自定义 P 列标题（"\\n" 换行），NULL 为自动
#' @param width_mm,height_mm 成图尺寸（mm），默认宽度单面板 140、多面板 190，高度自动
#' @param footer_note NULL（默认，无脚注）、"AUTO"（通用缩写说明）或自定义文本
#' @param lancet_style 是否使用 Lancet 排版（中点小数点、en dash），默认 FALSE
#' @param font_family 字体，默认 "Helvetica"；未安装时报错（不允许字体替换）
#' @param overwrite 是否允许替换已存在的输出文件，默认 FALSE
#' @return 不可见地返回 \code{myepi_forest} 对象：\code{summary} 为可记入台账的运行摘要，
#'   \code{file} 为输出路径；用 \code{print()} 查看
#' @export
plot_forest <- function(x, file, panels = NULL, subgroup_col = "Subgroup", category_col = "Category",
                        count_cols = NULL, ref_col = NULL, labels = NULL, exclude = NULL,
                        effect_axis_label = NULL, effect_abbreviation = NULL, null_value = NULL,
                        scale = c("linear", "log"), x_min = NULL, x_max = NULL, ticks = NULL, tick_digits = NULL,
                        digits = 2, p_digits = 3, ci_sep = c("auto", "dash", "to"),
                        p_kind = c("auto", "interaction", "heterogeneity", "trend", "row"), p_heading = NULL,
                        width_mm = NULL, height_mm = NULL, footer_note = NULL, lancet_style = FALSE,
                        font_family = "Helvetica", overwrite = FALSE) {
  scale <- match.arg(scale)
  ci_sep <- match.arg(ci_sep)
  p_kind <- match.arg(p_kind)
  measure <- .FP_MEASURES[[if (is.null(m <- .fp_input_measure(x))) "HR" else m]]
  if (is.null(effect_axis_label)) effect_axis_label <- measure$label
  if (is.null(effect_abbreviation)) effect_abbreviation <- measure$abbr
  if (is.null(null_value)) null_value <- measure$null
  null_value <- .fp_check_number(null_value, "null_value", allow_null = FALSE)
  digits <- .fp_check_digits(digits, "digits")
  p_digits <- .fp_check_digits(p_digits, "p_digits")
  tick_digits <- .fp_check_digits(tick_digits, "tick_digits")
  ticks <- .fp_check_ticks(ticks, "ticks")
  if (!is.character(font_family) || length(font_family) != 1L || !nzchar(font_family)) stop("font_family 必须为单个字体名", call. = FALSE)

  input <- .fp_table_input(x, panels, subgroup_col, category_col, count_cols, ref_col, p_digits, null_value)
  if (!is.null(labels)) {
    if (!is.character(labels) || is.null(names(labels)) || any(!nzchar(names(labels)))) {
      stop("labels 必须为具名字符向量（名称为原始标签，值为显示标签）", call. = FALSE)
    }
    for (col in c(input$subgroup_column, input$category_column)) {
      if (!col %in% names(input$data)) next
      v <- as.character(input$data[[col]])
      hit <- v %in% names(labels)
      v[hit] <- labels[v[hit]]
      input$data[[col]] <- v
    }
  }
  if (length(input$panels) < 1L || length(input$panels) > 3L) stop("需要 1 到 3 个面板", call. = FALSE)
  if (is.null(width_mm)) width_mm <- if (length(input$panels) == 1L) 140 else 190
  width_mm <- .fp_check_number(width_mm, "width_mm", allow_null = FALSE)
  if (width_mm < 90) stop("成图宽度至少为 90 mm", call. = FALSE)

  settings <- list(
    panels = input$panels,
    subgroup_column = input$subgroup_column,
    category_column = input$category_column,
    count_columns = input$count_columns,
    ref = input$ref,
    exclusions = if (is.null(exclude)) character() else exclude,
    effect_axis_label = effect_axis_label,
    effect_abbreviation = effect_abbreviation,
    null_value = null_value,
    log_scale = scale == "log",
    digits = digits,
    tick_digits = tick_digits,
    ci_sep = ci_sep,
    p_kind = p_kind,
    p_heading = p_heading,
    width_mm = width_mm,
    height_mm = .fp_check_number(height_mm, "height_mm"),
    footer_note = if (is.null(footer_note) || identical(tolower(footer_note), "none")) "none" else if (identical(toupper(footer_note), "AUTO")) "AUTO" else footer_note,
    lancet_style = isTRUE(lancet_style)
  )

  .fp_with_utf8({
    .fp_reset(font_family)
    .fp_validate_font(font_family)
    prepared <- .fp_prepare_table(input$data, settings)
    ap <- .fp_plan_with_axis(
      unlist(lapply(prepared$panels, function(p) p$lower[!prepared$ref])),
      unlist(lapply(prepared$panels, function(p) p$upper[!prepared$ref])),
      null_value, .fp_check_number(x_min, "x_min"), .fp_check_number(x_max, "x_max"), ticks,
      settings$log_scale, tick_digits, settings$lancet_style,
      function(axis, tick_labels) .fp_plan_table(prepared, settings, axis, tick_labels)
    )
    rendered <- .fp_render(
      file, overwrite, font_family,
      function(path) .fp_draw_table(path, prepared, settings, ap$axis, ap$tick_labels, ap$plan),
      c(.FP_STRUCTURE_LWD, .FP_TABLE_STYLE$ci_lwd), settings$lancet_style
    )
  })

  if (any(rendered$geometry$minimum_ci < 4)) {
    warning("有面板的最短 CI 不足 4 mm；请检查成图，必要时加宽面板或收紧坐标轴", call. = FALSE)
  }
  plan <- ap$plan
  axis <- ap$axis
  summary <- list(
    status = "ok",
    output = rendered$output,
    rows = paste0("source=", prepared$source_rows, ", excluded=", length(prepared$excluded),
                  ", displayed=", length(prepared$subgroup), ", display_rows=", nrow(plan$display)),
    excluded_rows = if (length(prepared$excluded)) paste(prepared$excluded, collapse = "; ") else "none",
    size_mm = sprintf("%.1f x %.1f", plan$width, plan$height),
    axis = paste0("scale=", if (axis$log) "log" else "linear", ", limits=", signif(axis$min, 6), " to ",
                  signif(axis$max, 6), ", ticks=", paste(axis$ticks, collapse = ","), ", null=", null_value),
    ci_separator = prepared$separator_kind,
    columns_mm = paste(sprintf("%s=%.1f", names(plan$widths), plan$widths), collapse = ", "),
    forest_width_mm = paste(sprintf("%.1f", plan$forest_widths), collapse = ", "),
    shortest_ci_mm = paste(sprintf("%.1f", rendered$geometry$minimum_ci), collapse = ", "),
    line_weights_pt = sprintf("structure=%.2f, ci=%.2f", .FP_STRUCTURE_LWD * 0.75, .FP_TABLE_STYLE$ci_lwd * 0.75),
    strokes_in_pdf = .fp_strokes_text(rendered$strokes),
    axis_baseline = "drawn from the first to the last tick centre (by construction)",
    fonts = if (anyNA(rendered$fonts)) "not verified (poppler unavailable)" else paste0(paste(rendered$fonts, collapse = ", "), " (all embedded; no other fonts)"),
    text_check = if (is.na(rendered$token_count)) "not verified (poppler unavailable)" else paste0(rendered$token_count, " drawn tokens reproduced by pdftotext"),
    lancet_style = settings$lancet_style
  )
  invisible(structure(list(file = rendered$output, summary = summary), class = "myepi_forest"))
}

.FP_COUNT_COLUMNS <- c("N_event", "N_total", "N_case", "N_control", "N")

.FP_TABLE_STYLE <- list(
  body = 7.25, subgroup = 7.5, header = 6.7, p_header = 6.5, panel = 7.8,
  row = 5.15, top = 5.0, header_zone = 10.0, axis_zone = 14.0, side = 4.0,
  gap = 1.0, lineheight = 0.88, square = 1.45, ci_lwd = 1.15,
  tick_length = 1.5, forest_pad = 0.7, min_forest = 25,
  min_tick_gap = 0.8
)

# ---- 输入转换：myepi 结果 / 数据框 → 统一的宽表 ----

.fp_is_result <- function(x) inherits(x, "myepi_result")

# 单个 myepi 结果 → 长表块（subgroup, category, estimate, lower, upper, ref, p, p_kind, 计数列）
.fp_result_blocks <- function(res) {
  r <- res$result
  if (!is.data.frame(r) || nrow(r) == 0L) stop("myepi 结果的 result 为空", call. = FALSE)
  if ("Group" %in% names(r)) stop("请使用 plot_shape = FALSE 的 cox_run_sub 结果", call. = FALSE)
  cols <- .FP_MEASURES[[.fp_result_measure(r)]]$cols
  ref <- if ("Reference" %in% names(r)) r$Reference %in% TRUE else rep(FALSE, nrow(r))
  num <- function(col) if (col %in% names(r)) as.numeric(r[[col]]) else rep(NA_real_, nrow(r))
  block <- function(rows, subgroup, category, p = NA_real_, p_kind = NA_character_) {
    data.frame(
      subgroup = subgroup, category = category,
      estimate = r[[cols[1]]][rows], lower = r[[cols[2]]][rows], upper = r[[cols[3]]][rows], ref = ref[rows],
      p = c(p, rep(NA_real_, length(rows) - 1L)), p_kind = p_kind,
      N_event = num("N_event")[rows], N_total = num("N_total")[rows],
      N_case = num("N_case")[rows], N_control = num("N_control")[rows], N = num("N")[rows],
      stringsAsFactors = FALSE
    )
  }

  if (!is.null(res$info$group_var)) {
    g <- res$info$group_var
    rows <- which(!ref)
    if (length(unique(r$varname[rows])) != 1L) {
      stop("每个 cox_run_sub 结果仅支持一个关注变量；请分别作图或使用数据框接口", call. = FALSE)
    }
    levels <- substring(r$Subgroup[rows], nchar(g) + 3L)
    multi <- anyDuplicated(levels) > 0L
    category <- if (multi) paste0(levels, ": ", r$Interval[rows]) else levels
    p <- if ("P_interaction" %in% names(r)) r$P_interaction[rows][1] else NA_real_
    return(block(rows, g, category, p, if (is.na(p)) NA_character_ else "interaction"))
  }
  if (!is.null(res$breaks)) {
    rows <- seq_len(nrow(r))
    q_label <- sub("^.*_(Q[0-9]+)$", "\\1", r$varname)
    p <- if ("P_trend" %in% names(r)) r$P_trend[1] else NA_real_
    return(block(rows, res$info$mainvar, paste(q_label, r$Interval), p, if (is.na(p)) NA_character_ else "trend"))
  }
  do.call(rbind, lapply(unique(r$varname), function(v) {
    rows <- which(r$varname == v)
    category <- if (any(ref[rows])) r$Interval[rows] else rep(v, length(rows))
    if (anyDuplicated(category)) category <- paste(category, seq_along(rows))
    block(rows, v, category)
  }))
}

.fp_panel_long <- function(element) {
  results <- if (.fp_is_result(element)) list(element) else element
  if (!is.list(results) || length(results) == 0L || !all(vapply(results, .fp_is_result, logical(1)))) {
    stop("面板元素必须为 myepi 结果对象或其列表", call. = FALSE)
  }
  long <- do.call(rbind, lapply(results, .fp_result_blocks))
  kinds <- unique(stats::na.omit(long$p_kind))
  if (length(kinds) > 1L) stop("同一面板混合了不同含义的 P 值（", paste(kinds, collapse = ", "), "）", call. = FALSE)
  attr(long, "p_kind") <- if (length(kinds) == 1L) kinds else NA_character_
  long
}

.fp_table_input <- function(x, panels, subgroup_col, category_col, count_cols, ref_col, p_digits, null_value) {
  count_columns <- if (is.null(count_cols)) character() else count_cols
  if (length(count_columns) > 0L && is.null(names(count_columns))) names(count_columns) <- count_columns
  if (length(count_columns) > 0L) names(count_columns)[!nzchar(names(count_columns))] <- count_columns[!nzchar(names(count_columns))]

  if (is.data.frame(x)) {
    data <- as.data.frame(x, stringsAsFactors = FALSE)
    if (is.null(panels)) panels <- list(list(title = "", estimate = "HR", lower = "HR_lower", upper = "HR_upper"))
    if (!is.list(panels) || !all(vapply(panels, is.list, logical(1)))) stop("panels 必须为面板定义列表的列表", call. = FALSE)
    panels <- lapply(seq_along(panels), function(i) .fp_panel_spec(panels[[i]], i))
    ref <- if (is.null(ref_col)) rep(FALSE, nrow(data)) else {
      if (!ref_col %in% names(data) || !is.logical(data[[ref_col]])) stop("ref_col 必须为数据框中的逻辑列", call. = FALSE)
      data[[ref_col]] %in% TRUE
    }
    for (panel in panels) {
      if (!is.null(panel$p) && panel$p %in% names(data) && is.numeric(data[[panel$p]])) {
        data[[panel$p]] <- .fp_format_p(data[[panel$p]], p_digits)
      }
      if (is.null(panel$ci_text)) {
        for (col in c(panel$estimate, panel$lower, panel$upper)) {
          if (col %in% names(data)) data[[col]][ref] <- null_value
        }
      }
    }
    return(list(data = data, panels = panels, subgroup_column = subgroup_col, category_column = category_col,
                count_columns = count_columns, ref = ref))
  }

  # myepi 结果
  if (.fp_is_result(x)) {
    elements <- list(x)
    titles <- ""
  } else if (is.list(x) && length(x) > 0L) {
    if (is.null(names(x)) || all(!nzchar(names(x)))) {
      elements <- list(x)
      titles <- ""
    } else {
      if (any(!nzchar(names(x)))) stop("具名列表的每个面板都需要名称", call. = FALSE)
      elements <- x
      titles <- names(x)
    }
  } else {
    stop("x 必须为 myepi 结果对象、其列表或数据框", call. = FALSE)
  }
  if (!is.null(panels)) stop("myepi 结果输入时请用具名列表定义面板，不使用 panels 参数", call. = FALSE)
  if (!is.null(ref_col)) stop("myepi 结果输入时参照行由 Reference 列自动识别，不使用 ref_col", call. = FALSE)

  longs <- lapply(elements, .fp_panel_long)
  base <- longs[[1]]
  identity <- paste(base$subgroup, base$category, sep = " | ")
  for (i in seq_along(longs)[-1]) {
    if (!identical(paste(longs[[i]]$subgroup, longs[[i]]$category, sep = " | "), identity) ||
        !identical(longs[[i]]$ref, base$ref)) {
      stop("各面板的亚组/类别行（及参照行）必须一一对应且顺序一致", call. = FALSE)
    }
  }
  if (length(count_columns) > 0L) {
    if (length(longs) > 1L) stop("多面板时计数因面板而异，请使用单面板或数据框接口", call. = FALSE)
    if (!all(count_columns %in% .FP_COUNT_COLUMNS)) {
      stop("myepi 结果输入时 count_cols 只能取 ", paste(.FP_COUNT_COLUMNS, collapse = "、"), call. = FALSE)
    }
  }

  data <- data.frame(Subgroup = base$subgroup, Category = base$category, stringsAsFactors = FALSE)
  for (col in unique(count_columns)) data[[col]] <- base[[col]]
  panel_specs <- list()
  for (i in seq_along(longs)) {
    l <- longs[[i]]
    est <- ifelse(l$ref, null_value, l$estimate)
    low <- ifelse(l$ref, null_value, l$lower)
    up <- ifelse(l$ref, null_value, l$upper)
    data[[paste0(".est_", i)]] <- est
    data[[paste0(".low_", i)]] <- low
    data[[paste0(".up_", i)]] <- up
    kind <- attr(l, "p_kind")
    p_column <- NULL
    if (!is.na(kind)) {
      p_column <- paste0(".p_", i)
      data[[p_column]] <- .fp_format_p(l$p, p_digits)
    }
    panel_specs[[i]] <- list(title = titles[[i]], estimate = paste0(".est_", i), lower = paste0(".low_", i),
                             upper = paste0(".up_", i), p = p_column, p_kind = if (is.na(kind)) NULL else kind,
                             color = .FP_INK, ci_text = NULL)
  }
  list(data = data, panels = panel_specs, subgroup_column = "Subgroup", category_column = "Category",
       count_columns = count_columns, ref = base$ref)
}

.fp_panel_spec <- function(spec, index) {
  if (is.null(spec$title) || !is.character(spec$title)) stop("面板 ", index, " 需要 title", call. = FALSE)
  has_numeric <- !is.null(spec$estimate) && !is.null(spec$lower) && !is.null(spec$upper)
  if (has_numeric == !is.null(spec$ci_text)) {
    stop("面板 ", index, " 需提供 estimate/lower/upper 三列或 ci_text 一列（二选一）", call. = FALSE)
  }
  color <- if (is.null(spec$color) || !nzchar(spec$color)) .FP_INK else spec$color
  tryCatch(grDevices::col2rgb(color), error = function(error) stop("面板 ", index, " 的颜色无效: ", color, call. = FALSE))
  list(title = spec$title, estimate = spec$estimate, lower = spec$lower, upper = spec$upper,
       ci_text = spec$ci_text, p = spec$p, p_kind = NULL, color = color)
}

# ---- 数据准备（移植自 skill 的 prepare_table_data，增加参照行与无 P 块的支持）----

.fp_parse_ci_cell <- function(value, data_row, column_name) {
  text <- .fp_normalise_text(value)
  number <- "(-?[0-9]+(?:\\.[0-9]+)?)"
  pattern <- paste0("^", number, "\\s*[\\(\\[]\\s*", number, "\\s*(?:-|(?i:to)|,|;)\\s*", number, "\\s*[\\)\\]]$")
  groups <- regmatches(text, regexec(pattern, text, perl = TRUE))[[1]]
  if (length(groups) != 4L) {
    stop("无法解析列 '", column_name, "' 第 ", data_row, " 行: ", dQuote(as.character(value), FALSE), call. = FALSE)
  }
  list(values = as.numeric(groups[2:4]), strings = groups[2:4])
}

.fp_resolve_p_kind <- function(kind_option, column_name) {
  if (kind_option != "auto") return(kind_option)
  if (grepl("heterogeneity", column_name, ignore.case = TRUE)) return("heterogeneity")
  if (grepl("interaction", column_name, ignore.case = TRUE)) return("interaction")
  if (grepl("trend", column_name, ignore.case = TRUE)) return("trend")
  "row"
}

.fp_validate_p_text <- function(text, column_name) {
  core <- sub("^[Pp]\\s*", "", text)
  number_text <- trimws(sub("^(<=|>=|<|>|=)\\s*", "", core))
  if (any(grepl("[eE]", number_text))) {
    stop("'", column_name, "' 含科学计数法；请提供与显示一致的 P 值文本", call. = FALSE)
  }
  numbers <- suppressWarnings(as.numeric(number_text))
  if (any(!is.finite(numbers) | numbers < 0 | numbers > 1)) {
    stop("'", column_name, "' 含无效 P 值: ", paste(text[!is.finite(numbers)], collapse = ", "), call. = FALSE)
  }
  if (any(numbers == 0 & !startsWith(core, "<"))) {
    stop("'", column_name, "' 显示的 P 值为 0；请使用 <0.001 等阈值", call. = FALSE)
  }
}

.fp_prepare_p <- function(raw, column_name, kind, subgroups) {
  text <- .fp_normalise_text(raw)
  present <- nzchar(text)
  .fp_validate_p_text(text[present], column_name)
  if (kind == "row") {
    if (!all(present)) stop("'", column_name, "' 在以下数据行缺少 P 值: ", paste(which(!present), collapse = ", "), call. = FALSE)
    return(list(kind = kind, row = text, subgroup = NULL))
  }
  by_group <- character()
  for (group in unique(subgroups)) {
    positions <- which(subgroups == group)
    here <- present[positions]
    if (!any(here)) next
    values <- unique(text[positions][here])
    layout_ok <- here[[1]] && (sum(here) == 1L || all(here))
    if (!layout_ok || length(values) != 1L) {
      stop("'", column_name, "' 每个亚组只能有一个 ", kind, " P 值（位于首行，或各行重复相同值）；请检查亚组 '",
           group, "'", call. = FALSE)
    }
    by_group[[group]] <- values
  }
  list(kind = kind, row = NULL, subgroup = by_group)
}

.fp_prepare_table <- function(data, settings) {
  required <- c(settings$subgroup_column, settings$category_column, unname(settings$count_columns))
  for (panel in settings$panels) required <- c(required, panel$ci_text, panel$estimate, panel$lower, panel$upper, panel$p)
  missing_columns <- setdiff(unique(required), names(data))
  if (length(missing_columns) > 0L) stop("缺少列: ", paste(missing_columns, collapse = ", "), call. = FALSE)
  if (nrow(data) == 0L) stop("输入没有数据行", call. = FALSE)
  ref <- settings$ref

  subgroups <- .fp_normalise_text(data[[settings$subgroup_column]])
  categories <- .fp_normalise_text(data[[settings$category_column]])
  blank <- which(!nzchar(subgroups) | !nzchar(categories))
  if (length(blank) > 0L) stop("以下数据行缺少亚组或类别标签: ", paste(blank, collapse = ", "), call. = FALSE)
  identities <- paste(subgroups, categories, sep = " | ")
  if (anyDuplicated(identities)) {
    stop("亚组-类别行重复: ", paste(unique(identities[duplicated(identities)]), collapse = "; "), call. = FALSE)
  }
  runs <- rle(subgroups)$values
  if (anyDuplicated(runs)) stop("同一亚组的行不连续: ", paste(unique(runs[duplicated(runs)]), collapse = ", "), call. = FALSE)

  counts <- lapply(unname(settings$count_columns), function(column) {
    values <- .fp_numeric_column(data, column)
    if (any(values < 0) || any(abs(values - round(values)) > 1e-8)) stop("计数列 '", column, "' 必须为非负整数", call. = FALSE)
    round(values)
  })

  non_ref <- which(!ref)
  panel_data <- lapply(settings$panels, function(panel) {
    if (!is.null(panel$ci_text)) {
      values <- matrix(NA_real_, nrow(data), 3L)
      strings <- matrix(NA_character_, nrow(data), 3L)
      for (row in non_ref) {
        parsed <- .fp_parse_ci_cell(data[[panel$ci_text]][[row]], row, panel$ci_text)
        values[row, ] <- parsed$values
        strings[row, ] <- parsed$strings
      }
    } else {
      values <- cbind(
        .fp_numeric_column(data, panel$estimate, non_ref),
        .fp_numeric_column(data, panel$lower, non_ref),
        .fp_numeric_column(data, panel$upper, non_ref)
      )
      strings <- NULL
    }
    invalid <- non_ref[values[non_ref, 2] > values[non_ref, 1] | values[non_ref, 1] > values[non_ref, 3]]
    if (length(invalid) > 0L) {
      stop("面板 '", panel$title, "' 在以下数据行的 CI 未包含估计值: ", paste(invalid, collapse = ", "), call. = FALSE)
    }
    p <- NULL
    if (!is.null(panel$p)) {
      kind <- if (settings$p_kind != "auto") settings$p_kind else if (!is.null(panel$p_kind)) panel$p_kind else {
        .fp_resolve_p_kind("auto", panel$p)
      }
      p <- .fp_prepare_p(data[[panel$p]], panel$p, kind, subgroups)
    }
    list(values = values, strings = strings, p = p, ci_text = panel$ci_text)
  })

  keep <- rep(TRUE, nrow(data))
  for (specification in settings$exclusions) {
    parts <- trimws(strsplit(specification, "|", fixed = TRUE)[[1]])
    if (length(parts) != 2L) stop("exclude 必须为 '亚组|类别' 或 '亚组|*'", call. = FALSE)
    hit <- subgroups == .fp_normalise_text(parts[[1]]) & (parts[[2]] == "*" | categories == .fp_normalise_text(parts[[2]]))
    if (!any(hit)) stop("exclude '", specification, "' 未匹配任何行；需使用精确的亚组/类别标签", call. = FALSE)
    keep[hit] <- FALSE
  }
  if (!any(keep)) stop("所有行都被剔除", call. = FALSE)
  if (!any(keep & !ref)) stop("没有可绘制 CI 的非参照行", call. = FALSE)
  partially_excluded <- unique(subgroups[!keep & subgroups %in% subgroups[keep]])
  has_group_p <- any(vapply(panel_data, function(panel) !is.null(panel$p) && panel$p$kind != "row", logical(1)))
  if (length(partially_excluded) > 0L && has_group_p) {
    warning("亚组 ", paste(dQuote(partially_excluded, FALSE), collapse = ", "),
            " 有行被 exclude 剔除，但仍显示基于全部类别计算的亚组水平 P 值", call. = FALSE)
  }

  kept_values <- unlist(lapply(panel_data, function(panel) panel$values[keep & !ref, , drop = FALSE]))
  separator_kind <- if (settings$ci_sep == "auto") {
    if (any(kept_values < 0)) "to" else "dash"
  } else {
    settings$ci_sep
  }
  separator <- if (separator_kind == "to") " to " else "-"
  ref_kept <- ref[keep]

  panels <- lapply(panel_data, function(panel) {
    values <- panel$values[keep, , drop = FALSE]
    strings <- if (is.null(panel$strings)) {
      vapply(1:3, function(j) .fp_format_fixed(values[, j], settings$digits), character(nrow(values)))
    } else {
      panel$strings[keep, , drop = FALSE]
    }
    strings <- matrix(strings, ncol = 3L)
    display <- sprintf("%s (%s%s%s)", strings[, 1], strings[, 2], separator, strings[, 3])
    if (any(ref_kept)) {
      display[ref_kept] <- if (is.null(panel$ci_text)) {
        paste0(.fp_format_fixed(settings$null_value, settings$digits), " (Ref)")
      } else {
        .fp_normalise_text(data[[panel$ci_text]][keep][ref_kept])
      }
    }
    p <- panel$p
    if (!is.null(p) && p$kind == "row") p$row <- p$row[keep]
    list(estimate = values[, 1], lower = values[, 2], upper = values[, 3],
         display = .fp_visible_text(display, settings$lancet_style), p = p)
  })

  list(
    subgroup = subgroups[keep], category = categories[keep], ref = ref_kept,
    counts = lapply(counts, function(values) values[keep]),
    panels = panels, source_rows = nrow(data), excluded = identities[!keep], separator_kind = separator_kind
  )
}

.fp_build_display_rows <- function(subgroups, categories) {
  rows <- list()
  for (group in unique(subgroups)) {
    positions <- which(subgroups == group)
    if (length(positions) == 1L && categories[[positions]] == group) {
      rows[[length(rows) + 1L]] <- data.frame(type = "collapsed", label = group, group = group, source = positions)
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(type = "subgroup", label = group, group = group, source = NA_integer_)
    for (position in positions) {
      rows[[length(rows) + 1L]] <- data.frame(type = "category", label = categories[[position]], group = group, source = position)
    }
  }
  rows <- do.call(rbind, rows)
  rownames(rows) <- NULL
  rows
}

.fp_p_heading_for <- function(kind, custom) {
  if (!is.null(custom)) return(.fp_heading_text(custom))
  switch(kind,
    interaction = "P for\ninteraction",
    heterogeneity = "P for\nheterogeneity",
    trend = "P for\ntrend",
    "P value"
  )
}

# ---- 版面规划与绘制（移植自 skill 的 plan_table / draw_table）----

.fp_plan_table <- function(prepared, settings, axis, tick_labels) {
  style <- .FP_TABLE_STYLE
  lancet <- settings$lancet_style
  panels <- settings$panels
  display <- .fp_build_display_rows(prepared$subgroup, prepared$category)
  display$text <- .fp_visible_text(display$label, lancet)
  display$bold <- display$type != "category"

  weights <- c(labels = 2.25)
  for (k in seq_along(settings$count_columns)) weights[[paste0("count_", k)]] <- 0.65
  for (i in seq_along(panels)) {
    weights[[paste0("forest_", i)]] <- 2.20
    weights[[paste0("effect_", i)]] <- 1.65
    if (!is.null(panels[[i]]$p)) weights[[paste0("p_", i)]] <- 0.65
  }
  available <- settings$width_mm - 2 * style$side - style$gap * (length(weights) - 1L)
  if (available <= 0) stop("成图宽度不足以容纳所选列", call. = FALSE)
  widths <- available * weights / sum(weights)

  count_headers <- .fp_visible_text(names(settings$count_columns), lancet)
  count_labels <- lapply(prepared$counts, function(values) format(values, big.mark = ",", scientific = FALSE, trim = TRUE))
  effect_header <- .fp_visible_text(paste0(settings$effect_axis_label, "\n(95% CI)"), lancet)
  p_headers <- lapply(prepared$panels, function(panel) {
    if (is.null(panel$p)) NULL else .fp_visible_text(.fp_p_heading_for(panel$p$kind, settings$p_heading), lancet)
  })
  p_labels <- lapply(prepared$panels, function(panel) {
    if (is.null(panel$p)) NULL else .fp_visible_text(c(panel$p$row, panel$p$subgroup), lancet)
  })

  required <- numeric()
  words <- unlist(strsplit(display$text, " ", fixed = TRUE))
  required[["labels"]] <- max(
    .fp_text_width_mm(unlist(strsplit(display$text[display$bold], " ", fixed = TRUE)), style$subgroup, "bold") + 1.4,
    .fp_text_width_mm(words, style$body) + 3.2
  )
  for (k in seq_along(settings$count_columns)) {
    required[[paste0("count_", k)]] <- max(
      .fp_text_width_mm(count_headers[[k]], style$header, "bold"),
      .fp_text_width_mm(count_labels[[k]], style$body)
    ) + 1.6
  }
  for (i in seq_along(panels)) {
    required[[paste0("effect_", i)]] <- max(
      .fp_text_width_mm(effect_header, style$header, "bold"),
      .fp_text_width_mm(prepared$panels[[i]]$display, style$body)
    ) + 1.0
    if (!is.null(p_headers[[i]])) {
      required[[paste0("p_", i)]] <- max(
        .fp_text_width_mm(p_headers[[i]], style$p_header, "bold"),
        .fp_text_width_mm(p_labels[[i]], style$body)
      ) + 1.0
    }
  }
  fitted <- names(required)
  shortfall <- sum(pmax(required - widths[fitted], 0))
  widths[fitted] <- pmax(widths[fitted], required)
  forest_names <- grep("^forest_", names(widths), value = TRUE)
  widths[forest_names] <- widths[forest_names] - shortfall / length(forest_names)
  forest_widths <- widths[forest_names] - 2 * style$forest_pad
  if (any(forest_widths < style$min_forest)) {
    stop("文本列排布后森林图面板仅剩 ", sprintf("%.1f", min(forest_widths)), " mm（最少 ", style$min_forest,
         " mm）；请加宽 width_mm、去掉计数列或缩短标签", call. = FALSE)
  }
  lefts <- style$side + c(0, utils::head(cumsum(widths + style$gap), -1L))
  names(lefts) <- names(widths)

  display$wrapped <- vapply(seq_len(nrow(display)), function(i) {
    if (display$bold[[i]]) {
      .fp_wrap_to_width(display$text[[i]], widths[["labels"]] - 1.4, style$subgroup, "bold")
    } else {
      .fp_wrap_to_width(display$text[[i]], widths[["labels"]] - 3.2, style$body)
    }
  }, character(1))
  sizes <- ifelse(display$bold, style$subgroup, style$body)
  display$height <- style$row + (.fp_line_count(display$wrapped) - 1L) *
    vapply(sizes, .fp_line_advance_mm, numeric(1), lineheight = style$lineheight)

  panel_titles <- .fp_visible_text(vapply(panels, `[[`, character(1), "title"), lancet)
  header_heights <- c(
    .fp_line_count(effect_header) * .fp_line_advance_mm(style$header),
    unlist(lapply(p_headers, function(h) if (is.null(h)) 0 else .fp_line_count(h) * .fp_line_advance_mm(style$p_header))),
    .fp_line_count(count_headers) * .fp_line_advance_mm(style$header),
    .fp_line_advance_mm(style$panel)
  )
  header_zone <- max(style$header_zone, max(header_heights) + 4)

  footer <- NULL
  if (!identical(settings$footer_note, "none")) {
    note <- settings$footer_note
    if (identical(note, "AUTO")) {
      note <- paste0(
        settings$effect_abbreviation, "=", tolower(settings$effect_axis_label),
        "; CI=confidence interval. Squares indicate point estimates, horizontal lines indicate 95% CIs, ",
        "and the solid vertical line indicates ", settings$effect_abbreviation, "=", format(settings$null_value), "."
      )
    }
    footer <- .fp_wrap_to_width(.fp_visible_text(note, lancet), settings$width_mm - 2 * style$side, style$body)
  }
  footer_zone <- if (is.null(footer)) 3 else 5 + .fp_line_count(footer) * .fp_line_advance_mm(style$body, 0.95)

  minimum_height <- style$top + header_zone + sum(display$height) + style$axis_zone + footer_zone
  height <- if (is.null(settings$height_mm)) minimum_height else settings$height_mm
  if (height < minimum_height - 0.01) {
    stop("height_mm 过小，无法容纳 7 pt 文字；最小值: ", sprintf("%.1f mm", minimum_height), call. = FALSE)
  }

  tick_widths <- .fp_text_width_mm(tick_labels, style$body)
  for (i in seq_along(panels)) {
    forest_left <- lefts[[paste0("forest_", i)]] + style$forest_pad
    tick_x <- .fp_make_scale(axis, forest_left, forest_widths[[i]])(axis$ticks)
    clearance <- diff(tick_x) - (utils::head(tick_widths, -1L) + utils::tail(tick_widths, -1L)) / 2
    if (any(clearance < style$min_tick_gap)) {
      stop(.fp_tick_overlap_error("相邻刻度标签重叠或过近；请减少 ticks 或加宽面板"))
    }
  }

  list(
    display = display, widths = widths, lefts = lefts, forest_widths = forest_widths,
    count_headers = count_headers, count_labels = count_labels,
    effect_header = effect_header, p_headers = p_headers, panel_titles = panel_titles,
    header_zone = header_zone, footer = footer, width = settings$width_mm,
    height = height, axis_title = .fp_visible_text(settings$effect_axis_label, lancet)
  )
}

.fp_draw_table <- function(path, prepared, settings, axis, tick_labels, plan) {
  style <- .FP_TABLE_STYLE
  display <- plan$display
  grDevices::cairo_pdf(path, width = plan$width / 25.4, height = plan$height / 25.4,
                       family = .fp_state$font, onefile = TRUE, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()

  data_top <- plan$height - style$top - plan$header_zone
  row_tops <- data_top - c(0, utils::head(cumsum(display$height), -1L))
  row_y <- row_tops - display$height / 2
  data_bottom <- data_top - sum(display$height)
  header_y <- data_top + plan$header_zone * 0.52
  line_gp <- function(lwd, col = .FP_INK) grid::gpar(col = col, lwd = lwd, lineend = "butt")

  grid::grid.lines(
    x = grid::unit(c(style$side, plan$width - style$side), "mm"),
    y = grid::unit(rep(data_top + 0.2, 2), "mm"),
    gp = line_gp(.FP_STRUCTURE_LWD)
  )

  label_left <- plan$lefts[["labels"]]
  for (i in seq_len(nrow(display))) {
    .fp_text_mm(
      display$wrapped[[i]], label_left + if (display$bold[[i]]) 0.7 else 2.5, row_y[[i]], c("left", "centre"),
      if (display$bold[[i]]) style$subgroup else style$body, if (display$bold[[i]]) "bold" else "plain"
    )
  }

  data_rows <- which(display$type != "subgroup")
  for (k in seq_along(settings$count_columns)) {
    key <- paste0("count_", k)
    x_right <- plan$lefts[[key]] + plan$widths[[key]] - 0.8
    .fp_text_mm(plan$count_headers[[k]], x_right, header_y, c("right", "centre"), style$header, "bold")
    for (i in data_rows) {
      .fp_text_mm(plan$count_labels[[k]][[display$source[[i]]]], x_right, row_y[[i]], c("right", "centre"), style$body)
    }
  }

  minimum_ci <- numeric(length(settings$panels))
  for (p in seq_along(settings$panels)) {
    panel <- settings$panels[[p]]
    values <- prepared$panels[[p]]
    forest_left <- plan$lefts[[paste0("forest_", p)]] + style$forest_pad
    forest_width <- plan$forest_widths[[p]]
    map_x <- .fp_make_scale(axis, forest_left, forest_width)
    effect_key <- paste0("effect_", p)
    effect_right <- plan$lefts[[effect_key]] + plan$widths[[effect_key]] - 0.5

    .fp_text_mm(plan$panel_titles[[p]], forest_left + forest_width / 2, header_y,
                fontsize = style$panel, fontface = "bold", col = panel$color)
    .fp_text_mm(plan$effect_header, plan$lefts[[effect_key]] + plan$widths[[effect_key]] / 2, header_y,
                fontsize = style$header, fontface = "bold")

    null_x <- map_x(settings$null_value)
    grid::grid.lines(x = grid::unit(rep(null_x, 2), "mm"), y = grid::unit(c(data_bottom, data_top), "mm"),
                     gp = line_gp(.FP_STRUCTURE_LWD))

    for (i in data_rows) {
      source <- display$source[[i]]
      if (!prepared$ref[[source]]) {
        grid::grid.lines(
          x = grid::unit(map_x(c(values$lower[[source]], values$upper[[source]])), "mm"),
          y = grid::unit(rep(row_y[[i]], 2), "mm"),
          gp = line_gp(style$ci_lwd, panel$color)
        )
        grid::grid.points(
          x = grid::unit(map_x(values$estimate[[source]]), "mm"), y = grid::unit(row_y[[i]], "mm"),
          pch = 15, size = grid::unit(style$square, "mm"), gp = grid::gpar(col = panel$color, fill = panel$color)
        )
      }
      .fp_text_mm(values$display[[source]], effect_right, row_y[[i]], c("right", "centre"), style$body)
    }
    shown <- !prepared$ref
    minimum_ci[[p]] <- min(map_x(values$upper[shown]) - map_x(values$lower[shown]))

    if (!is.null(values$p)) {
      p_key <- paste0("p_", p)
      p_right <- plan$lefts[[p_key]] + plan$widths[[p_key]] - 0.5
      .fp_text_mm(plan$p_headers[[p]], plan$lefts[[p_key]] + plan$widths[[p_key]] / 2, header_y,
                  fontsize = style$p_header, fontface = "bold")
      for (i in seq_len(nrow(display))) {
        label <- if (values$p$kind == "row") {
          if (display$type[[i]] == "subgroup") "" else values$p$row[[display$source[[i]]]]
        } else {
          if (display$type[[i]] == "category" || !display$group[[i]] %in% names(values$p$subgroup)) "" else {
            values$p$subgroup[[display$group[[i]]]]
          }
        }
        .fp_text_mm(.fp_visible_text(label, settings$lancet_style), p_right, row_y[[i]], c("right", "centre"), style$body)
      }
    }

    tick_x <- map_x(axis$ticks)
    grid::grid.lines(x = grid::unit(c(tick_x[[1]], tick_x[[length(tick_x)]]), "mm"),
                     y = grid::unit(rep(data_bottom, 2), "mm"), gp = line_gp(.FP_STRUCTURE_LWD))
    for (t in seq_along(tick_x)) {
      grid::grid.lines(x = grid::unit(rep(tick_x[[t]], 2), "mm"),
                       y = grid::unit(c(data_bottom, data_bottom - style$tick_length), "mm"),
                       gp = line_gp(.FP_STRUCTURE_LWD))
      .fp_text_mm(tick_labels[[t]], tick_x[[t]], data_bottom - 4.0, fontsize = style$body)
    }
    .fp_text_mm(plan$axis_title, mean(range(tick_x)), data_bottom - 8.8, fontsize = style$body)
  }

  if (!is.null(plan$footer)) {
    .fp_text_mm(plan$footer, style$side, 3.0, c("left", "bottom"), style$body, col = "#333333", lineheight = 0.95)
  }
  list(minimum_ci = minimum_ci)
}
