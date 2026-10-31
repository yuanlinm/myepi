#' 剂量反应曲线图（CRF）
#'
#' 直接读取 \code{cox_run_rcs} 或 \code{cox_run_psp} 的曲线数据（x、hr、ll、ul），按固定样式输出单页矢量 PDF：
#' 仅左轴与底轴、刻度朝外、无网格；坐标轴两端各留 5\% 空白；HR = 1 处为贯穿面板的灰色虚线；
#' 曲线为实线，CI 为阴影（可选）及上下边线（可选，两端不封口）。
#' 字体嵌入、文本提取与线宽校验与 \code{plot_forest} 相同；任一校验失败不生成文件。
#'
#' @param x \code{cox_run_rcs} 或 \code{cox_run_psp} 的返回对象
#' @param file 输出 PDF 路径（以 .pdf 结尾）
#' @param xlab x 轴标题，默认为暴露变量名。可为字符（可含 µ、³ 等 Unicode 字符）或 plotmath 表达式，
#'   如 \code{expression(PM[2.5]~"(µg/m"^3*")")}；表达式中请勿使用 plotmath 希腊字母（会引入 Symbol 字体而无法通过字体校验）
#' @param ylab y 轴标题，默认 "Hazard Ratio"
#' @param ylim y 轴范围（长度 2）；默认 NULL 时包含全部 CI 与 1，并两端各扩展 5\%。超出范围的部分在面板内裁剪
#' @param line_color,line_width,line_alpha 曲线颜色、线宽（pt）与透明度（0–1）
#' @param ci_fill 是否绘制 CI 阴影，默认 TRUE
#' @param ci_fill_color,ci_fill_alpha CI 阴影颜色与透明度
#' @param ci_border 是否绘制 CI 上下边线（两端不封口），默认 FALSE
#' @param ci_border_color,ci_border_width,ci_border_alpha CI 边线颜色、线宽（pt）与透明度
#' @param width_mm,height_mm 成图尺寸（mm），默认 140 × 110
#' @param font_family 字体，默认 "Helvetica"；未安装时报错（不允许字体替换）
#' @param overwrite 是否允许替换已存在的输出文件，默认 FALSE
#' @return 不可见地返回 \code{myepi_forest} 对象（\code{summary}、\code{file}）；用 \code{print()} 查看
#' @export
plot_crf <- function(x, file, xlab = NULL, ylab = "Hazard Ratio", ylim = NULL,
                     line_color = "black", line_width = 0.75, line_alpha = 1,
                     ci_fill = TRUE, ci_fill_color = "grey60", ci_fill_alpha = 0.35,
                     ci_border = FALSE, ci_border_color = "black", ci_border_width = 0.5, ci_border_alpha = 1,
                     width_mm = 140, height_mm = 110, font_family = "Helvetica", overwrite = FALSE) {
  if (!inherits(x, "myepi_result") || !isTRUE(x$info$method %in% c("rcs", "psp"))) {
    stop("x 必须为 cox_run_rcs 或 cox_run_psp 的返回对象", call. = FALSE)
  }
  curve <- x$result
  if (!all(c("x", "hr", "ll", "ul") %in% names(curve)) || nrow(curve) < 2L ||
      any(!is.finite(as.matrix(curve[c("x", "hr", "ll", "ul")])))) {
    stop("曲线数据需包含有限的 x、hr、ll、ul 列", call. = FALSE)
  }
  if (is.null(xlab)) xlab <- x$info$mainvar
  for (col in c(line_color, ci_fill_color, ci_border_color)) {
    tryCatch(grDevices::col2rgb(col), error = function(e) stop("颜色无效: ", col, call. = FALSE))
  }
  for (a in list(line_alpha = line_alpha, ci_fill_alpha = ci_fill_alpha, ci_border_alpha = ci_border_alpha)) {
    if (!is.numeric(a) || length(a) != 1L || is.na(a) || a < 0 || a > 1) stop("透明度必须为 0–1 之间的数值", call. = FALSE)
  }
  for (w in list(line_width, ci_border_width)) {
    if (!is.numeric(w) || length(w) != 1L || is.na(w) || w <= 0) stop("线宽必须为正数（pt）", call. = FALSE)
  }
  if (!is.null(ylim) && (!is.numeric(ylim) || length(ylim) != 2L || any(!is.finite(ylim)) || ylim[1] >= ylim[2])) {
    stop("ylim 必须为两个递增的有限数值", call. = FALSE)
  }
  width_mm <- .fp_check_number(width_mm, "width_mm", allow_null = FALSE)
  height_mm <- .fp_check_number(height_mm, "height_mm", allow_null = FALSE)
  if (width_mm < 60 || height_mm < 45) stop("成图尺寸至少为 60 × 45 mm", call. = FALSE)

  lwd <- list(line = line_width / 0.75, border = ci_border_width / 0.75)
  expected_lwd <- c(.FP_STRUCTURE_LWD, lwd$line, if (isTRUE(ci_border)) lwd$border)

  .fp_with_utf8({
    .fp_reset(font_family)
    .fp_validate_font(font_family)
    plan <- .fp_with_measuring_device(.crf_plan(curve, xlab, ylab, ylim, width_mm, height_mm))
    rendered <- .fp_render(
      file, overwrite, font_family,
      function(path) .crf_draw(path, curve, plan, lwd, list(
        line_color = line_color, line_alpha = line_alpha,
        ci_fill = isTRUE(ci_fill), ci_fill_color = ci_fill_color, ci_fill_alpha = ci_fill_alpha,
        ci_border = isTRUE(ci_border), ci_border_color = ci_border_color, ci_border_alpha = ci_border_alpha
      )),
      expected_lwd, FALSE
    )
  })

  summary <- list(
    status = "ok",
    output = rendered$output,
    source = sprintf("%s (%s), %d points", x$info$mainvar, x$info$method, nrow(curve)),
    size_mm = sprintf("%.1f x %.1f", width_mm, height_mm),
    x_axis = paste0("limits=", signif(plan$xlim[1], 6), " to ", signif(plan$xlim[2], 6), ", ticks=", paste(plan$xticks, collapse = ",")),
    y_axis = paste0("limits=", signif(plan$ylim[1], 6), " to ", signif(plan$ylim[2], 6), ", ticks=", paste(plan$yticks, collapse = ","),
                    if (plan$clipped) ", curve or CI clipped by ylim" else ""),
    elements = paste0("line=", line_width, " pt", if (isTRUE(ci_fill)) ", ci_fill" else "",
                      if (isTRUE(ci_border)) paste0(", ci_border=", ci_border_width, " pt") else ""),
    strokes_in_pdf = .fp_strokes_text(rendered$strokes),
    fonts = if (anyNA(rendered$fonts)) "not verified (poppler unavailable)" else paste0(paste(rendered$fonts, collapse = ", "), " (all embedded; no other fonts)"),
    text_check = if (is.na(rendered$token_count)) "not verified (poppler unavailable)" else paste0(rendered$token_count, " drawn tokens reproduced by pdftotext")
  )
  if (plan$clipped) warning("曲线或 CI 超出 ylim，已在面板内裁剪", call. = FALSE)
  invisible(structure(list(file = rendered$output, summary = summary), class = "myepi_forest"))
}

.CRF_STYLE <- list(
  tick_text = 8, title = 10, tick_length = 1.0, tick_label_gap = 0.8, title_gap = 1.8,
  outer = 3.0, top = 4.0, right = 4.0, expand = 0.05, ref_col = "#666666", min_tick_gap = 1.0
)

# 刻度：pretty 刻度限定在坐标范围内，标签重叠时减少刻度数
.crf_ticks <- function(lim, along_mm, horizontal) {
  style <- .CRF_STYLE
  for (n in 5:2) {
    ticks <- pretty(lim, n = n)
    ticks <- signif(ticks[ticks >= lim[1] - 1e-9 * abs(lim[1]) & ticks <= lim[2] + 1e-9 * abs(lim[2])], 12)
    if (length(ticks) < 2L) next
    labels <- .fp_format_ticks(ticks, NULL, FALSE, FALSE)
    pos <- (ticks - lim[1]) / diff(lim) * along_mm
    size <- if (horizontal) .fp_text_width_mm(labels, style$tick_text) else rep(style$tick_text * .FP_PT_TO_MM, length(ticks))
    clearance <- diff(pos) - (utils::head(size, -1L) + utils::tail(size, -1L)) / 2
    if (all(clearance >= style$min_tick_gap)) return(list(ticks = ticks, labels = labels))
  }
  stop("坐标轴刻度标签重叠；请增大成图尺寸", call. = FALSE)
}

.crf_title_size_mm <- function(label, fontsize) {
  grob <- if (is.expression(label)) grid::textGrob(label, gp = .fp_text_gpar(fontsize)) else {
    grid::textGrob(label, gp = .fp_text_gpar(fontsize))
  }
  c(width = grid::convertWidth(grid::grobWidth(grob), "mm", valueOnly = TRUE),
    height = grid::convertHeight(grid::grobHeight(grob), "mm", valueOnly = TRUE))
}

.crf_plan <- function(curve, xlab, ylab, ylim, width_mm, height_mm) {
  style <- .CRF_STYLE
  xr <- range(curve$x)
  xlim <- xr + c(-1, 1) * style$expand * diff(xr)
  data_y <- range(c(curve$ll, curve$ul, 1))
  if (is.null(ylim)) ylim <- data_y + c(-1, 1) * style$expand * diff(data_y)
  clipped <- min(curve$ll) < ylim[1] || max(curve$ul) > ylim[2]

  tick_h <- style$tick_text * .FP_PT_TO_MM
  xtitle <- .crf_title_size_mm(xlab, style$title)
  ytitle <- .crf_title_size_mm(ylab, style$title)
  # 先以估计的 y 刻度宽度确定左边距，再按实际刻度复核
  bottom <- style$outer + xtitle[["height"]] + style$title_gap + tick_h + style$tick_label_gap + style$tick_length
  panel_h <- height_mm - style$top - bottom
  y <- .crf_ticks(ylim, panel_h, FALSE)
  left <- style$outer + ytitle[["height"]] + style$title_gap + max(.fp_text_width_mm(y$labels, style$tick_text)) +
    style$tick_label_gap + style$tick_length
  panel_w <- width_mm - style$right - left
  if (panel_w < 30 || panel_h < 25) stop("成图尺寸过小，无法容纳坐标轴与文字", call. = FALSE)
  if (ytitle[["width"]] > panel_h || xtitle[["width"]] > panel_w) stop("坐标轴标题长于面板；请缩短标题或增大成图尺寸", call. = FALSE)
  x <- .crf_ticks(xlim, panel_w, TRUE)

  list(xlim = xlim, ylim = ylim, xticks = x$ticks, xlabels = x$labels, yticks = y$ticks, ylabels = y$labels,
       left = left, bottom = bottom, panel_w = panel_w, panel_h = panel_h, width = width_mm, height = height_mm,
       xlab = xlab, ylab = ylab, clipped = clipped)
}

.crf_draw <- function(path, curve, plan, lwd, opt) {
  style <- .CRF_STYLE
  grDevices::cairo_pdf(path, width = plan$width / 25.4, height = plan$height / 25.4,
                       family = .fp_state$font, onefile = TRUE, bg = "white")
  on.exit(grDevices::dev.off(), add = TRUE)
  grid::grid.newpage()
  structure_gp <- grid::gpar(col = .FP_INK, lwd = .FP_STRUCTURE_LWD, lineend = "butt")

  # 数据层：面板视口内按数据坐标绘制并裁剪
  grid::pushViewport(grid::viewport(
    x = grid::unit(plan$left, "mm"), y = grid::unit(plan$bottom, "mm"),
    width = grid::unit(plan$panel_w, "mm"), height = grid::unit(plan$panel_h, "mm"),
    just = c("left", "bottom"), xscale = plan$xlim, yscale = plan$ylim, clip = "on"
  ))
  if (opt$ci_fill) {
    grid::grid.polygon(
      x = c(curve$x, rev(curve$x)), y = c(curve$ll, rev(curve$ul)), default.units = "native",
      gp = grid::gpar(col = NA, fill = opt$ci_fill_color, alpha = opt$ci_fill_alpha)
    )
  }
  if (plan$ylim[1] <= 1 && plan$ylim[2] >= 1) {
    grid::grid.lines(x = plan$xlim, y = c(1, 1), default.units = "native",
                     gp = grid::gpar(col = style$ref_col, lwd = .FP_STRUCTURE_LWD, lty = "dashed", lineend = "butt"))
  }
  if (opt$ci_border) {
    border_gp <- grid::gpar(col = opt$ci_border_color, lwd = lwd$border, alpha = opt$ci_border_alpha, lineend = "butt")
    grid::grid.lines(x = curve$x, y = curve$ll, default.units = "native", gp = border_gp)
    grid::grid.lines(x = curve$x, y = curve$ul, default.units = "native", gp = border_gp)
  }
  grid::grid.lines(x = curve$x, y = curve$hr, default.units = "native",
                   gp = grid::gpar(col = opt$line_color, lwd = lwd$line, alpha = opt$line_alpha, lineend = "butt", linejoin = "round"))
  grid::popViewport()

  # 坐标轴：左轴与底轴贯穿面板，刻度朝外
  x0 <- plan$left
  y0 <- plan$bottom
  map_x <- function(v) x0 + (v - plan$xlim[1]) / diff(plan$xlim) * plan$panel_w
  map_y <- function(v) y0 + (v - plan$ylim[1]) / diff(plan$ylim) * plan$panel_h
  grid::grid.lines(x = grid::unit(c(x0, x0), "mm"), y = grid::unit(c(y0, y0 + plan$panel_h), "mm"), gp = structure_gp)
  grid::grid.lines(x = grid::unit(c(x0, x0 + plan$panel_w), "mm"), y = grid::unit(c(y0, y0), "mm"), gp = structure_gp)
  for (i in seq_along(plan$xticks)) {
    tx <- map_x(plan$xticks[i])
    grid::grid.lines(x = grid::unit(c(tx, tx), "mm"), y = grid::unit(c(y0, y0 - style$tick_length), "mm"), gp = structure_gp)
    .fp_text_mm(plan$xlabels[i], tx, y0 - style$tick_length - style$tick_label_gap, c("centre", "top"), style$tick_text)
  }
  for (i in seq_along(plan$yticks)) {
    ty <- map_y(plan$yticks[i])
    grid::grid.lines(x = grid::unit(c(x0, x0 - style$tick_length), "mm"), y = grid::unit(c(ty, ty), "mm"), gp = structure_gp)
    .fp_text_mm(plan$ylabels[i], x0 - style$tick_length - style$tick_label_gap, ty, c("right", "centre"), style$tick_text)
  }

  # 坐标轴标题：字符标题记录用于文本校验；表达式标题直接绘制
  draw_title <- function(label, x_mm, y_mm, rot) {
    if (is.expression(label)) {
      grid::grid.text(label, x = grid::unit(x_mm, "mm"), y = grid::unit(y_mm, "mm"), rot = rot,
                      gp = .fp_text_gpar(style$title))
    } else {
      .fp_text_mm(enc2utf8(as.character(label)), x_mm, y_mm, fontsize = style$title, rot = rot)
    }
  }
  xt_h <- .crf_title_size_mm(plan$xlab, style$title)[["height"]]
  yt_h <- .crf_title_size_mm(plan$ylab, style$title)[["height"]]
  draw_title(plan$xlab, x0 + plan$panel_w / 2, style$outer + xt_h / 2, 0)
  draw_title(plan$ylab, style$outer + yt_h / 2, y0 + plan$panel_h / 2, 90)
  list(minimum_ci = Inf)
}
