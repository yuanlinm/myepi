# 发表级森林图共享核心：移植自 forest-plot skill 的 shared core（文本规范化、Lancet 排版、刻度、
# 坐标轴、文本测量与换行、输出与 PDF 校验）。table 与 landscape 两种版式共用本文件。
# 与 skill 的差异：字体可通过 font_family 设定（默认 Helvetica）；poppler 不可用时跳过嵌入字体与
# 文本提取校验并给出 message；UTF-8 locale 仅在绘图期间临时切换，结束后恢复。

.FP_INK <- "#222222"
.FP_MIDDOT <- "\u00b7"
.FP_EN_DASH <- "\u2013"
.FP_PT_TO_MM <- 25.4 / 72
# 两种版式所有坐标轴、刻度、分隔线与无效线共用一个线宽（grid lwd 0.8 = 0.6 pt）
.FP_STRUCTURE_LWD <- 0.8

.FP_PLAIN_FROM <- c(
  "\u2264", "\u2265", "\u2013", "\u2014", "\u2212", "\u2010", "\u2011", "\u00a0", "\u03bc",
  "\u2070", "\u00b9", "\u00b2", "\u00b3", "\u2074", "\u2075", "\u2076", "\u2077", "\u2078", "\u2079",
  "\u2080", "\u2081", "\u2082", "\u2083", "\u2084", "\u2085", "\u2086", "\u2087", "\u2088", "\u2089"
)
.FP_PLAIN_TO <- c(
  "<=", ">=", "-", "-", "-", "-", "-", " ", "\u00b5",
  as.character(0:9),
  as.character(0:9)
)

# 效应量：myepi 结果中的列名、默认轴标题、缩写与无效值
.FP_MEASURES <- list(
  HR = list(cols = c("HR", "HR_lower", "HR_upper"), label = "Hazard ratio", abbr = "HR", null = 1),
  OR = list(cols = c("OR", "OR_lower", "OR_upper"), label = "Odds ratio", abbr = "OR", null = 1),
  beta = list(cols = c("beta", "lower", "upper"), label = "Beta", abbr = "Beta", null = 0)
)

.fp_result_measure <- function(r) {
  for (m in names(.FP_MEASURES)) if (all(.FP_MEASURES[[m]]$cols %in% names(r))) return(m)
  stop("myepi 结果中未找到 HR、OR 或 beta 效应量列", call. = FALSE)
}

.fp_collect_results <- function(x) {
  if (inherits(x, "myepi_result")) return(list(x))
  if (is.list(x) && !is.data.frame(x)) return(unlist(lapply(x, .fp_collect_results), recursive = FALSE))
  list()
}

# 输入中 myepi 结果的效应量类型；数据框输入返回 NULL
.fp_input_measure <- function(x) {
  results <- .fp_collect_results(x)
  if (length(results) == 0L) return(NULL)
  m <- unique(vapply(results, function(r) .fp_result_measure(r$result), character(1)))
  if (length(m) > 1L) stop("同一张图中混合了不同的效应量（", paste(m, collapse = ", "), "）", call. = FALSE)
  m
}

# 绘图状态：当前字体与已绘制文本（供 pdftotext 校验）
.fp_state <- new.env(parent = emptyenv())

.fp_reset <- function(font_family) {
  .fp_state$font <- font_family
  .fp_state$labels <- character()
}

.fp_with_utf8 <- function(code) {
  if (!isTRUE(l10n_info()[["UTF-8"]])) {
    old <- Sys.getlocale("LC_CTYPE")
    on.exit(suppressWarnings(Sys.setlocale("LC_CTYPE", old)), add = TRUE)
    for (candidate in c("en_US.UTF-8", "C.UTF-8", "UTF-8")) {
      suppressWarnings(Sys.setlocale("LC_CTYPE", candidate))
      if (isTRUE(l10n_info()[["UTF-8"]])) break
    }
    if (!isTRUE(l10n_info()[["UTF-8"]])) {
      stop("需要 UTF-8 locale：否则中点小数点与 en dash 会被逐字节绘制为乱码", call. = FALSE)
    }
  }
  force(code)
}

.fp_check_choice <- function(value, choices, name) {
  value <- tolower(trimws(value[[1]]))
  if (!value %in% choices) stop(name, " 必须为以下之一: ", paste(choices, collapse = ", "), call. = FALSE)
  value
}

.fp_check_number <- function(value, name, allow_null = TRUE) {
  if (is.null(value) && allow_null) return(NULL)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value)) stop(name, " 必须为单个有限数值", call. = FALSE)
  value
}

.fp_check_digits <- function(value, name) {
  value <- .fp_check_number(value, name)
  if (!is.null(value) && (value < 0 || value > 6 || value != round(value))) {
    stop(name, " 必须为 0 到 6 的整数", call. = FALSE)
  }
  value
}

.fp_check_ticks <- function(value, name) {
  if (is.null(value)) return(NULL)
  if (!is.numeric(value) || length(value) < 2L || any(!is.finite(value)) || is.unsorted(value, strictly = TRUE)) {
    stop(name, " 至少需要两个严格递增的数值", call. = FALSE)
  }
  value
}

.fp_heading_text <- function(value) {
  gsub("\\n", "\n", value, fixed = TRUE)
}

.fp_normalise_text <- function(value) {
  text <- as.character(value)
  text[is.na(text)] <- ""
  text <- enc2utf8(text)
  for (i in seq_along(.FP_PLAIN_FROM)) {
    text <- gsub(.FP_PLAIN_FROM[[i]], .FP_PLAIN_TO[[i]], text, fixed = TRUE)
  }
  text <- gsub("(?<=\\d)\u00b7(?=\\d)", ".", text, perl = TRUE)
  trimws(text)
}

.fp_lancet_text <- function(text) {
  text <- gsub("(?<=\\d)\\.(?=\\d)", .FP_MIDDOT, text, perl = TRUE)
  gsub("-", .FP_EN_DASH, text, fixed = TRUE)
}

.fp_visible_text <- function(value, lancet_style) {
  text <- .fp_normalise_text(value)
  if (lancet_style) text <- .fp_lancet_text(text)
  text
}

.fp_format_fixed <- function(values, digits) {
  text <- formatC(values, format = "f", digits = digits)
  sub("^-(0(\\.0+)?)$", "\\1", text)
}

# 数值 P 值转为显示文本：< 10^-p_digits 显示阈值，其余保留 p_digits 位小数
.fp_format_p <- function(p, p_digits) {
  out <- rep("", length(p))
  ok <- !is.na(p)
  eps <- 10^-p_digits
  out[ok] <- ifelse(p[ok] < eps, paste0("<", .fp_format_fixed(eps, p_digits)), .fp_format_fixed(p[ok], p_digits))
  out
}

.fp_decimals_needed <- function(values, max_digits = 4L) {
  for (digits in 0:max_digits) {
    if (all(abs(round(values, digits) - values) <= 1e-9 * pmax(1, abs(values)))) return(digits)
  }
  max_digits
}

.fp_format_ticks <- function(ticks, digits, log_scale, lancet_style) {
  labels <- if (!is.null(digits)) {
    .fp_format_fixed(ticks, digits)
  } else if (log_scale && max(ticks) / min(ticks) >= 3) {
    vapply(ticks, function(tick) .fp_format_fixed(tick, .fp_decimals_needed(tick)), character(1))
  } else {
    .fp_format_fixed(ticks, .fp_decimals_needed(ticks))
  }
  shown <- suppressWarnings(as.numeric(labels))
  if (anyDuplicated(labels) || any(abs(shown - ticks) > 1e-9 * pmax(1, abs(ticks)))) {
    stop("刻度标签会误表示刻度值（", paste(labels, collapse = ", "), "）；请提高 tick_digits 或改用其他刻度", call. = FALSE)
  }
  .fp_visible_text(labels, lancet_style)
}

.fp_numeric_column <- function(data, column, rows = seq_len(nrow(data))) {
  values <- suppressWarnings(as.numeric(.fp_normalise_text(data[[column]])))
  bad <- intersect(which(!is.finite(values)), rows)
  if (length(bad) > 0L) {
    stop("列 '", column, "' 在以下数据行不是有限数值: ", paste(utils::head(bad, 10), collapse = ", "), call. = FALSE)
  }
  values
}

.fp_validate_font <- function(family) {
  if (!capabilities("cairo")) stop("当前 R 未编译 Cairo 支持，无法生成嵌入字体的 PDF", call. = FALSE)
  if (!requireNamespace("systemfonts", quietly = TRUE)) {
    message("未安装 systemfonts：跳过字体安装检查（poppler 可用时仍会校验 PDF 中的嵌入字体）")
    return(invisible(FALSE))
  }
  families <- tolower(systemfonts::system_fonts()$family)
  if (!tolower(family) %in% families) {
    stop(sprintf("字体 '%s' 未安装；不允许字体替换。请安装该字体，或通过 font_family 指定已安装的字体（如 \"Arial\"）", family),
         call. = FALSE)
  }
  invisible(TRUE)
}

.fp_text_gpar <- function(fontsize, fontface = "plain", col = .FP_INK, lineheight = 0.88) {
  grid::gpar(fontfamily = .fp_state$font, fontsize = fontsize, fontface = fontface, col = col, lineheight = lineheight)
}

.fp_text_mm <- function(label, x_mm, y_mm, just = c("centre", "centre"), fontsize = 7.25,
                        fontface = "plain", col = .FP_INK, rot = 0, lineheight = 0.88) {
  if (!nzchar(label)) return(invisible(NULL))
  .fp_state$labels <- c(.fp_state$labels, label)
  grid::grid.text(
    label, x = grid::unit(x_mm, "mm"), y = grid::unit(y_mm, "mm"), just = just, rot = rot,
    gp = .fp_text_gpar(fontsize, fontface, col, lineheight)
  )
}

.fp_text_width_mm <- function(labels, fontsize, fontface = "plain", lineheight = 0.88) {
  vapply(labels, function(label) {
    if (!nzchar(label)) return(0)
    grob <- grid::textGrob(label, gp = .fp_text_gpar(fontsize, fontface, lineheight = lineheight))
    grid::convertWidth(grid::grobWidth(grob), "mm", valueOnly = TRUE)
  }, numeric(1), USE.NAMES = FALSE)
}

.fp_line_advance_mm <- function(fontsize, lineheight = 0.88) {
  fontsize * .FP_PT_TO_MM * lineheight
}

.fp_line_count <- function(text) {
  lengths(strsplit(text, "\n", fixed = TRUE))
}

.fp_wrap_to_width <- function(text, width_mm, fontsize, fontface = "plain") {
  if (!nzchar(text) || .fp_text_width_mm(text, fontsize, fontface) <= width_mm) return(text)
  words <- strsplit(text, " ", fixed = TRUE)[[1]]
  lines <- character()
  current <- ""
  for (word in words) {
    candidate <- if (nzchar(current)) paste(current, word) else word
    if (!nzchar(current) || .fp_text_width_mm(candidate, fontsize, fontface) <= width_mm) {
      current <- candidate
    } else {
      lines <- c(lines, current)
      current <- word
    }
  }
  lines <- c(lines, current)
  if (any(.fp_text_width_mm(lines, fontsize, fontface) > width_mm)) {
    stop("标签无法在不缩小字号的情况下换行至 ", sprintf("%.1f", width_mm), " mm: ", dQuote(text, FALSE), call. = FALSE)
  }
  paste(lines, collapse = "\n")
}

.fp_with_measuring_device <- function(code) {
  path <- tempfile(fileext = ".pdf")
  grDevices::cairo_pdf(path, width = 8, height = 8, family = .fp_state$font)
  on.exit({
    grDevices::dev.off()
    unlink(path)
  }, add = TRUE)
  grid::grid.newpage()
  force(code)
}

.fp_tick_overlap_error <- function(message) {
  structure(class = c("tick_overlap", "error", "condition"), list(message = message, call = NULL))
}

.fp_nice_ticks <- function(low, high, target) {
  n <- target
  repeat {
    ticks <- pretty(c(low, high), n = n)
    if (length(ticks) <= target + 2L || n <= 1L) return(ticks)
    n <- n - 1L
  }
}

.fp_log_ticks <- function(low, high, target) {
  if (high / low < 3) return(.fp_nice_ticks(low, high, target))
  decades <- 10^seq(floor(log10(low)) - 1L, ceiling(log10(high)) + 1L)
  ladders <- list(c(1, 2, 5), 1, c(1, 1.5, 2, 3, 5, 7), c(1, 1.25, 1.5, 2, 2.5, 3, 4, 5, 6, 8))
  best <- NULL
  best_waste <- Inf
  for (ladder in ladders) {
    candidates <- sort(unique(signif(as.vector(outer(ladder, decades)), 6)))
    lower <- max(candidates[candidates <= low * (1 + 1e-9)])
    upper <- min(candidates[candidates >= high * (1 - 1e-9)])
    ticks <- candidates[candidates >= lower & candidates <= upper]
    waste <- log(upper / lower) / log(high / low)
    if (length(ticks) >= 3L && length(ticks) <= target + 2L && waste < best_waste - 1e-9) {
      best <- ticks
      best_waste <- waste
    }
  }
  if (is.null(best)) best <- .fp_nice_ticks(low, high, target)
  best
}

.fp_choose_axis <- function(lowers, uppers, null_value, user_min, user_max, user_ticks, log_scale,
                            tick_target = 5L, padding = 0.04) {
  values <- c(null_value, lowers, uppers)
  if (log_scale && any(values <= 0)) stop("对数刻度要求无效值、估计值与 CI 均为正数", call. = FALSE)
  forward <- if (log_scale) log else identity
  inverse <- if (log_scale) exp else identity
  data_range <- range(values)
  auto_ticks <- is.null(user_ticks)
  ticks <- user_ticks
  if (auto_ticks) {
    tick_range <- c(
      if (is.null(user_min)) data_range[[1]] else user_min,
      if (is.null(user_max)) data_range[[2]] else user_max
    )
    ticks <- if (log_scale) .fp_log_ticks(tick_range[[1]], tick_range[[2]], tick_target) else {
      .fp_nice_ticks(tick_range[[1]], tick_range[[2]], tick_target)
    }
    ticks <- signif(ticks, 12)
  }
  if (log_scale && any(ticks <= 0)) stop("对数刻度的刻度值必须为正数", call. = FALSE)
  covered <- range(c(data_range, ticks))
  span <- diff(forward(covered))
  if (span <= 0) span <- 1
  axis_min <- if (is.null(user_min)) inverse(forward(covered[[1]]) - padding * span) else user_min
  axis_max <- if (is.null(user_max)) inverse(forward(covered[[2]]) + padding * span) else user_max
  tolerance <- 1e-9 * max(1, abs(c(axis_min, axis_max)))
  if (auto_ticks) ticks <- ticks[ticks >= axis_min - tolerance & ticks <= axis_max + tolerance]
  if (!is.finite(axis_min) || !is.finite(axis_max) || axis_min >= axis_max) stop("坐标轴范围无效", call. = FALSE)
  if (log_scale && axis_min <= 0) stop("对数刻度的坐标轴范围必须为正数", call. = FALSE)
  if (length(ticks) < 2L || min(ticks) < axis_min - tolerance || max(ticks) > axis_max + tolerance) {
    stop("坐标轴范围内至少需要两个刻度", call. = FALSE)
  }
  if (min(lowers) < axis_min - tolerance || max(uppers) > axis_max + tolerance ||
      null_value < axis_min - tolerance || null_value > axis_max + tolerance) {
    stop("坐标轴范围截断了 CI 或未包含无效值", call. = FALSE)
  }
  list(min = axis_min, max = axis_max, ticks = ticks, log = log_scale)
}

.fp_make_scale <- function(axis, start_mm, length_mm) {
  transform <- if (axis$log) log else identity
  low <- transform(axis$min)
  high <- transform(axis$max)
  function(value) start_mm + (transform(value) - low) / (high - low) * length_mm
}

.fp_prepare_output <- function(output, overwrite) {
  if (!is.character(output) || length(output) != 1L || tolower(tools::file_ext(output)) != "pdf") {
    stop("file 必须为以 .pdf 结尾的单个路径", call. = FALSE)
  }
  output <- file.path(normalizePath(dirname(output), mustWork = FALSE), basename(output))
  if (file.exists(output) && !isTRUE(overwrite)) {
    stop("输出文件已存在: ", output, "\n确认可以替换后再设置 overwrite = TRUE", call. = FALSE)
  }
  dir.create(dirname(output), recursive = TRUE, showWarnings = FALSE)
  list(output = output, temporary = tempfile(pattern = ".forest-", tmpdir = dirname(output), fileext = ".pdf"))
}

.fp_finalise_output <- function(temporary, output) {
  if (!file.rename(temporary, output)) {
    if (!file.copy(temporary, output, overwrite = TRUE)) stop("无法写入 ", output, call. = FALSE)
    unlink(temporary)
  }
}

.fp_poppler_available <- function() {
  nzchar(Sys.which("pdffonts")) && nzchar(Sys.which("pdftotext"))
}

.fp_verify_pdf_fonts <- function(path, family) {
  rows <- system2(Sys.which("pdffonts"), shQuote(path), stdout = TRUE, stderr = TRUE)
  rows <- rows[-seq_len(min(2L, length(rows)))]
  rows <- rows[nzchar(trimws(rows))]
  if (length(rows) == 0L) stop("PDF 中没有字体信息", call. = FALSE)
  pattern <- "^(\\S+)\\s+.*?\\s+(yes|no)\\s+(yes|no)\\s+(yes|no)\\s+\\d+\\s+\\d+\\s*$"
  parsed <- regmatches(rows, regexec(pattern, rows, perl = TRUE))
  if (any(lengths(parsed) != 5L)) stop("无法解析 pdffonts 输出:\n", paste(rows, collapse = "\n"), call. = FALSE)
  font_names <- sub("^[A-Z]{6}\\+", "", vapply(parsed, `[[`, character(1), 2L))
  embedded <- vapply(parsed, `[[`, character(1), 3L) == "yes"
  stem <- gsub("[^A-Za-z0-9]", "", family)
  foreign <- unique(font_names[!grepl(paste0("^", stem), font_names, ignore.case = TRUE)])
  if (length(foreign) > 0L) {
    stop("PDF 中出现非 ", family, " 字体: ", paste(foreign, collapse = ", "),
         "。可能是字体未安装而被替换，或标签含有该字体缺少的字形", call. = FALSE)
  }
  if (!all(embedded)) stop(family, " 未嵌入 PDF", call. = FALSE)
  unique(font_names)
}

.fp_verify_pdf_strokes <- function(path, expected_lwd) {
  bytes <- readBin(path, "raw", file.info(path)$size)
  starts <- grepRaw("stream\n", bytes, fixed = TRUE, all = TRUE)
  starts <- starts[starts > 3L & vapply(starts, function(i) rawToChar(bytes[(i - 3L):(i - 1L)]) != "end", logical(1))]
  ends <- grepRaw("endstream", bytes, fixed = TRUE, all = TRUE)
  widths <- numeric()
  for (start in starts) {
    end <- ends[ends > start][1]
    chunk <- bytes[(start + 7L):(end - 1L)]
    text <- tryCatch(rawToChar(memDecompress(chunk, type = "gzip")), error = function(error) "")
    operators <- regmatches(text, gregexpr("[0-9.]+ w\\b|\\bS\\b", text, perl = TRUE))[[1]]
    current <- NA_real_
    for (operator in operators) {
      if (operator == "S") {
        widths <- c(widths, current)
      } else {
        current <- as.numeric(sub(" w", "", operator, fixed = TRUE))
      }
    }
  }
  widths <- round(widths[!is.na(widths)], 4)
  expected <- sort(unique(round(expected_lwd * 0.75, 4)))
  found <- sort(unique(widths))
  if (!identical(found, expected)) {
    stop("PDF 中的线宽（", paste(found, collapse = ", "), " pt）与设定（", paste(expected, collapse = ", "), " pt）不一致",
         call. = FALSE)
  }
  table(widths)
}

.fp_verify_pdf_text <- function(path, labels, lancet_style) {
  extracted <- system2(Sys.which("pdftotext"), c("-enc", "UTF-8", "-raw", shQuote(path), "-"), stdout = TRUE, stderr = TRUE)
  extracted <- paste(extracted, collapse = "\n")
  Encoding(extracted) <- "UTF-8"
  compact <- gsub("\\s+", "", extracted, perl = TRUE)
  tokens <- unique(unlist(strsplit(labels, "\\s+", perl = TRUE)))
  tokens <- tokens[nzchar(tokens)]
  missing <- tokens[!vapply(tokens, grepl, logical(1), x = compact, fixed = TRUE)]
  if (length(missing) > 0L) {
    stop("PDF 文本提取无法还原所绘制的文本（编码或字形问题）: ",
         paste(dQuote(utils::head(missing, 8), FALSE), collapse = ", "), call. = FALSE)
  }
  if (lancet_style) {
    if (grepl("\\d\\.\\d", extracted, perl = TRUE)) stop("Lancet 模式的 PDF 中含有基线小数点", call. = FALSE)
    if (grepl("-", extracted, fixed = TRUE)) stop("Lancet 模式的 PDF 中含有 ASCII 连字符", call. = FALSE)
  }
  length(tokens)
}

# 写出 PDF 并执行全部校验；任一校验失败都不会留下输出文件
.fp_render <- function(file, overwrite, font_family, draw, expected_lwd, lancet_style) {
  out <- .fp_prepare_output(file, overwrite)
  on.exit(unlink(out$temporary), add = TRUE)
  geometry <- draw(out$temporary)
  strokes <- .fp_verify_pdf_strokes(out$temporary, expected_lwd)
  fonts <- NA_character_
  token_count <- NA_integer_
  if (.fp_poppler_available()) {
    fonts <- .fp_verify_pdf_fonts(out$temporary, font_family)
    token_count <- .fp_verify_pdf_text(out$temporary, .fp_state$labels, lancet_style)
  } else {
    message("未找到 poppler（pdffonts/pdftotext）：跳过嵌入字体与文本提取校验")
  }
  .fp_finalise_output(out$temporary, out$output)
  list(output = out$output, geometry = geometry, strokes = strokes, fonts = fonts, token_count = token_count)
}

# 自动刻度：依次尝试 5 到 2 个目标刻度，直到刻度标签不重叠
.fp_plan_with_axis <- function(lowers, uppers, null_value, user_min, user_max, user_ticks, log_scale,
                               tick_digits, lancet_style, plan_fun) {
  for (target in if (is.null(user_ticks)) 5:2 else 5L) {
    axis <- .fp_choose_axis(lowers, uppers, null_value, user_min, user_max, user_ticks, log_scale, target)
    tick_labels <- .fp_format_ticks(axis$ticks, tick_digits, axis$log, lancet_style)
    plan <- tryCatch(
      .fp_with_measuring_device(plan_fun(axis, tick_labels)),
      tick_overlap = function(condition) if (is.null(user_ticks) && target > 2L) NULL else stop(condition)
    )
    if (!is.null(plan)) return(list(axis = axis, tick_labels = tick_labels, plan = plan))
  }
}

.fp_strokes_text <- function(strokes) {
  paste(sprintf("%s pt x%d", names(strokes), strokes), collapse = ", ")
}

#' 打印森林图运行摘要
#'
#' @param x \code{plot_forest} 或 \code{plot_forest_landscape} 返回的对象
#' @param ... 未使用
#' @return 不可见地返回 x
#' @export
print.myepi_forest <- function(x, ...) {
  cat(paste0(names(x$summary), ": ", unlist(x$summary)), sep = "\n")
  invisible(x)
}
