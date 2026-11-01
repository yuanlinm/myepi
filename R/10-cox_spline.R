# 样条 Cox 回归共用：输入校验、分析样本、分位数计算单位与 500 点预测网格

.SPLINE_N_POINTS <- 500L

.spline_setup <- function(data, time1, time2, timediff, event, mainvar, covars, idL, robust,
                          range_probs, conf_level, main_term = NULL) {
  .validate_conf_level(conf_level)
  .validate_robust(robust)
  if (!is.data.frame(data)) stop("data 必须为数据框")
  if (!is.character(mainvar) || length(mainvar) != 1L || !mainvar %in% names(data)) stop("mainvar 必须为数据框中的单个变量名")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型连续变量")
  # 非线性分析中不允许 mainvar 出现在协变量项内（交互、分层、变换等）
  involves <- vapply(covars, function(term) mainvar %in% all.vars(str2lang(.term_label(term, data))),
                     logical(1), USE.NAMES = FALSE)
  if (any(involves)) stop("covars 中不允许含 mainvar 的项（交互、分层或变换）: ", paste(covars[involves], collapse = ", "))
  if (!is.null(range_probs)) {
    if (!is.numeric(range_probs) || length(range_probs) != 2L || anyNA(range_probs) ||
        range_probs[1] < 0 || range_probs[2] > 1 || range_probs[1] >= range_probs[2]) {
      stop("range_probs 必须为 [0, 1] 内两个递增的概率，如 c(0.01, 0.99)")
    }
  }

  prep <- .prep_cox(data, time1, time2, timediff, event, mainvar, covars, idL, robust, main_term = main_term)
  x <- prep$data[[mainvar]]
  # 分位数（节点、参照值、预测范围）的计算单位：指定 idL 时为去重后的“个体 × 取值”
  basis <- if (is.null(idL)) x else x[!duplicated(data.frame(prep$data[[idL]], x))]
  lim <- if (is.null(range_probs)) range(x) else stats::quantile(basis, range_probs, names = FALSE)
  if (lim[1] >= lim[2]) stop(sprintf("%s 在预测范围内只有一个取值", mainvar))
  grid <- seq(lim[1], lim[2], length.out = .SPLINE_N_POINTS)
  message(sprintf("预测网格: %s 在 [%s, %s] 内等距 %d 点%s", mainvar, format(lim[1], digits = 4), format(lim[2], digits = 4),
                  .SPLINE_N_POINTS, if (is.null(range_probs)) "（观测范围）" else
                    sprintf("（%g%%–%g%% 分位数）", 100 * range_probs[1], 100 * range_probs[2])))
  list(prep = prep, basis = basis, grid = grid)
}

.spline_info <- function(prep, n, nevent, robust, conf_level, mainvar, method, header) {
  list(
    mainvar = mainvar, method = method, n_input = prep$n_input, n_dropped = prep$n_drop, n = n, nevent = nevent,
    variance = if (isTRUE(robust)) "robust" else "model", conf_level = conf_level,
    header = c(header,
               sprintf("分析样本: %d 行，事件 %d（剔除缺失 %d 行）", n, nevent, prep$n_drop),
               sprintf("方差估计: %s；置信水平: %g%%", if (isTRUE(robust)) "稳健（三明治）方差" else "模型方差", 100 * conf_level))
  )
}

# ---- RCS 共用：节点、参照值、rms::anova 检验 ----

# 节点：knots（绝对值）、knot_probs（分位数）或 nk（Harrell 默认分位数，Hmisc::rcspline.eval）三选一
.rcs_knots <- function(basis, nk, knots, knot_probs, mainvar) {
  if (!is.null(knots) && !is.null(knot_probs)) stop("knots 与 knot_probs 只能二选一")
  if (!is.null(knots)) {
    if (!is.numeric(knots) || length(knots) < 3L || anyNA(knots) || anyDuplicated(knots)) stop("knots 至少需要 3 个互不相同的数值")
    knots <- sort(knots)
    knot_rule <- "用户指定（绝对值）"
  } else if (!is.null(knot_probs)) {
    if (!is.numeric(knot_probs) || length(knot_probs) < 3L || anyNA(knot_probs) || any(knot_probs <= 0 | knot_probs >= 1)) {
      stop("knot_probs 至少需要 3 个 (0, 1) 内的概率")
    }
    knot_probs <- sort(knot_probs)
    knots <- stats::quantile(basis, knot_probs, names = FALSE)
    if (anyDuplicated(knots)) stop("按 knot_probs 计算的节点存在重复值，请调整分位数或改用 knots")
    knot_rule <- sprintf("用户指定分位数 %s", paste(knot_probs, collapse = ", "))
  } else {
    if (!is.numeric(nk) || length(nk) != 1L || !nk %in% 3:7) stop("nk 必须为 3 到 7 的整数")
    knots <- Hmisc::rcspline.eval(basis, nk = nk, knots.only = TRUE)
    if (length(knots) < nk) warning(sprintf("%s 取值重复较多，实际仅得到 %d 个节点", mainvar, length(knots)), call. = FALSE)
    knot_rule <- "Harrell 默认分位数"
  }
  if (length(knots) < 3L) stop("至少需要 3 个节点")
  message(sprintf("RCS 节点（k = %d，%s）: %s", length(knots), knot_rule, paste(format(knots, digits = 4), collapse = ", ")))
  knots
}

# 参照值：默认中位数，须在观测范围内
.rcs_ref <- function(ref, basis, x, mainvar, measure) {
  if (is.null(ref)) ref <- stats::median(basis)
  if (!is.numeric(ref) || length(ref) != 1L || !is.finite(ref) || ref < min(x) || ref > max(x)) {
    stop("ref 必须为 mainvar 观测范围内的单个数值")
  }
  message(sprintf("参照值（%s = %s）: %s = %s", measure, if (measure == "beta") "0" else "1", mainvar, format(ref, digits = 4)))
  ref
}

# rms::anova 中 mainvar 的总体与非线性检验（Wald χ²；ols 为 F）
.rms_tests <- function(fit, mainvar) {
  a <- stats::anova(fit)
  row_overall <- match(mainvar, rownames(a))
  row_nonlinear <- row_overall + 1L
  if (is.na(row_overall) || trimws(rownames(a)[row_nonlinear]) != "Nonlinear") stop("无法从 anova 结果中定位 mainvar 的检验")
  rows <- c(row_overall, row_nonlinear)
  is_f <- "F" %in% colnames(a)
  data.frame(
    test = c("overall", "nonlinear"),
    method = if (is_f) sprintf("F (rms::anova), 分母自由度 %d", as.integer(a["ERROR", "d.f."])) else "Wald (rms::anova)",
    statistic = unname(a[rows, if (is_f) "F" else "Chi-Square"]),
    df = unname(a[rows, "d.f."]),
    P = unname(a[rows, "P"]),
    stringsAsFactors = FALSE
  )
}
