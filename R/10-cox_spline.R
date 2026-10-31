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
