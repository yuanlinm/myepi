#' 连续变量分位数Cox回归分析函数。
#'
#' 分位点在分析样本（完整病例）中计算；指定 idL 时按“个体 × 取值”去重后计算，避免随访区间多的个体权重过大。
#' 分位点存在重复值（离散变量常见）时自动合并并给出警告。运行时以 message 报告各组样本量；
#' 长格式数据中同一个体的不同记录可落入不同组，因此各组个体数之和可大于总个体数。
#'
#' @param data 数据框
#' @param mainvar 连续变量名（字符串）
#' @param q 分组定义：
#'   * 若为单个整数 n (>=2)，按 n 等份（分位数）将 `mainvar` 切分为 n 组（Q1 为参考）。
#'   * 若为数值型向量（长度 >=2），则被视为用户自定义分组断点（自动排序去重），相邻两个断点形成一个区间；向量的最小值与最大值作为总体边界，不自动扩展为 -Inf/Inf。
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串）。编码遵循 \code{survival::Surv}
#' @param covars 协变量（字符串向量，可为NULL），写法同 \code{cox_run}
#' @param idL 长格式数据的个体标识变量名（字符串）。指定后，各分组病例数和总人数按该变量去重，人时仍按各观察区间累加。
#' @param t_unit 随访时间单位：d=天（默认），y=年，m=月。用于将随访时长统一换算为人年后计算发病率。
#' @param incidence_scale 发病率缩放倍数，默认 1e5（即每10万人年）。
#' @param trend 是否进行趋势性检验（逻辑值，默认 FALSE）。
#' @param trend_method 趋势性检验方法："median" 使用各分组内原始连续变量的中位数构造连续变量；"ordinal" 使用 1,2,3,...,k 为分组得分。默认 "median"。
#' @param robust 方差估计，同 \code{cox_run}；趋势检验使用相同的方差设置
#' @param conf_level 置信水平，默认 0.95
#' @param ... 传递给 \code{survival::coxph} 的其他参数
#'
#' @details 当 `trend = TRUE` 时，会在结果表中新增两列：
#' * `beta_trend`：趋势性检验中连续变量的回归系数（log(HR)），仅在结果表第一行显示。
#' * `P_trend`：对应的显著性 P 值，仅在第一行显示。
#' 完整的趋势检验结果另见返回对象的 \code{trend} 元素。
#'
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model$main} 为分组模型，\code{model$trend} 为趋势模型，
#'   \code{result} 为结果表（Interval 为区间定义，带属性 \code{quantile_breaks}），\code{breaks} 为实际断点，
#'   \code{trend} 为趋势检验结果。
#' @export
cox_run_q <- function(data, mainvar, q = 3, time1 = NULL, time2 = NULL, timediff = NULL, event, covars = NULL, trend = FALSE, trend_method = c("median", "ordinal"), idL = NULL,
                      t_unit = c("d", "y", "m"), incidence_scale = 1e5, robust = NULL, conf_level = 0.95, ...) {
  trend_method <- match.arg(trend_method)
  t_unit <- match.arg(t_unit)
  .validate_conf_level(conf_level)
  if (!is.character(mainvar) || length(mainvar) != 1L || !mainvar %in% names(data)) stop("mainvar 不在数据框中")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型")

  prep0 <- .prep_cox(data, time1, time2, timediff, event, mainvar, covars, idL, robust)
  ana <- prep0$data
  x <- ana[[mainvar]]
  # 分位点与组内中位数的计算单位：指定 idL 时为去重后的“个体 × 取值”
  basis <- if (is.null(idL)) seq_along(x) else which(!duplicated(data.frame(ana[[idL]], x)))

  if (length(q) == 1L) {
    if (!is.numeric(q) || is.na(q) || q < 2 || q != round(q)) stop("当 q 为单个整数时需 >= 2")
    breaks <- stats::quantile(x[basis], probs = seq(0, 1, length.out = q + 1), names = FALSE)
    if (anyDuplicated(breaks)) {
      breaks <- unique(breaks)
      warning(sprintf("分位点存在重复值（%s 取值离散），合并后实际分为 %d 组", mainvar, length(breaks) - 1L), call. = FALSE)
    }
  } else {
    if (!is.numeric(q)) stop("当 q 为向量时必须为数值型")
    breaks <- sort(unique(q))
    if (any(x < breaks[1] | x > breaks[length(breaks)])) {
      stop("数据中存在超出自定义断点范围的值；请在 q 中包含该范围的边界（或添加更大/更小的端点）")
    }
  }
  if (length(breaks) < 2L) stop(sprintf("%s 只有一个取值，无法分组", mainvar))

  intervals <- levels(cut(x, breaks = breaks, include.lowest = TRUE, right = TRUE, dig.lab = 4))
  k <- length(intervals)
  grp <- factor(cut(x, breaks = breaks, labels = FALSE, include.lowest = TRUE, right = TRUE),
                levels = seq_len(k), labels = paste0("Q", seq_len(k)))
  gname <- make.unique(c(names(ana), paste0(mainvar, "_q")))[ncol(ana) + 1L]
  ana[[gname]] <- grp

  sizes <- table(grp)
  size_txt <- if (is.null(idL)) sprintf("%s %s: %d", names(sizes), intervals, as.integer(sizes)) else {
    n_id <- vapply(levels(grp), function(l) length(unique(ana[[idL]][grp == l])), integer(1))
    sprintf("%s %s: %d 行 / %d 人", names(sizes), intervals, as.integer(sizes), n_id)
  }
  message(sprintf("%s 分为 %d 组，各组样本量：", mainvar, k), paste(size_txt, collapse = "；"))
  empty <- names(sizes)[sizes == 0]
  if (length(empty) > 0) warning("以下分组无观测: ", paste(empty, collapse = ", "), call. = FALSE)

  prep <- .prep_cox(ana, time1, time2, timediff, event, gname, covars, idL, robust, quiet = TRUE)
  fit <- .fit_cox(prep, ...)
  vc <- .cox_vcov(fit, robust)
  res <- .tidy_cox(prep, fit, vc, gname, NULL, t_unit, incidence_scale, conf_level)
  res$varname <- paste0(mainvar, "_", res$Interval)
  res$Interval <- intervals[match(res$Interval, levels(grp))]

  fit_trend <- NULL
  trend_tab <- NULL
  if (isTRUE(trend)) {
    score <- if (trend_method == "median") {
      med <- tapply(x[basis], grp[basis], stats::median)
      as.numeric(med[as.integer(grp)])
    } else {
      as.integer(grp)
    }
    sname <- make.unique(c(names(ana), paste0(mainvar, "_trend")))[ncol(ana) + 1L]
    ana[[sname]] <- score
    if (length(unique(score)) < 2L) {
      warning("趋势检验得分只有一个取值，无法拟合趋势模型", call. = FALSE)
      trend_tab <- data.frame(method = trend_method, beta = NA_real_, se = NA_real_, P = NA_real_)
    } else {
      prep_t <- .prep_cox(ana, time1, time2, timediff, event, sname, covars, idL, robust, quiet = TRUE)
      fit_trend <- .fit_cox(prep_t, ...)
      wt <- .wald_table(fit_trend, .cox_vcov(fit_trend, robust), conf_level)
      wt <- wt[match(sname, wt$coef_name), ]
      trend_tab <- data.frame(method = trend_method, beta = wt$beta, se = wt$se, P = wt$P)
    }
    res$beta_trend <- c(trend_tab$beta, rep(NA_real_, nrow(res) - 1L))
    res$P_trend <- c(trend_tab$P, rep(NA_real_, nrow(res) - 1L))
  }
  attr(res, "quantile_breaks") <- breaks

  info <- .cox_info(prep, fit, vc, conf_level, t_unit, incidence_scale)
  info$n_dropped <- prep0$n_drop
  info$mainvar <- mainvar
  info$header <- c(
    sprintf("分组 Cox 回归: %s 分为 %d 组；断点 %s", mainvar, k, paste(format(breaks, digits = 4, trim = TRUE), collapse = ", ")),
    sprintf("分析样本: %d 行，事件 %d（剔除缺失 %d 行）", fit$n, fit$nevent, prep0$n_drop),
    info$header[2],
    if (!is.null(trend_tab)) sprintf("趋势检验（%s 得分）: P = %s", trend_method, format.pval(trend_tab$P, digits = 3, eps = 0.001))
  )
  invisible(.new_result(model = list(main = fit, trend = fit_trend), result = res,
                        breaks = breaks, trend = trend_tab, info = info))
}
