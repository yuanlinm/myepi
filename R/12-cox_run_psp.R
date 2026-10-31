#' 惩罚样条（PSP）Cox 回归
#'
#' 采用 survival 的标准流程：\code{survival::coxph} 拟合含 \code{pspline(mainvar)} 的 Cox 模型；
#' 曲线按 survival 的 spline vignette 计算，即 \code{predict(type = "terms", se.fit = TRUE)}（与 \code{termplot()} 相同），
#' HR = exp(项)，CI = exp(项 ± z × se)。该项按 survival 默认中心化（在分析样本中的均值为 0），
#' 不设参照值，CI 为该项自身的不确定性。检验取自 \code{summary()}：线性部分与非线性部分的 Wald 检验（Gray 1992，非整数自由度），
#' 不报告总体关联检验。pspline 的节点在 Boundary.knots（观测范围）内等距放置，不能指定位置。
#'
#' covars 支持与 \code{cox_run} 相同的写法（交互、\code{strata()}、长数据等），但不允许含 mainvar 的项。
#'
#' @inheritParams cox_run
#' @param mainvar 连续暴露变量名（字符串）
#' @param df 样条的目标自由度，默认 4；\code{df = 0} 时按 AIC 自动选择。与 theta 二选一
#' @param theta 惩罚强度（0–1），与 df 二选一
#' @param nterm 样条区间数（基函数个数为 nterm + 2），默认 NULL 使用 pspline 默认值（2.5 × df）
#' @param range_probs 预测范围的分位数，如 \code{c(0.01, 0.99)}；默认 NULL 为观测范围 [min, max]
#' @param robust 方差估计：FALSE（默认）使用模型方差；TRUE 使用稳健方差（按 \code{cluster()} 或 idL 聚类）。
#'   曲线 CI 与检验均基于所选方差
#' @param ... 传递给 \code{survival::coxph} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 coxph 模型，
#'   \code{result} 为 500 点曲线数据（x、hr、ll、ul），\code{tests} 为线性与非线性检验，
#'   \code{spline} 为有效自由度、theta、基函数个数（n_basis）与指定的 nterm，\code{info} 为样本与设置信息
#' @export
cox_run_psp <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, idL = NULL,
                        df = 4, theta = NULL, nterm = NULL, range_probs = NULL,
                        robust = FALSE, conf_level = 0.95, ...) {
  if (!is.null(theta) && !missing(df)) stop("df 与 theta 只能二选一")
  if (is.null(theta)) {
    if (!is.numeric(df) || length(df) != 1L || is.na(df) || df < 0) stop("df 必须为非负数值（0 表示按 AIC 选择）")
  } else if (!is.numeric(theta) || length(theta) != 1L || is.na(theta) || theta <= 0 || theta >= 1) {
    stop("theta 必须为 (0, 1) 内的单个数值")
  }
  if (!is.null(nterm) && (!is.numeric(nterm) || length(nterm) != 1L || nterm < 3 || nterm != round(nterm))) {
    stop("nterm 必须为不小于 3 的整数")
  }
  if (!is.character(mainvar) || length(mainvar) != 1L) stop("mainvar 必须为单个变量名")

  args <- c(.bt(mainvar),
            if (is.null(theta)) paste0("df = ", df) else paste0("theta = ", theta),
            if (!is.null(nterm)) paste0("nterm = ", nterm))
  main_term <- sprintf("pspline(%s)", paste(args, collapse = ", "))
  setup <- .spline_setup(data, time1, time2, timediff, event, mainvar, covars, idL, robust, range_probs, conf_level,
                         main_term = main_term)
  prep <- setup$prep
  # model = TRUE：predict(newdata) 时使用保存的模型框，无需在公式环境中重新查找原始数据
  fit <- .fit_cox(prep, model = TRUE, ...)
  # survival 将 pspline 的 printfun 环境设为全局环境，未 library(survival) 时 summary() 找不到 coxph.wtest；
  # 改为 survival 命名空间，检验仍由 survival 原代码计算
  for (i in seq_along(fit$printfun)) {
    if (is.function(fit$printfun[[i]])) environment(fit$printfun[[i]]) <- asNamespace("survival")
  }

  # 曲线：predict(type = "terms") 与 termplot() 等价；mainvar 的样条项为第 1 项
  newdata <- prep$data[rep(1L, length(setup$grid)), , drop = FALSE]
  newdata[[mainvar]] <- setup$grid
  pr <- stats::predict(fit, newdata = newdata, type = "terms", terms = 1L, se.fit = TRUE)
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  eta <- as.vector(pr$fit)
  se <- as.vector(pr$se.fit)
  curve <- data.frame(x = setup$grid, hr = exp(eta), ll = exp(eta - z * se), ul = exp(eta + z * se))

  # 样条项位于公式第 1 项，summary 表的前两行即其线性与非线性检验（行名可能被 survival 截断，故按位置取）
  coefs <- summary(fit)$coefficients
  rows <- 1:2
  if (nrow(coefs) < 2L || is.na(coefs[1, "coef"]) || !is.na(coefs[2, "coef"])) {
    stop("无法从模型摘要中定位样条的线性与非线性检验")
  }
  tests <- data.frame(
    test = c("linear", "nonlinear"),
    method = "Wald (survival pspline, Gray 1992)",
    statistic = unname(coefs[rows, "Chisq"]),
    df = unname(coefs[rows, "DF"]),
    P = unname(coefs[rows, "p"]),
    stringsAsFactors = FALSE
  )
  spline <- list(df = unname(fit$df[1]), theta = fit$history[[1]]$theta, n_basis = length(fit$assign[[1]]),
                 nterm = if (is.null(nterm)) NA_real_ else nterm,
                 df_requested = if (is.null(theta)) df else NA_real_, theta_requested = if (is.null(theta)) NA_real_ else theta)
  if (is.null(theta) && df == 0) message(sprintf("df = 0：按 AIC 选择的有效自由度为 %.2f", spline$df))

  info <- .spline_info(prep, fit$n, fit$nevent, robust, conf_level, mainvar, "psp", c(
    sprintf("惩罚样条 Cox 回归: %s，有效自由度 %.2f，theta = %.3g，基函数 %d 个", mainvar, spline$df, spline$theta, spline$n_basis),
    sprintf("线性 P = %s；非线性 P = %s（Wald）",
            format.pval(tests$P[1], digits = 3, eps = 0.001), format.pval(tests$P[2], digits = 3, eps = 0.001)),
    "曲线: HR = exp(样条项)，按 survival 默认中心化（样条项在分析样本中的均值为 0），无参照值"
  ))
  invisible(.new_result(model = fit, result = curve, tests = tests, spline = spline, info = info))
}
