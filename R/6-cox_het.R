#' Cox亚组异质性检验（对接cox_run_sub结果）
#'
#' 对 \code{cox_run_sub} 输出的亚组结果，按“亚组变量 × 关注变量 × 水平”分组，调用 \code{metafor::rma}
#' 检验各亚组估计值间的异质性（Cochran's Q、I2 等）。参照行及 se 缺失/非正的行自动排除。
#' 亚组差异的主要报告指标建议使用 \code{cox_run_sub} 的 \code{P_interaction}；本函数作为补充。
#'
#' @param x \code{cox_run_sub} 返回的结果对象，或其 \code{result} 数据框（plot_shape = FALSE）。
#'   需包含 Subgroup（形如 "sex: female"）、beta、se 列；varname、Interval、Reference 列存在时用于分组与排除参照行
#' @param method \code{metafor::rma} 的 method 参数，默认 "EE"（等效/固定效应，检验亚组间差异的标准设定）
#' @param ... 传递给 \code{metafor::rma} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为各组 rma 模型列表，
#'   \code{result} 为异质性检验结果表（k、Q、Q_df、Q_pvalue、I2、tau2），数值保存原始精度
#' @importFrom metafor rma
#' @export
cox_het <- function(x, method = "EE", ...) {
  df <- if (inherits(x, "myepi_result")) x$result else x
  if (!is.data.frame(df)) stop("x 必须为 cox_run_sub 的结果对象或数据框")
  if (!all(c("Subgroup", "beta", "se") %in% colnames(df))) stop("缺少Subgroup、beta、se列")
  if (!any(grepl(": ", df$Subgroup, fixed = TRUE))) stop("Subgroup 需为 \"亚组名: 水平\" 格式（请使用 plot_shape = FALSE 的结果）")

  if (!"varname" %in% names(df)) df$varname <- NA_character_
  # 仅分类变量（含参照行）按水平分组；数值变量各亚组的 Interval 为观测范围，不参与分组
  has_ref <- "Reference" %in% names(df)
  cat_vars <- if (has_ref && "Interval" %in% names(df)) unique(df$varname[df$Reference %in% TRUE]) else character(0)

  beta <- as.numeric(df$beta)
  se <- as.numeric(df$se)
  keep <- !is.na(beta) & !is.na(se) & se > 0
  if (has_ref) keep <- keep & !df$Reference %in% TRUE
  df <- df[keep, , drop = FALSE]
  if (nrow(df) == 0L) stop("没有可用于异质性检验的非参照行")

  group <- sub(": .*$", "", df$Subgroup)
  varname <- df$varname
  interval <- ifelse(varname %in% cat_vars, df$Interval, NA_character_)
  key <- paste(group, varname, interval, sep = "\r")
  keys <- unique(key)

  models <- list()
  rows <- list()
  for (kk in keys) {
    sel <- key == kk
    if (sum(sel) < 2L) next
    fit <- metafor::rma(yi = as.numeric(df$beta[sel]), sei = as.numeric(df$se[sel]), method = method, ...)
    first <- which(sel)[1]
    rows[[length(rows) + 1L]] <- data.frame(
      Subgroup = group[first], varname = varname[first], Interval = interval[first],
      k = fit$k, Q = fit$QE, Q_df = fit$k - fit$p, Q_pvalue = fit$QEp,
      I2 = fit$I2, tau2 = fit$tau2, stringsAsFactors = FALSE
    )
    models[[length(models) + 1L]] <- fit
  }
  if (length(rows) == 0L) stop("每组至少需要 2 个亚组估计值")
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  invisible(.new_result(model = models, result = out, info = list(
    method = method,
    header = sprintf("亚组异质性检验（metafor::rma, method = \"%s\"）", method)
  )))
}
