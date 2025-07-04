#' Cox亚组异质性检验（对接cox_run_sub结果）
#'
#' 对`cox_run_sub`/`cox_run_sub`等函数输出的亚组分析结果，按Subgroup列自动分组，调用metafor::rma进行异质性检验，输出Q统计量、p值、I2等。
#' @param df cox_run_sub等输出的数据框，需包含Subgroup、beta、se列，Subgroup格式为“亚组名: 水平”
#'   - Subgroup：字符型，格式如"sex: female"，冒号前为亚组名，后为水平。
#'   - beta：主效应log(HR)或log(OR)等，作为效应量。
#'   - se：beta的标准误。
#' @param method metafor::rma的method参数，默认"FE"（固定效应），可选"DL"、"REML"等
#' @param digits 保留小数位，默认2
#' @param ... 传递给metafor::rma的其他参数
#' @return 一个数据框，包含亚组、组数、Q统计量、p值、I2等异质性指标
#' @details
#'   - 自动识别Subgroup列（格式如"sex: female"），对每个亚组名分组，调用metafor::rma进行异质性检验。
#'   - 必须包含列：Subgroup（分组信息）、beta（效应量）、se（标准误）。
#'   - 若需自定义效应量或标准误，请确保列名与上述一致，或在调用前重命名。
#'   - 适用于cox_run_sub等标准输出。
#' @importFrom metafor rma
#' @export
cox_het <- function(df, method = "REML", digits = 3, ...) {
  if (!requireNamespace("metafor", quietly = TRUE)) stop("请先安装metafor包")
  if (!all(c("Subgroup", "beta", "se") %in% colnames(df))) stop("缺少Subgroup、beta、se列")
  # 提取亚组名
  df$._group <- sub(":.*$", "", df$Subgroup)
  groups <- unique(df$._group)
  out_list <- lapply(groups, function(g) {
    dat <- df[df$._group == g, ]
    # 强制转换为numeric，防止因字符型导致metafor报错
    dat$beta <- as.numeric(dat$beta)
    dat$se <- as.numeric(dat$se)
    if (nrow(dat) < 2) return(NULL)
    fit <- metafor::rma(yi = dat$beta, sei = dat$se, method = method, ...)
    data.frame(
      Subgroup = g,
      n = fit$k,
      I2 = round(100 * fit$I2, digits),
      tau2 = if (!is.null(fit$tau2)) round(fit$tau2, digits) else NA,
      Q = round(fit$QE, digits),
      Q_pvalue = signif(fit$QEp, digits),
      stringsAsFactors = FALSE
    )
  })
  out <- do.call(rbind, out_list)
  rownames(out) <- NULL
  print(out, row.names = FALSE)
  invisible(out)
}