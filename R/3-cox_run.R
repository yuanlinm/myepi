#' 常规Cox回归分析函数，本函数支持基本的Cox回归分析和时间依赖（长数据）变量的Cox回归分析。
#'
#' @param data 数据框
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param event 结局变量名（字符串，0/1）
#' @param mainvar 关注变量名（字符串）
#' @param covars 协变量名（字符串向量，可为NULL）
 #' @param extra_vars 额外关注变量名（字符串向量，可为NULL）
#' @return 一个列表，包含模型对象和格式化结果表
#' @export
cox_run <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, extra_vars = NULL) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("请先安装 survival 包")
  # 检查参数
  # 支持协变量中直接写 strata()，只检查变量名（不检查strata/交互项的语法）
  extract_vars <- function(x) {
    if (is.null(x)) return(character(0))
    # 提取strata()和cluster()中的变量名
    strata_vars <- unlist(regmatches(x, gregexpr("strata\\(([^)]+)\\)", x)))
    strata_vars <- gsub("strata\\(([^)]+)\\)", "\\1", strata_vars)
    cluster_vars <- unlist(regmatches(x, gregexpr("cluster\\(([^)]+)\\)", x)))
    cluster_vars <- gsub("cluster\\(([^)]+)\\)", "\\1", cluster_vars)
    # 提取交互项、普通变量（去除strata和cluster）
    tmp <- gsub("strata\\(([^)]+)\\)", "", x)
    tmp <- gsub("cluster\\(([^)]+)\\)", "", tmp)
    other_vars <- unlist(strsplit(tmp, ":|\\*|/|\\^|\\+"))
    all_vars <- c(strata_vars, cluster_vars, other_vars)
    all_vars <- trimws(all_vars)
    all_vars[all_vars != ""]
  }
  if (!is.null(timediff)) {
    check_vars <- unique(c(timediff, event, mainvar, unlist(lapply(covars, extract_vars))))
    if (!all(check_vars %in% names(data))) stop("部分变量不在数据框中")
    surv_str <- paste0("survival::Surv(", timediff, ", ", event, ")")
  } else {
    check_vars <- unique(c(time1, time2, event, mainvar, unlist(lapply(covars, extract_vars))))
    if (!all(check_vars %in% names(data))) stop("部分变量不在数据框中")
    surv_str <- paste0("survival::Surv(", time1, ", ", time2, ", ", event, ")")
  }
  # 构建公式
  # 支持协变量中直接写交互项、strata()、cluster()等
  rhs <- c(mainvar, covars)
  rhs_str <- paste(rhs, collapse = "+")
  fml <- as.formula(paste0(surv_str, " ~ ", rhs_str))
  # 检查是否包含cluster()，如有则用robust标准误
  has_cluster <- any(grepl("cluster\\(", rhs_str, fixed = FALSE))
  if (has_cluster) {
    fit <- survival::coxph(fml, data = data, robust = TRUE)
  } else {
    fit <- survival::coxph(fml, data = data)
  }
  # 提取结果
  summ <- summary(fit)
  conf <- as.data.frame(summ$conf.int)
  res <- as.data.frame(summ$coefficients)
  # 计算HR及置信区间上下界
  res$HR <- exp(res$coef)
  res$HR_lower <- conf$`lower .95`
  res$HR_upper <- conf$`upper .95`
  res$P <- res$`Pr(>|z|)`
  res$beta <- res$coef
  res$se <- res$`se(coef)`
  res$varname <- rownames(res)
  # 只返回 mainvar 及 extra_vars 的结果，且交互项仅在extra_vars中指定时输出
  show_vars <- c(mainvar, extra_vars)
  # 只匹配主变量（及其因子水平），不自动输出交互项，交互项需在extra_vars中显式指定
  var_match <- unique(unlist(lapply(mainvar, function(v) grep(paste0("^", v, "$|^", v, "[A-Za-z0-9_]+$"), res$varname, value = TRUE))))
  # 若extra_vars中有交互项，补充匹配
  if (!is.null(extra_vars)) {
    var_match <- unique(c(var_match, extra_vars[extra_vars %in% res$varname]))
  }
  out <- res[res$varname %in% var_match, c("varname", "HR", "HR_lower", "HR_upper", "P", "beta", "se"), drop = FALSE]
  # 格式化输出
  out$HR <- sprintf("%.2f", as.numeric(out$HR))
  out$HR_lower <- sprintf("%.2f", as.numeric(out$HR_lower))
  out$HR_upper <- sprintf("%.2f", as.numeric(out$HR_upper))
  out$P <- sprintf("%.3f", as.numeric(out$P))
  out$beta <- sprintf("%.3f", as.numeric(out$beta))
  out$se <- sprintf("%.3f", as.numeric(out$se))
  rownames(out) <- NULL
  # 控制台输出美化：只显示主要变量和额外关注变量，列名美化
  if (isTRUE(getOption("cox_run.print", TRUE))) {
    print(out, row.names = FALSE)
  }
  invisible(list(model = fit, result = out))
}
