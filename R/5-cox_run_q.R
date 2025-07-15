#' 连续变量分位数Cox回归分析函数。
#'
#' @param data 数据框
#' @param mainvar 连续变量名（字符串）
#' @param q 分位数组数（整数，默认3）
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串，0/1）
#' @param covars 协变量名（字符串向量，可为NULL）
#' @param ... 传递给cox_run的其他参数
#' @return 一个列表，包含模型对象和分位数变量分析结果表
#' @export
cox_run_q <- function(data, mainvar, q = 3, time1 = NULL, time2 = NULL, timediff = NULL, event, covars = NULL, ...) {
  if (!mainvar %in% names(data)) stop("mainvar 不在数据框中")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型")
  # 构建分位数变量
  qs <- quantile(data[[mainvar]], probs = seq(0, 1, length.out = q + 1), na.rm = TRUE)
  # 避免重复分位点
  qs[1] <- -Inf; qs[length(qs)] <- Inf
  var_q <- cut(data[[mainvar]], breaks = qs, labels = FALSE, include.lowest = TRUE, right = TRUE)
  var_q <- factor(var_q, levels = 1:q, labels = paste0(mainvar, "_Q", 1:q))
  data$var_q <- var_q
  # 以第一个分位组为参考
  var_q_name <- "var_q"
  all_levels <- levels(var_q)
  nonref_levels <- all_levels[-1]
  # 拟合模型
  # 临时关闭cox_run打印
  old_opt <- getOption("cox_run.print", TRUE)
  options(cox_run.print = FALSE)
  on.exit(options(cox_run.print = old_opt), add = TRUE)
  res <- cox_run(
    data,
    time1 = time1, time2 = time2, timediff = timediff, event = event,
    mainvar = var_q_name,
    covars = covars,
    extra_vars = NULL,
    ...
  )
  # 直接从模型对象提取分位数变量的非参考组结果
  summ <- summary(res$model)
  coef_df <- as.data.frame(summ$coefficients)
  # 只保留分位数变量的非参考组（以 var_q 开头且包含 _Q2 ~ _Qq）
  pattern <- paste0("^", var_q_name, ".*_Q[2-9][0-9]*$")
  keep_idx <- grepl(pattern, rownames(coef_df))
  if (any(keep_idx)) {
    sub <- coef_df[keep_idx, , drop = FALSE]
    # 只保留模型中真实存在的分位数变量名
    sub_varnames <- rownames(sub)
    # 先计算Q1参考组信息
    ref_label <- paste0("var_q", mainvar, "_Q1")
    idx_ref <- which(var_q == paste0(mainvar, "_Q1"))
    n_sub_ref <- sum(!is.na(var_q) & var_q == paste0(mainvar, "_Q1"))
    n_event_ref <- sum(data[[event]][idx_ref] == 1, na.rm = TRUE)
    min_ref <- min(data[[mainvar]][idx_ref], na.rm = TRUE)
    max_ref <- max(data[[mainvar]][idx_ref], na.rm = TRUE)
    interval_ref <- if (is.finite(min_ref) && is.finite(max_ref)) sprintf("%.2f-%.2f", min_ref, max_ref) else NA
    if (!is.null(timediff) && timediff %in% names(data)) {
      total_py_ref <- sum(data[[timediff]][idx_ref], na.rm = TRUE) / 365.25
    } else if (!is.null(time1) && !is.null(time2) && time1 %in% names(data) && time2 %in% names(data)) {
      total_py_ref <- sum(data[[time2]][idx_ref] - data[[time1]][idx_ref], na.rm = TRUE) / 365.25
    } else {
      total_py_ref <- NA
    }
    case_total_ref <- sprintf("%d/%d", n_event_ref, n_sub_ref)
    incidence_ref <- if (!is.na(total_py_ref) && total_py_ref > 0) sprintf("%.2f", n_event_ref / total_py_ref * 1e5) else NA
    ref_row <- data.frame(
      varname = ref_label,
      Interval = interval_ref,
      Case_Total = case_total_ref,
      Incidence = incidence_ref,
      HR = "1.00",
      HR_lower = "1.00",
      HR_upper = "1.00",
      P = "0.000",
      beta = "0.000",
      se = "0.000",
      stringsAsFactors = FALSE
    )
    # 其他分位组
    out_info <- lapply(sub_varnames, function(group_label) {
      idx <- which(var_q == gsub("^var_q", "", group_label))
      n_sub <- sum(!is.na(var_q) & var_q == gsub("^var_q", "", group_label))
      n_event <- sum(data[[event]][idx] == 1, na.rm = TRUE)
      minv <- min(data[[mainvar]][idx], na.rm = TRUE)
      maxv <- max(data[[mainvar]][idx], na.rm = TRUE)
      interval <- if (is.finite(minv) && is.finite(maxv)) sprintf("%.2f-%.2f", minv, maxv) else NA
      if (!is.null(timediff) && timediff %in% names(data)) {
        total_py <- sum(data[[timediff]][idx], na.rm = TRUE) / 365.25
      } else if (!is.null(time1) && !is.null(time2) && time1 %in% names(data) && time2 %in% names(data)) {
        total_py <- sum(data[[time2]][idx] - data[[time1]][idx], na.rm = TRUE) / 365.25
      } else {
        total_py <- NA
      }
      case_total <- sprintf("%d/%d", n_event, n_sub)
      incidence <- if (!is.na(total_py) && total_py > 0) sprintf("%.2f", n_event / total_py * 1e5) else NA
      data.frame(
        varname = group_label,
        Interval = interval,
        Case_Total = case_total,
        Incidence = incidence,
        stringsAsFactors = FALSE
      )
    })
    out_info <- do.call(rbind, out_info)
    sub_out <- data.frame(
      varname = rownames(sub),
      HR = as.numeric(sprintf("%.2f", exp(sub$coef))),
      HR_lower = as.numeric(sprintf("%.2f", exp(sub$coef - 1.96 * sub$`se(coef)`))),
      HR_upper = as.numeric(sprintf("%.2f", exp(sub$coef + 1.96 * sub$`se(coef)`))),
      P = as.numeric(sprintf("%.3f", sub$`Pr(>|z|)`)),
      beta = as.numeric(sprintf("%.3f", sub$coef)),
      se = as.numeric(sprintf("%.3f", sub$`se(coef)`)),
      row.names = NULL,
      stringsAsFactors = FALSE
    )
    out_final <- merge(out_info, sub_out, by = "varname", sort = FALSE)
    out_final <- rbind(ref_row, out_final)
    attr(out_final, "quantile_breaks") <- qs
    res$result <- out_final
  } else {
    res$result <- NULL
  }
  if (!is.null(res$result)) print(res$result, row.names = FALSE)
  return(res$result)
}
