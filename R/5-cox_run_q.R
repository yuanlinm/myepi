#' 连续变量分位数Cox回归分析函数。
#'
#' @param data 数据框
#' @param mainvar 连续变量名（字符串）
#' @param q 分组定义：
#'   * 若为单个整数 n (>=2)，按 n 等份（分位数）将 `mainvar` 切分为 n 组（原有行为，Q1 为参考）。
#'   * 若为数值型向量（长度 >=2），则被视为用户自定义分组断点（自动排序去重），相邻两个断点形成一个区间；
#'     向量的最小值与最大值作为总体边界，不再自动扩展为 -Inf/Inf（若需全覆盖请自行在向量中放入极端值，例如 c(-9999, 1, 6, 8, 23, 9999)）。
#'     生成的组仍命名为 mainvar_Q1, mainvar_Q2, ...，其中 Q1 作为参考组。
#'   * 当为向量时，如果存在重复或非严格递增（去重后长度 <2）将报错；若数据中存在超出范围的观测亦报错。
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串，0/1）
#' @param covars 协变量名（字符串向量，可为NULL）
#' @param trend 是否进行趋势性检验（逻辑值，默认 FALSE）。
#' @param trend_method 趋势性检验方法："median" 使用各分组内原始连续变量的中位数构造连续变量；"ordinal" 使用 1,2,3,...,k 为分组得分。默认 "median"。
#' @param ... 传递给cox_run的其他参数
#'
#' @details 当 `trend = TRUE` 时，会在结果表中新增两列：
#' * `beta_trend` ：趋势性检验中连续变量的回归系数（log(HR)），仅在结果表第一行显示，其余行为 NA。
#' * `P_trend` ：对应的显著性 P 值，仅在第一行显示，其余行为 NA。
#' 若趋势性模型无法拟合（例如全部缺失或单一值），则两列返回 NA。
#'
#' @return 一个数据框，包含分组Cox回归结果及（可选）趋势性检验列；同时带有属性 `quantile_breaks` 记录切分断点。
#' @export
cox_run_q <- function(data, mainvar, q = 3, time1 = NULL, time2 = NULL, timediff = NULL, event, covars = NULL, trend = FALSE, trend_method = c("median", "ordinal"), ...) {
  trend_method <- match.arg(trend_method)
  if (!mainvar %in% names(data)) stop("mainvar 不在数据框中")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型")
  x <- data[[mainvar]]
  # 根据 q 类型（标量/向量）构建分组
  if (length(q) == 1) {
    # --- 分位数等频切分 ---
    if (is.na(q) || q < 2) stop("当 q 为单个整数时需 >= 2")
    q_int <- as.integer(q)
    qs <- quantile(x, probs = seq(0, 1, length.out = q_int + 1), na.rm = TRUE)
    # 避免首尾重复，首尾扩展为 -Inf / Inf 以确保覆盖（保持原有行为）
    qs[1] <- -Inf; qs[length(qs)] <- Inf
    group_index <- cut(x, breaks = qs, labels = FALSE, include.lowest = TRUE, right = TRUE)
    n_group <- q_int
    labels_vec <- paste0(mainvar, "_Q", seq_len(n_group))
  } else {
    # --- 自定义断点切分 ---
    if (!is.numeric(q)) stop("当 q 为向量时必须为数值型")
    qs_raw <- sort(unique(q))
    if (length(qs_raw) < 2) stop("自定义断点向量去重后长度需 >= 2")
    # 检查数据是否超出边界
    if (any(x < qs_raw[1] | x > qs_raw[length(qs_raw)], na.rm = TRUE)) {
      stop("数据中存在超出自定义断点范围的值；请在 q 中包含该范围的边界（或添加更大/更小的端点）")
    }
    # 检查是否存在零宽区间
    if (any(diff(qs_raw) <= 0)) stop("断点必须严格递增")
    qs <- qs_raw  # 此时不再扩展 -Inf/Inf，尊重用户边界
    group_index <- cut(x, breaks = qs, labels = FALSE, include.lowest = TRUE, right = TRUE)
    n_group <- length(qs) - 1
    labels_vec <- paste0(mainvar, "_Q", seq_len(n_group))
  }
  var_q <- factor(group_index, levels = seq_len(n_group), labels = labels_vec)
  data$var_q <- var_q
  # 以第一个分位组为参考
  var_q_name <- "var_q"
  # （all_levels / nonref_levels 已无后续直接使用，不再保留显式变量）
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
      HR = 1.00,
      HR_lower = 1.00,
      HR_upper = 1.00,
      P = 0.000,
      beta = 0.000,
      se = 0.000,
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
    # 无非参考组（可能所有值缺失或无法分组），仍返回空数据框并附加断点属性
    empty_df <- data.frame(
      varname = character(0),
      Interval = character(0),
      Case_Total = character(0),
      Incidence = character(0),
      HR = numeric(0),
      HR_lower = numeric(0),
      HR_upper = numeric(0),
      P = numeric(0),
      beta = numeric(0),
      se = numeric(0),
      stringsAsFactors = FALSE
    )
    attr(empty_df, "quantile_breaks") <- qs
    res$result <- empty_df
  }
  # ------------------ 趋势性检验 ------------------
  if (trend && !is.null(res$result)) {
    # 仅在成功获得分组模型后进行
    grp <- data$var_q
    if (all(is.na(grp))) {
      beta_tr <- NA_real_; p_tr <- NA_real_
    } else {
      # 构建连续得分变量
      if (trend_method == "median") {
        med_map <- tapply(x, grp, function(v) if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE))
        score <- as.numeric(med_map[as.character(grp)])
      } else { # ordinal
        # 按因子水平顺序赋值 1,2,...
        score <- as.numeric(grp)
      }
      # 若得分全部缺失或无变异，则无法拟合
      if (all(is.na(score)) || length(unique(na.omit(score))) < 2) {
        beta_tr <- NA_real_; p_tr <- NA_real_
      } else {
        # 构建生存对象
        if (!requireNamespace("survival", quietly = TRUE)) stop("请先安装 survival 包")
        if (!is.null(timediff) && timediff %in% names(data)) {
          surv_obj <- survival::Surv(data[[timediff]], data[[event]])
        } else if (!is.null(time1) && !is.null(time2) && time1 %in% names(data) && time2 %in% names(data)) {
          surv_obj <- survival::Surv(data[[time1]], data[[time2]], data[[event]])
        } else {
          stop("时间变量(time1/time2或timediff) 设置不完整，无法进行趋势检验")
        }
        # 构建公式：score + 原协变量
        if (is.null(covars)) {
          f_trend <- as.formula("surv_obj ~ score")
        } else {
          f_trend <- as.formula(paste0("surv_obj ~ score + ", paste(covars, collapse = "+")))
        }
        # 尝试拟合模型（加入 try 以防报错）
        fit_trend <- try(survival::coxph(f_trend, data = data.frame(data, score = score)), silent = TRUE)
        if (inherits(fit_trend, "try-error")) {
          beta_tr <- NA_real_; p_tr <- NA_real_
        } else {
          summ_tr <- summary(fit_trend)
            # 提取 score 系数
          if ("score" %in% rownames(summ_tr$coefficients)) {
            beta_tr <- as.numeric(summ_tr$coefficients["score", "coef"])
            p_tr <- as.numeric(summ_tr$coefficients["score", "Pr(>|z|)"])
          } else {
            beta_tr <- NA_real_; p_tr <- NA_real_
          }
        }
      }
    }
    # 在结果表第一行填入
    res$result$beta_trend <- NA_real_
    res$result$P_trend <- NA_real_
    if (nrow(res$result) > 0) {
      res$result$beta_trend[1] <- if (!is.na(beta_tr)) round(beta_tr, 3) else NA_real_
      res$result$P_trend[1] <- if (!is.na(p_tr)) round(p_tr, 3) else NA_real_
    }
  }
  if (!is.null(res$result)) print(res$result, row.names = FALSE)
  return(res$result)
}
