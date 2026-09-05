#' 连续变量分位数Cox回归分析函数。
#'
#' @param data 数据框
#' @param mainvar 连续变量名（字符串）
#' @param q 分组定义：
#'   * 若为单个整数 n (>=2)，按 n 等份（分位数）将 `mainvar` 切分为 n 组（Q1 为参考）。
#'   * 若为数值型向量（长度 >=2），则被视为用户自定义分组断点（自动排序去重），相邻两个断点形成一个区间；向量的最小值与最大值作为总体边界，不自动扩展为 -Inf/Inf。
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串，0/1）
#' @param covars 协变量名（字符串向量，可为NULL）
#' @param idL 长格式数据的个体标识变量名（字符串）。指定后，各分组病例数和总人数按该变量去重，人时仍按各观察区间累加。
#' @param trend 是否进行趋势性检验（逻辑值，默认 FALSE）。
#' @param trend_method 趋势性检验方法："median" 使用各分组内原始连续变量的中位数构造连续变量；"ordinal" 使用 1,2,3,...,k 为分组得分。默认 "median"。
#' @param ... 传递给cox_run的其他参数
#'
#' @details 当 `trend = TRUE` 时，会在结果表中新增两列：
#' * `beta_trend`：趋势性检验中连续变量的回归系数（log(HR)），仅在结果表第一行显示。
#' * `P_trend`：对应的显著性 P 值，仅在第一行显示。
#' 若趋势性模型无法拟合（例如全部缺失或单一值），则两列返回 NA。
#'
#' @return 一个数据框，包含分组Cox回归结果及（可选）趋势性检验列；同时带有属性 `quantile_breaks` 记录切分断点。
#' @export
cox_run_q <- function(data, mainvar, q = 3, time1 = NULL, time2 = NULL, timediff = NULL, event, covars = NULL, trend = FALSE, trend_method = c("median", "ordinal"), idL = NULL, ...) {
  trend_method <- match.arg(trend_method)
  if (!mainvar %in% names(data)) stop("mainvar 不在数据框中")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型")
  .validate_idL(data, idL)
  x <- data[[mainvar]]

  if (length(q) == 1) {
    if (is.na(q) || q < 2) stop("当 q 为单个整数时需 >= 2")
    q_int <- as.integer(q)
    qs <- quantile(x, probs = seq(0, 1, length.out = q_int + 1), na.rm = TRUE)
    qs[1] <- -Inf
    qs[length(qs)] <- Inf
    group_index <- cut(x, breaks = qs, labels = FALSE, include.lowest = TRUE, right = TRUE)
    n_group <- q_int
  } else {
    if (!is.numeric(q)) stop("当 q 为向量时必须为数值型")
    qs <- sort(unique(q))
    if (length(qs) < 2) stop("自定义断点向量去重后长度需 >= 2")
    if (any(x < qs[1] | x > qs[length(qs)], na.rm = TRUE)) {
      stop("数据中存在超出自定义断点范围的值；请在 q 中包含该范围的边界（或添加更大/更小的端点）")
    }
    if (any(diff(qs) <= 0)) stop("断点必须严格递增")
    group_index <- cut(x, breaks = qs, labels = FALSE, include.lowest = TRUE, right = TRUE)
    n_group <- length(qs) - 1
  }

  labels_vec <- paste0(mainvar, "_Q", seq_len(n_group))
  data$var_q <- factor(group_index, levels = seq_len(n_group), labels = labels_vec)
  var_q <- data$var_q
  old_opt <- getOption("cox_run.print", TRUE)
  options(cox_run.print = FALSE)
  on.exit(options(cox_run.print = old_opt), add = TRUE)
  res <- cox_run(
    data,
    time1 = time1,
    time2 = time2,
    timediff = timediff,
    event = event,
    mainvar = "var_q",
    covars = covars,
    extra_vars = NULL,
    idL = idL,
    ...
  )

  summ <- summary(res$model)
  coef_df <- as.data.frame(summ$coefficients)
  keep_idx <- grepl("^var_q.*_Q[2-9][0-9]*$", rownames(coef_df))
  if (any(keep_idx)) {
    sub <- coef_df[keep_idx, , drop = FALSE]
    sub_varnames <- rownames(sub)
    total_time <- .followup_time(data, time1, time2, timediff)

    group_info <- function(group_label, varname) {
      rows <- !is.na(var_q) & var_q == group_label
      counts <- .count_cases_total(data, event, idL, rows)
      values <- data[[mainvar]][rows]
      interval <- if (length(values) > 0) {
        sprintf("%.2f-%.2f", min(values, na.rm = TRUE), max(values, na.rm = TRUE))
      } else {
        NA_character_
      }
      total_py <- sum(total_time[rows], na.rm = TRUE) / 365.25
      data.frame(
        varname = varname,
        Interval = interval,
        Case_Total = sprintf("%d/%d", counts$cases, counts$total),
        Incidence = if (total_py > 0) sprintf("%.2f", counts$cases / total_py * 1e5) else NA_character_,
        stringsAsFactors = FALSE
      )
    }

    ref_row <- cbind(
      group_info(labels_vec[1], paste0("var_q", labels_vec[1])),
      HR = 1.00,
      HR_lower = 1.00,
      HR_upper = 1.00,
      P = 0.000,
      beta = 0.000,
      se = 0.000
    )
    out_info <- do.call(rbind, lapply(sub_varnames, function(varname) {
      group_info(base::sub("^var_q", "", varname), varname)
    }))
    sub_out <- data.frame(
      varname = sub_varnames,
      HR = as.numeric(sprintf("%.2f", exp(sub$coef))),
      HR_lower = as.numeric(sprintf("%.2f", exp(sub$coef - 1.96 * sub$`se(coef)`))),
      HR_upper = as.numeric(sprintf("%.2f", exp(sub$coef + 1.96 * sub$`se(coef)`))),
      P = as.numeric(sprintf("%.3f", sub$`Pr(>|z|)`)),
      beta = as.numeric(sprintf("%.3f", sub$coef)),
      se = as.numeric(sprintf("%.3f", sub$`se(coef)`)),
      stringsAsFactors = FALSE
    )
    out_final <- merge(out_info, sub_out, by = "varname", sort = FALSE)
    out_final <- rbind(ref_row, out_final)
    attr(out_final, "quantile_breaks") <- qs
    res$result <- out_final
  } else {
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

  if (trend && !is.null(res$result)) {
    grp <- data$var_q
    if (all(is.na(grp))) {
      beta_tr <- NA_real_
      p_tr <- NA_real_
    } else {
      score <- if (trend_method == "median") {
        med_map <- tapply(x, grp, function(v) if (all(is.na(v))) NA_real_ else stats::median(v, na.rm = TRUE))
        as.numeric(med_map[as.character(grp)])
      } else {
        as.numeric(grp)
      }

      if (all(is.na(score)) || length(unique(na.omit(score))) < 2) {
        beta_tr <- NA_real_
        p_tr <- NA_real_
      } else {
        surv_obj <- if (!is.null(timediff) && timediff %in% names(data)) {
          survival::Surv(data[[timediff]], data[[event]])
        } else if (!is.null(time1) && !is.null(time2) && time1 %in% names(data) && time2 %in% names(data)) {
          survival::Surv(data[[time1]], data[[time2]], data[[event]])
        } else {
          stop("时间变量(time1/time2或timediff) 设置不完整，无法进行趋势检验")
        }
        f_trend <- if (is.null(covars)) {
          as.formula("surv_obj ~ score")
        } else {
          as.formula(paste0("surv_obj ~ score + ", paste(covars, collapse = "+")))
        }
        fit_trend <- try(survival::coxph(f_trend, data = data.frame(data, score = score)), silent = TRUE)
        if (inherits(fit_trend, "try-error")) {
          beta_tr <- NA_real_
          p_tr <- NA_real_
        } else {
          summ_tr <- summary(fit_trend)
          if ("score" %in% rownames(summ_tr$coefficients)) {
            beta_tr <- as.numeric(summ_tr$coefficients["score", "coef"])
            p_tr <- as.numeric(summ_tr$coefficients["score", "Pr(>|z|)"])
          } else {
            beta_tr <- NA_real_
            p_tr <- NA_real_
          }
        }
      }
    }

    res$result$beta_trend <- NA_real_
    res$result$P_trend <- NA_real_
    if (nrow(res$result) > 0) {
      res$result$beta_trend[1] <- if (!is.na(beta_tr)) round(beta_tr, 3) else NA_real_
      res$result$P_trend[1] <- if (!is.na(p_tr)) round(p_tr, 3) else NA_real_
    }
  }

  if (!is.null(res$result)) print(res$result, row.names = FALSE)
  res$result
}
