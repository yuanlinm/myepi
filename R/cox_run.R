#' my Cox Model tools and Group Statistics
#'
#' Extracts hazard ratios, coefficients, and summary statistics (cases, incidence rate)
#' from a Cox proportional hazards model and a data frame, for a specific group level.
#'
#' @param data A data frame containing the grouping variable, event variable, and time variable.
#' @param event A string specifying the event variable in \code{df} (1 = event, 0 = censored).
#' @param time A string specifying the time variable in \code{df} (in days).
#' @param riskvar A string specifying the focused risk variable in \code{df} .
#' @param covars A string specifying the covariates in \code{df} .
#' @param groupvar A string specifying the grouping variable in \code{df}.
#' @param ref Reset the ref level in risk_var
#' @return A data frame combining group statistics (cases, total, incidence rate and Cox model results (HR, 95% CI, beta, SE, p-value).

#' @export
cox_run = function(data, time, event, riskvar, covars, groupvar = NULL, ref = NULL) {
  
  # 转换为data.frame
  data = as.data.frame(data)
  
  # 输入校验
  if (!riskvar %in% colnames(data)) stop("Risk variable not found in data")
  if (!all(covars %in% colnames(data))) stop("Some covariates not found in data")
  if (!is.null(groupvar) && !groupvar %in% colnames(data)) stop("Grouping variable not found in data")
  
  # 判断riskvar类型
  risk_data <- data[[riskvar]]
  is_categorical <- is.factor(risk_data) || is.character(risk_data) || is.logical(risk_data)
  
  # 如果是分类变量，转为factor并设定参考组（如提供）
  if (is_categorical) {
    data[[riskvar]] <- factor(data[[riskvar]])
    if (!is.null(ref)) data[[riskvar]] <- relevel(data[[riskvar]], ref = ref)
  }
  
  # 构造Cox公式
  vars <- c(riskvar, covars)
  cox_formula <- as.formula(paste0('Surv(', time, ',', event, ') ~ ', paste(vars, collapse = '+')))
  
  # 定义单个数据集的模型计算函数
  run_model <- function(df, subgroup = "All") {
    if (is_categorical) {
      m <- coxph(cox_formula, data = df)
      sm <- summary(m)
      # 只提取 riskvar 的系数行
      coef_names <- rownames(sm$coef)
      target_rows <- grep(paste0("^", riskvar), coef_names)  # riskvar 的 dummy 变量行
      res <- as.data.frame(sm$conf.int)[target_rows, ]
      stat <- as.data.frame(sm$coef)[target_rows, c("coef", "se(coef)", "Pr(>|z|)")]
      # 提取 levels（不含参考组）
      levels_var <- levels(df[[riskvar]])
      non_ref_levels <- levels_var[levels_var != levels(df[[riskvar]])[1]]
      
      df_out <- data.frame(
        group = ifelse(is.null(groupvar), riskvar, groupvar),
        g_levels = subgroup,
        level = levels_var,
        hr = round(c(1, res[, "exp(coef)"]), 4),
        hr_lower = round(c(1, res[, "lower .95"]), 4),
        hr_upper = round(c(1, res[, "upper .95"]), 4),
        beta = round(c(0, stat[, "coef"]), 4),
        se = round(c(0, stat[, "se(coef)"]), 4),
        p = c(1, stat[, "Pr(>|z|)"]),
        case_total = NA,
        incidence = NA
      )
      
      # 按水平计算 case_total 与 incidence
      level_data <- lapply(levels_var, function(lv) {
        sub <- df[df[[riskvar]] == lv, ]
        ct <- paste0(sum(sub[[event]]), '/', nrow(sub))
        inc <- round((sum(sub[[event]]) / sum(sub[[time]] / 365.25)) * 1e5, 2)
        return(data.frame(case_total = ct, incidence = inc))
      }) |> bind_rows()
      
      df_out$case_total <- level_data$case_total
      df_out$incidence <- level_data$incidence
      df_out = df_out |> select(group, level, g_levels,case_total, incidence, hr, hr_lower, hr_upper, beta, se, p)
      rownames(df_out) = NULL
      return(df_out)
      
    } else {
      m <- coxph(cox_formula, data = df)
      sm <- summary(m)
      ci <- sm$conf.int[1, ]
      stat <- sm$coef[1, c("coef", "se(coef)", "Pr(>|z|)")]
      out <- data.frame(
        group = ifelse(is.null(groupvar), riskvar, groupvar),
        g_levels = subgroup,
        level = "All",
        hr = round(ci["exp(coef)"], 4),
        hr_lower = round(ci["lower .95"], 4),
        hr_upper = round(ci["upper .95"], 4),
        beta = round(stat["coef"], 4),
        se = round(stat["se(coef)"], 4),
        p = stat["Pr(>|z|)"],
        case_total = paste0(sum(df[[event]]), '/', nrow(df)),
        incidence = round((sum(df[[event]]) / sum(df[[time]] / 365.25)) * 1e5, 2)
      )
      out = out |> select(group, level, g_levels,case_total, incidence, hr, hr_lower, hr_upper, beta, se, p)
      rownames(out) = NULL
      return(out)
    }
  }
  
  # 主体逻辑：按是否提供group变量分支
  if (is.null(groupvar)) {
    return(run_model(data))
  } else {
    data[[groupvar]] <- factor(data[[groupvar]])
    res <- lapply(levels(data[[groupvar]]), function(lv) {
      sub_data <- data[data[[groupvar]] == lv, ]
      run_model(sub_data, subgroup = lv)
    }) |> bind_rows()
    rownames(res) = NULL
    return(res)
  }
}