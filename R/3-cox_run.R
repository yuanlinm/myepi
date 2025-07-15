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
  var_match <- unique(unlist(lapply(mainvar, function(v) grep(paste0("^", v, "$|^", v, ".+$"), res$varname, value = TRUE))))
  # 若extra_vars中有交互项，补充匹配
  if (!is.null(extra_vars)) {
    var_match <- unique(c(var_match, extra_vars[extra_vars %in% res$varname]))
  }
  out <- res[res$varname %in% var_match, c("varname", "HR", "HR_lower", "HR_upper", "P", "beta", "se"), drop = FALSE]
  
  # 计算新增的三列：Interval、Case_Total、Incidence
  calculate_descriptive_stats <- function(varname, data, event, time1, time2, timediff) {
    # 提取原始变量名（去除因子水平后缀）
    base_var <- varname
    
    # 检查是否为因子水平变量（如education2, education高中等）
    is_factor_level <- FALSE
    for (main_v in mainvar) {
      if (varname != main_v && startsWith(varname, main_v)) {
        base_var <- main_v
        is_factor_level <- TRUE
        break
      }
    }
    
    # 如果变量不在数据中，返回默认值
    if (!base_var %in% names(data)) {
      return(list(interval = "NA", case_total = "NA", incidence = "NA"))
    }
    
    var_data <- data[[base_var]]
    event_data <- data[[event]]
    
    # 计算总人年（整体数据集）
    if (!is.null(timediff)) {
      person_years <- sum(data[[timediff]], na.rm = TRUE)
    } else {
      person_years <- sum(data[[time2]] - data[[time1]], na.rm = TRUE)
    }
    
    # 计算整体数据集的Case_Total和Incidence
    total_cases <- sum(event_data, na.rm = TRUE)
    total_n <- nrow(data)
    case_total <- paste0(total_cases, "/", total_n)
    if (person_years > 0) {
      incidence <- sprintf("%.2f", (total_cases / (person_years/365.25)) * 1e5)
    } else {
      incidence <- "0.00"
    }
    
    # 判断变量类型并计算Interval
    if (is.numeric(var_data) && !is.factor(var_data)) {
      # 连续变量：min-max格式
      interval <- paste0(sprintf("%.2f", min(var_data, na.rm = TRUE)), "-", 
                         sprintf("%.2f", max(var_data, na.rm = TRUE)))
    } else {
      # 分类变量：显示对应的水平
      if (is_factor_level) {
        # 这是一个因子水平，提取水平名称
        level_name <- gsub(paste0("^", base_var), "", varname)
        
        # 检查是否为纯数字（如education2中的"2"）
        if (grepl("^[0-9]+$", level_name)) {
          # 纯数字，获取对应的因子水平
          if (is.factor(var_data)) {
            level_index <- as.numeric(level_name)
            if (level_index <= length(levels(var_data))) {
              interval <- levels(var_data)[level_index]
            } else {
              interval <- level_name
            }
          } else {
            # 非因子变量，显示数字水平
            interval <- level_name
          }
        } else {
          # 非数字后缀，直接显示（如education高中中的"高中"）
          interval <- level_name
        }
      } else {
        # 整个分类变量，显示"-"
        interval <- "-"
      }
    }
    
    return(list(interval = interval, case_total = case_total, incidence = incidence))
  }
  
  # 为每个变量计算新列
  descriptive_stats <- lapply(out$varname, function(v) {
    calculate_descriptive_stats(v, data, event, time1, time2, timediff)
  })
  
  # 添加新列到输出结果
  out$Interval <- sapply(descriptive_stats, function(x) x$interval)
  out$Case_Total <- sapply(descriptive_stats, function(x) x$case_total)
  out$Incidence <- sapply(descriptive_stats, function(x) x$incidence)
  
  # 将varname列替换为基础变量名（去除水平后缀）
  out$varname_clean <- sapply(out$varname, function(varname) {
    base_var <- varname
    for (main_v in mainvar) {
      if (varname != main_v && startsWith(varname, main_v)) {
        base_var <- main_v
        break
      }
    }
    return(base_var)
  })
  
  # 重新排列列顺序，使用清理后的变量名
  out <- out[, c("varname_clean", "Interval", "Case_Total", "Incidence", "HR", "HR_lower", "HR_upper", "P", "beta", "se")]
  names(out)[1] <- "varname"  # 重命名第一列为varname
  
  # 格式化输出，无须
  out$HR <- as.numeric(sprintf("%.2f", as.numeric(out$HR)))
  out$HR_lower <- as.numeric(sprintf("%.2f", as.numeric(out$HR_lower)))
  out$HR_upper <- as.numeric(sprintf("%.2f", as.numeric(out$HR_upper)))
  out$P <- as.numeric(sprintf("%.3f", as.numeric(out$P)))
  out$beta <- as.numeric(sprintf("%.3f", as.numeric(out$beta)))
  out$se <- as.numeric(sprintf("%.3f", as.numeric(out$se)))
  rownames(out) <- NULL
  # 控制台输出美化：只显示主要变量和额外关注变量，列名美化
  if (isTRUE(getOption("cox_run.print", TRUE))) {
    print(out, row.names = FALSE)
  }
  invisible(list(model = fit, result = out))
}
