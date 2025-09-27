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
    # 提取基础变量名及识别是否为分类水平
    base_var <- varname
    is_factor_level <- FALSE
    for (mv in mainvar) {
      if (varname != mv && startsWith(varname, mv)) {
        base_var <- mv
        is_factor_level <- TRUE
        break
      }
    }
    if (!base_var %in% names(data)) {
      return(list(interval = NA_character_, case_total = NA_character_, incidence = NA_character_))
    }
    x <- data[[base_var]]
    ev <- data[[event]]
    # 计算人年（整体用于连续变量；分类水平时用该水平子集）
    if (is.null(timediff)) {
      time_vec <- data[[time2]] - data[[time1]]
    } else {
      time_vec <- data[[timediff]]
    }
    # 连续变量：整体区间 + 总体事件率
    if (is.numeric(x) && !is.factor(x)) {
      person_years <- sum(time_vec, na.rm = TRUE)
      total_cases <- sum(ev == 1, na.rm = TRUE)
      total_n <- sum(!is.na(x))
      incidence <- if (person_years > 0) sprintf("%.2f", total_cases / (person_years/365.25) * 1e5) else NA_character_
      interval <- paste0(sprintf("%.2f", min(x, na.rm = TRUE)), "-", sprintf("%.2f", max(x, na.rm = TRUE)))
      return(list(interval = interval,
                  case_total = paste0(total_cases, "/", total_n),
                  incidence = incidence))
    }
    # 分类变量：若是变量整体行（非水平）仍给整体；若是某个水平行则给该水平特异统计
    # 推断水平名称（适配编码：变量名后直接拼接水平或数字索引）
    level_interval <- "-"
    case_total <- NA_character_
    incidence <- NA_character_
    if (is_factor_level) {
      # 去掉前缀
      raw_level <- sub(paste0("^", base_var), "", varname)
      # 若后缀是纯数字且 x 为 factor, 按 index 取水平；否则直接使用后缀字符
      if (is.factor(x) && grepl("^[0-9]+$", raw_level)) {
        idx <- suppressWarnings(as.integer(raw_level))
        if (!is.na(idx) && idx >= 1 && idx <= length(levels(x))) {
          level_interval <- levels(x)[idx]
        } else {
          level_interval <- raw_level
        }
      } else {
        level_interval <- raw_level
      }
      sel <- if (is.factor(x)) {
        # 与真实水平匹配（优先用水平文字）
        if (level_interval %in% levels(x)) x == level_interval else x == raw_level
      } else {
        # 非 factor 但被当作分类：匹配字符化后缀（可能极少见）
        as.character(x) == raw_level
      }
      n_level <- sum(sel, na.rm = TRUE)
      cases_level <- sum(ev[sel] == 1, na.rm = TRUE)
      # 该水平人年
      py_level <- sum(time_vec[sel], na.rm = TRUE)
      incidence <- if (py_level > 0) sprintf("%.2f", cases_level / (py_level/365.25) * 1e5) else NA_character_
      case_total <- paste0(cases_level, "/", n_level)
      return(list(interval = level_interval, case_total = case_total, incidence = incidence))
    } else {
      # 分类变量整体行：提供整体病例/样本 与 总体发病率
      n_total <- sum(!is.na(x))
      cases_total <- sum(ev == 1, na.rm = TRUE)
      py_total <- sum(time_vec, na.rm = TRUE)
      incidence <- if (py_total > 0) sprintf("%.2f", cases_total / (py_total/365.25) * 1e5) else NA_character_
      case_total <- paste0(cases_total, "/", n_total)
      return(list(interval = "-", case_total = case_total, incidence = incidence))
    }
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

  # 若 mainvar 中存在分类变量，补充参考水平（HR=1）行：
  add_ref_rows <- list()
  for (mv in mainvar) {
    if (!mv %in% names(data)) next
    x <- data[[mv]]
    if (is.factor(x) || is.character(x)) {
      # 识别参考水平：若为 factor 用第一个 level，否则用排序后第一个唯一值
      if (is.factor(x)) {
        ref_level <- levels(x)[1]
        sel <- x == ref_level
      } else {
        ux <- sort(unique(x))
        ref_level <- ux[1]
        sel <- x == ref_level
      }
      # 构造该参考水平的 varname 编码（匹配建模展开的命名规则：变量整体行 + 水平行）
      ref_varname_pattern <- paste0("^", mv, "$")
      # 如果结果里已经包含该参考水平（有些编码方式可能已经出现），则跳过
      already_has <- any(grepl(paste0("^", mv, ref_level, "$"), out$varname)) || any(out$varname == mv & out$Interval == ref_level)
      if (already_has) next
      # 计算参考水平统计
      if (is.null(timediff)) {
        time_vec <- data[[time2]] - data[[time1]]
      } else {
        time_vec <- data[[timediff]]
      }
      n_level <- sum(sel, na.rm = TRUE)
      cases_level <- sum(data[[event]][sel] == 1, na.rm = TRUE)
      py_level <- sum(time_vec[sel], na.rm = TRUE)
      incidence_level <- if (py_level > 0) sprintf("%.2f", cases_level / (py_level/365.25) * 1e5) else NA_character_
      # 插入行（放在该变量其它水平之前）
      add_ref_rows[[length(add_ref_rows)+1]] <- data.frame(
        varname = mv,
        Interval = ref_level,
        Case_Total = paste0(cases_level, "/", n_level),
        Incidence = incidence_level,
        HR = 1.00,
        HR_lower = 1.00,
        HR_upper = 1.00,
        P = NA_real_,
        beta = 0.000,
        se = 0.000,
        stringsAsFactors = FALSE
      )
    }
  }
  if (length(add_ref_rows) > 0) {
    ref_df <- do.call(rbind, add_ref_rows)
    # 绑定后按变量名排序并保证参考水平在同变量其它水平之前
    out <- rbind(ref_df, out)
    rownames(out) <- NULL
  }
  
  # 格式化输出，无须
  out$HR <- as.numeric(sprintf("%.2f", as.numeric(out$HR)))
  out$HR_lower <- as.numeric(sprintf("%.2f", as.numeric(out$HR_lower)))
  out$HR_upper <- as.numeric(sprintf("%.2f", as.numeric(out$HR_upper)))
  out$P <- as.numeric(out$P)
  out$beta <- as.numeric(sprintf("%.3f", as.numeric(out$beta)))
  out$se <- as.numeric(sprintf("%.3f", as.numeric(out$se)))
  rownames(out) <- NULL
  # 控制台输出美化：只显示主要变量和额外关注变量，列名美化
  if (isTRUE(getOption("cox_run.print", TRUE))) {
    print(out, row.names = FALSE)
  }
  invisible(list(model = fit, result = out))
}
