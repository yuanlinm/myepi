#' 常规Cox回归分析函数，本函数支持基本的Cox回归分析和时间依赖（长数据）变量的Cox回归分析。
#'
#' @param data 数据框
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串，0/1）
#' @param mainvar 关注变量名（字符串）
#' @param covars 协变量名（字符串向量，可为NULL）
#' @param extra_vars 额外关注变量名（字符串向量，可为NULL）
#' @param idL 长格式数据的个体标识变量名（字符串）。默认 NULL，按每行代表一名个体统计；指定后，Case_Total 中的病例数和总人数按该变量去重，Incidence 的病例数也按该变量去重，人时仍按各观察区间累加。
#' @return 一个列表，包含模型对象和格式化结果表
#' @export
cox_run <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, extra_vars = NULL, idL = NULL) {
  if (!requireNamespace("survival", quietly = TRUE)) stop("请先安装 survival 包")
  .validate_idL(data, idL)

  extract_vars <- function(x) {
    if (is.null(x)) return(character(0))
    strata_vars <- unlist(regmatches(x, gregexpr("strata\\(([^)]+)\\)", x)))
    strata_vars <- gsub("strata\\(([^)]+)\\)", "\\1", strata_vars)
    cluster_vars <- unlist(regmatches(x, gregexpr("cluster\\(([^)]+)\\)", x)))
    cluster_vars <- gsub("cluster\\(([^)]+)\\)", "\\1", cluster_vars)
    tmp <- gsub("strata\\(([^)]+)\\)", "", x)
    tmp <- gsub("cluster\\(([^)]+)\\)", "", tmp)
    other_vars <- unlist(strsplit(tmp, ":|\\*|/|\\^|\\+"))
    all_vars <- trimws(c(strata_vars, cluster_vars, other_vars))
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

  rhs_str <- paste(c(mainvar, covars), collapse = "+")
  fml <- as.formula(paste0(surv_str, " ~ ", rhs_str))
  fit <- if (any(grepl("cluster\\(", rhs_str))) {
    survival::coxph(fml, data = data, robust = TRUE)
  } else {
    survival::coxph(fml, data = data)
  }

  summ <- summary(fit)
  conf <- as.data.frame(summ$conf.int)
  res <- as.data.frame(summ$coefficients)
  res$HR <- exp(res$coef)
  res$HR_lower <- conf$`lower .95`
  res$HR_upper <- conf$`upper .95`
  res$P <- res$`Pr(>|z|)`
  res$beta <- res$coef
  res$se <- res$`se(coef)`
  res$varname <- rownames(res)

  var_match <- unique(unlist(lapply(mainvar, function(v) {
    grep(paste0("^", v, "$|^", v, ".+$"), res$varname, value = TRUE)
  })))
  if (!is.null(extra_vars)) var_match <- unique(c(var_match, extra_vars[extra_vars %in% res$varname]))
  out <- res[res$varname %in% var_match, c("varname", "HR", "HR_lower", "HR_upper", "P", "beta", "se"), drop = FALSE]
  time_vec <- .followup_time(data, time1, time2, timediff)

  calculate_descriptive_stats <- function(varname) {
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
    if (is.numeric(x) && !is.factor(x)) {
      rows <- !is.na(x)
      counts <- .count_cases_total(data, event, idL, rows)
      person_years <- sum(time_vec[rows], na.rm = TRUE)
      incidence <- if (person_years > 0) sprintf("%.2f", counts$cases / (person_years / 365.25) * 1e5) else NA_character_
      interval <- if (any(rows)) {
        paste0(sprintf("%.2f", min(x[rows])), "-", sprintf("%.2f", max(x[rows])))
      } else {
        NA_character_
      }
      return(list(
        interval = interval,
        case_total = paste0(counts$cases, "/", counts$total),
        incidence = incidence
      ))
    }

    if (is_factor_level) {
      raw_level <- sub(paste0("^", base_var), "", varname)
      if (is.factor(x) && grepl("^[0-9]+$", raw_level)) {
        idx <- suppressWarnings(as.integer(raw_level))
        level_interval <- if (!is.na(idx) && idx >= 1 && idx <= length(levels(x))) levels(x)[idx] else raw_level
      } else {
        level_interval <- raw_level
      }
      rows <- if (is.factor(x)) {
        if (level_interval %in% levels(x)) x == level_interval else x == raw_level
      } else {
        as.character(x) == raw_level
      }
      counts <- .count_cases_total(data, event, idL, rows)
      person_years <- sum(time_vec[rows], na.rm = TRUE)
      incidence <- if (person_years > 0) sprintf("%.2f", counts$cases / (person_years / 365.25) * 1e5) else NA_character_
      return(list(
        interval = level_interval,
        case_total = paste0(counts$cases, "/", counts$total),
        incidence = incidence
      ))
    }

    rows <- !is.na(x)
    counts <- .count_cases_total(data, event, idL, rows)
    person_years <- sum(time_vec[rows], na.rm = TRUE)
    incidence <- if (person_years > 0) sprintf("%.2f", counts$cases / (person_years / 365.25) * 1e5) else NA_character_
    list(
      interval = "-",
      case_total = paste0(counts$cases, "/", counts$total),
      incidence = incidence
    )
  }

  descriptive_stats <- lapply(out$varname, calculate_descriptive_stats)
  out$Interval <- vapply(descriptive_stats, `[[`, character(1), "interval")
  out$Case_Total <- vapply(descriptive_stats, `[[`, character(1), "case_total")
  out$Incidence <- vapply(descriptive_stats, `[[`, character(1), "incidence")
  out$varname_clean <- vapply(out$varname, function(varname) {
    base_var <- varname
    for (main_v in mainvar) {
      if (varname != main_v && startsWith(varname, main_v)) {
        base_var <- main_v
        break
      }
    }
    base_var
  }, character(1))

  out <- out[, c("varname_clean", "Interval", "Case_Total", "Incidence", "HR", "HR_lower", "HR_upper", "P", "beta", "se")]
  names(out)[1] <- "varname"

  add_ref_rows <- list()
  for (mv in mainvar) {
    if (!mv %in% names(data)) next
    x <- data[[mv]]
    if (is.factor(x) || is.character(x)) {
      if (is.factor(x)) {
        ref_level <- levels(x)[1]
        rows <- x == ref_level
      } else {
        ref_level <- sort(unique(x[!is.na(x)]))[1]
        rows <- x == ref_level
      }
      already_has <- any(grepl(paste0("^", mv, ref_level, "$"), out$varname)) ||
        any(out$varname == mv & out$Interval == ref_level)
      if (already_has) next

      counts <- .count_cases_total(data, event, idL, rows)
      person_years <- sum(time_vec[rows], na.rm = TRUE)
      incidence <- if (person_years > 0) sprintf("%.2f", counts$cases / (person_years / 365.25) * 1e5) else NA_character_
      add_ref_rows[[length(add_ref_rows) + 1L]] <- data.frame(
        varname = mv,
        Interval = ref_level,
        Case_Total = paste0(counts$cases, "/", counts$total),
        Incidence = incidence,
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
    out <- rbind(do.call(rbind, add_ref_rows), out)
    rownames(out) <- NULL
  }

  out$HR <- as.numeric(sprintf("%.2f", as.numeric(out$HR)))
  out$HR_lower <- as.numeric(sprintf("%.2f", as.numeric(out$HR_lower)))
  out$HR_upper <- as.numeric(sprintf("%.2f", as.numeric(out$HR_upper)))
  out$P <- as.numeric(out$P)
  out$beta <- as.numeric(sprintf("%.3f", as.numeric(out$beta)))
  out$se <- as.numeric(sprintf("%.3f", as.numeric(out$se)))
  rownames(out) <- NULL
  if (isTRUE(getOption("cox_run.print", TRUE))) print(out, row.names = FALSE)
  invisible(list(model = fit, result = out))
}
