#' 分组/亚组Cox回归批量分析函数，本函数支持基本的Cox回归分析和时间依赖（长数据）变量的Cox回归亚组分析。
#'
#' @param data 数据框
#' @param group_var 分组变量名（字符串）
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串，0/1）
#' @param mainvar 关注变量名（字符串）
#' @param covars 协变量名（字符串向量，可为NULL）
#' @param extra_vars 额外关注变量名（字符串向量，可为NULL）
#' @param idL 长格式数据的个体标识变量名（字符串）。指定后，亚组病例数和总人数按该变量去重，人时仍按各观察区间累加。
#' @param plot_shape 是否返回绘图友好格式（TRUE 返回分组标题 + 缩进结构；FALSE 返回原始长表, 后续异质性检验友好），默认 FALSE
#' @param ... 传递给cox_run的其他参数
#' @return 一个数据框：
#'   - 当 plot_shape = FALSE：原始长表（每亚组一行）
#'   - 当 plot_shape = TRUE：包含列 Group / Subgroup（Group 行为空 Subgroup，子行 Group 为空或缩进），便于直接用于森林图分面或分组标题展示
#' @export
cox_run_sub <- function(data, group_var, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, extra_vars = NULL, plot_shape = FALSE, idL = NULL, ...) {
  if (!group_var %in% names(data)) stop("分组变量不在数据框中")
  .validate_idL(data, idL)
  group_levels <- unique(data[[group_var]])

  res_list <- lapply(group_levels, function(g) {
    sub_data <- data[data[[group_var]] == g, , drop = FALSE]
    counts <- .count_cases_total(sub_data, event, idL)
    total_time <- .followup_time(sub_data, time1, time2, timediff)
    total_py <- sum(total_time, na.rm = TRUE) / 365.25
    case_total <- sprintf("%d/%d", counts$cases, counts$total)
    incidence <- if (total_py > 0) sprintf("%.2f", counts$cases / total_py * 1e5) else NA_character_

    res <- tryCatch({
      old_opt <- getOption("cox_run.print", TRUE)
      options(cox_run.print = FALSE)
      on.exit(options(cox_run.print = old_opt), add = TRUE)
      cox_run(
        sub_data,
        time1 = time1,
        time2 = time2,
        timediff = timediff,
        event = event,
        mainvar = mainvar,
        covars = covars,
        extra_vars = extra_vars,
        idL = idL,
        ...
      )$result
    }, error = function(e) {
      warning(sprintf("分组 %s 建模失败: %s", g, e$message))
      NULL
    })

    if (is.null(res) || nrow(res) == 0) return(NULL)
    res$Subgroup <- paste0(group_var, ": ", as.character(g))
    res$Case_Total <- case_total
    res$Incidence <- incidence
    col_order <- c(
      "varname", "Subgroup", "Case_Total", "Incidence",
      setdiff(colnames(res), c("varname", "Subgroup", "Case_Total", "Incidence", group_var))
    )
    res[, col_order, drop = FALSE]
  })

  out <- do.call(rbind, res_list)
  if (is.null(out)) return(invisible(NULL))
  if (!is.null(out)) {
    col_order <- c(
      "varname", "Subgroup", "Case_Total", "Incidence",
      setdiff(colnames(out), c("varname", "Subgroup", "Case_Total", "Incidence", group_var))
    )
    out <- out[, col_order, drop = FALSE]
    out <- out[order(out$varname, out$Subgroup), , drop = FALSE]

    if (isTRUE(plot_shape)) {
      subgroup_parts <- strsplit(out$Subgroup, ": ")
      group_names <- vapply(subgroup_parts, function(x) if (length(x) >= 1) x[1] else NA_character_, character(1))
      subgroup_levels <- vapply(subgroup_parts, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
      unique_groups <- unique(group_names)
      formatted_list <- vector("list", length(unique_groups))

      for (i in seq_along(unique_groups)) {
        g <- unique_groups[i]
        block <- out[group_names == g, , drop = FALSE]
        title_row <- block[1, , drop = FALSE]
        title_row[, setdiff(names(title_row), c("varname", "Subgroup"))] <- NA
        title_row$Subgroup <- ""
        title_row$varname <- g
        block$Subgroup <- subgroup_levels[group_names == g]
        formatted_list[[i]] <- rbind(title_row, block)
      }

      formatted <- do.call(rbind, formatted_list)
      if (!"Interval" %in% names(formatted)) formatted$Interval <- NA
      formatted$Group <- formatted$varname
      formatted$Group[formatted$Subgroup != ""] <- ""
      formatted$varname <- NULL
      keep_cols <- c(
        "Group", "Subgroup", "Case_Total", "Incidence", "Interval",
        "HR", "HR_lower", "HR_upper", "P", "beta", "se"
      )
      for (column in keep_cols) if (!column %in% names(formatted)) formatted[[column]] <- NA
      out <- formatted[, keep_cols, drop = FALSE]
    }
  }

  rownames(out) <- NULL
  invisible(out)
}
