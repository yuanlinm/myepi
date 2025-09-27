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
#' @param plot_shape 是否返回绘图友好格式（TRUE 返回分组标题 + 缩进结构；FALSE 返回原始长表, 后续异质性检验友好），默认 FALSE
#' @param ... 传递给cox_run的其他参数
#' @return 一个数据框：
#'   - 当 plot_shape = FALSE：原始长表（每亚组一行）
#'   - 当 plot_shape = TRUE：包含列 Group / Subgroup（Group 行为空 Subgroup，子行 Group 为空或缩进），便于直接用于森林图分面或分组标题展示
#' @export
cox_run_sub <- function(data, group_var, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, extra_vars = NULL, plot_shape = FALSE, ...) {
  if (!group_var %in% names(data)) stop("分组变量不在数据框中")
  group_levels <- unique(data[[group_var]])
  res_list <- lapply(group_levels, function(g) {
    sub_data <- data[data[[group_var]] == g, , drop = FALSE]
    # 计算亚组统计量
    n_sub <- nrow(sub_data)
    n_event <- sum(sub_data[[event]] == 1, na.rm = TRUE)
    # 计算总人年（假定时间单位为天，需除以365.25）
    if (!is.null(timediff) && timediff %in% names(sub_data)) {
      total_py <- sum(sub_data[[timediff]], na.rm = TRUE) / 365.25
    } else if (!is.null(time1) && !is.null(time2) && time1 %in% names(sub_data) && time2 %in% names(sub_data)) {
      total_py <- sum(sub_data[[time2]] - sub_data[[time1]], na.rm = TRUE) / 365.25
    } else {
      total_py <- NA
    }
    case_total <- sprintf("%d/%d", n_event, n_sub)
    incidence <- if (!is.na(total_py) && total_py > 0) sprintf("%.2f", n_event / total_py * 1e5) else NA
    res <- tryCatch({
      # 临时关闭cox_run打印
      old_opt <- getOption("cox_run.print", TRUE)
      options(cox_run.print = FALSE)
      on.exit(options(cox_run.print = old_opt), add = TRUE)
      cox_run(sub_data, time1 = time1, time2 = time2, timediff = timediff, event = event, mainvar = mainvar, covars = covars, extra_vars = extra_vars, ...)$result
    }, error = function(e) {
      warning(sprintf("分组 %s 建模失败: %s", g, e$message));
      NULL
    })
    if (!is.null(res) && nrow(res) > 0) {
      res$Subgroup <- paste0(group_var, ": ", as.character(g))
      res$Case_Total <- case_total
      res$Incidence <- incidence
      # 调整列顺序：varname、Subgroup、Case_Total、Incidence、其余
      col_order <- c("varname", "Subgroup", "Case_Total", "Incidence", setdiff(colnames(res), c("varname", "Subgroup", "Case_Total", "Incidence", group_var)))
      res <- res[, col_order, drop = FALSE]
      res
    } else {
      NULL
    }
  })
  out <- do.call(rbind, res_list)
  # 强制调整列顺序：varname、Subgroup、Case_Total、Incidence、其余
  if (!is.null(out)) {
    col_order <- c("varname", "Subgroup", "Case_Total", "Incidence", setdiff(colnames(out), c("varname", "Subgroup", "Case_Total", "Incidence", group_var)))
    out <- out[, col_order, drop = FALSE]
    # base R 排序：先 varname 再 Subgroup
    out <- out[order(out$varname, out$Subgroup), , drop = FALSE]
    if (isTRUE(plot_shape)) {
      # 拆分 Subgroup -> Group 名称 与 具体水平
      # Subgroup 形式: group_var: level
      tmp <- strsplit(out$Subgroup, ": ")
      group_names <- vapply(tmp, function(x) if (length(x) >= 1) x[1] else NA_character_, character(1))
      subgroup_levels <- vapply(tmp, function(x) if (length(x) >= 2) x[2] else NA_character_, character(1))
      # 构造分组标题行
      unique_groups <- unique(group_names)
      formatted_list <- vector("list", length = length(unique_groups))
      for (i in seq_along(unique_groups)) {
        g <- unique_groups[i]
        block <- out[group_names == g, , drop = FALSE]
        # 标题行：复制结构，除 varname/ Subgroup 外其他列 NA
        title_row <- block[1, , drop = FALSE]
        title_row[ , setdiff(names(title_row), c("varname", "Subgroup"))] <- NA
        title_row$Subgroup <- ""
        title_row$varname <- g
        # 子行：保留 varname 以便 rbind 一致；稍后再统一处理
        block$Subgroup <- subgroup_levels[group_names == g]
        formatted_block <- rbind(title_row, block)
        formatted_list[[i]] <- formatted_block
      }
      formatted <- do.call(rbind, formatted_list)
      # 重建列：Group, Subgroup, Case_Total, Incidence, Interval, HR, HR_lower, HR_upper, P, beta, se
      # 原列中 Interval 可能存在（源自 cox_run），确保存在即取；若没有则补 NA
      if (!"Interval" %in% names(formatted)) formatted$Interval <- NA
      # 新建 Group 列：来自 varname（标题行），子行置空
      formatted$Group <- formatted$varname
      formatted$Group[formatted$Subgroup != ""] <- ""
      # 去掉旧 varname 列
      formatted$varname <- NULL
      # 统一列顺序
      keep_cols <- c("Group", "Subgroup", "Case_Total", "Incidence", "Interval",
                     "HR", "HR_lower", "HR_upper", "P", "beta", "se")
      # 补全缺失列
      for (cc in keep_cols) if (!cc %in% names(formatted)) formatted[[cc]] <- NA
      formatted <- formatted[, keep_cols, drop = FALSE]
      rownames(formatted) <- NULL
      out <- formatted
    }
  }
  rownames(out) <- NULL
  # 只返回结果，不自动打印
  invisible(out)
}