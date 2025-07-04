#' my NAs reporting func
#'
#' report NAs as a ordered printed data frame
#'
#' @param dat A data frame to be checked for missing values.
#' @param group_var (可选) 分组变量名（字符串），如指定则按该变量分组统计缺失。
#' @return 一个数据框，包含变量、类型、缺失数、缺失率，若分组则每组一行。
#'
#'
#' @export
count_na <- function(dat, group_var = NULL) {
  if (!is.data.frame(dat)) stop("Input must be a data frame.")
  if (!is.null(group_var)) {
    if (!group_var %in% names(dat)) stop("分组变量不存在于数据框中");
    # 计算每个变量的整体缺失率
    all_vars <- setdiff(names(dat), group_var)
    n_total_all <- nrow(dat)
    overall_na_count <- sapply(all_vars, function(varname) sum(is.na(dat[[varname]])))
    # 只保留整体存在缺失的变量
    vars_with_na <- names(overall_na_count[overall_na_count > 0])
    if (length(vars_with_na) == 0) {
      return(data.frame(Message = "No missing values found."))
    }
    overall_na_rate <- overall_na_count[vars_with_na] / n_total_all
    var_order <- names(sort(overall_na_rate, decreasing = TRUE))
    # 分组统计
    groups <- unique(dat[[group_var]])
    # 向量化分组统计，提升速度
    group_vec <- dat[[group_var]]
    res_list <- vector("list", length = length(var_order) * length(groups))
    idx <- 1
    for (varname in var_order) {
      vec <- dat[[varname]]
      var_type <- if (is.numeric(vec) || is.integer(vec)) {
        "Continuous"
      } else if (is.factor(vec) || is.character(vec)) {
        "Categorical"
      } else if (inherits(vec, "Date") || inherits(vec, "POSIXct")) {
        "Date/Time"
      } else {
        class(vec)[1]
      }
      for (g in groups) {
        sel <- group_vec == g
        n_total <- sum(sel)
        na_total <- sum(is.na(vec[sel]))
        na_rate_num <- if (n_total == 0) 0 else round(100 * na_total / n_total, 2)
        res_list[[idx]] <- data.frame(
          Variable = varname,
          Group = g,
          Type = var_type,
          Missing = na_total,
          MissingRate = paste0(na_rate_num, "%"),
          stringsAsFactors = FALSE
        )
        idx <- idx + 1
      }
    }
    res <- do.call(rbind, res_list)
    # 只保留整体存在缺失的变量（即var_order）
    if (nrow(res) == 0) {
      return(data.frame(Message = "No missing values found."))
    }
    rownames(res) <- NULL
    return(res)
  } else {
    n_total <- nrow(dat)
    res <- lapply(names(dat), function(varname) {
      vec <- dat[[varname]]
      na_total <- sum(is.na(vec))
      if (na_total == 0) return(NULL)
      var_type <- if (is.numeric(vec) || is.integer(vec)) {
        "Continuous"
      } else if (is.factor(vec) || is.character(vec)) {
        "Categorical"
      } else if (inherits(vec, "Date") || inherits(vec, "POSIXct")) {
        "Date/Time"
      } else {
        class(vec)[1]
      }
      na_rate_num <- round(100 * na_total / n_total, 2)
      data.frame(
        Variable = varname,
        Type = var_type,
        Missing = na_total,
        MissingRate = paste0(na_rate_num, "%"),
        RateSort = na_rate_num,
        stringsAsFactors = FALSE
      )
    })
    res <- do.call(rbind, res)
    if (is.null(res)) {
      return(data.frame(Message = "No missing values found."))
    }
    res <- res[order(-res$RateSort), ]
    res$RateSort <- NULL
    rownames(res) <- NULL
    return(res)
  }
}