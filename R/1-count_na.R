#' my NAs reporting func
#'
#' report NAs as a ordered data frame
#'
#' @param dat A data frame to be checked for missing values.
#' @param group_var (可选) 分组变量名（字符串），如指定则按该变量分组统计缺失；分组变量本身的缺失作为单独一组（Group 为 NA）。
#' @return 不可见地返回数据框：变量、（分组）、类型、样本量 N、缺失数、缺失率（%，原始精度），仅含存在缺失的变量，按整体缺失率降序。
#'   无缺失时返回 0 行数据框并给出 message。
#'
#' @export
count_na <- function(dat, group_var = NULL) {
  if (!is.data.frame(dat)) stop("Input must be a data frame.")
  dat <- as.data.frame(dat)
  if (!is.null(group_var) && (length(group_var) != 1L || !group_var %in% names(dat))) stop("分组变量不存在于数据框中")

  vars <- setdiff(names(dat), group_var)
  na_mat <- is.na(dat[vars])
  overall <- colSums(na_mat)
  vars_na <- vars[overall > 0]
  vars_na <- vars_na[order(-overall[vars_na])]
  types <- vapply(dat[vars_na], .var_type, character(1))

  if (length(vars_na) == 0L) {
    message("未发现缺失值")
    empty <- data.frame(Variable = character(0), Type = character(0), N = integer(0),
                        Missing = numeric(0), MissingRate = numeric(0), stringsAsFactors = FALSE)
    if (!is.null(group_var)) empty <- cbind(empty[1], Group = character(0), empty[-1])
    return(invisible(empty))
  }

  if (is.null(group_var)) {
    res <- data.frame(
      Variable = vars_na, Type = unname(types), N = nrow(dat),
      Missing = unname(overall[vars_na]), MissingRate = unname(100 * overall[vars_na] / nrow(dat)),
      stringsAsFactors = FALSE
    )
  } else {
    g <- addNA(factor(dat[[group_var]]), ifany = TRUE)
    counts <- rowsum(na_mat[, vars_na, drop = FALSE] * 1L, as.integer(g), reorder = TRUE)
    present <- sort(unique(as.integer(g)))
    n_g <- tabulate(as.integer(g), nbins = nlevels(g))[present]
    lev <- as.character(levels(g))[present]
    res <- data.frame(
      Variable = rep(vars_na, each = length(present)),
      Group = rep(lev, times = length(vars_na)),
      Type = rep(unname(types), each = length(present)),
      N = rep(n_g, times = length(vars_na)),
      Missing = as.vector(counts),
      stringsAsFactors = FALSE
    )
    res$MissingRate <- 100 * res$Missing / res$N
  }
  rownames(res) <- NULL
  invisible(res)
}

.var_type <- function(vec) {
  if (is.numeric(vec)) {
    "Continuous"
  } else if (is.factor(vec) || is.character(vec) || is.logical(vec)) {
    "Categorical"
  } else if (inherits(vec, "Date") || inherits(vec, "POSIXt")) {
    "Date/Time"
  } else {
    class(vec)[1]
  }
}
