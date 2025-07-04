#' 交叉表/分组统计函数
#'
#' @param dat 数据框

#' 交叉表/分组统计函数（支持多分组变量，单一目标变量）
#'
#' @param dat 数据框
#' @param var  目标变量名（字符串，长度为1）
#' @param by   分组变量名（字符串向量，长度>=1）
#' @return 一个数据框，分类变量返回频数和比例，连续变量返回均值和中位数

#' @export
cross_tb <- function(dat, var, by) {
  # 先强制转换为data.frame，保证后续分析统一
  dat <- as.data.frame(dat)
  if (length(var) != 1) stop("var 只能为单个变量名")
  if (!var %in% names(dat) || !all(by %in% names(dat))) stop("变量名不存在于数据框")
  x <- dat[[var]]
  by_df <- dat[, by, drop = FALSE]
  group_id <- do.call(paste, c(by_df, sep = "_"))
  # 连续变量
  if (is.numeric(x) || is.integer(x)) {
    agg <- aggregate(x, by = by_df, FUN = function(vv) c(mean = mean(vv, na.rm=TRUE), median = median(vv, na.rm=TRUE)))
    agg$Mean <- sprintf("%.2f", agg$x[,1])
    agg$Median <- sprintf("%.2f", agg$x[,2])
    agg$x <- NULL
    out <- agg[, c(by, "Mean", "Median")]
    rownames(out) <- NULL
    return(out)
  } else {
    # 分类变量，长表输出：每行为一个by组合和一个var水平
    levs <- levels(as.factor(x))
    by_df[] <- lapply(by_df, as.character)
    group_id <- do.call(paste, c(by_df, sep = "_"))
    tab <- table(x, group_id, useNA = "ifany")
    n_mat <- as.matrix(tab)
    pct_mat <- prop.table(n_mat, 2) * 100
    # 组装长表
    out_list <- list()
    for (col in colnames(n_mat)) {
      # 拆分分组变量
      group_vals <- strsplit(col, "_")[[1]]
      for (i in seq_along(levs)) {
        out_list[[length(out_list)+1]] <- c(as.list(group_vals),
                                           Level = levs[i],
                                           N = n_mat[i, col],
                                           Freq = sprintf("%.1f%%", pct_mat[i, col]))
      }
    }
    out <- as.data.frame(do.call(rbind, out_list), stringsAsFactors = FALSE)
    names(out)[1:length(by)] <- by
    names(out)[length(by)+1] <- var
    # 合计
    n_sum <- rowSums(n_mat)
    pct_sum <- prop.table(n_sum) * 100
    total_df <- data.frame(matrix(NA, nrow=length(levs), ncol=length(by)), stringsAsFactors = FALSE)
    names(total_df) <- by
    total_df[[var]] <- levs
    total_df$N <- n_sum
    total_df$Freq <- sprintf("%.1f%%", pct_sum)
    out <- rbind(out, total_df)
    rownames(out) <- NULL
    return(out)
  }
}
