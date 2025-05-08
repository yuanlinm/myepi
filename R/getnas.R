#' my NAs reporting func
#'
#' report NAs as a ordered printed data frame
#'
#' @param dat A data frame containing the grouping variable, event variable, and time variable.
#' @return A printed data frame combining group statistics (Variable，Type，Missing，MissingRate).

#' @export
report_missing <- function(dat) {
    dat <- as.data.frame(dat)
    output <- list()
    n_total <- nrow(dat)
    
    for (varname in names(dat)) {
        vec <- dat[[varname]]
        na_total <- sum(is.na(vec))
        if (na_total == 0) next
        
        var_type <- if (is.numeric(vec) || is.integer(vec)) {
            "Continuous"
        } else if (is.factor(vec) || is.character(vec)) {
            "Categorical"
        } else {
            class(vec)[1]
        }
        
        na_rate_num <- round(100 * na_total / n_total, 2)
        na_rate_str <- paste0(na_rate_num, "%")
        
        output[[length(output) + 1]] <- 
            data.frame(Variable = varname, 
                       Type = var_type, 
                       Missing = na_total, 
                       MissingRate = na_rate_str, 
                       RateSort = na_rate_num,  # 排序用列
                       stringsAsFactors = FALSE)
    }
    
    if (length(output) == 0) {
        return(data.frame(Message = "No missing values found."))
    } else {
        df_out <- do.call(rbind, output)
        df_out <- df_out[order(-df_out$RateSort), ]
        df_out$RateSort <- NULL  # 删除排序辅助列
        rownames(df_out) <- NULL
        return(df_out)
    }
}