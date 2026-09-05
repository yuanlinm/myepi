.validate_idL <- function(data, idL) {
  if (is.null(idL)) return(invisible(NULL))
  if (!is.character(idL) || length(idL) != 1L || is.na(idL) || !nzchar(idL)) {
    stop("idL 必须是单个非空变量名字符串，或设为 NULL")
  }
  if (!idL %in% names(data)) stop("idL 指定的个体标识变量不在数据框中")
  if (anyNA(data[[idL]])) stop("idL 指定的个体标识变量不能包含缺失值")
  invisible(NULL)
}

.count_cases_total <- function(data, event, idL = NULL, rows = rep(TRUE, nrow(data))) {
  rows[is.na(rows)] <- FALSE
  if (is.null(idL)) {
    return(list(
      cases = sum(data[[event]][rows] == 1, na.rm = TRUE),
      total = sum(rows)
    ))
  }
  ids <- data[[idL]][rows]
  events <- data[[event]][rows]
  list(
    cases = length(unique(ids[!is.na(events) & events == 1])),
    total = length(unique(ids))
  )
}

.followup_time <- function(data, time1 = NULL, time2 = NULL, timediff = NULL) {
  if (!is.null(timediff)) data[[timediff]] else data[[time2]] - data[[time1]]
}
