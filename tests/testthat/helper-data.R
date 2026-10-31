quiet <- function(expr) suppressMessages(expr)

make_lung <- function() {
  d <- survival::lung
  d$ev <- as.integer(d$status == 2)
  d$id <- seq_len(nrow(d))
  d
}
