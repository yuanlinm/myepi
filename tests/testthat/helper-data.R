quiet <- function(expr) suppressMessages(expr)

make_lung <- function() {
  d <- survival::lung
  d$ev <- as.integer(d$status == 2)
  d$id <- seq_len(nrow(d))
  d
}

test_font <- function() {
  testthat::skip_if_not(capabilities("cairo"), "Cairo unavailable")
  testthat::skip_if_not_installed("systemfonts")
  families <- unique(systemfonts::system_fonts()$family)
  candidates <- c("Helvetica", "Arial", "Liberation Sans", "DejaVu Sans")
  found <- candidates[candidates %in% families]
  testthat::skip_if(length(found) == 0L, "No suitable font installed")
  found[[1]]
}

pdf_text <- function(path) {
  testthat::skip_if_not(nzchar(Sys.which("pdftotext")), "poppler unavailable")
  out <- system2(Sys.which("pdftotext"), c("-enc", "UTF-8", "-raw", shQuote(path), "-"), stdout = TRUE)
  out <- paste(out, collapse = "\n")
  Encoding(out) <- "UTF-8"
  out
}
