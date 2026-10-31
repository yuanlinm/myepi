make_subgroups <- function() {
  d <- make_lung()
  d$sex_f <- factor(d$sex, labels = c("Male", "Female"))
  d$age_group <- factor(ifelse(d$age >= 65, "65+", "<65"), levels = c("<65", "65+"))
  list(
    sex = quiet(cox_run_sub(d, group_var = "sex_f", timediff = "time", event = "ev", mainvar = "ph.karno", covars = "age")),
    age = quiet(cox_run_sub(d, group_var = "age_group", timediff = "time", event = "ev", mainvar = "ph.karno", covars = "sex"))
  )
}

test_that("plot_forest draws cox_run_sub results with the displayed numbers of the source", {
  font <- test_font()
  s <- make_subgroups()
  path <- tempfile(fileext = ".pdf")
  f <- plot_forest(list(s$sex, s$age), file = path, font_family = font,
                   labels = c(sex_f = "Sex", age_group = "Age"))
  expect_true(file.exists(path))
  expect_s3_class(f, "myepi_forest")
  expect_equal(f$summary$status, "ok")
  expect_match(f$summary$strokes_in_pdf, "^0.6 pt x[0-9]+, 0.8625 pt x[0-9]+$")

  txt <- pdf_text(path)
  r <- s$sex$result
  shown <- sprintf("%.2f (%.2f-%.2f)", r$HR, r$HR_lower, r$HR_upper)
  for (label in shown) expect_true(grepl(label, txt, fixed = TRUE))
  expect_true(grepl(sprintf("%.3f", r$P_interaction[1]), txt, fixed = TRUE))
  expect_true(grepl("Sex", txt, fixed = TRUE))
  expect_true(grepl("interaction", txt, fixed = TRUE))
})

test_that("plot_forest shows reference rows and P for trend for cox_run_q", {
  font <- test_font()
  d <- make_lung()
  q <- quiet(cox_run_q(d, mainvar = "age", q = 3, timediff = "time", event = "ev", trend = TRUE))
  path <- tempfile(fileext = ".pdf")
  f <- plot_forest(q, file = path, font_family = font)
  txt <- pdf_text(path)
  expect_true(grepl("1.00 (Ref)", txt, fixed = TRUE))
  expect_true(grepl("trend", txt, fixed = TRUE))
  expect_match(f$summary$rows, "displayed=3")
})

test_that("Lancet style uses midline decimals and en dashes only", {
  font <- test_font()
  s <- make_subgroups()
  path <- tempfile(fileext = ".pdf")
  plot_forest(list(A = list(s$sex), B = list(s$sex)), file = path, font_family = font, lancet_style = TRUE)
  txt <- pdf_text(path)
  expect_true(grepl("·", txt, fixed = TRUE))
  expect_false(grepl("\\d\\.\\d", txt, perl = TRUE))
  expect_false(grepl("-", txt, fixed = TRUE))
})

test_that("a failed check leaves no file and existing files are protected", {
  font <- test_font()
  s <- make_subgroups()
  path <- tempfile(fileext = ".pdf")
  expect_error(plot_forest(s$sex, file = path, font_family = font, x_max = 0.99))
  expect_false(file.exists(path))
  expect_false(any(grepl("^\\.forest-", list.files(dirname(path), all.files = TRUE))))

  plot_forest(s$sex, file = path, font_family = font)
  expect_error(plot_forest(s$sex, file = path, font_family = font))
  expect_no_error(plot_forest(s$sex, file = path, font_family = font, overwrite = TRUE))
})

test_that("tick labels never misrepresent tick values", {
  font <- test_font()
  s <- make_subgroups()
  expect_error(plot_forest(s$sex, file = tempfile(fileext = ".pdf"), font_family = font,
                           ticks = c(0.95, 0.975, 1, 1.025), tick_digits = 2))
})

test_that("missing fonts are an error, not a silent substitution", {
  test_font()
  s <- make_subgroups()
  expect_error(plot_forest(s$sex, file = tempfile(fileext = ".pdf"), font_family = "No Such Font Family"))
})

test_that("plot_forest accepts data frames with combined CI text and reference rows", {
  font <- test_font()
  df <- data.frame(
    Subgroup = c("Overall", "Smoking", "Smoking", "Smoking"),
    Category = c("Overall", "Never", "Former", "Current"),
    CI = c("1.07 (1.03-1.12)", "1.00 (Ref)", "1.10 (0.95-1.27)", "1.35 (1.10-1.66)"),
    P_interaction = c(NA, 0.042, NA, NA),
    ref = c(FALSE, TRUE, FALSE, FALSE)
  )
  path <- tempfile(fileext = ".pdf")
  expect_warning(
    f <- plot_forest(df, file = path, font_family = font, ref_col = "ref",
                     panels = list(list(title = "PM2.5", ci_text = "CI", p = "P_interaction"))),
    "4 mm"
  )
  txt <- pdf_text(path)
  expect_true(grepl("1.35 (1.10-1.66)", txt, fixed = TRUE))
  expect_true(grepl("0.042", txt, fixed = TRUE))
  expect_match(f$summary$rows, "display_rows=5")
})

test_that("plot_forest rejects inputs it cannot draw faithfully", {
  font <- test_font()
  s <- make_subgroups()
  d <- make_lung()
  shaped <- quiet(cox_run_sub(d, group_var = "sex", timediff = "time", event = "ev", mainvar = "age", plot_shape = TRUE))
  expect_error(plot_forest(shaped, file = tempfile(fileext = ".pdf")), "plot_shape")
  expect_error(plot_forest(list(A = list(s$sex), B = list(s$age)), file = tempfile(fileext = ".pdf")))
  bad <- data.frame(Subgroup = "A", Category = "a", HR = 1.2, HR_lower = 1.3, HR_upper = 1.5)
  expect_error(plot_forest(bad, file = tempfile(fileext = ".pdf"), font_family = font))
})

test_that("plot_forest_landscape draws stacked panels from myepi results", {
  font <- test_font()
  d <- make_lung()
  fits <- lapply(c("ph.karno", "pat.karno", "meal.cal"), function(v) {
    quiet(cox_run(d, timediff = "time", event = "ev", mainvar = v))
  })
  names(fits) <- c("Concurrent", "1-year lag", "2-year lag")
  path <- tempfile(fileext = ".pdf")
  expect_warning(f <- plot_forest_landscape(list(Ozone = fits, PM2.5 = fits), file = path, font_family = font), "4 mm")
  expect_equal(f$summary$category_order, "Concurrent | 1-year lag | 2-year lag")
  expect_match(f$summary$rows, "plotted=6")
  expect_match(f$summary$strokes_in_pdf, "^0.6 pt x[0-9]+, 0.8625 pt x[0-9]+$")
  txt <- pdf_text(path)
  expect_true(grepl("Ozone", txt, fixed = TRUE))
  expect_true(grepl("2-year lag", txt, fixed = TRUE))

  expect_error(plot_forest_landscape(list(Ozone = fits, PM2.5 = fits[1:2]), file = tempfile(fileext = ".pdf"), font_family = font))
})

test_that("plot_forest_landscape accepts data frames with panel regexes", {
  font <- test_font()
  df <- data.frame(
    varname = c("o3_0", "o3_1", "pm25_0", "pm25_1", "no2_0"),
    HR = c(1.05, 1.08, 1.02, 1.04, 1.01),
    HR_lower = c(1.01, 1.03, 0.99, 1.00, 0.98),
    HR_upper = c(1.09, 1.13, 1.05, 1.08, 1.04)
  )
  path <- tempfile(fileext = ".pdf")
  expect_error(plot_forest_landscape(df, file = path, panels = c(Ozone = "^o3_", PM2.5 = "^pm25_"),
                                     panel_col = "varname", category_labels = c("Concurrent", "1-year lag"),
                                     font_family = font))
  expect_warning(f <- plot_forest_landscape(df, file = path, panels = c(Ozone = "^o3_", PM2.5 = "^pm25_"),
                                            panel_col = "varname", category_labels = c("Concurrent", "1-year lag"),
                                            unmatched = "drop", font_family = font))
  expect_match(f$summary$rows, "dropped=1")
})
