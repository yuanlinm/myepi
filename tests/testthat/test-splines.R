library(survival)

make_long <- function(d) {
  long <- rbind(transform(d, t1 = 0, t2 = time / 2, ev = 0L), transform(d, t1 = time / 2, t2 = time))
  long[order(long$id, long$t1), ]
}

test_that("cox_run_rcs matches an independent spline contrast and rms::anova", {
  skip_if_not_installed("rms")
  d <- make_lung()
  r <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", covars = "sex", ref = 60))
  expect_equal(nrow(r$result), 500L)
  expect_equal(names(r$result), c("x", "hr", "ll", "ul"))
  expect_equal(range(r$result$x), range(d$age))
  expect_equal(r$knots, Hmisc::rcspline.eval(d$age, nk = 3, knots.only = TRUE))

  # 独立实现：coxph + 同一 RCS 基函数，按 vcov 计算相对参照值的对比
  X <- Hmisc::rcspline.eval(d$age, knots = r$knots, inclx = TRUE)
  fit <- coxph(Surv(time, ev) ~ X + sex, data = d)
  G <- Hmisc::rcspline.eval(r$result$x, knots = r$knots, inclx = TRUE)
  D <- sweep(G, 2, Hmisc::rcspline.eval(60, knots = r$knots, inclx = TRUE))
  eta <- as.vector(D %*% coef(fit)[1:2])
  se <- sqrt(rowSums((D %*% vcov(fit)[1:2, 1:2]) * D))
  expect_equal(r$result$hr, exp(eta), tolerance = 1e-6)
  expect_equal(r$result$ll, exp(eta - qnorm(0.975) * se), tolerance = 1e-6)

  # 非线性检验：第二个样条系数的 Wald 检验
  w <- coef(fit)[2]^2 / vcov(fit)[2, 2]
  expect_equal(r$tests$P[r$tests$test == "nonlinear"], unname(pchisq(w, 1, lower.tail = FALSE)), tolerance = 1e-6)
  expect_null(getOption("datadist"))
})

test_that("cox_run_rcs knot options, reference value and prediction range", {
  skip_if_not_installed("rms")
  d <- make_lung()
  r1 <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", knots = c(45, 60, 70, 75), ref = min(d$age)))
  expect_equal(r1$knots, c(45, 60, 70, 75))
  expect_equal(r1$result$hr[1], 1)
  r2 <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", knot_probs = c(0.1, 0.5, 0.9)))
  expect_equal(r2$knots, unname(quantile(d$age, c(0.1, 0.5, 0.9))))
  expect_equal(r2$ref, median(d$age))
  r3 <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", range_probs = c(0.05, 0.95)))
  expect_equal(range(r3$result$x), unname(quantile(d$age, c(0.05, 0.95))))
  expect_error(quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", knots = c(45, 60, 70), knot_probs = c(0.1, 0.5, 0.9))))
  expect_error(quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", nk = 8)))
})

test_that("cox_run_rcs supports strata, long data with idL, and opt-in robust variance", {
  skip_if_not_installed("rms")
  d <- make_lung()
  long <- make_long(d)
  wide <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", covars = "sex", robust = TRUE))
  lng <- quiet(cox_run_rcs(long, time1 = "t1", time2 = "t2", event = "ev", mainvar = "age", covars = "sex", idL = "id", robust = TRUE))
  expect_equal(lng$knots, wide$knots)
  expect_equal(lng$result, wide$result, tolerance = 1e-8)
  expect_equal(lng$tests$P, wide$tests$P, tolerance = 1e-8)
  expect_equal(lng$info$variance, "robust")

  model <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", covars = "sex"))
  expect_equal(model$info$variance, "model")
  expect_false(isTRUE(all.equal(model$result$ll, wide$result$ll)))
  expect_message(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", covars = c("sex", "cluster(inst)")))

  s <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", covars = c("sex", "strata(inst)")))
  rcs <- rms::rcs
  strat <- rms::strat
  dd <- d[!is.na(d$inst), ]
  kn <- s$knots
  direct <- rms::cph(Surv(time, ev) ~ rcs(age, kn) + sex + strat(inst), data = dd, x = TRUE, y = TRUE, eps = 1e-9, tol = 1e-12)
  a <- anova(direct)
  expect_equal(s$tests$P, unname(a[c("age", " Nonlinear"), "P"]))
})

test_that("spline functions forbid terms involving mainvar", {
  d <- make_lung()
  expect_error(quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", covars = c("sex", "age:sex"))))
  expect_error(quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", covars = "strata(age)")))
  skip_if_not_installed("rms")
  expect_error(quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", covars = c("sex", "age*sex"))))
})

test_that("cox_run_psp follows the survival vignette (termplot) and summary tests", {
  d <- make_lung()
  p <- quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", covars = "sex"))
  expect_equal(nrow(p$result), 500L)
  expect_equal(names(p$result), c("x", "hr", "ll", "ul"))
  fit <- coxph(Surv(time, ev) ~ pspline(age, df = 4) + sex, data = d)
  tp <- termplot(fit, se = TRUE, plot = FALSE, terms = 1)$age
  i <- match(range(d$age), tp$x)
  expect_equal(p$result$hr[c(1, 500)], exp(tp$y[i]), tolerance = 1e-8)
  expect_equal(p$result$ll[c(1, 500)], exp(tp$y[i] - qnorm(0.975) * tp$se[i]), tolerance = 1e-8)
  cf <- summary(fit)$coefficients
  expect_equal(p$tests$P, unname(cf[1:2, "p"]))
  expect_equal(p$tests$test, c("linear", "nonlinear"))
  expect_equal(p$spline$df, unname(fit$df[1]))
})

test_that("cox_run_psp works without attaching survival", {
  skip_on_cran()
  script <- tempfile(fileext = ".R")
  writeLines(c(
    "d <- survival::lung; d$ev <- as.integer(d$status == 2)",
    "p <- suppressMessages(myepi::cox_run_psp(d, timediff = 'time', event = 'ev', mainvar = 'age'))",
    "cat(!'package:survival' %in% search(), nrow(p$result), is.finite(p$tests$P[2]))"
  ), script)
  out <- system2(file.path(R.home("bin"), "Rscript"), script, stdout = TRUE, stderr = TRUE)
  expect_equal(utils::tail(out, 1), "TRUE 500 TRUE")
})

test_that("cox_run_psp parameters and opt-in robust variance", {
  d <- make_lung()
  expect_error(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", df = 3, theta = 0.5))
  p0 <- quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", df = 0))
  f0 <- coxph(Surv(time, ev) ~ pspline(age, df = 0), data = d)
  expect_equal(p0$spline$df, unname(f0$df[1]))
  pt <- quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", theta = 0.9, nterm = 8))
  expect_equal(pt$spline$nterm, 8)
  expect_equal(pt$spline$n_basis, 8L + 3L - 1L)

  model <- quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age"))
  robust <- quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age", robust = TRUE))
  fr <- coxph(Surv(time, ev) ~ pspline(age, df = 4), data = d, robust = TRUE)
  expect_equal(robust$tests$P, unname(summary(fr)$coefficients[1:2, "p"]))
  expect_false(isTRUE(all.equal(model$tests$P, robust$tests$P)))

  long <- make_long(d)
  pl <- quiet(cox_run_psp(long, time1 = "t1", time2 = "t2", event = "ev", mainvar = "age", idL = "id", robust = TRUE))
  fl <- coxph(Surv(t1, t2, ev) ~ pspline(age, df = 4) + cluster(id), data = long)
  expect_equal(pl$tests$P, unname(summary(fl)$coefficients[1:2, "p"]))
})

test_that("plot_crf draws RCS and PSP curves with the requested elements only", {
  font <- test_font()
  d <- make_lung()
  r <- quiet(cox_run_psp(d, timediff = "time", event = "ev", mainvar = "age"))
  path <- tempfile(fileext = ".pdf")
  f <- plot_crf(r, path, xlab = "Age (years)", font_family = font)
  expect_true(file.exists(path))
  expect_equal(f$summary$status, "ok")
  expect_match(f$summary$strokes_in_pdf, "^0.6 pt x[0-9]+, 0.75 pt x1$")
  txt <- pdf_text(path)
  expect_true(grepl("Age (years)", txt, fixed = TRUE))
  expect_true(grepl("HazardRatio", gsub("\\s+", "", txt), fixed = TRUE))

  path2 <- tempfile(fileext = ".pdf")
  f2 <- plot_crf(r, path2, ci_fill = FALSE, ci_border = TRUE, ci_border_width = 0.4, line_width = 1.2, font_family = font)
  expect_match(f2$summary$strokes_in_pdf, "^0.4 pt x2, 0.6 pt x[0-9]+, 1.2 pt x1$")

  expect_warning(plot_crf(r, tempfile(fileext = ".pdf"), ylim = c(0.8, 1.2), font_family = font), "ylim")
  expect_error(plot_crf(quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age")), tempfile(fileext = ".pdf")))
  expect_error(plot_crf(r, tempfile(fileext = ".pdf"), line_alpha = 2, font_family = font))
})

test_that("plot_crf accepts cox_run_rcs output", {
  skip_if_not_installed("rms")
  font <- test_font()
  d <- make_lung()
  r <- quiet(cox_run_rcs(d, timediff = "time", event = "ev", mainvar = "age", ref = 60))
  f <- plot_crf(r, tempfile(fileext = ".pdf"), xlab = expression(Age[years]), font_family = font)
  expect_match(f$summary$source, "age \\(rcs\\), 500 points")
})
