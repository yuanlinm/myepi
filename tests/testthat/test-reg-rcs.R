library(survival)

rcs_lung <- function() {
  d <- make_lung()
  d$y <- as.integer(d$status == 2)
  d
}

rcs_infert <- function() {
  d <- datasets::infert
  set.seed(7)
  d$x <- rnorm(nrow(d), 25, 5) + 3 * d$case
  d
}

test_that("logit_run_rcs matches glm with the same spline basis", {
  skip_if_not_installed("rms")
  d <- rcs_lung()
  r <- quiet(logit_run_rcs(d, "y", "age", covars = "sex", ref = 60))
  expect_equal(names(r$result), c("x", "or", "ll", "ul"))
  expect_equal(nrow(r$result), 500L)
  expect_equal(r$knots, Hmisc::rcspline.eval(d$age, nk = 3, knots.only = TRUE))
  X <- Hmisc::rcspline.eval(d$age, knots = r$knots, inclx = TRUE)
  fit <- glm(y ~ X + sex, family = binomial, data = d)
  D <- sweep(Hmisc::rcspline.eval(r$result$x, knots = r$knots, inclx = TRUE), 2,
             Hmisc::rcspline.eval(60, knots = r$knots, inclx = TRUE))
  eta <- as.vector(D %*% coef(fit)[2:3])
  se <- sqrt(rowSums((D %*% vcov(fit)[2:3, 2:3]) * D))
  expect_equal(r$result$or, exp(eta), tolerance = 1e-6)
  expect_equal(r$result$ll, exp(eta - qnorm(0.975) * se), tolerance = 1e-6)
  w <- coef(fit)[3]^2 / vcov(fit)[3, 3]
  expect_equal(r$tests$P[2], unname(pchisq(w, 1, lower.tail = FALSE)), tolerance = 1e-6)
  expect_null(getOption("datadist"))
})

test_that("lm_run_rcs matches lm with t-based contrasts and nested F test", {
  skip_if_not_installed("rms")
  d <- rcs_lung()
  dd <- d[!is.na(d$wt.loss), ]
  r <- quiet(lm_run_rcs(d, "wt.loss", "age", covars = "sex", ref = 60))
  expect_equal(names(r$result), c("x", "beta", "ll", "ul"))
  X <- Hmisc::rcspline.eval(dd$age, knots = r$knots, inclx = TRUE)
  fit <- lm(wt.loss ~ X + sex, data = dd)
  D <- sweep(Hmisc::rcspline.eval(r$result$x, knots = r$knots, inclx = TRUE), 2,
             Hmisc::rcspline.eval(60, knots = r$knots, inclx = TRUE))
  eta <- as.vector(D %*% coef(fit)[2:3])
  se <- sqrt(rowSums((D %*% vcov(fit)[2:3, 2:3]) * D))
  expect_equal(r$result$beta, eta, tolerance = 1e-8)
  expect_equal(r$result$ll, eta - qt(0.975, fit$df.residual) * se, tolerance = 1e-8)
  nested <- anova(lm(wt.loss ~ age + sex, data = dd), fit)
  expect_equal(r$tests$P[2], nested[2, "Pr(>F)"], tolerance = 1e-8)
})

test_that("clogit_run_rcs matches rms::cph + Predict for 1:M matched sets", {
  skip_if_not_installed("rms")
  d <- rcs_infert()
  r <- quiet(clogit_run_rcs(d, "case", "x", "stratum", covars = "spontaneous"))
  expect_equal(names(r$result), c("x", "or", "ll", "ul"))
  expect_equal(r$info$clogit_method, "exact")
  # 每个匹配组恰有 1 个病例时 Breslow 与精确条件似然相同
  expect_true(all(tapply(d$case, d$stratum, sum) == 1))
  rcs <- rms::rcs
  strat <- rms::strat
  kn <- r$knots
  d$t <- 1
  dd <- rms::datadist(d[c("x", "spontaneous", "stratum")])
  dd$limits["Adjust to", "x"] <- r$ref
  old <- options(datadist = dd)
  on.exit(options(old))
  fml <- as.formula(sprintf("Surv(t, case) ~ rcs(x, %s) + spontaneous + strat(stratum)",
                            paste(deparse(kn, control = "digits17"), collapse = "")))
  cp <- rms::cph(fml, data = d, x = TRUE, y = TRUE, eps = 1e-9, tol = 1e-12, method = "breslow")
  pick <- c(1, 250, 500)
  pp <- as.data.frame(rms::Predict(cp, x = r$result$x[pick], ref.zero = TRUE, fun = exp))
  expect_equal(r$result$or[pick], as.vector(pp$yhat), tolerance = 1e-6)
  expect_equal(r$result$ul[pick], as.vector(pp$upper), tolerance = 1e-6)
  expect_equal(r$tests$P, unname(anova(cp)[c("x", " Nonlinear"), "P"]), tolerance = 1e-6)
})

test_that("clogit_run_rcs rejects matched variables and robust variance with the exact method", {
  d <- rcs_infert()
  expect_error(suppressWarnings(quiet(clogit_run_rcs(d, "case", "age", "stratum"))), "age")
  expect_error(clogit_run_rcs(d, "case", "x", "stratum", covars = "cluster(stratum)", robust = TRUE))
  r <- quiet(clogit_run_rcs(d, "case", "x", "stratum", covars = "cluster(stratum)", robust = TRUE, method = "efron"))
  expect_equal(r$info$variance, "robust")
})

test_that("regression RCS functions forbid terms involving mainvar", {
  skip_if_not_installed("rms")
  d <- rcs_lung()
  expect_error(quiet(logit_run_rcs(d, "y", "age", covars = c("sex", "age:sex"))))
  expect_error(quiet(lm_run_rcs(d, "wt.loss", "age", covars = "log(age)")))
})

test_that("plot_crf draws OR and beta curves with the matching null line", {
  skip_if_not_installed("rms")
  font <- test_font()
  d <- rcs_lung()
  lo <- quiet(logit_run_rcs(d, "y", "age", covars = "sex"))
  path <- tempfile(fileext = ".pdf")
  f <- plot_crf(lo, path, font_family = font)
  expect_match(f$summary$source, "rcs, logit")
  expect_true(grepl("OddsRatio", gsub("\\s+", "", pdf_text(path)), fixed = TRUE))

  li <- quiet(lm_run_rcs(d, "wt.loss", "age"))
  path2 <- tempfile(fileext = ".pdf")
  f2 <- plot_crf(li, path2, font_family = font)
  expect_match(f2$summary$y_axis, "ticks=.*\\b0\\b")
  expect_true(grepl("Beta", pdf_text(path2), fixed = TRUE))
})
