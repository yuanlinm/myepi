library(survival)

make_bin <- function() {
  d <- make_lung()
  d$y <- as.integer(d$status == 2)
  d$ecog <- factor(d$ph.ecog)
  d
}

make_infert <- function() {
  d <- datasets::infert
  d$spont <- factor(d$spontaneous)
  d
}

test_that("logit_run matches glm with Wald and profile CIs", {
  d <- make_bin()
  r <- suppressWarnings(quiet(logit_run(d, outcome = "y", mainvar = "ecog", covars = c("age", "sex"))))
  dd <- d[!is.na(d$ecog), ]
  fit <- glm(y ~ ecog + age + sex, family = binomial, data = dd)
  idx <- c("ecog1", "ecog2", "ecog3")
  expect_equal(r$result$OR[-1], unname(exp(coef(fit))[idx]))
  expect_equal(r$result$OR_lower[-1], unname(exp(confint.default(fit))[idx, 1]))
  expect_equal(r$result$P[-1], unname(summary(fit)$coefficients[idx, "Pr(>|z|)"]))
  expect_equal(r$result$N_event, as.vector(tapply(dd$y, dd$ecog, sum)))
  expect_equal(r$result$N_total, as.vector(table(dd$ecog)))
  expect_true(r$result$Reference[1])

  p <- quiet(logit_run(d, "y", "age", covars = "sex", ci_method = "profile"))
  fp <- glm(y ~ age + sex, family = binomial, data = d)
  expect_equal(c(p$result$OR_lower, p$result$OR_upper), unname(exp(suppressMessages(confint(fp)))["age", ]))
  expect_error(logit_run(d, "y", "age", ci_method = "profile", robust = TRUE))
})

test_that("logit_run accepts logical and two-level factor outcomes", {
  d <- make_bin()
  d$y_lgl <- d$y == 1
  d$y_fac <- factor(ifelse(d$y == 1, "dead", "alive"), levels = c("alive", "dead"))
  base <- quiet(logit_run(d, "y", "age"))$result
  expect_equal(quiet(logit_run(d, "y_lgl", "age"))$result$OR, base$OR)
  expect_equal(quiet(logit_run(d, "y_fac", "age"))$result$OR, base$OR)
  d$y_bad <- d$ph.ecog
  expect_error(quiet(logit_run(d, "y_bad", "age")))
})

test_that("logit_run robust variance uses sandwich only when requested", {
  skip_if_not_installed("sandwich")
  d <- make_bin()
  d <- d[!is.na(d$inst), ]
  fit <- glm(y ~ age + sex, family = binomial, data = d)
  rc <- quiet(logit_run(d, "y", "age", covars = c("sex", "cluster(inst)"), robust = TRUE))
  expect_equal(rc$result$se, unname(sqrt(diag(sandwich::vcovCL(fit, cluster = d$inst)))["age"]))
  rh <- quiet(logit_run(d, "y", "age", covars = "sex", robust = TRUE))
  expect_equal(rh$result$se, unname(sqrt(diag(sandwich::vcovHC(fit)))["age"]))
  expect_message(rm <- logit_run(d, "y", "age", covars = c("sex", "cluster(inst)")))
  expect_equal(rm$info$variance, "model")
  expect_equal(rm$result$se, unname(sqrt(diag(vcov(fit)))["age"]))
})

test_that("lm_run matches lm with t-based inference and reports group means", {
  d <- make_bin()
  r <- quiet(lm_run(d, outcome = "wt.loss", mainvar = "ecog", covars = "age"))
  dd <- d[!is.na(d$ecog) & !is.na(d$wt.loss), ]
  fit <- lm(wt.loss ~ ecog + age, data = dd)
  idx <- c("ecog1", "ecog2", "ecog3")
  expect_equal(r$result$beta[-1], unname(coef(fit)[idx]))
  expect_equal(r$result$lower[-1], unname(confint(fit)[idx, 1]))
  expect_equal(r$result$P[-1], unname(summary(fit)$coefficients[idx, "Pr(>|t|)"]))
  expect_equal(r$result$Mean, as.vector(tapply(dd$wt.loss, dd$ecog, mean)))
  expect_equal(r$result$N, as.vector(table(dd$ecog)))
  expect_equal(r$info$r_squared, summary(fit)$r.squared)

  skip_if_not_installed("sandwich")
  rr <- quiet(lm_run(d, "wt.loss", "age", robust = TRUE))
  f2 <- lm(wt.loss ~ age, data = d)
  se <- sqrt(diag(sandwich::vcovHC(f2)))["age"]
  expect_equal(rr$result$se, unname(se))
  expect_equal(rr$result$P, unname(2 * pt(-abs(coef(f2)["age"] / se), f2$df.residual)))
})

test_that("clogit_run matches survival::clogit and reports matched sets", {
  d <- make_infert()
  r <- quiet(clogit_run(d, outcome = "case", mainvar = "spont", match_id = "stratum", covars = "induced"))
  fit <- clogit(case ~ spont + induced + strata(stratum), data = d)
  expect_equal(r$result$OR[-1], unname(exp(coef(fit))[c("spont1", "spont2")]))
  expect_equal(r$result$OR_lower[-1], unname(exp(confint(fit))[c("spont1", "spont2"), 1]))
  expect_equal(r$result$N_case, as.vector(tapply(d$case == 1, d$spont, sum)))
  expect_equal(r$result$N_control, as.vector(tapply(d$case == 0, d$spont, sum)))
  expect_equal(r$info$n_sets, length(unique(d$stratum)))
  expect_equal(r$info$method, "exact")

  d2 <- d[!(d$stratum == 1 & d$case == 1), ]
  expect_message(r2 <- clogit_run(d2, "case", "spont", "stratum"), "83")
  expect_equal(r2$info$n_informative_sets, 82L)
})

test_that("clogit_run robust variance requires a non-exact method", {
  d <- make_infert()
  expect_error(clogit_run(d, "case", "spont", "stratum", covars = c("induced", "cluster(stratum)"), robust = TRUE))
  r <- quiet(clogit_run(d, "case", "spont", "stratum", covars = c("induced", "cluster(stratum)"), robust = TRUE,
                        method = "approximate"))
  fit <- clogit(case ~ spont + induced + strata(stratum) + cluster(stratum), data = d, method = "approximate")
  expect_equal(r$result$se[-1], unname(sqrt(diag(fit$var))[1:2]))
  expect_equal(r$info$variance, "robust")
  expect_error(quiet(clogit_run(d, "case", "spont", "stratum", covars = "strata(education)")))
})

test_that("plot_forest detects OR and beta measures", {
  font <- test_font()
  d <- make_bin()
  d$ecog <- factor(pmin(d$ph.ecog, 2))
  lo <- quiet(logit_run(d, "y", "ecog", covars = "age"))
  path <- tempfile(fileext = ".pdf")
  f <- plot_forest(lo, file = path, font_family = font, count_cols = c(Events = "N_event", Total = "N_total"))
  expect_match(f$summary$axis, "null=1")
  txt <- pdf_text(path)
  expect_true(grepl("Odds", txt, fixed = TRUE))

  li <- quiet(lm_run(d, "wt.loss", "ecog"))
  path2 <- tempfile(fileext = ".pdf")
  f2 <- plot_forest(li, file = path2, font_family = font)
  expect_match(f2$summary$axis, "null=0")
  expect_true(grepl("0.00 (Ref)", pdf_text(path2), fixed = TRUE))

  expect_error(plot_forest(list(lo, li), file = tempfile(fileext = ".pdf"), font_family = font))
})

test_that("logit_run warns about separation", {
  d <- make_bin()
  expect_warning(r <- quiet(logit_run(d, "y", "ecog", covars = "age")), "ecog = 3")
  expect_true(r$info$separation)
  set.seed(1)
  sep <- data.frame(x = c(rnorm(50, -2), rnorm(50, 2)), y = rep(0:1, each = 50))
  sep$y[sep$x < 0] <- 0L
  sep$y[sep$x > 0] <- 1L
  expect_warning(quiet(logit_run(sep, "y", "x")), "glm")
  d$ecog2 <- factor(pmin(d$ph.ecog, 2))
  expect_no_warning(r2 <- quiet(logit_run(d, "y", "ecog2", covars = "age")))
  expect_false(r2$info$separation)
})
