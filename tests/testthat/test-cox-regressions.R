library(survival)

test_that("numeric-labelled factor levels map to the right coefficients and counts (V1)", {
  d <- make_lung()
  d$grp <- factor(d$ph.ecog)
  res <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "grp"))$result
  fit <- coxph(Surv(time, ev) ~ grp, data = d)
  dd <- d[!is.na(d$grp), ]

  expect_equal(res$Interval, levels(d$grp))
  expect_equal(res$Reference, c(TRUE, FALSE, FALSE, FALSE))
  expect_equal(res$HR[-1], unname(exp(coef(fit))))
  expect_equal(res$N_event, as.vector(tapply(dd$ev, dd$grp, sum)))
  expect_equal(res$N_total, as.vector(table(dd$grp)))
})

test_that("descriptive counts use the model's analytic sample (V2)", {
  d <- make_lung()
  r <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age", covars = c("sex", "ph.karno", "wt.loss")))
  expect_equal(r$result$N_total, r$model$n)
  expect_equal(r$result$N_event, r$model$nevent)
  expect_message(r2 <- cox_run(d, timediff = "time", event = "ev", mainvar = "age", covars = "wt.loss"))
  expect_equal(r2$info$n_dropped, 14L)
})

test_that("event coding follows Surv, including 1/2 coding (V3)", {
  d <- make_lung()
  r12 <- quiet(cox_run(d, timediff = "time", event = "status", mainvar = "age"))
  r01 <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age"))
  expect_equal(r12$result$N_event, r12$model$nevent)
  expect_equal(r12$result[, c("N_event", "HR", "P")], r01$result[, c("N_event", "HR", "P")])
})

test_that("... is passed to coxph (V4)", {
  d <- make_lung()
  r <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age", ties = "breslow"))
  expect_equal(r$model$method, "breslow")
  q <- quiet(cox_run_q(d, mainvar = "age", q = 3, timediff = "time", event = "ev", ties = "breslow"))
  expect_equal(q$model$main$method, "breslow")
})

test_that("cox_run_sub excludes missing group rows and reports them (V5)", {
  d <- make_lung()
  d$g <- d$sex
  d$g[1:20] <- NA
  expect_message(s <- cox_run_sub(d, group_var = "g", timediff = "time", event = "ev", mainvar = "age"))
  expect_equal(s$info$n_group_na, 20L)
  keep <- !is.na(d$g)
  expect_equal(s$result$N_total, as.vector(table(d$g[keep])))
  s_keep <- quiet(cox_run_sub(d, group_var = "g", timediff = "time", event = "ev", mainvar = "age", group_na = "keep"))
  expect_equal(s_keep$result$Subgroup, c("g: 1", "g: 2", "g: (Missing)"))
  expect_equal(sum(s_keep$result$N_total), nrow(d))
})

test_that("cox_run_sub keeps per-level counts for categorical exposures (V6)", {
  d <- make_lung()
  d$ecog2 <- factor(ifelse(d$ph.ecog >= 1, "ge1", "zero"), levels = c("zero", "ge1"))
  s <- quiet(cox_run_sub(d, group_var = "sex", timediff = "time", event = "ev", mainvar = "ecog2"))$result
  dd <- d[!is.na(d$ecog2), ]
  expected <- as.vector(t(table(dd$sex, dd$ecog2)))
  expect_equal(s$N_total, expected)
})

test_that("cox_run_sub interaction P equals the likelihood ratio test from anova", {
  d <- make_lung()
  d$ecog2 <- factor(ifelse(d$ph.ecog >= 1, "ge1", "zero"), levels = c("zero", "ge1"))
  s <- quiet(cox_run_sub(d, group_var = "sex", timediff = "time", event = "ev", mainvar = "ecog2", covars = "age"))
  dd <- d[!is.na(d$ecog2), ]
  dd$sex <- factor(dd$sex)
  f0 <- coxph(Surv(time, ev) ~ ecog2 + age + sex, data = dd)
  f1 <- coxph(Surv(time, ev) ~ ecog2 + age + sex + ecog2:sex, data = dd)
  expect_equal(s$interaction$P, anova(f0, f1)[2, "Pr(>|Chi|)"])
  expect_true(all(s$result$P_interaction == s$interaction$P))
})

test_that("cox_run_sub drops covariate terms involving group_var", {
  d <- make_lung()
  expect_message(
    s <- cox_run_sub(d, group_var = "sex", timediff = "time", event = "ev", mainvar = "age", covars = c("ph.ecog", "strata(sex)"))
  )
  expect_false(any(grepl("strata", deparse(formula(s$model$subgroups[[1]])), fixed = TRUE)))
  expect_equal(nrow(s$result), 2L)
})

test_that("cox_het excludes reference rows and pools per level (V7)", {
  d <- make_lung()
  d$ecog2 <- factor(ifelse(d$ph.ecog >= 1, "ge1", "zero"), levels = c("zero", "ge1"))
  s <- quiet(cox_run_sub(d, group_var = "sex", timediff = "time", event = "ev", mainvar = "ecog2"))
  h <- cox_het(s)$result
  est <- s$result[!s$result$Reference, ]
  w <- 1 / est$se^2
  q_manual <- sum(w * (est$beta - sum(w * est$beta) / sum(w))^2)
  expect_equal(nrow(h), 1L)
  expect_equal(h$k, 2L)
  expect_equal(h$Q, q_manual)
})

test_that("CI and P use the same variance, selectable by robust (V8)", {
  d <- make_lung()
  d <- d[!is.na(d$inst), ]
  fit <- coxph(Surv(time, ev) ~ age + cluster(inst), data = d)
  r_auto <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age", covars = "cluster(inst)"))
  r_model <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age", covars = "cluster(inst)", robust = FALSE))
  expect_equal(r_auto$info$variance, "robust")
  expect_equal(r_model$info$variance, "model")
  expect_equal(c(r_auto$result$HR_lower, r_auto$result$HR_upper), unname(exp(confint(fit))[1, ]))
  expect_equal(r_model$result$se, sqrt(fit$naive.var)[1, 1])
  expect_equal(r_auto$result$P, summary(fit)$coefficients[1, "Pr(>|z|)"])

  q <- quiet(cox_run_q(d, mainvar = "age", q = 3, timediff = "time", event = "ev", covars = "cluster(inst)"))
  br <- quantile(d$age, 0:3 / 3)
  fq <- coxph(Surv(time, ev) ~ cut(age, br, include.lowest = TRUE) + cluster(inst), data = d)
  expect_equal(q$result$HR_lower[-1], unname(exp(confint(fq))[, 1]))
  expect_equal(q$result$P[-1], unname(summary(fq)$coefficients[, "Pr(>|z|)"]))
  expect_true(is.na(q$result$P[1]))
})

test_that("robust = TRUE clusters on idL for long-format data", {
  d <- make_lung()
  long <- rbind(transform(d, t1 = 0, t2 = time / 2, ev = 0L), transform(d, t1 = time / 2, t2 = time))
  r <- quiet(cox_run(long, time1 = "t1", time2 = "t2", event = "ev", mainvar = "age", idL = "id", robust = TRUE))
  fit <- coxph(Surv(t1, t2, ev) ~ age + cluster(id), data = long)
  expect_equal(r$result$se, sqrt(fit$var)[1, 1])
})

test_that("tied quantiles are merged with a warning (V9)", {
  d <- make_lung()
  expect_warning(q <- quiet(cox_run_q(d, mainvar = "ph.ecog", q = 4, timediff = "time", event = "ev")))
  expect_equal(q$breaks, c(0, 1, 3))
  expect_equal(nrow(q$result), 2L)
})

test_that("cox_run_q reports group sizes and real breaks", {
  d <- make_lung()
  expect_message(q <- cox_run_q(d, mainvar = "age", q = 3, timediff = "time", event = "ev"))
  expect_equal(attr(q$result, "quantile_breaks"), unname(quantile(d$age, 0:3 / 3)))
  expect_equal(sum(q$result$N_total), nrow(d))
})

test_that("cox_run_q trend test matches a direct coxph fit", {
  d <- make_lung()
  q <- quiet(cox_run_q(d, mainvar = "age", q = 3, timediff = "time", event = "ev", covars = "sex", trend = TRUE))
  br <- quantile(d$age, 0:3 / 3)
  g <- cut(d$age, br, include.lowest = TRUE)
  d$score <- tapply(d$age, g, median)[as.integer(g)]
  fit <- coxph(Surv(time, ev) ~ score + sex, data = d)
  expect_equal(q$trend$P, summary(fit)$coefficients["score", "Pr(>|z|)"])
  expect_equal(q$result$P_trend[1], q$trend$P)
})

test_that("count_na handles missing values in the grouping variable (V10)", {
  d <- make_lung()
  d$g <- d$sex
  d$g[1:20] <- NA
  res <- count_na(d, group_var = "g")
  wl <- res[res$Variable == "wt.loss", ]
  expect_equal(wl$Group, c("1", "2", NA))
  expect_equal(wl$Missing, c(sum(is.na(d$wt.loss[which(d$g == 1)])), sum(is.na(d$wt.loss[which(d$g == 2)])), sum(is.na(d$wt.loss[1:20]))))
  expect_equal(sum(wl$N), nrow(d))
})

test_that("count_na reports overall missingness in decreasing order", {
  d <- make_lung()
  res <- count_na(d)
  expect_equal(res$Missing, sort(colSums(is.na(d))[colSums(is.na(d)) > 0], decreasing = TRUE), ignore_attr = TRUE)
  expect_message(none <- count_na(d[c("time", "sex")]))
  expect_equal(nrow(none), 0L)
})

test_that("interaction terms are not mistaken for exposure levels (V11)", {
  d <- make_lung()
  d$ecog2 <- factor(ifelse(d$ph.ecog >= 1, "ge1", "zero"), levels = c("zero", "ge1"))
  res <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "ecog2", covars = c("sex", "ecog2:sex")))$result
  expect_equal(res$Interval, c("zero", "ge1"))
})

test_that("non-syntactic names and expression covariates are supported (V12)", {
  d <- make_lung()
  d$`my var` <- d$age
  r <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "my var", covars = "pspline(wt.loss, df = 3)"))
  fit <- coxph(Surv(time, ev) ~ age + pspline(wt.loss, df = 3), data = d)
  expect_equal(r$result$HR, unname(exp(coef(fit))["age"]))
})

test_that("results keep full precision and functions do not print (V13)", {
  d <- make_lung()
  fit <- coxph(Surv(time, ev) ~ age, data = d)
  expect_silent(r <- suppressMessages(cox_run(d, timediff = "time", event = "ev", mainvar = "age")))
  expect_identical(r$result$beta, unname(coef(fit)))
  expect_s3_class(r, "myepi_result")
  expect_output(print(r), "HR")
})

test_that("person-years and incidence CI follow survival::pyears and cipoisson", {
  d <- make_lung()
  r <- quiet(cox_run(d, timediff = "time", event = "ev", mainvar = "age"))$result
  py <- sum(d$time) / 365.25
  expect_equal(r$Person_years, py)
  expect_equal(c(r$Incidence_lower, r$Incidence_upper), unname(cipoisson(sum(d$ev), time = py)) * 1e5)
})

test_that("invalid survival outcomes are rejected", {
  d <- make_lung()
  d$bad <- d$ev
  d$bad[1] <- 3
  expect_error(quiet(cox_run(d, timediff = "time", event = "bad", mainvar = "age")))
  d$t_neg <- d$time
  d$t_neg[1] <- -1
  expect_error(quiet(cox_run(d, timediff = "t_neg", event = "ev", mainvar = "age")))
})
