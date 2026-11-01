library(survival)

sq_lung <- function() {
  d <- make_lung()
  d$y <- as.integer(d$status == 2)
  d$sex_f <- factor(d$sex, labels = c("M", "F"))
  d$ecog2 <- factor(pmin(d$ph.ecog, 2))
  d
}

sq_infert <- function() {
  d <- datasets::infert
  set.seed(7)
  d$x <- rnorm(nrow(d), 25, 5) + 3 * d$case
  d$spont <- factor(d$spontaneous)
  d
}

test_that("logit_run_sub fits each subgroup and matches the glm likelihood ratio test", {
  d <- sq_lung()
  s <- quiet(logit_run_sub(d, "sex_f", "y", "age", covars = "ph.karno"))
  dd <- d[!is.na(d$ph.karno), ]
  for (lev in c("M", "F")) {
    fit <- glm(y ~ age + ph.karno, family = binomial, data = dd[dd$sex_f == lev, ])
    expect_equal(s$result$OR[s$result$Subgroup == paste0("sex_f: ", lev)], unname(exp(coef(fit))["age"]))
  }
  f0 <- glm(y ~ age + ph.karno + sex_f, family = binomial, data = dd)
  f1 <- glm(y ~ age + ph.karno + sex_f + age:sex_f, family = binomial, data = dd)
  expect_equal(s$interaction$P, anova(f0, f1, test = "LRT")[2, "Pr(>Chi)"])
  expect_equal(s$interaction$test, "LRT")
  h <- cox_het(s)$result
  expect_equal(h$k, 2L)
})

test_that("logit_run_sub uses a robust Wald interaction test when requested", {
  skip_if_not_installed("sandwich")
  d <- sq_lung()
  s <- quiet(logit_run_sub(d, "sex_f", "y", "age", robust = TRUE))
  f1 <- glm(y ~ age + sex_f + age:sex_f, family = binomial, data = d)
  V <- sandwich::vcovHC(f1)
  w <- coef(f1)["age:sex_fF"]^2 / V["age:sex_fF", "age:sex_fF"]
  expect_equal(s$interaction$P, unname(pchisq(w, 1, lower.tail = FALSE)))
  expect_equal(s$interaction$test, "Wald")
})

test_that("lm_run_sub matches the nested F test, robust Wald F and subgroup fits", {
  d <- sq_lung()
  s <- quiet(lm_run_sub(d, "sex_f", "wt.loss", "ecog2", covars = "age"))
  dd <- d[!is.na(d$wt.loss) & !is.na(d$ecog2), ]
  f0 <- lm(wt.loss ~ ecog2 + age + sex_f, data = dd)
  f1 <- lm(wt.loss ~ ecog2 + age + sex_f + ecog2:sex_f, data = dd)
  expect_equal(s$interaction$P, anova(f0, f1)[2, "Pr(>F)"])
  sub_m <- lm(wt.loss ~ ecog2 + age, data = dd[dd$sex_f == "M", ])
  rows <- s$result$Subgroup == "sex_f: M" & !s$result$Reference
  expect_equal(s$result$beta[rows], unname(coef(sub_m)[c("ecog21", "ecog22")]))
  expect_equal(s$result$Mean[s$result$Subgroup == "sex_f: M"], as.vector(tapply(dd$wt.loss[dd$sex_f == "M"], dd$ecog2[dd$sex_f == "M"], mean)))

  skip_if_not_installed("sandwich")
  r <- quiet(lm_run_sub(d, "sex_f", "wt.loss", "age", robust = TRUE))
  g1 <- lm(wt.loss ~ age + sex_f + age:sex_f, data = d[!is.na(d$wt.loss), ])
  V <- sandwich::vcovHC(g1)
  fstat <- coef(g1)["age:sex_fF"]^2 / V["age:sex_fF", "age:sex_fF"]
  expect_equal(r$interaction$P, unname(pf(fstat, 1, g1$df.residual, lower.tail = FALSE)))
  expect_equal(r$interaction$test, "Wald F")
})

test_that("clogit_run_sub handles a matching factor and rejects groups that split matched sets", {
  d <- sq_infert()
  expect_message(s <- clogit_run_sub(d, "education", "case", "x", "stratum"), "education")
  f0 <- clogit(case ~ x + strata(stratum), data = d)
  f1 <- clogit(case ~ x + x:education + strata(stratum), data = d)
  expect_equal(s$interaction$P, anova(f0, f1)[2, "Pr(>|Chi|)"])
  sub <- clogit(case ~ x + strata(stratum), data = d[d$education == "6-11yrs", ])
  expect_equal(s$result$OR[s$result$Subgroup == "education: 6-11yrs"], unname(exp(coef(sub))))
  expect_error(quiet(clogit_run_sub(d, "spont", "case", "x", "stratum")), "logit_run_sub")
})

test_that("regression _q functions match direct fits on the same groups", {
  d <- sq_lung()
  q <- quiet(logit_run_q(d, "y", "age", q = 3, covars = "sex", trend = TRUE))
  br <- quantile(d$age, 0:3 / 3)
  g <- cut(d$age, br, include.lowest = TRUE)
  fit <- glm(y ~ g + sex, family = binomial, data = d)
  expect_equal(q$result$OR[-1], unname(exp(coef(fit))[2:3]))
  expect_equal(q$result$N_total, as.vector(table(g)))
  d$score <- tapply(d$age, g, median)[as.integer(g)]
  ft <- glm(y ~ score + sex, family = binomial, data = d)
  expect_equal(q$trend$P, summary(ft)$coefficients["score", "Pr(>|z|)"])
  expect_equal(attr(q$result, "quantile_breaks"), unname(br))

  l <- quiet(lm_run_q(d, "wt.loss", "age", q = c(30, 60, 70, 90)))
  dl <- d[!is.na(d$wt.loss), ]
  gl <- cut(dl$age, c(30, 60, 70, 90), include.lowest = TRUE)
  fl <- lm(wt.loss ~ gl, data = dl)
  expect_equal(l$result$beta[-1], unname(coef(fl)[2:3]))
  expect_equal(l$result$P[-1], unname(summary(fl)$coefficients[2:3, "Pr(>|t|)"]))

  di <- sq_infert()
  cq <- quiet(clogit_run_q(di, "case", "x", "stratum", q = 3, trend = TRUE, trend_method = "ordinal"))
  gi <- cut(di$x, quantile(di$x, 0:3 / 3), include.lowest = TRUE)
  fc <- clogit(case ~ gi + strata(stratum), data = di)
  expect_equal(cq$result$OR[-1], unname(exp(coef(fc))))
  di$o <- as.integer(gi)
  expect_equal(cq$trend$P, summary(clogit(case ~ o + strata(stratum), data = di))$coefficients["o", "Pr(>|z|)"])
})

test_that("plot_forest and cox_het accept regression _sub and _q results", {
  font <- test_font()
  d <- sq_lung()
  s1 <- quiet(logit_run_sub(d, "sex_f", "y", "ph.karno"))
  d$old <- factor(ifelse(d$age >= 65, "65+", "<65"))
  s2 <- quiet(logit_run_sub(d, "old", "y", "ph.karno"))
  path <- tempfile(fileext = ".pdf")
  f <- plot_forest(list(s1, s2), file = path, font_family = font)
  expect_match(f$summary$rows, "display_rows=6")
  expect_true(grepl("interaction", pdf_text(path), fixed = TRUE))

  q <- quiet(lm_run_q(d, "wt.loss", "age", q = 3, trend = TRUE))
  path2 <- tempfile(fileext = ".pdf")
  f2 <- plot_forest(q, file = path2, font_family = font)
  txt <- pdf_text(path2)
  expect_true(grepl("0.00 (Ref)", txt, fixed = TRUE))
  expect_true(grepl("trend", txt, fixed = TRUE))
  expect_match(f2$summary$axis, "null=0")
})
