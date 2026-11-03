library(survival)

add_sim <- function(n = 3000, seed = 11) {
  set.seed(seed)
  prs <- rnorm(n)
  life <- rnorm(n, 10, 2)            # 生活方式评分：越高越健康
  ga <- cut(prs, quantile(prs, 0:3 / 3), include.lowest = TRUE, labels = FALSE)
  gb <- 4L - cut(life, quantile(life, 0:3 / 3), include.lowest = TRUE, labels = FALSE)
  lp <- 0.35 * (ga - 1) + 0.3 * (gb - 1) + 0.25 * (ga == 3 & gb == 3)
  t <- rexp(n, 0.01 * exp(lp))
  cens <- runif(n, 5, 40)
  data.frame(id = seq_len(n), time = pmin(t, cens), ev = as.integer(t <= cens), prs = prs, life = life,
             age = rnorm(n, 55, 8), y = rbinom(n, 1, plogis(-3.2 + lp)))
}

num_se <- function(b, V, f, h = 1e-6) {
  g <- vapply(seq_along(b), function(k) {
    up <- b; dn <- b
    up[k] <- up[k] + h; dn[k] <- dn[k] - h
    (f(up) - f(dn)) / (2 * h)
  }, numeric(1))
  sqrt(drop(t(g) %*% V %*% g))
}

test_that("cox_run_add matches the product-term model and analytic delta gradients", {
  d <- make_lung()
  d$poor <- as.integer(d$ph.ecog >= 1)
  d$old <- as.integer(d$age >= 65)
  r <- quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "poor", var_b = "old", covars = "sex"))
  f <- coxph(Surv(time, ev) ~ poor * old + sex, data = d)
  b <- coef(f)
  e10 <- exp(b[["poor"]]); e01 <- exp(b[["old"]]); e11 <- exp(b[["poor"]] + b[["old"]] + b[["poor:old"]])
  a <- r$additive
  expect_equal(a$estimate[a$measure == "RERI"], e11 - e10 - e01 + 1)
  expect_equal(a$estimate[a$measure == "AP"], (e11 - e10 - e01 + 1) / e11)
  expect_equal(a$estimate[a$measure == "multiplicative"], exp(b[["poor:old"]]))
  expect_equal(sum(a$estimate[a$measure %in% c("prop_a", "prop_b", "prop_interaction")]), 1)

  # 联合分组模型的系数与协方差，数值梯度核对 delta 法 SE
  dd <- d[!is.na(d$poor), ]
  dd$j <- factor(paste(dd$poor, dd$old), levels = c("0 0", "1 0", "0 1", "1 1"))
  fj <- coxph(Surv(time, ev) ~ j + sex, data = dd)
  bj <- coef(fj)[c("j1 0", "j0 1", "j1 1")]
  Vj <- vcov(fj)[names(bj), names(bj)]
  reri <- function(x) exp(x[3]) - exp(x[1]) - exp(x[2]) + 1
  se_num <- num_se(bj, Vj, reri)
  z <- qnorm(0.975)
  expect_equal(a$upper[a$measure == "RERI"] - a$estimate[a$measure == "RERI"], z * se_num, tolerance = 1e-5)
  ap <- function(x) reri(x) / exp(x[3])
  expect_equal(a$upper[a$measure == "AP"] - a$estimate[a$measure == "AP"], z * num_se(bj, Vj, ap), tolerance = 1e-5)
  pa <- function(x) (exp(x[1]) - 1) / (exp(x[3]) - 1)
  expect_equal(a$upper[a$measure == "prop_a"] - a$estimate[a$measure == "prop_a"], z * num_se(bj, Vj, pa), tolerance = 1e-5)
})

test_that("continuous factors are grouped on the analytic sample with the declared risk direction", {
  d <- add_sim()
  r <- quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", risk_b = "lower", covars = "age"))
  qa <- quantile(d$prs, 0:3 / 3)
  expect_equal(r$info$levels_a[1], levels(cut(d$prs, qa, include.lowest = TRUE, dig.lab = 4))[1])
  # 生活方式越高越健康：参照为最高三分位
  ql <- levels(cut(d$life, quantile(d$life, 0:3 / 3), include.lowest = TRUE, dig.lab = 4))
  expect_equal(r$info$levels_b, rev(ql))
  expect_equal(nrow(r$result), 9L)
  expect_equal(sum(r$additive$primary), 7L)
  expect_equal(unique(r$additive$contrast[r$additive$primary]), sprintf("prs %s & life %s", r$info$levels_a[3], r$info$levels_b[3]))
  expect_equal(nrow(r$additive), 4L * 7L)
  # 3 × 3 中 (2, 3) 组合的 RERI 与联合模型系数一致
  b <- log(r$result$HR)
  pos <- function(i, j) (i - 1) * 3 + j
  reri23 <- exp(b[pos(2, 3)]) - exp(b[pos(2, 1)]) - exp(b[pos(1, 3)]) + 1
  sel <- r$additive$level_a == r$info$levels_a[2] & r$additive$level_b == r$info$levels_b[3] & r$additive$measure == "RERI"
  expect_equal(r$additive$estimate[sel], reri23)

  r2 <- quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", cut_a = c(-0.5, 0.5),
                          q_b = 2, risk_b = "lower", contrast = c(2, 2)))
  expect_equal(r2$info$levels_a, c("<=-0.5", "(-0.5,0.5]", ">0.5"))
  expect_true(all(r2$additive$primary[r2$additive$level_a == "(-0.5,0.5]"]))
})

test_that("bootstrap follows the reference-repository approach and is reproducible", {
  d <- add_sim(n = 1500)
  r1 <- quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", q_a = 2, q_b = 2,
                          risk_b = "lower", ci_method = "boot", B = 60, seed = 2026))
  r2 <- quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", q_a = 2, q_b = 2,
                          risk_b = "lower", ci_method = "boot", B = 60, seed = 2026))
  expect_equal(r1$additive, r2$additive)
  expect_equal(r1$boot$B_valid, 60L)
  expect_true(all(is.na(r1$additive$P)))

  # 与参考仓库相同：在固定的联合分组上逐次重抽样、重拟合 Cox 模型，取百分位数
  set.seed(2026)
  ga <- cut(d$prs, quantile(d$prs, 0:2 / 2), include.lowest = TRUE, labels = FALSE)
  gb <- 3L - cut(d$life, quantile(d$life, 0:2 / 2), include.lowest = TRUE, labels = FALSE)
  d$j <- factor(paste(ga, gb), levels = c("1 1", "1 2", "2 1", "2 2"))
  reri <- replicate(60, {
    db <- d[sample.int(nrow(d), nrow(d), replace = TRUE), ]
    b <- coef(coxph(Surv(time, ev) ~ j, data = db))
    exp(b[["j2 2"]]) - exp(b[["j2 1"]]) - exp(b[["j1 2"]]) + 1
  })
  a <- r1$additive[r1$additive$measure == "RERI", ]
  expect_equal(c(a$lower, a$upper), unname(quantile(reri, c(0.025, 0.975))))
  expect_false(exists("j_unused", inherits = FALSE))
})

test_that("preventive single exposures and common logistic outcomes trigger warnings", {
  d <- add_sim()
  expect_warning(quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", q_a = 2, q_b = 2)),
                 "Knol")
  d$common <- rbinom(nrow(d), 1, 0.4)
  warns <- character()
  withCallingHandlers(quiet(logit_run_add(d, "common", "prs", "life", q_a = 2, q_b = 2, risk_b = "lower")),
                      warning = function(w) {
                        warns <<- c(warns, conditionMessage(w))
                        invokeRestart("muffleWarning")
                      })
  expect_true(any(grepl("10%", warns, fixed = TRUE)))
})

test_that("logit_run_add and clogit_run_add match their joint-group models", {
  d <- add_sim()
  r <- quiet(logit_run_add(d, "y", "prs", "life", q_a = 2, q_b = 2, risk_b = "lower", covars = "age"))
  ga <- cut(d$prs, quantile(d$prs, 0:2 / 2), include.lowest = TRUE, labels = FALSE)
  gb <- 3L - cut(d$life, quantile(d$life, 0:2 / 2), include.lowest = TRUE, labels = FALSE)
  d$j <- factor(paste(ga, gb), levels = c("1 1", "1 2", "2 1", "2 2"))
  f <- glm(y ~ j + age, family = binomial, data = d)
  b <- coef(f)
  expect_equal(r$additive$estimate[r$additive$measure == "RERI"],
               unname(exp(b["j2 2"]) - exp(b["j2 1"]) - exp(b["j1 2"]) + 1))
  expect_equal(r$info$measure, "OR")

  di <- datasets::infert
  set.seed(5)
  di$x <- rnorm(nrow(di)) + 0.6 * di$case
  di$z <- rbinom(nrow(di), 1, 0.4 + 0.2 * di$case)
  rc <- quiet(clogit_run_add(di, "case", "x", "z", "stratum", q_a = 2, ci_method = "boot", B = 50, seed = 1))
  expect_match(rc$boot$unit, "stratum")
  ga <- cut(di$x, quantile(di$x, 0:2 / 2), include.lowest = TRUE, labels = FALSE)
  di$j <- factor(paste(ga, di$z), levels = c("1 0", "1 1", "2 0", "2 1"))
  fc <- clogit(case ~ j + strata(stratum), data = di)
  bc <- coef(fc)
  expect_equal(rc$additive$estimate[rc$additive$measure == "RERI"],
               unname(exp(bc["j2 1"]) - exp(bc["j2 0"]) - exp(bc["j1 1"]) + 1))
  expect_error(quiet(clogit_run_add(di, "case", "age", "z", "stratum", q_a = 2)), "匹配")
})

test_that("additive-interaction input checks", {
  d <- add_sim(n = 500)
  expect_error(quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", covars = "prs:age")))
  expect_error(quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", cut_a = 0, q_a = 2)))
  expect_error(quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", contrast = c(1, 3))),
               "contrast")
})

test_that("plot_forest draws the joint-effect table", {
  font <- test_font()
  d <- add_sim()
  r <- suppressWarnings(quiet(cox_run_add(d, timediff = "time", event = "ev", var_a = "prs", var_b = "life", risk_b = "lower")))
  path <- tempfile(fileext = ".pdf")
  f <- plot_forest(r, file = path, font_family = font, width_mm = 190)
  expect_match(f$summary$rows, "displayed=9")
})
