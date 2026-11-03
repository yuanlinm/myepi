# 加性交互：联合分组模型，RERI、AP、S、两因素贡献分解（VanderWeele & Tchetgen Tchetgen 2014）、乘法交互与分层效应

# ---- 因素分组：连续变量按界值或分位数分组；按风险方向排序，使第一个水平为低风险参照 ----

.add_group <- function(x, name, cut, q, risk, levels, basis, ids = NULL) {
  if (!is.null(levels) || is.factor(x) || is.character(x) || is.logical(x)) {
    if (!is.null(cut) || !is.null(q)) stop(sprintf("%s 为分类变量，不使用 cut / q", name))
    vals <- as.character(x)
    lev <- if (!is.null(levels)) as.character(levels) else if (is.factor(x)) levels(droplevels(x)) else if (is.logical(x)) {
      c("FALSE", "TRUE")
    } else {
      sort(unique(vals))
    }
    if (!all(vals %in% lev)) stop(sprintf("%s 存在不在 levels 中的取值: %s", name, paste(setdiff(unique(vals), lev), collapse = ", ")))
    lev <- lev[lev %in% vals]
    rule <- if (is.null(levels)) "按现有水平顺序视为低 → 高风险（可用 levels 参数指定）" else "按 levels 指定的低 → 高风险顺序"
    return(list(group = factor(vals, levels = lev), rule = rule))
  }
  if (!is.numeric(x)) stop(sprintf("%s 必须为数值型或分类变量", name))
  if (!is.null(cut) && !is.null(q)) stop(sprintf("%s 的 cut 与 q 只能二选一", name))
  if (is.null(cut) && is.null(q) && length(unique(x)) <= 5L) {
    lev <- sort(unique(x))
    if (risk == "lower") lev <- rev(lev)
    return(list(group = factor(x, levels = lev), rule = sprintf("取值不超过 5 个，按原值分组（风险方向: %s）", risk)))
  }
  if (is.null(cut) && is.null(q)) q <- 3L
  if (!is.null(cut)) {
    if (!is.numeric(cut) || anyNA(cut) || anyDuplicated(cut)) stop(sprintf("%s 的 cut 必须为互不相同的数值", name))
    cut <- sort(cut)
    labels <- c(paste0("<=", format(cut[1], trim = TRUE)),
                if (length(cut) > 1L) paste0("(", format(cut[-length(cut)], trim = TRUE), ",", format(cut[-1], trim = TRUE), "]"),
                paste0(">", format(cut[length(cut)], trim = TRUE)))
    g <- cut(x, breaks = c(-Inf, cut, Inf), labels = labels, right = TRUE)
    rule <- sprintf("界值 %s", paste(format(cut, trim = TRUE), collapse = ", "))
  } else {
    qg <- .q_groups(x, basis, q, name, ids)
    g <- factor(qg$intervals[as.integer(qg$grp)], levels = qg$intervals)
    rule <- sprintf("%d 分位", qg$k)
  }
  g <- droplevels(g)
  if (risk == "lower") g <- factor(g, levels = rev(levels(g)))
  list(group = g, rule = sprintf("%s，风险方向: 数值越%s风险越高", rule, if (risk == "higher") "大" else "小"))
}

# ---- 加性交互指标：点估计与 delta 法 ----

.add_point <- function(b10, b01, b11) {
  e10 <- exp(b10); e01 <- exp(b01); e11 <- exp(b11)
  reri <- e11 - e10 - e01 + 1
  d <- e11 - 1
  den_s <- e10 + e01 - 2
  c(RERI = reri, AP = reri / e11, S = if (d > 0 && den_s > 0) d / den_s else NA_real_,
    prop_a = (e10 - 1) / d, prop_b = (e01 - 1) / d, prop_interaction = reri / d,
    multiplicative = exp(b11 - b10 - b01))
}

# 各指标对 (b10, b01, b11) 的梯度；S 与乘法交互在对数尺度上
.add_gradient <- function(b10, b01, b11) {
  e10 <- exp(b10); e01 <- exp(b01); e11 <- exp(b11)
  d <- e11 - 1
  den_s <- e10 + e01 - 2
  g_pa <- c(e10 / d, 0, -(e10 - 1) * e11 / d^2)
  g_pb <- c(0, e01 / d, -(e01 - 1) * e11 / d^2)
  rbind(
    RERI = c(-e10, -e01, e11),
    AP = c(-e10 / e11, -e01 / e11, (e10 + e01 - 1) / e11),
    S = c(-e10 / den_s, -e01 / den_s, e11 / d),
    prop_a = g_pa,
    prop_b = g_pb,
    prop_interaction = -g_pa - g_pb,
    multiplicative = c(-1, -1, 1)
  )
}

.ADD_LOG_SCALE <- c("S", "multiplicative")

.add_delta <- function(b, V, conf_level) {
  est <- .add_point(b[1], b[2], b[3])
  G <- .add_gradient(b[1], b[2], b[3])
  se <- sqrt(pmax(rowSums((G %*% V) * G), 0))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  lower <- upper <- p <- rep(NA_real_, length(est))
  names(lower) <- names(upper) <- names(p) <- names(est)
  for (m in names(est)) {
    if (is.na(est[[m]])) next
    if (m %in% .ADD_LOG_SCALE) {
      l <- log(est[[m]])
      lower[[m]] <- exp(l - z * se[[m]])
      upper[[m]] <- exp(l + z * se[[m]])
      p[[m]] <- 2 * stats::pnorm(-abs(l / se[[m]]))
    } else {
      lower[[m]] <- est[[m]] - z * se[[m]]
      upper[[m]] <- est[[m]] + z * se[[m]]
      if (m %in% c("RERI", "AP")) p[[m]] <- 2 * stats::pnorm(-abs(est[[m]] / se[[m]]))
    }
  }
  list(estimate = est, lower = lower, upper = upper, P = p, se = se)
}

.ADD_DESCRIPTION <- function(a, b) {
  c(RERI = "相对超额危险度（RERI）", AP = "交互归因比（AP = RERI / RR11）", S = "协同指数（S）",
    prop_a = sprintf("联合超额风险中 %s 单独的贡献", a), prop_b = sprintf("联合超额风险中 %s 单独的贡献", b),
    prop_interaction = "联合超额风险中交互的贡献", multiplicative = "乘法交互（RR11 / (RR10 × RR01)）")
}

# ---- 共用流程 ----

.run_add <- function(engine, var_a, var_b, cut_a, cut_b, q_a, q_b, risk_a, risk_b, levels_a, levels_b, contrast,
                     covars, ci_method, B, seed, conf_level) {
  .validate_conf_level(conf_level)
  if (!is.character(var_a) || !is.character(var_b) || length(var_a) != 1L || length(var_b) != 1L || var_a == var_b) {
    stop("var_a 与 var_b 必须为两个不同的变量名")
  }
  involves <- vapply(covars, function(term) any(c(var_a, var_b) %in% all.vars(str2lang(term))), logical(1), USE.NAMES = FALSE)
  if (any(involves)) stop("covars 中不允许含 var_a 或 var_b 的项: ", paste(covars[involves], collapse = ", "))
  if (ci_method == "boot" && (!is.numeric(B) || length(B) != 1L || B < 50 || B != round(B))) stop("B 必须为不小于 50 的整数")

  # 1. 分析样本（完整病例）上分组
  ana <- engine$sample()
  basis <- engine$basis(ana)
  ids <- engine$ids(ana)
  ga <- .add_group(ana[[var_a]], var_a, cut_a, q_a, risk_a, levels_a, basis, ids)
  gb <- .add_group(ana[[var_b]], var_b, cut_b, q_b, risk_b, levels_b, basis, ids)
  la <- levels(ga$group)
  lb <- levels(gb$group)
  if (length(la) < 2L || length(lb) < 2L) stop("两个因素各自至少需要 2 个水平")
  if (is.null(contrast)) contrast <- c(length(la), length(lb))
  if (!is.numeric(contrast) || length(contrast) != 2L || contrast[1] < 2 || contrast[1] > length(la) ||
      contrast[2] < 2 || contrast[2] > length(lb)) {
    stop(sprintf("contrast 必须为 c(i, j)，i 取 2–%d，j 取 2–%d（1 为参照水平）", length(la), length(lb)))
  }
  for (g in list(list(var_a, ga), list(var_b, gb))) {
    tab <- table(g[[2]]$group)
    message(sprintf("%s 分组（%s）: ", g[[1]], g[[2]]$rule), paste(sprintf("%s: %d", names(tab), as.integer(tab)), collapse = "；"))
  }

  # 2. 联合分组变量（a 为外层），参照为两者的低风险水平
  jlab <- function(i, j) paste0(var_a, " ", la[i], " & ", var_b, " ", lb[j])
  grid_ij <- expand.grid(j = seq_along(lb), i = seq_along(la))[, c("i", "j")]
  jlevels <- jlab(grid_ij$i, grid_ij$j)
  joint <- factor(jlab(as.integer(ga$group), as.integer(gb$group)), levels = jlevels)
  empty <- jlevels[table(joint) == 0L]
  if (length(empty) > 0L) stop("以下联合分组无观测，无法估计: ", paste(empty, collapse = "; "))
  jname <- make.unique(c(names(ana), "joint_ab"))[ncol(ana) + 1L]
  ana[[jname]] <- joint

  # 3. 拟合联合分组模型
  fitted <- engine$fit(ana, jname)
  fit <- fitted$fit
  b_all <- stats::coef(fit)
  idx <- .coef_index(fit, .bt(jname))
  if (length(idx) != length(jlevels) - 1L) stop("联合分组的系数与水平数不一致")
  coef_names <- names(b_all)[idx]
  if (anyNA(b_all[idx])) {
    stop("部分联合分组的系数无法估计（", paste(jlevels[-1][is.na(b_all[idx])], collapse = "; "),
         "）；若某因素为匹配变量或在匹配组内恒定，条件似然中不含其信息", call. = FALSE)
  }
  V_all <- as.matrix(fitted$V)
  if (is.null(dimnames(V_all))) dimnames(V_all) <- list(names(b_all), names(b_all))
  K <- length(jlevels)
  bj <- c(0, unname(b_all[idx]))
  Vj <- matrix(0, K, K)
  Vj[-1, -1] <- V_all[coef_names, coef_names]
  pos <- function(i, j) (i - 1L) * length(lb) + j

  # 4. 保护性暴露检查：单独暴露组的效应应 ≥ 1
  single <- c(vapply(seq_along(la)[-1], function(i) bj[pos(i, 1L)], numeric(1)),
              vapply(seq_along(lb)[-1], function(j) bj[pos(1L, j)], numeric(1)))
  single_lab <- c(jlevels[pos(seq_along(la)[-1], 1L)], jlevels[pos(1L, seq_along(lb)[-1])])
  if (any(single < 0)) {
    warning(sprintf("以下单独暴露组的%s < 1：%s。参照组可能不是风险最低的组合，RERI、AP、S 的解释会失真；",
                    engine$measure, paste(single_lab[single < 0], collapse = "; ")),
            "请按 Knol 等（2011）的建议重新定义风险方向（risk_* / levels_*），使风险最低的组合为参照", call. = FALSE)
  }

  # 5. 各非参照组合的加性交互指标（delta 法）
  pairs <- expand.grid(j = seq_along(lb)[-1], i = seq_along(la)[-1])[, c("i", "j")]
  desc <- .ADD_DESCRIPTION(var_a, var_b)
  rows <- list()
  boot_keys <- list()
  for (r in seq_len(nrow(pairs))) {
    i <- pairs$i[r]
    j <- pairs$j[r]
    p3 <- c(pos(i, 1L), pos(1L, j), pos(i, j))
    dl <- .add_delta(bj[p3], Vj[p3, p3], conf_level)
    rows[[r]] <- data.frame(
      contrast = sprintf("%s %s & %s %s", var_a, la[i], var_b, lb[j]), level_a = la[i], level_b = lb[j],
      primary = i == contrast[1] && j == contrast[2], measure = names(dl$estimate), description = unname(desc[names(dl$estimate)]),
      estimate = unname(dl$estimate), lower = unname(dl$lower), upper = unname(dl$upper), P = unname(dl$P),
      stringsAsFactors = FALSE
    )
    boot_keys[[r]] <- p3
  }
  additive <- do.call(rbind, rows)

  # 6. bootstrap 百分位数 CI（分组固定；重抽样单位按设计）
  boot_info <- NULL
  if (ci_method == "boot") {
    unit <- engine$unit(ana)
    groups <- split(seq_len(nrow(ana)), unit$value)
    if (!is.null(seed)) {
      had_seed <- exists(".Random.seed", envir = globalenv(), inherits = FALSE)
      if (had_seed) old_seed <- get(".Random.seed", envir = globalenv())
      on.exit(if (had_seed) assign(".Random.seed", old_seed, envir = globalenv()) else rm(".Random.seed", envir = globalenv()),
              add = TRUE)
      set.seed(seed)
    }
    reps <- matrix(NA_real_, B, nrow(additive))
    for (r in seq_len(B)) {
      draw <- sample.int(length(groups), length(groups), replace = TRUE)
      rows_b <- unlist(groups[draw], use.names = FALSE)
      d_b <- ana[rows_b, , drop = FALSE]
      if (!is.null(unit$column)) d_b[[unit$column]] <- rep(seq_along(draw), lengths(groups[draw]))
      cb <- tryCatch(suppressWarnings(engine$refit(d_b, jname)), error = function(e) NULL)
      if (is.null(cb)) next
      bb <- c(0, unname(cb[coef_names]))
      reps[r, ] <- unlist(lapply(boot_keys, function(p3) .add_point(bb[p3[1]], bb[p3[2]], bb[p3[3]])), use.names = FALSE)
    }
    ok <- stats::complete.cases(reps[, additive$measure == "RERI", drop = FALSE])
    if (sum(ok) < B) warning(sprintf("bootstrap 有 %d / %d 次重抽样未能拟合或缺少联合分组，已排除", B - sum(ok), B), call. = FALSE)
    probs <- c((1 - conf_level) / 2, 1 - (1 - conf_level) / 2)
    ci <- apply(reps[ok, , drop = FALSE], 2, function(v) stats::quantile(v, probs, na.rm = TRUE, names = FALSE))
    additive$lower <- ci[1, ]
    additive$upper <- ci[2, ]
    additive$P <- NA_real_
    boot_info <- list(B = B, B_valid = sum(ok), seed = seed, unit = unit$label, replicates = reps)
    message(sprintf("bootstrap: B = %d（有效 %d），重抽样单位: %s", B, sum(ok), unit$label))
  }

  # 7. 分层效应（同一模型、所选方差，Wald）
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  strat_row <- function(within, comparison, p_from, p_to) {
    est <- bj[p_to] - bj[p_from]
    se <- sqrt(Vj[p_to, p_to] + Vj[p_from, p_from] - 2 * Vj[p_to, p_from])
    data.frame(within = within, comparison = comparison, estimate = exp(est), lower = exp(est - z * se),
               upper = exp(est + z * se), P = 2 * stats::pnorm(-abs(est / se)), stringsAsFactors = FALSE)
  }
  stratified <- do.call(rbind, c(
    lapply(seq_along(lb), function(j) do.call(rbind, lapply(seq_along(la)[-1], function(i) {
      strat_row(sprintf("%s %s", var_b, lb[j]), sprintf("%s %s vs %s", var_a, la[i], la[1]), pos(1L, j), pos(i, j))
    }))),
    lapply(seq_along(la), function(i) do.call(rbind, lapply(seq_along(lb)[-1], function(j) {
      strat_row(sprintf("%s %s", var_a, la[i]), sprintf("%s %s vs %s", var_b, lb[j], lb[1]), pos(i, 1L), pos(i, j))
    })))
  ))

  # 8. 联合效应表（结构同 *_run，可直接用于 plot_forest）
  result <- fitted$table
  result$varname <- paste0(var_a, " × ", var_b)

  prim <- additive[additive$primary, ]
  fmt <- function(m, pct = FALSE) {
    r <- prim[prim$measure == m, ]
    k <- if (pct) 100 else 1
    sprintf("%s %.2f (%.2f, %.2f)", m, k * r$estimate, k * r$lower, k * r$upper)
  }
  info <- fitted$info
  info$var_a <- var_a
  info$var_b <- var_b
  info$levels_a <- la
  info$levels_b <- lb
  info$grouping <- c(a = ga$rule, b = gb$rule)
  info$contrast <- contrast
  info$ci_method <- ci_method
  info$measure <- engine$measure
  info$header <- c(
    sprintf("加性交互（%s）: %s × %s；联合分组 %d × %d，参照 %s", engine$title, var_a, var_b, length(la), length(lb), jlevels[1]),
    sprintf("主要对比: %s；CI 方法: %s", prim$contrast[1], if (ci_method == "delta") "delta 法" else sprintf("bootstrap 百分位数（B = %d）", B)),
    paste(fmt("RERI"), fmt("AP"), fmt("S"), sep = "；"),
    sprintf("两因素贡献（%%）: %s %.1f，%s %.1f，交互 %.1f", var_a, 100 * prim$estimate[prim$measure == "prop_a"],
            var_b, 100 * prim$estimate[prim$measure == "prop_b"], 100 * prim$estimate[prim$measure == "prop_interaction"]),
    info$header[-1]
  )
  invisible(.new_result(model = fit, result = result, additive = additive, stratified = stratified,
                        boot = boot_info, info = info))
}

# ---- 模型引擎 ----

.add_cox_engine <- function(data, time1, time2, timediff, event, var_a, var_b, covars, idL, robust, conf_level, dots,
                            t_unit, incidence_scale) {
  env <- environment()
  list(
    title = "Cox 回归", measure = "HR",
    sample = function() {
      env$prep0 <- .prep_cox(data, time1, time2, timediff, event, c(var_a, var_b), covars, idL, robust)
      env$prep0$data
    },
    basis = function(ana) {
      if (is.null(idL)) seq_len(nrow(ana)) else which(!duplicated(data.frame(ana[[idL]], ana[[var_a]], ana[[var_b]])))
    },
    ids = function(ana) if (is.null(idL)) NULL else ana[[idL]],
    fit = function(ana, jname) {
      prep <- .prep_cox(ana, time1, time2, timediff, event, jname, covars, idL, robust, quiet = TRUE)
      env$prep <- prep
      fit <- do.call(.fit_cox, c(list(prep), dots))
      vc <- .cox_vcov(fit, robust)
      info <- .cox_info(prep, fit, vc, conf_level, t_unit, incidence_scale)
      info$n_dropped <- env$prep0$n_drop
      list(fit = fit, V = vc$V, table = .tidy_cox(prep, fit, vc, jname, NULL, t_unit, incidence_scale, conf_level), info = info)
    },
    refit = function(d, jname) {
      p <- env$prep
      p$data <- d
      stats::coef(do.call(.fit_cox, c(list(p), dots)))
    },
    unit = function(ana) {
      cl <- covars[grepl("^\\s*cluster\\s*\\(", covars)]
      if (!is.null(idL)) return(list(value = ana[[idL]], column = idL, label = paste0("个体（", idL, "）")))
      if (length(cl) > 0L) {
        v <- all.vars(str2lang(cl[1]))
        return(list(value = ana[[v]], column = v, label = paste0("聚类（", v, "）")))
      }
      list(value = seq_len(nrow(ana)), column = NULL, label = "观测")
    }
  )
}

.add_reg_engine <- function(model, data, outcome, var_a, var_b, covars, robust, conf_level, match_id, dots) {
  env <- environment()
  list(
    title = .REG_TITLES[[model]], measure = "OR",
    sample = function() {
      env$prep0 <- .reg_prepare(model, data, outcome, c(var_a, var_b), covars, robust, match_id)
      y <- env$prep0$data[[outcome]]
      if (model == "logit" && mean(y) > 0.1) {
        warning(sprintf("结局比例为 %.1f%%（> 10%%），OR 明显偏离 RR，基于 OR 的 RERI、AP、S 会有偏", 100 * mean(y)), call. = FALSE)
      }
      env$prep0$data
    },
    basis = function(ana) seq_len(nrow(ana)),
    ids = function(ana) NULL,
    fit = function(ana, jname) {
      core <- .reg_core(model, ana, outcome, jname, covars, NULL, robust, conf_level, "wald", match_id, dots, quiet = TRUE)
      env$prep <- core$prep
      core$info$n_dropped <- env$prep0$n_drop
      list(fit = core$fit, V = core$vc$V, table = core$result, info = core$info)
    },
    refit = function(d, jname) {
      p <- env$prep
      p$data <- d
      stats::coef(.reg_fit(p, p$formula, FALSE, dots))
    },
    unit = function(ana) {
      if (model == "clogit") return(list(value = ana[[match_id]], column = match_id, label = paste0("匹配组（", match_id, "）")))
      cl <- covars[grepl("^\\s*cluster\\s*\\(", covars)]
      if (length(cl) > 0L) {
        v <- all.vars(str2lang(cl[1]))
        return(list(value = ana[[v]], column = v, label = paste0("聚类（", v, "）")))
      }
      list(value = seq_len(nrow(ana)), column = NULL, label = "观测")
    }
  )
}

#' 加性交互（Cox 回归）
#'
#' 将两个因素分组后组成联合分组变量（参照为两者的低风险水平），拟合 Cox 模型，报告：
#' 联合效应表（各组合相对共同参照的 HR）、加性交互指标 RERI、AP、S、两因素对联合超额风险的贡献分解
#' （各自单独与交互，合计 100\%；VanderWeele & Tchetgen Tchetgen 2014）、乘法交互及分层效应（Knol & VanderWeele 2012）。
#' 多水平因素对每个非参照组合分别计算，\code{contrast} 指定主要报告的组合。
#'
#' 风险方向：连续变量用 \code{risk_*} 声明数值越大风险越高（"higher"）或越低（"lower"），分组后自动排序使第一个水平为低风险参照；
#' 分类变量用 \code{levels_*} 按低 → 高风险给出水平顺序。单独暴露组的效应 < 1 时警告（参照可能不是风险最低的组合）。
#'
#' @inheritParams cox_run
#' @param var_a,var_b 两个交互因素的变量名
#' @param cut_a,cut_b 连续变量的分组界值（与 q_* 二选一）
#' @param q_a,q_b 连续变量的分位数组数（与 cut_* 二选一）；二者均未指定时，取值超过 5 个的连续变量默认三分位，否则按原值分组
#' @param risk_a,risk_b 连续变量的风险方向："higher"（数值越大风险越高，默认）或 "lower"
#' @param levels_a,levels_b 分类变量的水平顺序（低 → 高风险）
#' @param contrast 主要报告的组合 \code{c(i, j)}（水平序号，1 为参照），默认两个因素的最高风险水平
#' @param covars 协变量；不允许含 var_a 或 var_b 的项
#' @param ci_method "delta"（默认，delta 法，与所选方差一致）或 "boot"（bootstrap 百分位数）
#' @param B bootstrap 次数，默认 999
#' @param seed bootstrap 随机种子（结束后恢复原随机数状态）；NULL 时不设定
#' @param ... 传递给 \code{survival::coxph} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model}、\code{result}（联合效应表，可用于 \code{plot_forest}）、
#'   \code{additive}（各组合的 RERI、AP、S、prop_a、prop_b、prop_interaction、multiplicative 及 CI；AP 与贡献为比例）、
#'   \code{stratified}（分层效应）、\code{boot}、\code{info}
#' @export
cox_run_add <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event, var_a, var_b,
                        cut_a = NULL, cut_b = NULL, q_a = NULL, q_b = NULL, risk_a = c("higher", "lower"), risk_b = c("higher", "lower"),
                        levels_a = NULL, levels_b = NULL, contrast = NULL, covars = NULL, idL = NULL, robust = FALSE,
                        ci_method = c("delta", "boot"), B = 999, seed = NULL, t_unit = c("d", "y", "m"), incidence_scale = 1e5,
                        conf_level = 0.95, ...) {
  engine <- .add_cox_engine(data, time1, time2, timediff, event, var_a, var_b, covars, idL, robust, conf_level, list(...),
                            match.arg(t_unit), incidence_scale)
  .run_add(engine, var_a, var_b, cut_a, cut_b, q_a, q_b, match.arg(risk_a), match.arg(risk_b), levels_a, levels_b, contrast,
           covars, match.arg(ci_method), B, seed, conf_level)
}

#' 加性交互（逻辑回归）
#'
#' 同 \code{cox_run_add}，模型为逻辑回归（OR 近似 RR）。结局比例超过 10\% 时警告：OR 明显偏离 RR，基于 OR 的加性交互指标有偏。
#'
#' @inheritParams logit_run
#' @inheritParams cox_run_add
#' @param ... 传递给 \code{glm} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象，结构同 \code{cox_run_add}
#' @export
logit_run_add <- function(data, outcome, var_a, var_b, cut_a = NULL, cut_b = NULL, q_a = NULL, q_b = NULL,
                          risk_a = c("higher", "lower"), risk_b = c("higher", "lower"), levels_a = NULL, levels_b = NULL,
                          contrast = NULL, covars = NULL, robust = FALSE, ci_method = c("delta", "boot"), B = 999, seed = NULL,
                          conf_level = 0.95, ...) {
  engine <- .add_reg_engine("logit", data, outcome, var_a, var_b, covars, robust, conf_level, NULL, list(...))
  .run_add(engine, var_a, var_b, cut_a, cut_b, q_a, q_b, match.arg(risk_a), match.arg(risk_b), levels_a, levels_b, contrast,
           covars, match.arg(ci_method), B, seed, conf_level)
}

#' 加性交互（条件逻辑回归）
#'
#' 同 \code{cox_run_add}，模型为条件逻辑回归；bootstrap 以匹配组为重抽样单位。两个因素均须在匹配组内有变异，
#' 匹配变量的效应不可估计。
#'
#' @inheritParams clogit_run
#' @inheritParams cox_run_add
#' @param ... 传递给 \code{survival::clogit} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象，结构同 \code{cox_run_add}
#' @export
clogit_run_add <- function(data, outcome, var_a, var_b, match_id, cut_a = NULL, cut_b = NULL, q_a = NULL, q_b = NULL,
                           risk_a = c("higher", "lower"), risk_b = c("higher", "lower"), levels_a = NULL, levels_b = NULL,
                           contrast = NULL, covars = NULL, robust = FALSE, ci_method = c("delta", "boot"), B = 999, seed = NULL,
                           conf_level = 0.95, ...) {
  if (missing(match_id)) match_id <- NULL
  .reg_check_args("clogit", robust, "wald", match_id, list(...)$method)
  engine <- .add_reg_engine("clogit", data, outcome, var_a, var_b, covars, robust, conf_level, match_id, list(...))
  .run_add(engine, var_a, var_b, cut_a, cut_b, q_a, q_b, match.arg(risk_a), match.arg(risk_b), levels_a, levels_b, contrast,
           covars, match.arg(ci_method), B, seed, conf_level)
}
