# 逻辑回归、条件逻辑回归与线性回归的亚组分析（含交互检验）与分位数分组分析

# mainvar × group_var 交互检验：模型方差下 logit / clogit 用似然比检验，lm 用嵌套模型 F 检验；
# 稳健方差下用 Wald 检验（lm 为 Wald F）。clogit 中分组变量在匹配组内恒定时，其主效应已被条件似然吸收，不纳入模型
.reg_interaction_test <- function(model, data, group_var, outcome, mainvar, covars, robust, match_id, fit_args) {
  cov_full <- c(covars, group_var)
  if (model == "clogit") {
    constant <- all(tapply(as.character(data[[group_var]]), data[[match_id]], function(v) length(unique(v)) == 1L))
    if (constant) {
      message("[交互检验] 分组变量 ", group_var, " 在匹配组内恒定，其主效应已被条件似然吸收，模型仅纳入交互项")
      cov_full <- covars
    }
  }
  prep <- .reg_prepare(model, data, outcome, mainvar, cov_full, robust, match_id, label = "交互检验")
  fit0 <- .reg_fit(prep, prep$formula, robust, fit_args)
  models <- list()
  tab <- lapply(mainvar, function(mv) {
    f1 <- stats::update(prep$formula, stats::as.formula(paste(". ~ . +", paste0(.bt(mv), ":", .bt(group_var)))))
    fit1 <- tryCatch(.reg_fit(prep, f1, robust, fit_args), error = function(e) {
      warning(sprintf("%s 的交互模型拟合失败: %s", mv, conditionMessage(e)), call. = FALSE)
      NULL
    })
    models[[mv]] <<- list(reduced = fit0, full = fit1)
    if (is.null(fit1)) return(data.frame(varname = mv, test = NA_character_, df = NA_real_, statistic = NA_real_, P = NA_real_))
    if (isTRUE(robust)) {
      b_all <- stats::coef(fit1)
      idx <- setdiff(names(b_all), names(stats::coef(fit0)))
      V <- as.matrix(.reg_vcov(fit1, prep, TRUE)$V)
      if (is.null(dimnames(V))) dimnames(V) <- list(names(b_all), names(b_all))
      idx <- idx[!is.na(b_all[idx])]
      b <- b_all[idx]
      w <- as.numeric(t(b) %*% solve(V[idx, idx, drop = FALSE], b))
      q <- length(idx)
      if (model == "lm") {
        data.frame(varname = mv, test = "Wald F", df = q, statistic = w / q,
                   P = stats::pf(w / q, q, fit1$df.residual, lower.tail = FALSE))
      } else {
        data.frame(varname = mv, test = "Wald", df = q, statistic = w, P = stats::pchisq(w, q, lower.tail = FALSE))
      }
    } else {
      a <- switch(model, logit = stats::anova(fit0, fit1, test = "LRT"), lm = stats::anova(fit0, fit1),
                  clogit = stats::anova(fit0, fit1))
      stat_col <- switch(model, logit = "Deviance", lm = "F", clogit = "Chisq")
      data.frame(varname = mv, test = switch(model, logit = "LRT", lm = "F", clogit = "LRT"),
                 df = a$Df[2], statistic = a[2, stat_col], P = a[2, ncol(a)])
    }
  })
  list(models = models, table = do.call(rbind, tab))
}

.reg_run_sub <- function(model, data, group_var, outcome, mainvar, covars, extra_vars, plot_shape, group_na, interaction,
                         robust, conf_level, ci_method, match_id, fit_args) {
  .validate_conf_level(conf_level)
  .reg_check_args(model, robust, ci_method, match_id, fit_args$method)
  grp <- .prepare_group(data, group_var, group_na, mainvar)
  data <- grp$data
  if (model == "clogit") {
    if (!match_id %in% names(data)) stop("match_id 不在数据框中")
    varies <- tapply(as.character(data[[group_var]]), data[[match_id]], function(v) length(unique(v)) > 1L)
    if (any(varies, na.rm = TRUE)) {
      stop(sprintf(paste0("分组变量 %s 在 %d 个匹配组内不一致，按其分亚组会拆分这些匹配组、破坏匹配设计，",
                          "不能用条件逻辑回归做亚组分析。请改用匹配因素（或以匹配组为单位定义的特征）分组；",
                          "或改用 logit_run_sub() 做一般逻辑回归的亚组分析，并在 covars 中校正匹配因素"),
                   group_var, sum(varies, na.rm = TRUE)), call. = FALSE)
    }
  }
  sub_covars <- .sub_covars(covars, group_var, data)
  focus <- c(mainvar, .split_extra(extra_vars, data, sub_covars)$focus)

  fit_level <- function(sub_data, label) {
    core <- .reg_core(model, sub_data, outcome, mainvar, sub_covars, extra_vars, robust, conf_level, ci_method,
                      match_id, fit_args, label = label)
    list(fit = core$fit, table = core$result)
  }
  interaction_fun <- if (isTRUE(interaction)) {
    function() .reg_interaction_test(model, data, group_var, outcome, mainvar, sub_covars, robust, match_id, fit_args)
  }
  sub <- .run_sub(data, group_var, focus, fit_level, interaction_fun, plot_shape)
  info <- list(
    model = model, group_var = group_var, n_group_na = grp$n_na, group_na = group_na,
    variance = if (isTRUE(robust)) "robust" else "model", conf_level = conf_level,
    header = c(
      sprintf("亚组%s: 按 %s 分为 %d 组（成功拟合 %d 组）", .REG_TITLES[[model]], group_var, nlevels(data[[group_var]]), length(sub$fits)),
      sprintf("方差估计: %s；置信水平: %g%%", if (isTRUE(robust)) "稳健（三明治）方差" else "模型方差", 100 * conf_level),
      .inter_header(sub$inter_tab)
    )
  )
  invisible(.new_result(model = list(subgroups = sub$fits, interaction = sub$inter_models), result = sub$result,
                        interaction = sub$inter_tab, info = info))
}

.reg_run_q <- function(model, data, outcome, mainvar, q, covars, trend, trend_method, robust, conf_level, ci_method,
                       match_id, fit_args) {
  .validate_conf_level(conf_level)
  .reg_check_args(model, robust, ci_method, match_id, fit_args$method)
  if (!is.character(mainvar) || length(mainvar) != 1L || !mainvar %in% names(data)) stop("mainvar 不在数据框中")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型")
  prep0 <- .reg_prepare(model, data, outcome, mainvar, covars, robust, match_id)
  ana <- prep0$data
  x <- ana[[mainvar]]
  qg <- .q_groups(x, seq_along(x), q, mainvar)
  gname <- make.unique(c(names(ana), paste0(mainvar, "_q")))[ncol(ana) + 1L]
  ana[[gname]] <- qg$grp

  core <- .reg_core(model, ana, outcome, gname, covars, NULL, robust, conf_level, ci_method, match_id, fit_args, quiet = TRUE)
  res <- core$result
  res$varname <- paste0(mainvar, "_", res$Interval)
  res$Interval <- qg$intervals[match(res$Interval, levels(qg$grp))]

  fit_trend <- NULL
  trend_tab <- NULL
  if (isTRUE(trend)) {
    score <- .q_score(x, qg$grp, seq_along(x), trend_method)
    sname <- make.unique(c(names(ana), paste0(mainvar, "_trend")))[ncol(ana) + 1L]
    ana[[sname]] <- score
    if (length(unique(score)) < 2L) {
      warning("趋势检验得分只有一个取值，无法拟合趋势模型", call. = FALSE)
      trend_tab <- data.frame(method = trend_method, beta = NA_real_, se = NA_real_, P = NA_real_)
    } else {
      prep_t <- .reg_prepare(model, ana, outcome, sname, covars, robust, match_id, quiet = TRUE)
      fit_trend <- .reg_fit(prep_t, prep_t$formula, robust, fit_args)
      wt <- .reg_wald(fit_trend, .reg_vcov(fit_trend, prep_t, robust), conf_level)
      wt <- wt[match(sname, wt$coef_name), ]
      trend_tab <- data.frame(method = trend_method, beta = wt$beta, se = wt$se, P = wt$P)
    }
    res$beta_trend <- c(trend_tab$beta, rep(NA_real_, nrow(res) - 1L))
    res$P_trend <- c(trend_tab$P, rep(NA_real_, nrow(res) - 1L))
  }
  attr(res, "quantile_breaks") <- qg$breaks

  info <- core$info
  info$mainvar <- mainvar
  info$model <- model
  info$n_input <- prep0$n_input
  info$n_dropped <- prep0$n_drop
  info$header <- c(
    sprintf("分组%s: %s 分为 %d 组；断点 %s", .REG_TITLES[[model]], mainvar, qg$k,
            paste(format(qg$breaks, digits = 4, trim = TRUE), collapse = ", ")),
    sub("剔除缺失 0 行", sprintf("剔除缺失 %d 行", prep0$n_drop), info$header[-1], fixed = TRUE),
    if (!is.null(trend_tab)) sprintf("趋势检验（%s 得分）: P = %s", trend_method, format.pval(trend_tab$P, digits = 3, eps = 0.001))
  )
  invisible(.new_result(model = list(main = core$fit, trend = fit_trend), result = res,
                        breaks = qg$breaks, trend = trend_tab, info = info))
}

#' 亚组逻辑回归（含交互检验）
#'
#' 按 group_var 的每个水平分别拟合逻辑回归；同时在全样本中检验 mainvar 与 group_var 的交互作用
#' （模型方差时为似然比检验，稳健方差时为 Wald 检验），结果列 \code{P_interaction}。含 group_var 的协变量项在亚组模型中自动剔除。
#' 结果可直接用于 \code{cox_het} 与 \code{plot_forest}。
#'
#' @inheritParams logit_run
#' @param group_var 分组变量名（字符串）
#' @param plot_shape 是否返回绘图友好格式，同 \code{cox_run_sub}，默认 FALSE
#' @param group_na group_var 缺失的处理："drop"（默认）或 "keep"（作为单独一组 "(Missing)"）
#' @param interaction 是否检验 mainvar × group_var 交互作用，默认 TRUE
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model$subgroups}、\code{model$interaction}、\code{result}、
#'   \code{interaction}（交互检验表）、\code{info}
#' @export
logit_run_sub <- function(data, group_var, outcome, mainvar, covars = NULL, extra_vars = NULL, plot_shape = FALSE,
                          group_na = c("drop", "keep"), interaction = TRUE, robust = FALSE,
                          ci_method = c("wald", "profile"), conf_level = 0.95, ...) {
  .reg_run_sub("logit", data, group_var, outcome, mainvar, covars, extra_vars, plot_shape, match.arg(group_na), interaction,
               robust, conf_level, match.arg(ci_method), NULL, list(...))
}

#' 亚组条件逻辑回归（含交互检验）
#'
#' 按 group_var 的每个水平分别拟合条件逻辑回归；交互检验同 \code{logit_run_sub}。
#' 分组变量须在每个匹配组内恒定（如匹配因素），其主效应已被条件似然吸收，交互模型仅纳入交互项。
#' 分组变量在任一匹配组内不一致时报错：按其分亚组会拆分匹配组、破坏匹配设计；
#' 此时应改用匹配因素分组，或用 \code{logit_run_sub} 做一般逻辑回归的亚组分析并在 covars 中校正匹配因素。
#'
#' @inheritParams clogit_run
#' @inheritParams logit_run_sub
#' @return 不可见地返回 \code{myepi_result} 对象，结构同 \code{logit_run_sub}
#' @export
clogit_run_sub <- function(data, group_var, outcome, mainvar, match_id, covars = NULL, extra_vars = NULL, plot_shape = FALSE,
                           group_na = c("drop", "keep"), interaction = TRUE, robust = FALSE, conf_level = 0.95, ...) {
  if (missing(match_id)) match_id <- NULL
  .reg_run_sub("clogit", data, group_var, outcome, mainvar, covars, extra_vars, plot_shape, match.arg(group_na), interaction,
               robust, conf_level, "wald", match_id, list(...))
}

#' 亚组线性回归（含交互检验）
#'
#' 按 group_var 的每个水平分别拟合线性回归；交互检验在模型方差时为嵌套模型 F 检验，稳健方差时为 Wald F 检验。
#'
#' @inheritParams lm_run
#' @inheritParams logit_run_sub
#' @return 不可见地返回 \code{myepi_result} 对象，结构同 \code{logit_run_sub}
#' @export
lm_run_sub <- function(data, group_var, outcome, mainvar, covars = NULL, extra_vars = NULL, plot_shape = FALSE,
                       group_na = c("drop", "keep"), interaction = TRUE, robust = FALSE, conf_level = 0.95, ...) {
  .reg_run_sub("lm", data, group_var, outcome, mainvar, covars, extra_vars, plot_shape, match.arg(group_na), interaction,
               robust, conf_level, "wald", NULL, list(...))
}

#' 分位数分组逻辑回归
#'
#' 连续变量按分位数或自定义断点分组后拟合逻辑回归（Q1 为参照），可选趋势检验；分位点在分析样本中计算，
#' 重复时自动合并并警告，运行时报告各组样本量。结果可直接用于 \code{plot_forest}。
#'
#' @inheritParams logit_run
#' @param mainvar 连续变量名（字符串）
#' @param q 分组定义：单个整数 n（>= 2）为 n 等分位；数值向量为自定义断点，同 \code{cox_run_q}
#' @param trend 是否进行趋势检验，默认 FALSE
#' @param trend_method 趋势得分："median"（组内中位数，默认）或 "ordinal"（组序号）
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model$main}、\code{model$trend}、\code{result}（带属性
#'   \code{quantile_breaks}）、\code{breaks}、\code{trend}、\code{info}
#' @export
logit_run_q <- function(data, outcome, mainvar, q = 3, covars = NULL, trend = FALSE, trend_method = c("median", "ordinal"),
                        robust = FALSE, ci_method = c("wald", "profile"), conf_level = 0.95, ...) {
  .reg_run_q("logit", data, outcome, mainvar, q, covars, trend, match.arg(trend_method), robust, conf_level,
             match.arg(ci_method), NULL, list(...))
}

#' 分位数分组条件逻辑回归
#'
#' 同 \code{logit_run_q}，模型为条件逻辑回归。
#'
#' @inheritParams clogit_run
#' @inheritParams logit_run_q
#' @return 不可见地返回 \code{myepi_result} 对象，结构同 \code{logit_run_q}
#' @export
clogit_run_q <- function(data, outcome, mainvar, match_id, q = 3, covars = NULL, trend = FALSE,
                         trend_method = c("median", "ordinal"), robust = FALSE, conf_level = 0.95, ...) {
  if (missing(match_id)) match_id <- NULL
  .reg_run_q("clogit", data, outcome, mainvar, q, covars, trend, match.arg(trend_method), robust, conf_level,
             "wald", match_id, list(...))
}

#' 分位数分组线性回归
#'
#' 同 \code{logit_run_q}，模型为线性回归（效应量为 beta，推断采用 t 分布）。
#'
#' @inheritParams lm_run
#' @inheritParams logit_run_q
#' @return 不可见地返回 \code{myepi_result} 对象，结构同 \code{logit_run_q}
#' @export
lm_run_q <- function(data, outcome, mainvar, q = 3, covars = NULL, trend = FALSE, trend_method = c("median", "ordinal"),
                     robust = FALSE, conf_level = 0.95, ...) {
  .reg_run_q("lm", data, outcome, mainvar, q, covars, trend, match.arg(trend_method), robust, conf_level,
             "wald", NULL, list(...))
}
