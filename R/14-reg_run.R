# 逻辑回归、条件逻辑回归与线性回归：共用的数据准备、系数映射与结果表

# 构建公式、校验变量、按完整病例生成分析样本并报告缺失处理；cluster() 项不进入公式，按 robust 决定是否用于聚类
.prep_reg <- function(data, outcome, mainvar, covars, robust, extra_vars = character(), match_id = NULL,
                      label = NULL, quiet = FALSE) {
  if (!is.data.frame(data)) stop("data 必须为数据框")
  data <- as.data.frame(data)
  .validate_robust(robust)
  if (!is.character(outcome) || length(outcome) != 1L || !outcome %in% names(data)) stop("outcome 不在数据框中")
  if (!is.character(mainvar) || length(mainvar) < 1L) stop("mainvar 必须为变量名字符串")
  if (!all(mainvar %in% names(data))) stop("mainvar 不在数据框中: ", paste(setdiff(mainvar, names(data)), collapse = ", "))
  if (any(mainvar %in% covars)) stop("mainvar 不应同时出现在 covars 中")
  for (v in mainvar) if (is.logical(data[[v]])) data[[v]] <- factor(data[[v]], levels = c(FALSE, TRUE))

  is_cluster <- grepl("^\\s*cluster\\s*\\(", covars)
  cluster_var <- NULL
  if (any(is_cluster)) {
    if (isTRUE(robust)) {
      cluster_var <- all.vars(str2lang(covars[is_cluster][1]))
    } else if (!quiet) {
      .msg(label, "robust = FALSE：忽略 ", paste(covars[is_cluster], collapse = ", "), "，使用模型方差（如需稳健方差请设置 robust = TRUE）")
    }
  }
  if (any(grepl("\\bstrata\\s*\\(", covars))) stop("covars 中不支持 strata()；条件逻辑回归的匹配组请用 match_id 指定")
  covars <- covars[!is_cluster]

  rhs <- c(.bt(mainvar), vapply(covars, .term_label, character(1), data = data, USE.NAMES = FALSE))
  fml <- stats::reformulate(rhs, response = str2lang(.bt(outcome)))
  vars <- unique(c(all.vars(fml), cluster_var, match_id))
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0L) stop("以下变量不在数据框中: ", paste(missing_vars, collapse = ", "))

  n_input <- nrow(data)
  na_by_var <- colSums(is.na(data[vars]))
  complete <- stats::complete.cases(data[vars])
  n_drop <- sum(!complete)
  ana <- data[complete, , drop = FALSE]
  rownames(ana) <- NULL
  if (nrow(ana) == 0L) stop("剔除模型变量缺失后无可用观测")
  if (!quiet) {
    if (n_drop == 0L) {
      .msg(label, "模型变量无缺失，分析样本 ", nrow(ana), " 行")
    } else {
      na_txt <- paste0(names(na_by_var)[na_by_var > 0], "=", na_by_var[na_by_var > 0], collapse = ", ")
      .msg(label, sprintf("模型变量存在缺失（%s），按完整病例剔除 %d/%d 行（%.1f%%），分析样本 %d 行",
                          na_txt, n_drop, n_input, 100 * n_drop / n_input, nrow(ana)))
    }
  }
  list(formula = fml, data = ana, covars = covars, cluster_var = cluster_var, n_input = n_input, n_drop = n_drop)
}

# 二分类结局统一为 0/1：数值 0/1、逻辑值，或二水平因子（第二个水平为事件）
.binary_outcome <- function(y, outcome, quiet = FALSE) {
  if (is.logical(y)) return(as.integer(y))
  if (is.factor(y)) {
    y <- droplevels(y)
    if (nlevels(y) != 2L) stop("结局 ", outcome, " 为因子时必须恰有两个水平")
    if (!quiet) message(sprintf("结局 %s 为因子：以 \"%s\" 为事件（1），\"%s\" 为参照（0）", outcome, levels(y)[2], levels(y)[1]))
    return(as.integer(y == levels(y)[2]))
  }
  if (!is.numeric(y) || any(!y %in% c(0, 1))) stop("结局 ", outcome, " 必须为 0/1、TRUE/FALSE 或二水平因子")
  as.integer(y)
}

# 分离检测：(1) 模型中各分类变量是否存在只含事件或只含非事件的水平（单变量准完全分离，glm 不会提示）；
# (2) 拟合过程中 glm 关于不收敛或拟合概率为 0/1 的警告。仅给出警告，不改变结果
.check_separation <- function(prep, y, glm_warnings = character(), label = NULL) {
  vars <- intersect(c(all.vars(prep$formula)[-1]), names(prep$data))
  issues <- character()
  for (v in vars) {
    x <- prep$data[[v]]
    if (!(is.factor(x) || is.character(x) || is.logical(x))) next
    tab <- table(factor(as.character(x)), y)
    if (ncol(tab) < 2L) next
    bad <- rownames(tab)[tab[, 1] == 0 | tab[, 2] == 0]
    if (length(bad) > 0L) {
      issues <- c(issues, sprintf("%s = %s（%s）", v, bad, ifelse(tab[bad, 1] == 0, "全部为事件", "无事件")))
    }
  }
  sep_warn <- unique(glm_warnings[grepl("fitted probabilities numerically 0 or 1|did not converge", glm_warnings)])
  if (length(issues) > 0L || length(sep_warn) > 0L) {
    warning(if (!is.null(label)) paste0("[", label, "] "), "可能存在（准）完全分离，相关 OR 及其 CI 不可靠：",
            paste(c(issues, if (length(sep_warn)) paste0("glm: ", sep_warn)), collapse = "；"),
            "。可考虑合并稀疏水平或改用 Firth 校正逻辑回归", call. = FALSE)
    return(invisible(TRUE))
  }
  invisible(FALSE)
}

# 模型项对应的系数位置：coxph 的 assign 为列表；lm/glm 的 assign 为模型矩阵列号到项序号的向量
.coef_index <- function(fit, term) {
  if (is.list(fit$assign)) return(fit$assign[[term]])
  k <- match(term, attr(stats::terms(fit), "term.labels"))
  if (is.na(k)) return(NULL)
  which(attr(stats::model.matrix(fit), "assign") == k)
}

# 方差：robust = TRUE 时 lm/glm 用 sandwich（有聚类变量为 vcovCL，否则 vcovHC，均为 sandwich 默认类型）；clogit 用 coxph 的稳健方差
.reg_vcov <- function(fit, prep, robust) {
  if (!isTRUE(robust)) return(list(V = stats::vcov(fit), type = "model"))
  if (inherits(fit, "coxph")) return(list(V = fit$var, type = "robust"))
  if (!requireNamespace("sandwich", quietly = TRUE)) {
    stop("robust = TRUE 需要 sandwich 包，请先安装: install.packages(\"sandwich\")", call. = FALSE)
  }
  V <- if (is.null(prep$cluster_var)) sandwich::vcovHC(fit) else sandwich::vcovCL(fit, cluster = prep$data[[prep$cluster_var]])
  list(V = V, type = "robust")
}

# Wald 推断：lm 用 t 分布（残差自由度），glm/clogit 用正态分布；ci_method = "profile" 时 CI 取 confint()（剖面似然）
.reg_wald <- function(fit, vc, conf_level, ci_method = "wald") {
  b <- stats::coef(fit)
  se <- sqrt(diag(as.matrix(vc$V)))
  if (is.null(names(se))) {
    if (length(se) != length(b)) stop("方差矩阵维度与系数不一致")
    names(se) <- names(b)
  }
  se <- se[names(b)]
  se[is.na(b)] <- NA_real_
  use_t <- inherits(fit, "lm") && !inherits(fit, "glm")
  stat <- b / se
  if (use_t) {
    q <- stats::qt(1 - (1 - conf_level) / 2, fit$df.residual)
    p <- 2 * stats::pt(-abs(stat), fit$df.residual)
  } else {
    q <- stats::qnorm(1 - (1 - conf_level) / 2)
    p <- 2 * stats::pnorm(-abs(stat))
  }
  lower <- b - q * se
  upper <- b + q * se
  if (ci_method == "profile") {
    ci <- suppressMessages(stats::confint(fit, level = conf_level))
    if (is.null(dim(ci))) ci <- matrix(ci, nrow = 1L, dimnames = list(names(b), NULL))
    lower <- ci[names(b), 1]
    upper <- ci[names(b), 2]
  }
  data.frame(coef_name = names(b), beta = unname(b), se = unname(se), lower = unname(lower), upper = unname(upper),
             P = unname(p), stringsAsFactors = FALSE)
}

# 结果表：按 fit$xlevels 精确映射各水平；描述统计由 describe(rows) 提供
.reg_table <- function(prep, fit, est, focus, extra, describe, exp_effect) {
  measure <- if (exp_effect) c("OR", "OR_lower", "OR_upper") else c("beta", "lower", "upper")
  effect <- function(rows) {
    out <- if (exp_effect) {
      data.frame(OR = exp(rows$beta), OR_lower = exp(rows$lower), OR_upper = exp(rows$upper), P = rows$P,
                 beta = rows$beta, se = rows$se)
    } else {
      data.frame(beta = rows$beta, lower = rows$lower, upper = rows$upper, P = rows$P, se = rows$se)
    }
    rownames(out) <- NULL
    out
  }
  ref_row <- effect(data.frame(beta = 0, se = NA_real_, lower = NA_real_, upper = NA_real_, P = NA_real_))
  if (exp_effect) ref_row[c("OR_lower", "OR_upper")] <- NA_real_
  rows <- list()
  for (v in focus) {
    term <- .bt(v)
    idx <- .coef_index(fit, term)
    if (length(idx) == 0L) stop(sprintf("模型中未找到变量 %s 的系数", v))
    x <- prep$data[[v]]
    if (is.factor(x) || is.character(x)) {
      lev <- fit$xlevels[[term]]
      if (!identical(fit$contrasts[[term]], "contr.treatment")) stop(sprintf("变量 %s 仅支持 treatment contrasts（默认对照编码）", v))
      if (length(idx) != length(lev) - 1L) stop(sprintf("变量 %s 的系数与水平数不一致", v))
      g <- as.character(x)
      desc <- do.call(rbind, lapply(lev, function(l) describe(g == l)))
      rows[[length(rows) + 1L]] <- data.frame(varname = v, Interval = lev, Reference = c(TRUE, rep(FALSE, length(idx))),
                                              desc, rbind(ref_row, effect(est[idx, ])), stringsAsFactors = FALSE)
    } else {
      if (length(idx) != 1L) stop(sprintf("变量 %s 应为单一数值变量或分类变量", v))
      rng <- format(range(x), digits = 4, trim = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(varname = v, Interval = paste(rng[1], "to", rng[2]), Reference = FALSE,
                                              describe(rep(TRUE, length(x))), effect(est[idx, ]), stringsAsFactors = FALSE)
    }
  }
  blank <- describe(rep(FALSE, nrow(prep$data)))
  blank[] <- lapply(blank, function(col) if (is.character(col)) NA_character_ else NA_real_)
  for (e in extra) {
    idx <- if (e %in% attr(stats::terms(fit), "term.labels")) .coef_index(fit, e) else match(e, est$coef_name)
    if (length(idx) == 0L || anyNA(idx)) {
      warning(sprintf("extra_vars 中的 %s 未在模型系数中找到，已忽略", e))
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(varname = est$coef_name[idx], Interval = NA_character_, Reference = FALSE,
                                            blank[rep(1L, length(idx)), , drop = FALSE], effect(est[idx, ]),
                                            stringsAsFactors = FALSE)
  }
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

.reg_info <- function(title, prep, vc, conf_level, extra_lines = character(), ...) {
  c(list(...), list(
    n_input = prep$n_input, n_dropped = prep$n_drop, variance = vc$type, conf_level = conf_level,
    header = c(title, extra_lines,
               sprintf("方差估计: %s；置信水平: %g%%",
                       if (vc$type == "robust") "稳健（三明治）方差" else "模型方差", 100 * conf_level))
  ))
}

# ---- 回归引擎：结局校验与分析样本、按模型族拟合任意公式、结果表 ----

.REG_TITLES <- c(logit = "逻辑回归", lm = "线性回归", clogit = "条件逻辑回归")

.reg_check_args <- function(model, robust, ci_method, match_id, method) {
  if (identical(ci_method, "profile") && isTRUE(robust)) stop("剖面似然 CI 不能与 robust = TRUE 同时使用")
  if (model == "clogit") {
    if (is.null(match_id) || !is.character(match_id) || length(match_id) != 1L) stop("match_id 必须为单个匹配组变量名")
    if (isTRUE(robust) && (is.null(method) || identical(method, "exact"))) {
      stop("survival::clogit 的精确法（method = \"exact\"，默认）不支持稳健方差；",
           "如需稳健方差，请同时设置 method = \"approximate\"（Breslow 近似）或 \"efron\"", call. = FALSE)
    }
  }
  invisible(NULL)
}

# 分析样本与结局：logit 统一为 0/1，clogit 校验 0/1 并报告匹配组，lm 校验数值型
.reg_prepare <- function(model, data, outcome, mainvar, covars, robust, match_id = NULL, label = NULL, quiet = FALSE) {
  prep <- .prep_reg(data, outcome, mainvar, covars, robust, match_id = match_id, label = label, quiet = quiet)
  y <- prep$data[[outcome]]
  if (model == "logit") {
    y <- .binary_outcome(y, outcome, quiet = quiet)
  } else if (model == "clogit") {
    if (is.logical(y)) y <- as.integer(y)
    if (!is.numeric(y) || any(!y %in% c(0, 1))) stop("结局 ", outcome, " 必须为 0/1 或 TRUE/FALSE")
    informative <- tapply(y, prep$data[[match_id]], function(v) any(v == 1) && any(v == 0))
    prep$n_sets <- length(informative)
    prep$n_informative <- sum(informative)
    if (!quiet) .msg(label, sprintf("匹配组: %d 个，其中同时含病例与对照的有效匹配组 %d 个", prep$n_sets, prep$n_informative))
    if (prep$n_informative == 0L) stop("没有同时含病例与对照的匹配组")
  } else if (!is.numeric(y)) {
    stop("结局 ", outcome, " 必须为数值型")
  }
  prep$data[[outcome]] <- y
  prep$model <- model
  prep$outcome <- outcome
  prep$match_id <- match_id
  prep
}

# 按模型族拟合 formula（clogit 自动加入 strata(match_id) 与稳健方差时的 cluster()）；fit_args 为用户传入的 ...
.reg_fit <- function(prep, formula, robust, fit_args = list()) {
  d <- prep$data
  if (prep$model == "logit") {
    glm_warnings <- character()
    fit_with <- function(...) stats::glm(formula, data = d, family = stats::binomial(), ...)
    fit <- withCallingHandlers(do.call(fit_with, fit_args), warning = function(w) {
      if (grepl("fitted probabilities numerically 0 or 1|did not converge", conditionMessage(w))) {
        glm_warnings <<- c(glm_warnings, conditionMessage(w))
        invokeRestart("muffleWarning")
      }
    })
    attr(fit, "glm_warnings") <- glm_warnings
    return(fit)
  }
  if (prep$model == "lm") {
    fit_with <- function(...) stats::lm(formula, data = d, ...)
    return(do.call(fit_with, fit_args))
  }
  rhs <- c(attr(stats::terms(formula), "term.labels"), sprintf("strata(%s)", .bt(prep$match_id)),
           if (isTRUE(robust) && !is.null(prep$cluster_var)) sprintf("cluster(%s)", .bt(prep$cluster_var)))
  fml <- stats::reformulate(rhs, response = formula[[2]])
  environment(fml) <- new.env(parent = asNamespace("survival"))
  # clogit 在调用者环境中构造并求值 coxph(Surv(...)) 调用，需在此局部提供这两个函数
  coxph <- survival::coxph
  Surv <- survival::Surv
  fit_with <- function(...) {
    if (isTRUE(robust)) survival::clogit(fml, data = d, robust = TRUE, ...) else survival::clogit(fml, data = d, ...)
  }
  do.call(fit_with, fit_args)
}

.reg_describe <- function(prep) {
  y <- prep$data[[prep$outcome]]
  switch(prep$model,
    logit = function(rows) {
      n_event <- sum(y[rows] == 1L)
      n_total <- sum(rows)
      data.frame(N_event = n_event, N_total = n_total, Case_Total = paste0(n_event, "/", n_total), stringsAsFactors = FALSE)
    },
    clogit = function(rows) data.frame(N_case = sum(y[rows] == 1), N_control = sum(y[rows] == 0)),
    lm = function(rows) {
      data.frame(N = sum(rows), Mean = if (any(rows)) mean(y[rows]) else NA_real_,
                 SD = if (sum(rows) > 1L) stats::sd(y[rows]) else NA_real_)
    }
  )
}

.reg_sample_line <- function(prep, fit) {
  y <- prep$data[[prep$outcome]]
  switch(prep$model,
    logit = sprintf("分析样本: %d 例，事件 %d（剔除缺失 %d 行）", nrow(prep$data), sum(y), prep$n_drop),
    clogit = sprintf("分析样本: %d 例（病例 %d，对照 %d；剔除缺失 %d 行）；匹配组 %d 个，有效 %d 个",
                     nrow(prep$data), sum(y == 1), sum(y == 0), prep$n_drop, prep$n_sets, prep$n_informative),
    lm = {
      s <- summary(fit)
      sprintf("分析样本: %d 例（剔除缺失 %d 行）；R² = %.3f，调整 R² = %.3f", nrow(prep$data), prep$n_drop, s$r.squared, s$adj.r.squared)
    }
  )
}

# 单个模型：准备、拟合、分离检测（logit）、方差、结果表与 info
.reg_core <- function(model, data, outcome, mainvar, covars, extra_vars, robust, conf_level, ci_method = "wald",
                      match_id = NULL, fit_args = list(), label = NULL, quiet = FALSE) {
  extra <- .split_extra(extra_vars, data, covars)
  prep <- .reg_prepare(model, data, outcome, mainvar, covars, robust, match_id, label, quiet)
  fit <- .reg_fit(prep, prep$formula, robust, fit_args)
  separation <- if (model == "logit") .check_separation(prep, prep$data[[outcome]], attr(fit, "glm_warnings"), label) else NULL
  vc <- .reg_vcov(fit, prep, robust)
  est <- .reg_wald(fit, vc, conf_level, ci_method)
  res <- .reg_table(prep, fit, est, c(mainvar, extra$focus), extra$coef, .reg_describe(prep), exp_effect = model != "lm")
  y <- prep$data[[outcome]]
  extra_info <- switch(model,
    logit = list(ci_method = ci_method, n = nrow(prep$data), nevent = sum(y), separation = separation),
    clogit = list(n = nrow(prep$data), n_case = sum(y == 1), n_sets = prep$n_sets, n_informative_sets = prep$n_informative,
                  method = fit$method),
    lm = {
      s <- summary(fit)
      list(n = nrow(prep$data), r_squared = s$r.squared, adj_r_squared = s$adj.r.squared)
    }
  )
  lines <- c(.reg_sample_line(prep, fit), if (model == "logit") sprintf("CI 方法: %s", if (ci_method == "wald") "Wald" else "剖面似然"))
  info <- do.call(.reg_info, c(list(.REG_TITLES[[model]], prep, vc, conf_level, lines), extra_info))
  list(prep = prep, fit = fit, vc = vc, est = est, result = res, info = info)
}

#' 逻辑回归
#'
#' 调用 \code{glm(family = binomial)}；输出暴露各水平的病例数/总人数、OR 及 CI、P 值。
#' 检测（准）完全分离并给出警告：模型中分类变量的某水平只含事件或只含非事件，或 glm 提示不收敛/拟合概率为 0 或 1；
#' 结果照常返回，\code{info$separation} 记录是否检出。
#' 拟合前按完整病例生成分析样本并以 message 报告缺失处理。
#'
#' @param data 数据框
#' @param outcome 二分类结局变量名：0/1、TRUE/FALSE，或二水平因子（第二个水平为事件）
#' @param mainvar 关注变量名（字符串向量）。数值型输出一行；因子/字符型输出参照水平与各水平
#' @param covars 协变量（字符串向量，可为NULL）。列名按变量处理；也可写表达式，如 \code{"a*b"}、\code{"log(x)"}；
#'   \code{"cluster(id)"} 仅用于 robust = TRUE 时指定聚类变量
#' @param extra_vars 额外输出的项：covars 中的列名（与 mainvar 同样输出）或项名/系数名（仅输出效应量）
#' @param robust 方差估计：FALSE（默认）使用模型方差；TRUE 使用 sandwich 稳健方差
#'   （有 \code{cluster()} 时为 \code{sandwich::vcovCL}，否则为 \code{sandwich::vcovHC}，均取 sandwich 默认类型）
#' @param ci_method CI 方法："wald"（默认，与 P 值口径一致）或 "profile"（剖面似然，\code{confint()}；不能与 robust = TRUE 同用）
#' @param conf_level 置信水平，默认 0.95
#' @param ... 传递给 \code{glm} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 glm 模型，\code{result} 为结果表，\code{info} 为样本与设置信息
#' @export
logit_run <- function(data, outcome, mainvar, covars = NULL, extra_vars = NULL, robust = FALSE,
                      ci_method = c("wald", "profile"), conf_level = 0.95, ...) {
  ci_method <- match.arg(ci_method)
  .validate_conf_level(conf_level)
  .reg_check_args("logit", robust, ci_method, NULL, NULL)
  core <- .reg_core("logit", data, outcome, mainvar, covars, extra_vars, robust, conf_level, ci_method, fit_args = list(...))
  invisible(.new_result(model = core$fit, result = core$result, info = core$info))
}

#' 线性回归
#'
#' 调用 \code{lm}；推断基于 t 分布（残差自由度）。输出暴露各水平的人数、结局均值与标准差、beta 及 CI、P 值。
#'
#' @inheritParams logit_run
#' @param outcome 连续结局变量名（数值型）
#' @param robust 方差估计：FALSE（默认）使用模型方差；TRUE 使用 sandwich 稳健方差
#'   （有 \code{cluster()} 时为 \code{sandwich::vcovCL}，否则为 \code{sandwich::vcovHC}，均取 sandwich 默认类型）
#' @param ... 传递给 \code{lm} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 lm 模型，\code{result} 为结果表，\code{info} 为样本与设置信息
#' @export
lm_run <- function(data, outcome, mainvar, covars = NULL, extra_vars = NULL, robust = FALSE, conf_level = 0.95, ...) {
  .validate_conf_level(conf_level)
  core <- .reg_core("lm", data, outcome, mainvar, covars, extra_vars, robust, conf_level, fit_args = list(...))
  invisible(.new_result(model = core$fit, result = core$result, info = core$info))
}

#' 条件逻辑回归
#'
#' 调用 \code{survival::clogit}（默认 \code{method = "exact"}，精确条件似然）；匹配组由 \code{match_id} 指定。
#' 输出暴露各水平的病例数与对照数、OR 及 CI、P 值，并报告有效匹配组数
#' （仅含病例或仅含对照的匹配组对条件似然无贡献）。匹配组大、结多时精确法可能很慢，可经 \code{...} 设置 \code{method = "approximate"}。
#'
#' @inheritParams logit_run
#' @param outcome 病例/对照指示变量名：0/1 或 TRUE/FALSE（1 为病例）
#' @param match_id 匹配组变量名（必填）
#' @param covars 协变量（字符串向量，可为NULL）；不支持 \code{strata()}（匹配组用 match_id 指定）；
#'   \code{"cluster(id)"} 仅用于 robust = TRUE 时指定聚类变量
#' @param robust 方差估计：FALSE（默认）使用模型方差；TRUE 使用 coxph 稳健方差（按 \code{cluster()} 聚类，未指定则按观测）。
#'   survival 的精确法不支持稳健方差，此时须同时设置 \code{method = "approximate"} 或 \code{"efron"}
#' @param ... 传递给 \code{survival::clogit} 的其他参数（如 \code{method}）
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 clogit 模型，\code{result} 为结果表，\code{info} 为样本与设置信息
#' @export
clogit_run <- function(data, outcome, mainvar, match_id, covars = NULL, extra_vars = NULL, robust = FALSE,
                       conf_level = 0.95, ...) {
  .validate_conf_level(conf_level)
  if (missing(match_id)) match_id <- NULL
  .reg_check_args("clogit", robust, "wald", match_id, list(...)$method)
  core <- .reg_core("clogit", data, outcome, mainvar, covars, extra_vars, robust, conf_level, match_id = match_id,
                    fit_args = list(...))
  invisible(.new_result(model = core$fit, result = core$result, info = core$info))
}
