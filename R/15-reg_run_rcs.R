# 逻辑回归、条件逻辑回归与线性回归的限制性立方样条（RCS）：输出 500 点曲线，可直接用于 plot_crf()

.reg_spline_setup <- function(data, outcome, mainvar, covars, robust, range_probs, conf_level, match_id = NULL) {
  .validate_conf_level(conf_level)
  if (!is.data.frame(data)) stop("data 必须为数据框")
  if (!is.character(mainvar) || length(mainvar) != 1L || !mainvar %in% names(data)) stop("mainvar 必须为数据框中的单个变量名")
  if (!is.numeric(data[[mainvar]])) stop("mainvar 必须为数值型连续变量")
  involves <- vapply(covars, function(term) mainvar %in% all.vars(str2lang(.term_label(term, data))),
                     logical(1), USE.NAMES = FALSE)
  if (any(involves)) stop("covars 中不允许含 mainvar 的项（交互或变换）: ", paste(covars[involves], collapse = ", "))
  if (!is.null(range_probs) && (!is.numeric(range_probs) || length(range_probs) != 2L || anyNA(range_probs) ||
                                range_probs[1] < 0 || range_probs[2] > 1 || range_probs[1] >= range_probs[2])) {
    stop("range_probs 必须为 [0, 1] 内两个递增的概率，如 c(0.01, 0.99)")
  }
  prep <- .prep_reg(data, outcome, mainvar, covars, robust, match_id = match_id)
  x <- prep$data[[mainvar]]
  lim <- if (is.null(range_probs)) range(x) else stats::quantile(x, range_probs, names = FALSE)
  if (lim[1] >= lim[2]) stop(sprintf("%s 在预测范围内只有一个取值", mainvar))
  grid <- seq(lim[1], lim[2], length.out = .SPLINE_N_POINTS)
  message(sprintf("预测网格: %s 在 [%s, %s] 内等距 %d 点%s", mainvar, format(lim[1], digits = 4), format(lim[2], digits = 4),
                  .SPLINE_N_POINTS, if (is.null(range_probs)) "（观测范围）" else
                    sprintf("（%g%%–%g%% 分位数）", 100 * range_probs[1], 100 * range_probs[2])))
  list(prep = prep, basis = x, grid = grid)
}

.rms_check_names <- function(prep) {
  vars <- all.vars(prep$formula)
  bad <- vars[make.names(vars) != vars]
  if (length(bad) > 0L) stop("rms 要求变量名符合 R 语法，请先重命名: ", paste(bad, collapse = ", "))
}

.reg_rcs_info <- function(title, prep, robust, conf_level, mainvar, knots, ref, tests, model, ...) {
  c(list(...), list(
    mainvar = mainvar, method = "rcs", model = model, n_input = prep$n_input, n_dropped = prep$n_drop,
    variance = if (isTRUE(robust)) "robust" else "model", conf_level = conf_level,
    header = c(
      sprintf("%s: %s，k = %d 个节点（%s），参照值 %s", title, mainvar, length(knots),
              paste(format(knots, digits = 4), collapse = ", "), format(ref, digits = 4)),
      sprintf("总体关联 P = %s；非线性 P = %s（%s）", format.pval(tests$P[1], digits = 3, eps = 0.001),
              format.pval(tests$P[2], digits = 3, eps = 0.001), tests$method[1]),
      sprintf("分析样本: %d 例（剔除缺失 %d 行）", nrow(prep$data), prep$n_drop),
      sprintf("方差估计: %s；置信水平: %g%%", if (isTRUE(robust)) "稳健（三明治）方差，rms::robcov" else "模型方差", 100 * conf_level)
    )
  ))
}

# lrm / ols 共用：rcs 公式、datadist、拟合、robcov、anova 与 Predict
.rms_reg_rcs <- function(fitter, prep, outcome, mainvar, knots, ref, grid, robust, conf_level, fun, ...) {
  .rms_check_names(prep)
  rhs <- c(sprintf("rcs(%s, %s)", mainvar, paste(deparse(knots, control = "digits17"), collapse = "")), prep$covars)
  fml <- stats::as.formula(paste(outcome, "~", paste(rhs, collapse = " + ")))
  # rms 在调用拟合函数的环境中解析 rcs；datadist 仅在本函数内临时设置
  rcs <- rms::rcs
  dd <- rms::datadist(prep$data[setdiff(all.vars(fml), outcome)])
  dd$limits["Adjust to", mainvar] <- ref
  old <- options(datadist = dd)
  on.exit(options(old), add = TRUE)
  fit <- fitter(fml, data = prep$data, x = TRUE, y = TRUE, ...)
  if (isTRUE(robust)) {
    fit <- if (is.null(prep$cluster_var)) rms::robcov(fit) else rms::robcov(fit, cluster = prep$data[[prep$cluster_var]])
  }
  tests <- .rms_tests(fit, mainvar)
  pred_args <- c(list(fit), stats::setNames(list(grid), mainvar), list(ref.zero = TRUE, conf.int = conf_level))
  if (!is.null(fun)) pred_args$fun <- fun
  pred <- as.data.frame(do.call(rms::Predict, pred_args))
  list(fit = fit, tests = tests, pred = pred)
}

.need_rms <- function(fname) {
  if (!requireNamespace("rms", quietly = TRUE)) stop(fname, " 需要 rms 包，请先安装: install.packages(\"rms\")", call. = FALSE)
}

#' 限制性立方样条（RCS）逻辑回归
#'
#' 采用 rms 的标准流程：\code{rms::lrm} 拟合含 \code{rcs(mainvar, 节点)} 的逻辑回归，
#' \code{rms::Predict(ref.zero = TRUE, fun = exp)} 计算相对参照值的 OR 曲线及 CI，\code{anova()} 给出总体关联与非线性 Wald 检验。
#' 输出可直接用于 \code{plot_crf()}。需要安装 rms 包。
#'
#' @inheritParams logit_run
#' @param mainvar 连续暴露变量名（字符串）
#' @param covars 协变量（字符串向量，可为NULL），写法同 \code{logit_run}；不允许含 mainvar 的项
#' @param nk,knots,knot_probs 节点设置，同 \code{cox_run_rcs}（nk 默认 3）
#' @param ref 参照值（OR = 1），默认为 mainvar 的中位数
#' @param range_probs 预测范围的分位数，如 \code{c(0.01, 0.99)}；默认 NULL 为观测范围
#' @param robust 方差估计：FALSE（默认）使用模型方差；TRUE 使用 \code{rms::robcov}（按 \code{cluster()} 聚类，未指定则按观测）
#' @param ... 传递给 \code{rms::lrm} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 lrm 模型，\code{result} 为 500 点曲线数据（x、or、ll、ul），
#'   \code{tests} 为总体与非线性检验，\code{knots}、\code{ref}、\code{info}
#' @export
logit_run_rcs <- function(data, outcome, mainvar, covars = NULL, nk = 3, knots = NULL, knot_probs = NULL, ref = NULL,
                          range_probs = NULL, robust = FALSE, conf_level = 0.95, ...) {
  .need_rms("logit_run_rcs")
  setup <- .reg_spline_setup(data, outcome, mainvar, covars, robust, range_probs, conf_level)
  prep <- setup$prep
  prep$data[[outcome]] <- .binary_outcome(prep$data[[outcome]], outcome)
  .check_separation(prep, prep$data[[outcome]])
  knots <- .rcs_knots(setup$basis, nk, knots, knot_probs, mainvar)
  ref <- .rcs_ref(ref, setup$basis, prep$data[[mainvar]], mainvar, "OR")
  r <- .rms_reg_rcs(rms::lrm, prep, outcome, mainvar, knots, ref, setup$grid, robust, conf_level, exp, ...)
  if (isTRUE(r$fit$fail)) warning("rms::lrm 未收敛，结果不可靠（可能存在分离）", call. = FALSE)
  curve <- data.frame(x = r$pred[[mainvar]], or = r$pred$yhat, ll = r$pred$lower, ul = r$pred$upper)
  y <- prep$data[[outcome]]
  info <- .reg_rcs_info("RCS 逻辑回归", prep, robust, conf_level, mainvar, knots, ref, r$tests, "logit",
                        n = nrow(prep$data), nevent = sum(y))
  invisible(.new_result(model = r$fit, result = curve, tests = r$tests, knots = knots, ref = ref, info = info))
}

#' 限制性立方样条（RCS）线性回归
#'
#' 采用 rms 的标准流程：\code{rms::ols} 拟合含 \code{rcs(mainvar, 节点)} 的线性回归，
#' \code{rms::Predict(ref.zero = TRUE)} 计算相对参照值的均值差（beta）曲线及 CI（t 分布），\code{anova()} 给出总体关联与非线性 F 检验
#' （robust = TRUE 时 rms 改用 Wald χ²）。输出可直接用于 \code{plot_crf()}。需要安装 rms 包。
#'
#' @inheritParams logit_run_rcs
#' @param outcome 连续结局变量名（数值型）
#' @param ref 参照值（beta = 0），默认为 mainvar 的中位数
#' @param ... 传递给 \code{rms::ols} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 ols 模型，\code{result} 为 500 点曲线数据（x、beta、ll、ul），
#'   \code{tests} 为总体与非线性检验，\code{knots}、\code{ref}、\code{info}
#' @export
lm_run_rcs <- function(data, outcome, mainvar, covars = NULL, nk = 3, knots = NULL, knot_probs = NULL, ref = NULL,
                       range_probs = NULL, robust = FALSE, conf_level = 0.95, ...) {
  .need_rms("lm_run_rcs")
  setup <- .reg_spline_setup(data, outcome, mainvar, covars, robust, range_probs, conf_level)
  prep <- setup$prep
  if (!is.numeric(prep$data[[outcome]])) stop("结局 ", outcome, " 必须为数值型")
  knots <- .rcs_knots(setup$basis, nk, knots, knot_probs, mainvar)
  ref <- .rcs_ref(ref, setup$basis, prep$data[[mainvar]], mainvar, "beta")
  r <- .rms_reg_rcs(rms::ols, prep, outcome, mainvar, knots, ref, setup$grid, robust, conf_level, NULL, ...)
  curve <- data.frame(x = r$pred[[mainvar]], beta = r$pred$yhat, ll = r$pred$lower, ul = r$pred$upper)
  info <- .reg_rcs_info("RCS 线性回归", prep, robust, conf_level, mainvar, knots, ref, r$tests, "lm",
                        n = nrow(prep$data))
  invisible(.new_result(model = r$fit, result = curve, tests = r$tests, knots = knots, ref = ref, info = info))
}

#' 限制性立方样条（RCS）条件逻辑回归
#'
#' rms 无条件逻辑回归，故采用 \code{survival::clogit}（默认精确条件似然）拟合含 RCS 基函数的模型；
#' 基函数与节点与 \code{rms::rcs} 相同（\code{Hmisc::rcspline.eval}），曲线为相对参照值的对比
#' （定义与 \code{rms::Predict(ref.zero = TRUE)} 相同），总体关联与非线性检验为样条系数的 Wald χ²（定义与 \code{rms::anova} 相同）。
#' 输出可直接用于 \code{plot_crf()}。需要安装 Hmisc 包。
#'
#' @inheritParams clogit_run
#' @param mainvar 连续暴露变量名（字符串）
#' @param covars 协变量（字符串向量，可为NULL），写法同 \code{clogit_run}；不允许含 mainvar 的项
#' @param nk,knots,knot_probs 节点设置，同 \code{cox_run_rcs}（nk 默认 3）
#' @param ref 参照值（OR = 1），默认为 mainvar 的中位数
#' @param range_probs 预测范围的分位数；默认 NULL 为观测范围
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 clogit 模型，\code{result} 为 500 点曲线数据（x、or、ll、ul），
#'   \code{tests} 为总体与非线性检验，\code{knots}、\code{ref}、\code{info}
#' @export
clogit_run_rcs <- function(data, outcome, mainvar, match_id, covars = NULL, nk = 3, knots = NULL, knot_probs = NULL,
                           ref = NULL, range_probs = NULL, robust = FALSE, conf_level = 0.95, ...) {
  if (!requireNamespace("Hmisc", quietly = TRUE)) stop("clogit_run_rcs 需要 Hmisc 包，请先安装: install.packages(\"Hmisc\")", call. = FALSE)
  if (missing(match_id) || !is.character(match_id) || length(match_id) != 1L) stop("match_id 必须为单个匹配组变量名")
  method <- list(...)$method
  if (isTRUE(robust) && (is.null(method) || identical(method, "exact"))) {
    stop("survival::clogit 的精确法（method = \"exact\"，默认）不支持稳健方差；",
         "如需稳健方差，请同时设置 method = \"approximate\"（Breslow 近似）或 \"efron\"", call. = FALSE)
  }
  setup <- .reg_spline_setup(data, outcome, mainvar, covars, robust, range_probs, conf_level, match_id = match_id)
  prep <- setup$prep
  y <- prep$data[[outcome]]
  if (is.logical(y)) y <- as.integer(y)
  if (!is.numeric(y) || any(!y %in% c(0, 1))) stop("结局 ", outcome, " 必须为 0/1 或 TRUE/FALSE")
  prep$data[[outcome]] <- y
  informative <- tapply(y, prep$data[[match_id]], function(v) any(v == 1) && any(v == 0))
  message(sprintf("匹配组: %d 个，其中同时含病例与对照的有效匹配组 %d 个", length(informative), sum(informative)))
  if (sum(informative) == 0L) stop("没有同时含病例与对照的匹配组")

  knots <- .rcs_knots(setup$basis, nk, knots, knot_probs, mainvar)
  ref <- .rcs_ref(ref, setup$basis, prep$data[[mainvar]], mainvar, "OR")

  # RCS 基函数作为独立列进入模型（与 rms::rcs 相同的 Hmisc::rcspline.eval 基）
  B <- Hmisc::rcspline.eval(prep$data[[mainvar]], knots = knots, inclx = TRUE)
  basis_names <- make.unique(c(names(prep$data), paste0(".rcs_", seq_len(ncol(B)))))[ncol(prep$data) + seq_len(ncol(B))]
  for (j in seq_len(ncol(B))) prep$data[[basis_names[j]]] <- B[, j]
  rhs <- c(basis_names, vapply(prep$covars, .term_label, character(1), data = prep$data, USE.NAMES = FALSE),
           sprintf("strata(%s)", .bt(match_id)),
           if (isTRUE(robust) && !is.null(prep$cluster_var)) sprintf("cluster(%s)", .bt(prep$cluster_var)))
  fml <- stats::reformulate(rhs, response = str2lang(.bt(outcome)))
  environment(fml) <- new.env(parent = asNamespace("survival"))
  # clogit 在调用者环境中构造并求值 coxph(Surv(...)) 调用，需在此局部提供这两个函数
  coxph <- survival::coxph
  Surv <- survival::Surv
  fit <- if (isTRUE(robust)) {
    survival::clogit(fml, data = prep$data, robust = TRUE, ...)
  } else {
    survival::clogit(fml, data = prep$data, ...)
  }
  vc <- .reg_vcov(fit, prep, robust)
  b <- stats::coef(fit)[basis_names]
  if (anyNA(b)) {
    stop(sprintf("%s 的样条系数无法估计：%s 在匹配组内无变异（可能为匹配变量），条件似然不含其信息", mainvar, mainvar), call. = FALSE)
  }
  V <- as.matrix(vc$V)
  if (is.null(dimnames(V))) dimnames(V) <- list(names(stats::coef(fit)), names(stats::coef(fit)))
  V <- V[basis_names, basis_names, drop = FALSE]

  # 相对参照值的对比：Δ = (B(x) − B(ref)) b，Var = Δ V Δ'
  D <- sweep(Hmisc::rcspline.eval(setup$grid, knots = knots, inclx = TRUE), 2,
             Hmisc::rcspline.eval(ref, knots = knots, inclx = TRUE))
  eta <- as.vector(D %*% b)
  se <- sqrt(rowSums((D %*% V) * D))
  z <- stats::qnorm(1 - (1 - conf_level) / 2)
  curve <- data.frame(x = setup$grid, or = exp(eta), ll = exp(eta - z * se), ul = exp(eta + z * se))

  wald <- function(idx) {
    stat <- as.numeric(t(b[idx]) %*% solve(V[idx, idx, drop = FALSE], b[idx]))
    c(stat, length(idx), stats::pchisq(stat, length(idx), lower.tail = FALSE))
  }
  w <- rbind(wald(seq_along(b)), wald(seq_along(b)[-1]))
  tests <- data.frame(test = c("overall", "nonlinear"), method = "Wald（样条系数）", statistic = w[, 1], df = w[, 2], P = w[, 3],
                      stringsAsFactors = FALSE)

  info <- .reg_rcs_info("RCS 条件逻辑回归", prep, robust, conf_level, mainvar, knots, ref, tests, "clogit",
                        n = nrow(prep$data), n_case = sum(y == 1), n_sets = length(informative),
                        n_informative_sets = sum(informative), clogit_method = fit$method)
  if (isTRUE(robust)) info$header[4] <- sub("rms::robcov", "survival::clogit", info$header[4], fixed = TRUE)
  invisible(.new_result(model = fit, result = curve, tests = tests, knots = knots, ref = ref, info = info))
}
