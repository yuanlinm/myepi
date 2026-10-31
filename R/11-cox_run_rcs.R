#' 限制性立方样条（RCS）Cox 回归
#'
#' 采用 rms 的标准流程：\code{rms::cph} 拟合含 \code{rms::rcs(mainvar, 节点)} 的 Cox 模型，
#' \code{rms::Predict(ref.zero = TRUE, fun = exp)} 计算相对参照值的 HR 曲线及 CI，
#' \code{anova()} 给出总体关联与非线性的 Wald 检验。需要安装 rms 包。
#'
#' 节点与参照值在分析样本（完整病例）中计算；指定 idL 时按“个体 × 取值”去重后计算。
#' covars 中的 \code{strata(x)} 自动转为 rms 的 \code{strat(x)}；稳健方差由 \code{rms::robcov} 计算
#' （按 \code{cluster()} 变量聚类，未写 \code{cluster()} 但指定了 idL 时按 idL 聚类），此时 Predict 与 anova 均使用稳健方差。
#' 不允许 covars 中出现含 mainvar 的项；不支持 \code{pspline()} 等 cph 不支持的项。变量名需符合 R 语法。
#'
#' @inheritParams cox_run
#' @param mainvar 连续暴露变量名（字符串）
#' @param nk 节点数，默认 3（允许 3–7）；未提供 knots / knot_probs 时按 Harrell 推荐的分位数放置（\code{Hmisc::rcspline.eval}）
#' @param knots 节点位置（绝对值，至少 3 个），与 knot_probs 二选一
#' @param knot_probs 节点位置（分位数，至少 3 个），与 knots 二选一
#' @param ref 参照值（HR = 1），默认为 mainvar 的中位数
#' @param range_probs 预测范围的分位数，如 \code{c(0.01, 0.99)}；默认 NULL 为观测范围 [min, max]
#' @param robust 方差估计：FALSE（默认）使用模型方差；TRUE 使用稳健方差（\code{rms::robcov}）
#' @param ... 传递给 \code{rms::cph} 的其他参数；收敛标准默认 \code{eps = 1e-9, tol = 1e-12}（与 \code{survival::coxph} 一致，rms 默认为 1e-4），可在此覆盖
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model} 为 cph 模型，
#'   \code{result} 为 500 点曲线数据（x、hr、ll、ul），\code{tests} 为总体与非线性检验，
#'   \code{knots} 为节点，\code{ref} 为参照值，\code{info} 为样本与设置信息
#' @export
cox_run_rcs <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, idL = NULL,
                        nk = 3, knots = NULL, knot_probs = NULL, ref = NULL, range_probs = NULL,
                        robust = FALSE, conf_level = 0.95, ...) {
  if (!requireNamespace("rms", quietly = TRUE)) {
    stop("cox_run_rcs 需要 rms 包，请先安装: install.packages(\"rms\")", call. = FALSE)
  }
  setup <- .spline_setup(data, time1, time2, timediff, event, mainvar, covars, idL, robust, range_probs, conf_level)
  prep <- setup$prep
  ana <- prep$data
  basis <- setup$basis

  unsupported <- grepl("\\b(pspline|ridge|frailty|tt)\\s*\\(", prep$covars)
  if (any(unsupported)) stop("rms::cph 不支持以下协变量项: ", paste(prep$covars[unsupported], collapse = ", "))
  model_vars <- setdiff(all.vars(prep$formula), character(0))
  bad_names <- model_vars[make.names(model_vars) != model_vars]
  if (length(bad_names) > 0L) stop("rms::cph 要求变量名符合 R 语法，请先重命名: ", paste(bad_names, collapse = ", "))

  # 节点
  if (!is.null(knots) && !is.null(knot_probs)) stop("knots 与 knot_probs 只能二选一")
  if (!is.null(knots)) {
    if (!is.numeric(knots) || length(knots) < 3L || anyNA(knots) || anyDuplicated(knots)) stop("knots 至少需要 3 个互不相同的数值")
    knots <- sort(knots)
    knot_rule <- "用户指定（绝对值）"
  } else if (!is.null(knot_probs)) {
    if (!is.numeric(knot_probs) || length(knot_probs) < 3L || anyNA(knot_probs) || any(knot_probs <= 0 | knot_probs >= 1)) {
      stop("knot_probs 至少需要 3 个 (0, 1) 内的概率")
    }
    knot_probs <- sort(knot_probs)
    knots <- stats::quantile(basis, knot_probs, names = FALSE)
    if (anyDuplicated(knots)) stop("按 knot_probs 计算的节点存在重复值，请调整分位数或改用 knots")
    knot_rule <- sprintf("用户指定分位数 %s", paste(knot_probs, collapse = ", "))
  } else {
    if (!is.numeric(nk) || length(nk) != 1L || !nk %in% 3:7) stop("nk 必须为 3 到 7 的整数")
    knots <- Hmisc::rcspline.eval(basis, nk = nk, knots.only = TRUE)
    if (length(knots) < nk) warning(sprintf("%s 取值重复较多，实际仅得到 %d 个节点", mainvar, length(knots)), call. = FALSE)
    knot_rule <- "Harrell 默认分位数"
  }
  if (length(knots) < 3L) stop("至少需要 3 个节点")
  message(sprintf("RCS 节点（k = %d，%s）: %s", length(knots), knot_rule, paste(format(knots, digits = 4), collapse = ", ")))

  # 参照值
  if (is.null(ref)) ref <- stats::median(basis)
  if (!is.numeric(ref) || length(ref) != 1L || !is.finite(ref) || ref < min(ana[[mainvar]]) || ref > max(ana[[mainvar]])) {
    stop("ref 必须为 mainvar 观测范围内的单个数值")
  }
  message(sprintf("参照值（HR = 1）: %s = %s", mainvar, format(ref, digits = 4)))

  # cph 公式：strata() → strat()；cluster() 不进入公式，由 robcov 处理
  is_cluster <- grepl("^\\s*cluster\\s*\\(", prep$covars)
  cluster_var <- NULL
  if (isTRUE(robust)) {
    if (any(is_cluster)) {
      cluster_var <- all.vars(str2lang(prep$covars[is_cluster][1]))
    } else if (!is.null(idL)) {
      cluster_var <- idL
      message("robust = TRUE：按 idL（", idL, "）聚类计算稳健方差")
    }
  }
  cph_covars <- gsub("\\bstrata\\s*\\(", "strat(", prep$covars[!is_cluster])
  lhs <- sub("^survival::", "", prep$lhs)
  rhs <- c(sprintf("rcs(%s, %s)", mainvar, paste(deparse(knots, control = "digits17"), collapse = "")), cph_covars)
  fml <- stats::as.formula(paste(lhs, "~", paste(rhs, collapse = " + ")))

  # rms 在调用 cph 的函数环境中解析 rcs/strat/Surv；datadist 仅在本函数内临时设置
  rcs <- rms::rcs
  strat <- rms::strat
  Surv <- survival::Surv
  dd_vars <- setdiff(all.vars(fml), all.vars(str2lang(lhs)))
  dd <- rms::datadist(ana[dd_vars])
  dd$limits["Adjust to", mainvar] <- ref
  old <- options(datadist = dd)
  on.exit(options(old), add = TRUE)

  # 收敛标准与 survival::coxph 一致（eps = 1e-9）；rms 默认 eps = 1e-4 会提前停止迭代并影响检验数值。可经 ... 覆盖
  fit_cph <- function(..., eps = 1e-9, tol = 1e-12) rms::cph(fml, data = ana, x = TRUE, y = TRUE, eps = eps, tol = tol, ...)
  fit <- fit_cph(...)
  if (isTRUE(robust)) {
    fit <- if (is.null(cluster_var)) rms::robcov(fit) else rms::robcov(fit, cluster = ana[[cluster_var]])
  }

  a <- stats::anova(fit)
  row_overall <- match(mainvar, rownames(a))
  row_nonlinear <- row_overall + 1L
  if (is.na(row_overall) || trimws(rownames(a)[row_nonlinear]) != "Nonlinear") stop("无法从 anova 结果中定位 mainvar 的检验")
  tests <- data.frame(
    test = c("overall", "nonlinear"),
    method = "Wald (rms::anova)",
    statistic = unname(a[c(row_overall, row_nonlinear), "Chi-Square"]),
    df = unname(a[c(row_overall, row_nonlinear), "d.f."]),
    P = unname(a[c(row_overall, row_nonlinear), "P"]),
    stringsAsFactors = FALSE
  )

  pred_args <- c(list(fit), stats::setNames(list(setup$grid), mainvar),
                 list(ref.zero = TRUE, fun = exp, conf.int = conf_level))
  pred <- as.data.frame(do.call(rms::Predict, pred_args))
  curve <- data.frame(x = pred[[mainvar]], hr = pred$yhat, ll = pred$lower, ul = pred$upper)

  n <- unname(fit$stats["Obs"])
  nevent <- unname(fit$stats["Events"])
  info <- .spline_info(prep, n, nevent, robust, conf_level, mainvar, "rcs", c(
    sprintf("RCS Cox 回归: %s，k = %d 个节点（%s），参照值 %s", mainvar, length(knots),
            paste(format(knots, digits = 4), collapse = ", "), format(ref, digits = 4)),
    sprintf("总体关联 P = %s；非线性 P = %s（Wald, rms::anova）",
            format.pval(tests$P[1], digits = 3, eps = 0.001), format.pval(tests$P[2], digits = 3, eps = 0.001))
  ))
  invisible(.new_result(model = fit, result = curve, tests = tests, knots = knots, ref = ref, info = info))
}
