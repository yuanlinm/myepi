# 内部工具函数：Cox 系列函数共用的公式构建、分析样本、方差选择、效应量提取与描述统计

.validate_idL <- function(data, idL) {
  if (is.null(idL)) return(invisible(NULL))
  if (!is.character(idL) || length(idL) != 1L || is.na(idL) || !nzchar(idL)) {
    stop("idL 必须是单个非空变量名字符串，或设为 NULL")
  }
  if (!idL %in% names(data)) stop("idL 指定的个体标识变量不在数据框中")
  if (anyNA(data[[idL]])) stop("idL 指定的个体标识变量不能包含缺失值")
  invisible(NULL)
}

.validate_conf_level <- function(conf_level) {
  if (!is.numeric(conf_level) || length(conf_level) != 1L || is.na(conf_level) ||
      conf_level <= 0 || conf_level >= 1) {
    stop("conf_level 必须为 (0, 1) 区间内的单个数值")
  }
  invisible(NULL)
}

.validate_robust <- function(robust) {
  if (!isTRUE(robust) && !isFALSE(robust)) {
    stop("robust 只能为 FALSE（模型方差，默认）或 TRUE（稳健方差）")
  }
  invisible(NULL)
}

.msg <- function(label, ...) {
  message(if (!is.null(label)) paste0("[", label, "] "), ...)
}

# 非语法变量名加反引号，保证公式可解析
.bt <- function(x) {
  ifelse(make.names(x) == x, x, paste0("`", gsub("`", "\\\\`", x), "`"))
}

# 协变量：若恰为列名则视为变量（必要时加反引号），否则按 R 表达式原样使用（如 strata(x)、pspline(x)、a*b）
.term_label <- function(x, data) {
  if (x %in% names(data)) .bt(x) else x
}

.unit_divisor <- function(t_unit) {
  switch(t_unit, d = 365.25, y = 1, m = 12)
}

# 构建公式、校验变量与随访时间、按完整病例生成分析样本，并报告缺失处理结果。
# 方差只由 robust 决定：robust = FALSE 时忽略 cluster() 项；main_term 可将 mainvar 替换为样条等项
.prep_cox <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event,
                      mainvar, covars = NULL, idL = NULL, robust = FALSE,
                      label = NULL, quiet = FALSE, main_term = NULL) {
  if (!is.data.frame(data)) stop("data 必须为数据框")
  data <- as.data.frame(data)
  .validate_idL(data, idL)
  .validate_robust(robust)
  if (!is.character(mainvar) || length(mainvar) < 1L) stop("mainvar 必须为变量名字符串")
  if (!all(mainvar %in% names(data))) {
    stop("mainvar 不在数据框中: ", paste(setdiff(mainvar, names(data)), collapse = ", "))
  }
  if (any(mainvar %in% covars)) stop("mainvar 不应同时出现在 covars 中")

  if (!is.null(timediff)) {
    if (!is.null(time1) || !is.null(time2)) stop("timediff 与 time1/time2 只能二选一")
    lhs <- sprintf("survival::Surv(%s, %s)", .bt(timediff), .bt(event))
  } else {
    if (is.null(time1) || is.null(time2)) stop("请提供 timediff，或同时提供 time1 与 time2")
    lhs <- sprintf("survival::Surv(%s, %s, %s)", .bt(time1), .bt(time2), .bt(event))
  }

  for (v in mainvar) if (is.logical(data[[v]])) data[[v]] <- factor(data[[v]], levels = c(FALSE, TRUE))

  is_cluster <- grepl("^\\s*cluster\\s*\\(", covars)
  if (any(is_cluster) && !isTRUE(robust)) {
    if (!quiet) .msg(label, "robust = FALSE：忽略 ", paste(covars[is_cluster], collapse = ", "),
                     "，使用模型方差（如需稳健方差请设置 robust = TRUE）")
    covars <- covars[!is_cluster]
  }
  main <- if (is.null(main_term)) .bt(mainvar) else main_term
  rhs <- c(main, vapply(covars, .term_label, character(1), data = data, USE.NAMES = FALSE))
  fml <- stats::reformulate(rhs, response = str2lang(lhs))
  has_cluster <- !is.null(attr(stats::terms(fml, specials = "cluster"), "specials")$cluster)
  if (isTRUE(robust) && !has_cluster && !is.null(idL)) {
    rhs <- c(rhs, sprintf("cluster(%s)", .bt(idL)))
    fml <- stats::reformulate(rhs, response = str2lang(lhs))
    has_cluster <- TRUE
    if (!quiet) .msg(label, "robust = TRUE：按 idL（", idL, "）聚类计算稳健方差")
  }
  # 公式环境以 survival 命名空间为父环境，使 strata()/cluster()/pspline() 等无需用户 library(survival)
  environment(fml) <- new.env(parent = asNamespace("survival"))

  vars <- unique(c(all.vars(fml), idL))
  missing_vars <- setdiff(vars, names(data))
  if (length(missing_vars) > 0) stop("以下变量不在数据框中: ", paste(missing_vars, collapse = ", "))

  n_input <- nrow(data)
  na_by_var <- colSums(is.na(data[vars]))
  complete <- stats::complete.cases(data[vars])
  n_drop <- sum(!complete)
  ana <- data[complete, , drop = FALSE]
  rownames(ana) <- NULL
  if (nrow(ana) == 0L) stop("剔除模型变量缺失后无可用观测")

  y <- suppressWarnings(eval(str2lang(lhs), ana, environment(fml)))
  type <- attr(y, "type")
  if (!type %in% c("right", "counting")) stop("仅支持右删失或 (start, stop] 计数过程格式的生存数据")
  if (anyNA(y)) {
    stop(sprintf("有 %d 行生存结局无效（结局编码不是 0/1、TRUE/FALSE 或 1/2，或 time2 <= time1）", sum(is.na(y))))
  }
  time <- if (type == "counting") y[, "stop"] - y[, "start"] else y[, "time"]
  if (any(time < 0)) stop(sprintf("有 %d 行随访时间为负值", sum(time < 0)))

  if (!quiet) {
    n_txt <- if (is.null(idL)) sprintf("%d 行", nrow(ana)) else {
      sprintf("%d 行（%d 名个体）", nrow(ana), length(unique(ana[[idL]])))
    }
    if (n_drop == 0L) {
      .msg(label, "模型变量无缺失，分析样本 ", n_txt)
    } else {
      na_txt <- paste0(names(na_by_var)[na_by_var > 0], "=", na_by_var[na_by_var > 0], collapse = ", ")
      .msg(label, sprintf("模型变量存在缺失（%s），按完整病例剔除 %d/%d 行（%.1f%%），分析样本 %s",
                          na_txt, n_drop, n_input, 100 * n_drop / n_input, n_txt))
    }
  }

  list(
    formula = fml, data = ana, y = y, status = y[, "status"], time = time, lhs = lhs, covars = covars,
    idL = idL, robust = robust, has_cluster = has_cluster,
    n_input = n_input, n_drop = n_drop
  )
}

# 仅在需要时显式传 robust = TRUE：survival 在 robust = FALSE 时会忽略 cluster()；模型方差统一由 naive.var 提供
.fit_cox <- function(prep, ...) {
  if (isTRUE(prep$robust)) {
    survival::coxph(prep$formula, data = prep$data, robust = TRUE, ...)
  } else {
    survival::coxph(prep$formula, data = prep$data, ...)
  }
}

# 方差只由用户的 robust 设定决定：TRUE 为稳健方差，FALSE 为模型方差
.cox_vcov <- function(fit, robust = FALSE) {
  use_robust <- isTRUE(robust)
  V <- if (use_robust || is.null(fit$naive.var)) fit$var else fit$naive.var
  list(V = V, type = if (use_robust) "robust" else "model")
}

# 由系数与所选方差计算 Wald 统计量：与 confint.default 的公式一致
.wald_table <- function(fit, vc, conf_level) {
  b <- stats::coef(fit)
  se <- sqrt(diag(as.matrix(vc$V)))
  if (length(se) != length(b)) stop("方差矩阵维度与系数不一致")
  se[is.na(b)] <- NA_real_
  zc <- stats::qnorm(1 - (1 - conf_level) / 2)
  data.frame(
    coef_name = names(b),
    HR = exp(b),
    HR_lower = exp(b - zc * se),
    HR_upper = exp(b + zc * se),
    P = 2 * stats::pnorm(-abs(b / se)),
    beta = unname(b),
    se = unname(se),
    stringsAsFactors = FALSE
  )
}

# 各组事件数、人数、人年、发病率及精确 Poisson 置信区间；人年由 survival::pyears 计算
.describe_groups <- function(prep, group = NULL, t_unit, incidence_scale, conf_level) {
  if (is.null(group)) group <- factor(rep("all", nrow(prep$data)))
  y <- prep$y
  py <- survival::pyears(y ~ group, scale = .unit_divisor(t_unit))
  person_years <- as.vector(py$pyears)
  is_event <- prep$status == 1
  levs <- levels(group)
  if (is.null(prep$idL)) {
    n_total <- as.vector(table(group))
    n_event <- as.vector(table(group[is_event]))
  } else {
    ids <- prep$data[[prep$idL]]
    n_total <- vapply(levs, function(l) length(unique(ids[group == l])), numeric(1), USE.NAMES = FALSE)
    n_event <- vapply(levs, function(l) length(unique(ids[is_event & group == l])), numeric(1), USE.NAMES = FALSE)
  }
  ci <- vapply(seq_along(levs), function(i) {
    if (person_years[i] > 0) {
      survival::cipoisson(n_event[i], time = person_years[i], p = conf_level) * incidence_scale
    } else {
      c(NA_real_, NA_real_)
    }
  }, numeric(2))
  data.frame(
    N_event = n_event,
    N_total = n_total,
    Case_Total = paste0(n_event, "/", n_total),
    Person_years = person_years,
    Incidence = ifelse(person_years > 0, n_event / person_years * incidence_scale, NA_real_),
    Incidence_lower = ci[1, ],
    Incidence_upper = ci[2, ],
    stringsAsFactors = FALSE
  )
}

# 由 fit$assign / fit$xlevels 将系数精确映射到变量与水平，并合并描述统计
.tidy_cox <- function(prep, fit, vc, focus, extra = NULL, t_unit, incidence_scale, conf_level) {
  est <- .wald_table(fit, vc, conf_level)
  est_cols <- c("HR", "HR_lower", "HR_upper", "P", "beta", "se")
  ref_eff <- data.frame(HR = 1, HR_lower = NA_real_, HR_upper = NA_real_, P = NA_real_, beta = 0, se = NA_real_)
  rows <- list()

  for (v in focus) {
    term <- .bt(v)
    idx <- fit$assign[[term]]
    if (is.null(idx)) stop(sprintf("模型中未找到变量 %s 的系数", v))
    x <- prep$data[[v]]
    if (is.factor(x) || is.character(x)) {
      lev <- fit$xlevels[[term]]
      if (!identical(fit$contrasts[[term]], "contr.treatment")) {
        stop(sprintf("变量 %s 仅支持 treatment contrasts（默认对照编码）", v))
      }
      if (length(idx) != length(lev) - 1L) stop(sprintf("变量 %s 的系数与水平数不一致", v))
      desc <- .describe_groups(prep, factor(as.character(x), levels = lev), t_unit, incidence_scale, conf_level)
      eff <- rbind(ref_eff, est[idx, est_cols])
      rows[[length(rows) + 1L]] <- data.frame(
        varname = v, Interval = lev, Reference = c(TRUE, rep(FALSE, length(idx))),
        desc, eff, stringsAsFactors = FALSE
      )
    } else {
      if (length(idx) != 1L) stop(sprintf("变量 %s 应为单一数值变量或分类变量", v))
      desc <- .describe_groups(prep, NULL, t_unit, incidence_scale, conf_level)
      rng <- format(range(x), digits = 4, trim = TRUE)
      rows[[length(rows) + 1L]] <- data.frame(
        varname = v, Interval = paste(rng[1], "to", rng[2]), Reference = FALSE,
        desc, est[idx, est_cols], stringsAsFactors = FALSE
      )
    }
  }

  # extra：按项名（如 "a:b"）或系数名匹配的额外系数，不计算描述统计
  for (e in extra) {
    idx <- if (e %in% names(fit$assign)) fit$assign[[e]] else match(e, est$coef_name)
    if (length(idx) == 0L || anyNA(idx)) {
      warning(sprintf("extra_vars 中的 %s 未在模型系数中找到，已忽略", e))
      next
    }
    rows[[length(rows) + 1L]] <- data.frame(
      varname = est$coef_name[idx], Interval = NA_character_, Reference = FALSE,
      N_event = NA_real_, N_total = NA_real_, Case_Total = NA_character_, Person_years = NA_real_,
      Incidence = NA_real_, Incidence_lower = NA_real_, Incidence_upper = NA_real_,
      est[idx, est_cols], stringsAsFactors = FALSE
    )
  }

  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out
}

# focus：已作为协变量进入模型的 extra_vars 列名，按关注变量输出（带描述统计）；其余视为项名/系数名
.split_extra <- function(extra_vars, data, covars) {
  is_col <- extra_vars %in% names(data) & extra_vars %in% covars
  list(focus = extra_vars[is_col], coef = extra_vars[!is_col])
}

.new_result <- function(model, result, type, info, ...) {
  structure(list(model = model, result = result, ..., info = info), class = "myepi_result")
}

#' 打印 myepi 分析结果
#'
#' 结果对象中的数值均保存原始精度；本方法仅在显示时格式化。
#'
#' @param x myepi 分析函数返回的结果对象
#' @param digits 效应量显示的小数位数，默认 2
#' @param ... 未使用
#' @return 不可见地返回 x
#' @export
print.myepi_result <- function(x, digits = 2, ...) {
  if (length(x$info$header) > 0) cat(paste0(x$info$header, "\n"), sep = "")
  if (is.data.frame(x$tests)) {
    cat("\n检验:\n")
    .print_table(x$tests, digits)
    cat("\n")
  }
  .print_table(x$result, digits)
  invisible(x)
}

.print_table <- function(r, digits, max_rows = 20L) {
  if (is.data.frame(r) && nrow(r) > max_rows) {
    n <- nrow(r)
    .print_table(r[c(1:5, (n - 4):n), , drop = FALSE], digits, max_rows)
    cat(sprintf("…… 共 %d 行，仅显示首尾各 5 行（完整数据见 $result）\n", n))
    return(invisible(NULL))
  }
  if (is.data.frame(r) && nrow(r) > 0) {
    fmt_fixed <- function(v, d) ifelse(is.na(v), NA_character_, formatC(v, format = "f", digits = d))
    for (col in names(r)) {
      v <- r[[col]]
      if (is.logical(v)) r[[col]] <- ifelse(is.na(v), NA_character_, ifelse(v, "Yes", ""))
      if (!is.numeric(v)) next
      r[[col]] <- if (grepl("^P($|_)|_pvalue$", col)) {
        ifelse(is.na(v), NA_character_, format.pval(v, digits = digits, eps = 0.001))
      } else if (col %in% c("N_event", "N_total", "n", "k", "Missing", "N", "Q_df")) {
        ifelse(is.na(v), NA_character_, format(v, trim = TRUE))
      } else if (col %in% c("beta", "se", "beta_trend", "tau2", "statistic", "df", "x")) {
        fmt_fixed(v, digits + 1)
      } else {
        fmt_fixed(v, digits)
      }
    }
    print(r, row.names = FALSE, na.print = "")
  } else {
    cat("（结果为空）\n")
  }
  invisible(NULL)
}
