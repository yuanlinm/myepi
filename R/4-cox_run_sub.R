#' 分组/亚组Cox回归批量分析函数，本函数支持基本的Cox回归分析和时间依赖（长数据）变量的Cox回归亚组分析。
#'
#' 按 group_var 的每个水平分别拟合 Cox 模型；同时在全样本中检验 mainvar 与 group_var 的交互作用
#' （模型方差时为似然比检验，稳健方差时为 Wald 检验），作为亚组差异的主要报告指标 \code{P_interaction}。
#' 含 group_var 的协变量项会在亚组模型中自动剔除。
#'
#' @param data 数据框
#' @param group_var 分组变量名（字符串）
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串）。编码遵循 \code{survival::Surv}
#' @param mainvar 关注变量名（字符串向量）
#' @param covars 协变量（字符串向量，可为NULL），写法同 \code{cox_run}
#' @param extra_vars 额外输出的项（字符串向量，可为NULL），写法同 \code{cox_run}
#' @param idL 长格式数据的个体标识变量名（字符串）。指定后，亚组病例数和总人数按该变量去重，人时仍按各观察区间累加。
#' @param t_unit 随访时间单位：d=天（默认），y=年，m=月。用于将随访时长统一换算为人年后计算发病率。
#' @param incidence_scale 发病率缩放倍数，默认 1e5（即每10万人年）。
#' @param robust 方差估计，同 \code{cox_run}
#' @param conf_level 置信水平，默认 0.95
#' @param group_na group_var 缺失的处理："drop"（默认，剔除并报告）或 "keep"（作为单独一组 "(Missing)"）
#' @param interaction 是否检验 mainvar × group_var 交互作用，默认 TRUE
#' @param plot_shape 是否返回绘图友好格式（TRUE 返回分组标题 + 缩进结构；FALSE 返回原始长表, 后续异质性检验友好），默认 FALSE
#' @param ... 传递给 \code{survival::coxph} 的其他参数
#' @return 不可见地返回 \code{myepi_result} 对象：\code{model$subgroups} 为各亚组模型，
#'   \code{model$interaction} 为交互检验的简化/完整模型，\code{result} 为结果表，
#'   \code{interaction} 为交互检验结果表。
#'   \code{result} 在 plot_shape = FALSE 时为原始长表（Subgroup 形如 "sex: 1"，可直接用于 \code{cox_het}）；
#'   plot_shape = TRUE 时包含列 Group / Subgroup（标题行为空 Subgroup），便于森林图展示。
#' @export
cox_run_sub <- function(data, group_var, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, extra_vars = NULL, plot_shape = FALSE, idL = NULL,
                        t_unit = c("d", "y", "m"), incidence_scale = 1e5, robust = NULL, conf_level = 0.95,
                        group_na = c("drop", "keep"), interaction = TRUE, ...) {
  t_unit <- match.arg(t_unit)
  group_na <- match.arg(group_na)
  .validate_conf_level(conf_level)
  if (!is.data.frame(data)) stop("data 必须为数据框")
  data <- as.data.frame(data)
  if (!is.character(group_var) || length(group_var) != 1L || !group_var %in% names(data)) stop("分组变量不在数据框中")
  if (group_var %in% mainvar) stop("group_var 不能同时作为 mainvar")

  # 分组变量：保留因子水平顺序；缺失按 group_na 处理
  g <- data[[group_var]]
  g <- if (is.factor(g)) droplevels(g) else factor(g)
  n_na <- sum(is.na(g))
  if (n_na > 0) {
    if (group_na == "drop") {
      message(sprintf("分组变量 %s 有 %d 行缺失，已剔除", group_var, n_na))
    } else {
      g <- factor(ifelse(is.na(g), "(Missing)", as.character(g)), levels = c(levels(g), "(Missing)"))
      message(sprintf("分组变量 %s 有 %d 行缺失，作为单独一组 (Missing)", group_var, n_na))
    }
  }
  data[[group_var]] <- g
  data <- data[!is.na(g), , drop = FALSE]

  # 亚组内 group_var 为常数：剔除含 group_var 的协变量项
  involves_group <- vapply(covars, function(term) group_var %in% all.vars(str2lang(.term_label(term, data))),
                           logical(1), USE.NAMES = FALSE)
  if (any(involves_group)) {
    message("亚组模型中剔除含分组变量的协变量项: ", paste(covars[involves_group], collapse = ", "))
  }
  sub_covars <- covars[!involves_group]
  extra <- .split_extra(extra_vars, data, sub_covars)
  focus <- c(mainvar, extra$focus)

  # 各亚组模型
  fits <- list()
  rows <- list()
  for (lev in levels(data[[group_var]])) {
    label <- paste0(group_var, ": ", lev)
    sub_data <- data[data[[group_var]] == lev, , drop = FALSE]
    res <- tryCatch({
      prep <- .prep_cox(sub_data, time1, time2, timediff, event, mainvar, sub_covars, idL, robust, label = label)
      fit <- .fit_cox(prep, ...)
      vc <- .cox_vcov(fit, robust)
      list(fit = fit, vc = vc,
           table = .tidy_cox(prep, fit, vc, focus, extra$coef, t_unit, incidence_scale, conf_level))
    }, error = function(e) {
      warning(sprintf("亚组 %s 建模失败: %s", label, conditionMessage(e)), call. = FALSE)
      NULL
    })
    if (is.null(res)) next
    fits[[label]] <- res$fit
    rows[[label]] <- data.frame(Subgroup = label, res$table, stringsAsFactors = FALSE)
  }
  if (length(rows) == 0L) stop("所有亚组均建模失败")
  out <- do.call(rbind, rows)

  # 交互检验（全样本）
  inter_models <- list()
  inter_tab <- NULL
  if (isTRUE(interaction)) {
    inter <- .interaction_test(data, group_var, time1, time2, timediff, event, mainvar, sub_covars, idL, robust, ...)
    inter_models <- inter$models
    inter_tab <- inter$table
    out$P_interaction <- inter_tab$P[match(out$varname, inter_tab$varname)]
  }

  # 排序：关注变量顺序 → 亚组水平顺序 → 亚组内行序
  sub_levels <- paste0(group_var, ": ", levels(data[[group_var]]))
  out <- out[order(match(out$varname, unique(c(focus, out$varname))), match(out$Subgroup, sub_levels),
                   seq_len(nrow(out))), , drop = FALSE]
  rownames(out) <- NULL

  if (isTRUE(plot_shape)) out <- .sub_plot_shape(out, group_var)

  vc_type <- if (length(fits) > 0) .cox_vcov(fits[[1]], robust)$type else NA_character_
  info <- list(
    group_var = group_var, n_group_na = n_na, group_na = group_na, variance = vc_type,
    conf_level = conf_level, t_unit = t_unit, incidence_scale = incidence_scale,
    header = c(
      sprintf("亚组 Cox 回归: 按 %s 分为 %d 组（成功拟合 %d 组）", group_var, nlevels(data[[group_var]]), length(fits)),
      sprintf("方差估计: %s；置信水平: %g%%", if (identical(vc_type, "robust")) "稳健（三明治）方差" else "模型方差", 100 * conf_level),
      if (!is.null(inter_tab)) sprintf("交互检验（%s）: %s", inter_tab$test[1],
                                      paste0(inter_tab$varname, " P = ", format.pval(inter_tab$P, digits = 3, eps = 0.001), collapse = "; "))
    )
  )
  invisible(.new_result(model = list(subgroups = fits, interaction = inter_models), result = out,
                        interaction = inter_tab, info = info))
}

# mainvar × group_var 交互检验：模型方差用似然比检验（anova），稳健方差用 Wald 检验
.interaction_test <- function(data, group_var, time1, time2, timediff, event, mainvar, covars, idL, robust, ...) {
  prep <- .prep_cox(data, time1, time2, timediff, event, mainvar, c(covars, group_var), idL, robust,
                    label = "交互检验")
  fit0 <- .fit_cox(prep, ...)
  use_robust <- .cox_vcov(fit0, robust)$type == "robust"
  models <- list()
  tab <- lapply(mainvar, function(mv) {
    inter_term <- paste0(.bt(mv), ":", .bt(group_var))
    prep1 <- prep
    prep1$formula <- stats::update(prep$formula, stats::as.formula(paste(". ~ . +", inter_term)))
    fit1 <- tryCatch(.fit_cox(prep1, ...), error = function(e) {
      warning(sprintf("%s 的交互模型拟合失败: %s", mv, conditionMessage(e)), call. = FALSE)
      NULL
    })
    models[[mv]] <<- list(reduced = fit0, full = fit1)
    if (is.null(fit1)) return(data.frame(varname = mv, test = NA_character_, df = NA_real_, statistic = NA_real_, P = NA_real_))
    if (use_robust) {
      idx <- unlist(fit1$assign[setdiff(names(fit1$assign), names(fit0$assign))])
      b <- stats::coef(fit1)[idx]
      keep <- !is.na(b)
      V <- .cox_vcov(fit1, robust)$V[idx, idx, drop = FALSE][keep, keep, drop = FALSE]
      stat <- as.numeric(t(b[keep]) %*% solve(V, b[keep]))
      df <- sum(keep)
      data.frame(varname = mv, test = "Wald", df = df, statistic = stat,
                 P = stats::pchisq(stat, df, lower.tail = FALSE))
    } else {
      a <- stats::anova(fit0, fit1)
      data.frame(varname = mv, test = "LRT", df = a$Df[2], statistic = a$Chisq[2], P = a[2, "Pr(>|Chi|)"])
    }
  })
  list(models = models, table = do.call(rbind, tab))
}

.sub_plot_shape <- function(out, group_var) {
  blocks <- lapply(unique(out$varname), function(v) {
    block <- out[out$varname == v, , drop = FALSE]
    title_row <- block[1, , drop = FALSE]
    title_row[, setdiff(names(title_row), c("varname", "P_interaction"))] <- NA
    title_row$Group <- if (length(unique(out$varname)) > 1) paste0(v, " × ", group_var) else group_var
    title_row$Subgroup <- ""
    block$Group <- ""
    block$Subgroup <- substring(block$Subgroup, nchar(group_var) + 3L)
    if ("P_interaction" %in% names(block)) block$P_interaction <- NA_real_
    rbind(title_row, block)
  })
  formatted <- do.call(rbind, blocks)
  formatted <- formatted[, c("Group", "Subgroup", setdiff(names(formatted), c("Group", "Subgroup", "varname"))), drop = FALSE]
  rownames(formatted) <- NULL
  formatted
}
