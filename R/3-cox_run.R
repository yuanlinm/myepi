#' 常规Cox回归分析函数，本函数支持基本的Cox回归分析和时间依赖（长数据）变量的Cox回归分析。
#'
#' 拟合前按完整病例（所有模型变量及 idL 无缺失）生成分析样本，并以 message 报告缺失处理结果；
#' 结果表中的事件数、人数、人年与发病率均基于该分析样本，与模型一致。
#' 结果表保存原始精度数值，显示时由 \code{print()} 格式化。
#'
#' @param data 数据框
#' @param time1 随访开始时间变量名（字符串）
#' @param time2 随访结束时间变量名（字符串）
#' @param timediff 生存时间变量名（字符串，可选）
#' @param event 结局变量名（字符串）。编码遵循 \code{survival::Surv}：0/1、TRUE/FALSE 或 1/2（2 为事件）
#' @param mainvar 关注变量名（字符串向量）。数值型输出一行；因子/字符型输出参照水平与各水平
#' @param covars 协变量（字符串向量，可为NULL）。列名按变量处理；也可写表达式，如 \code{"strata(center)"}、
#'   \code{"cluster(id)"}、\code{"pspline(age, df = 3)"}、\code{"a*b"}
#' @param extra_vars 额外输出的项（字符串向量，可为NULL）。若为 covars 中的列名则与 mainvar 同样输出（含描述统计）；
#'   否则按模型项名（如 \code{"x:z"}）或系数名匹配，仅输出效应量
#' @param idL 长格式数据的个体标识变量名（字符串）。默认 NULL，按每行代表一名个体统计；指定后，病例数和总人数按该变量去重，Incidence 的病例数也按该变量去重，人时仍按各观察区间累加。
#' @param t_unit 随访时间单位：d=天（默认），y=年，m=月。用于将随访时长统一换算为人年后计算发病率。
#' @param incidence_scale 发病率缩放倍数，默认 1e5（即每10万人年）。
#' @param robust 方差估计，必须由用户明确指定：FALSE（默认）使用模型方差，此时 covars 中的 \code{cluster()} 被忽略；
#'   TRUE 使用稳健（三明治）方差，按 \code{cluster()} 聚类，未写 \code{cluster()} 但指定了 idL 时按 idL 聚类。
#'   系数、CI、P 值与所有检验均基于所选方差。
#' @param conf_level 置信水平，默认 0.95
#' @param ... 传递给 \code{survival::coxph} 的其他参数（如 \code{ties}）
#' @return 不可见地返回 \code{myepi_result} 对象（列表）：\code{model} 为 coxph 模型，\code{result} 为结果表，
#'   \code{info} 为样本与设置信息。用 \code{print()} 查看。
#' @export
cox_run <- function(data, time1 = NULL, time2 = NULL, timediff = NULL, event, mainvar, covars = NULL, extra_vars = NULL, idL = NULL,
                    t_unit = c("d", "y", "m"), incidence_scale = 1e5, robust = FALSE, conf_level = 0.95, ...) {
  t_unit <- match.arg(t_unit)
  .validate_conf_level(conf_level)
  extra <- .split_extra(extra_vars, data, covars)

  prep <- .prep_cox(data, time1, time2, timediff, event, mainvar, covars, idL, robust)
  fit <- .fit_cox(prep, ...)
  vc <- .cox_vcov(fit, robust)
  res <- .tidy_cox(prep, fit, vc, c(mainvar, extra$focus), extra$coef, t_unit, incidence_scale, conf_level)

  info <- .cox_info(prep, fit, vc, conf_level, t_unit, incidence_scale)
  info$header <- c("Cox 回归", info$header)
  invisible(.new_result(model = fit, result = res, info = info))
}

.cox_info <- function(prep, fit, vc, conf_level, t_unit, incidence_scale) {
  list(
    n_input = prep$n_input, n_dropped = prep$n_drop, n = fit$n, nevent = fit$nevent,
    variance = vc$type, conf_level = conf_level, t_unit = t_unit, incidence_scale = incidence_scale,
    header = c(
      sprintf("分析样本: %d 行，事件 %d（剔除缺失 %d 行）", fit$n, fit$nevent, prep$n_drop),
      sprintf("方差估计: %s；置信水平: %g%%；发病率单位: 每 %s 人年",
              if (vc$type == "robust") "稳健（三明治）方差" else "模型方差",
              100 * conf_level, format(incidence_scale, big.mark = ",", scientific = FALSE))
    )
  )
}
