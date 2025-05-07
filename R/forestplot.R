#' 基于 tidyverse 创建一个绘制森林图的函数
#' 这是在赶DDL的时候制作的R包，因此诸多部分在后续会被再次更新
#' 由于语法简单，强烈建议在必要的时候，直接对本函数进行修改以及重编
#' 复杂需求并不支持，复杂需求请自行使用tidyverse绘制
#' 本函数的主要目的是，结合我们常用的，需要在PPT中完成后续的进一步调整的需求之上，快速稳定合理地输出森林图
#' 并尽最大可能减少后续在PPT中的调整门槛和步骤

# 声明本函数中调用的外部包的及其函数功能
# 依赖包的声明见DESCRIPTION文件
#' @importFrom ggplot2 ggplot aes annotate geom_errorbar geom_point geom_text aes_string geom_hline
#' @importFrom ggplot2 scale_x_continuous scale_y_continuous theme_classic
#' @importFrom ggplot2 theme element_blank element_line element_text expansion
#' @importFrom stringr str_replace_all
#' @importFrom rlang sym
#' @importFrom stats setNames

# 声明本函数中的参数解释
#' @param df dataframe 用于绘图的总数据集，必须提供
#' @param left_side_cols num_vector，用于展示在森林图左侧内容的列号
#' @param right_side_cols num_vector，用于展示在森林图右侧内容的列号
#' @param estimate chr，列名，指定森林图box的值，例如beta/OR/HR等
#' @param lower chr，列名，指定森林图box的error bar 下限
#' @param upper chr，列名，指定森林图box的error bar 上限
#' @param bar_scale num，调整森林图box的error bar整理宽窄，默认为1
#' @param h_scale num，调整森林图水平方向的整体宽窄，默认为0.08
#' @param gap_value num，调整图形部分和文字部分的间隔，默认为0.4
#' @param x_limit num_vector，长度为2，指定x轴显示的上下限，默认为0.8 to 3.2
#' @param x_step num，指定x轴坐标的值的步进，默认为0.5
#' @param x_text_size num，指定x轴文字大小，默认为12
#' @param x_text_color chr，指定x轴文字颜色，默认为black
#' @param x_text_face chr，指定x轴文字是否加粗，默认为NULL，设置为bold则加粗
#' @param box_shape num，指定box的形状，默认为15
#' @param box_size num，指定box的大小，默认为4
#' @param box_color chr，指定box的颜色，默认为black
#' @param bar_width num，指定error bar的ticks长度，默认为0
#' @param bar_size num，指定error bar的粗细，默认为0.5
#' @param bar_color chr，指定error bar的颜色，默认为black
#' @param ref_line_type chr，指定参考线的类型，默认为dashed
#' @param ref_line_value num，指定参考线的位置，默认为1
#' @param ref_line_size num，指定参考线的粗细，默认为0.5
#' @param ref_line_color num，指定参考线的颜色，默认为black
#' @param col_name_face chr，指定纵标目是否加粗，默认为bold，其他为plain/italic
#' @param col_line_type chr，指定标目线的类型，默认为solid
#' @param col_line_color chr，指定标目线的颜色，默认为black
#' @param col_line_size num，指定标目线的粗细，默认为0.5
#' @param low_line_type chr，指定x轴线的类型，默认为solid
#' @param low_line_color chr，指定x轴线的颜色，默认为black
#' @param low_line_size num，指定x轴线的粗细，默认为0.5
#' @param text_size num，指定全局文字的大小，默认为4.5
#' @param text_color chr，指定全局文字的颜色，默认为black
#' @param text_family chr，指定全局文字的字体，默认为Arial
#' @param p_value_col chr，指定P值列的列名用于对P值进行round，默认为NULL，若指定则p_value_round不能为空
#' @param p_value_round num，指定P值列round保留小数位数，，默认为NULL，若指定则p_value_col不能为空
#' @param round_cols chr_vector，字符串向量，指定需要round的列名，默认为NULL，若指定则round_digit不能为空
#' @param round_digit num，指定round列保留小数位数，，默认为NULL，若指定则round_cols不能为空

# 声明返回对象类型
#' @return A ggplot2 object

# 声明本函数名，外显函数名给用户使用
#' @export
forestplot = function(df,
                     left_side_cols = NULL,
                     right_side_cols = NULL,
                     estimate = NULL,
                     lower = NULL,
                     upper = NULL,
                     bar_scale = 1,
                     h_scale = 0.08,
                     gap_value = 0.4,
                     x_limit = c(0.8, 3.2),
                     x_step = 0.5,
                     x_text_size = 12,
                     x_text_color = 'black',
                     x_text_face = NULL,
                     box_shape = 15,
                     box_size = 4,
                     box_color = 'black',
                     bar_width = 0,
                     bar_size = 0.5,
                     bar_color = 'black',
                     ref_line_type = 'dashed',
                     ref_line_value = 1,
                     ref_line_size = 0.5,
                     ref_line_color = 'black',
                     col_name_face = 'bold',
                     col_line_type = 'solid',
                     col_line_color = 'black',
                     col_line_size = 0.5,
                     low_line_type = 'solid',
                     low_line_color = 'black',
                     low_line_size = 0.5,
                     text_size = 4.5,
                     text_color = 'black',
                     text_family = 'Arial',
                     p_value_col = NULL,
                     p_value_round = 3,
                     round_cols = NULL,
                     round_digit = 2) {
  # 错误检测
  if (!requireNamespace("ggplot2", quietly = TRUE)) {
    stop(
      "The 'ggplot2' package is required but not loaded. Please install and load 'ggplot2' before using this function.",
      call. = FALSE
    )
  }
  if (is.null(df)) {
    cat('---- df is null, please check your data! ---- \n')
  }
  if (!is.null(df)) {
    # -------------------------- 绘图函数 --------------------------
    # 森林图
    # 设计数据缩放及森林图坐标映射以调整bar的scale
    # scale的尺度与x_limit的限值相关
    # 设定x_limit步进和映射
    # 注意定义初始的左侧，右侧数据极限值
    df$order = c(nrow(df):1)
    df$x_fake_value = df[[estimate]] * bar_scale
    df$x_ll_fake_value = df[[lower]] * bar_scale
    df$x_ul_fake_value = df[[upper]] * bar_scale
    # 定义数据映射
    true_min = x_limit[1]
    true_max = x_limit[2]
    x_axis_df = data.frame(
      ref_value = seq(true_min, true_max, by = x_step) * bar_scale,
      lable_value = seq(true_min, true_max, by = x_step)
    )
    p = ggplot(df, aes(x = x_fake_value, y = order)) +
      annotate(
        "segment",
        x = ref_line_value * bar_scale,
        xend = ref_line_value * bar_scale,
        y = min(df$order) - 0.5,
        yend = max(df$order) + 0.5,
        linetype = ref_line_type,
        colour = ref_line_color,
        size = ref_line_size
      ) +
      geom_errorbar(
        aes(xmin = x_ll_fake_value, xmax = x_ul_fake_value),
        width = bar_width,
        size = bar_size,
        color = bar_color
      ) +
      geom_point(shape = box_shape,
                 size = box_size,
                 color = box_color) +
      scale_x_continuous(
        breaks = c(x_axis_df$ref_value, ref_line_value * bar_scale),
        labels = c(x_axis_df$lable_value, ref_line_value),
        expand = c(h_scale, h_scale)
      ) +
      scale_y_continuous(expand = expansion(mult = c(0.01, 0.1))) +
      theme_classic(base_family = "Arial") +
      theme(
        axis.line.y = element_blank(),
        axis.line.x = element_line(
          color = low_line_color,
          size = low_line_size,
          linetype = low_line_type
        ),
        axis.text.x = element_text(
          size = x_text_size,
          color = x_text_color,
          face = x_text_face,
          family = text_family
        ),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank()
      )
    # 定义森林图左右侧极限值
    left_max_limit = true_min * bar_scale + gap_value
    right_min_limit = true_max * bar_scale - gap_value
    # 数据round
    if (!is.null(p_value_col) & !is.null(p_value_round)) {
      df[[p_value_col]] = str_replace_all(df[[p_value_col]], "\\d+\\.\\d+", function(x) {
        round(as.numeric(x), digits = p_value_round)
      })
      # 处理P值科学计数法展示
      format_pvalue <- function(pvalue) {
        # 处理科学记数法（包含"e"或"E"）
        if (is.na(pvalue)) {
          return(NA) # 空行处理
        } else if (grepl("e|E", pvalue)) {
          base_fmt = sprintf("%.2f", as.numeric(strsplit(pvalue, "e|E")[[1]][1]))
          exp = strsplit(pvalue, "e|E")[[1]][2]
          formatted <- paste0(base_fmt, " %*% 10^{", exp, "}")
          return(paste0("plain('", base_fmt, "') %*% 10^{", exp, "}"))
        }
        else if (pvalue == "-") {
          return("'-'")  # 引号包裹避免解析为减号
        } else {
          return(paste0("'", pvalue, "'"))
        }
      }
      df[[p_value_col]] = sapply(df[[p_value_col]], format_pvalue)
    }
    if (!is.null(round_cols) & !is.null(round_digit)) {
      for (i in seq_along(round_cols)) {
        df[[round_cols[i]]] = str_replace_all(df[[round_cols[i]]], "\\d+\\.\\d+", function(x) {
          sprintf(paste0("%.", round_digit, "f"),
                  round(as.numeric(x), digits = round_digit))
        })
      }
    }
    left_side_df = df[, left_side_cols]
    right_side_df = df[, right_side_cols]
    # 森林图左侧数据展示
    left_side_df$order = c(nrow(left_side_df):1)
    for (i in seq_along(names(left_side_df))) {
      col_name <- names(left_side_df)[i]
      if (col_name != 'order') {
        col_mvs <- -max(seq_along(names(left_side_df))) + i + left_max_limit
        p <- p + geom_text(
          data = left_side_df,
          aes_string(
            x = col_mvs,
            y = "order",
            label = sym(col_name)
          ),
          family = text_family,
          color = text_color,
          size = text_size
        )
      }
    }
    # 森林图右侧数据展示
    right_side_df$order = c(nrow(right_side_df):1)
    # parse参数预设
    parse_flag <- if (!is.null(p_value_col) &&
                      !is.null(p_value_round))
      TRUE
    else
      FALSE
    for (i in seq_along(names(right_side_df))) {
      col_name <- names(right_side_df)[i]
      if (col_name != 'order') {
        col_mvs <- i + right_min_limit
        p <- p + geom_text(
          data = right_side_df,
          parse = parse_flag,
          aes_string(
            x = col_mvs,
            y = "order",
            label = sym(col_name)
          ),
          family = text_family,
          color = text_color,
          size = text_size
        )
      }
    }
    # 绘制纵标目线以及纵标目
    # 左侧纵标目
    left_cols = colnames(left_side_df)
    left_cols = left_cols[left_cols != 'order']
    left_col_lable = data.frame(
      col_x_value = -max(seq_along(left_cols)) + seq_along(left_cols) + left_max_limit - 1,
      col_x_lable = left_cols
    )
    for (i in seq_along(left_cols)) {
      col_name <- left_cols[i]
      if (col_name != 'order') {
        col_mvs <- -max(seq_along(left_cols)) + i + left_max_limit
        p <- p + geom_text(
          data = left_col_lable,
          aes_string(
            x = 'col_x_value',
            y = max(df$order) + 1,
            label = 'col_x_lable'
          ),
          fontface = col_name_face,
          family = text_family,
          color = text_color,
          size = text_size
        )
      }
    }
    # 右侧纵标目
    right_cols = colnames(right_side_df)
    right_cols = right_cols[right_cols != 'order']
    right_col_lable = data.frame(col_x_value = seq_along(right_cols) + right_min_limit,
                                 col_x_lable = right_cols)
    for (i in seq_along(right_cols)) {
      col_name <- right_cols[i]
      if (col_name != 'order') {
        col_mvs <- -max(seq_along(right_cols)) + i + left_max_limit
        p <- p + geom_text(
          data = right_col_lable,
          aes_string(
            x = 'col_x_value',
            y = max(df$order) + 1,
            label = 'col_x_lable'
          ),
          fontface = col_name_face,
          family = text_family,
          color = text_color,
          size = text_size
        )
      }
    }
    # 纵标目线
    p = p + geom_hline(
      yintercept = max(df$order) + 0.5,
      linetype = col_line_type,
      color = col_line_color,
      size = col_line_size
    )
    # 返回一个ggplot2对象
    return(p)
  }
}
