% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/7-plot_forest.R
\name{plot_forest}
\alias{plot_forest}
\title{基于 tidyverse 创建一个绘制森林图的函数
这是在赶DDL的时候制作的R包，因此诸多部分在后续会被再次更新
由于语法简单，强烈建议在必要的时候，直接对本函数进行修改以及重编
复杂需求并不支持，复杂需求请自行使用tidyverse绘制
本函数的主要目的是，结合我们常用的，需要在PPT中完成后续的进一步调整的需求之上，快速稳定合理地输出森林图
并尽最大可能减少后续在PPT中的调整门槛和步骤}
\usage{
plot_forest(
  df,
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
  x_text_color = "black",
  x_text_face = NULL,
  box_shape = 15,
  box_size = 4,
  box_color = "black",
  bar_width = 0,
  bar_size = 0.5,
  bar_color = "black",
  ref_line_type = "dashed",
  ref_line_value = 1,
  ref_line_size = 0.5,
  ref_line_color = "black",
  col_name_face = "bold",
  col_line_type = "solid",
  col_line_color = "black",
  col_line_size = 0.5,
  low_line_type = "solid",
  low_line_color = "black",
  low_line_size = 0.5,
  text_size = 4.5,
  text_color = "black",
  text_family = "Arial",
  p_value_col = NULL,
  p_value_round = 3,
  round_cols = NULL,
  round_digit = 2
)
}
\arguments{
\item{df}{dataframe 用于绘图的总数据集，必须提供}

\item{left_side_cols}{num_vector，用于展示在森林图左侧内容的列号}

\item{right_side_cols}{num_vector，用于展示在森林图右侧内容的列号}

\item{estimate}{chr，列名，指定森林图box的值，例如beta/OR/HR等}

\item{lower}{chr，列名，指定森林图box的error bar 下限}

\item{upper}{chr，列名，指定森林图box的error bar 上限}

\item{bar_scale}{num，调整森林图box的error bar整理宽窄，默认为1}

\item{h_scale}{num，调整森林图水平方向的整体宽窄，默认为0.08}

\item{gap_value}{num，调整图形部分和文字部分的间隔，默认为0.4}

\item{x_limit}{num_vector，长度为2，指定x轴显示的上下限，默认为0.8 to 3.2}

\item{x_step}{num，指定x轴坐标的值的步进，默认为0.5}

\item{x_text_size}{num，指定x轴文字大小，默认为12}

\item{x_text_color}{chr，指定x轴文字颜色，默认为black}

\item{x_text_face}{chr，指定x轴文字是否加粗，默认为NULL，设置为bold则加粗}

\item{box_shape}{num，指定box的形状，默认为15}

\item{box_size}{num，指定box的大小，默认为4}

\item{box_color}{chr，指定box的颜色，默认为black}

\item{bar_width}{num，指定error bar的ticks长度，默认为0}

\item{bar_size}{num，指定error bar的粗细，默认为0.5}

\item{bar_color}{chr，指定error bar的颜色，默认为black}

\item{ref_line_type}{chr，指定参考线的类型，默认为dashed}

\item{ref_line_value}{num，指定参考线的位置，默认为1}

\item{ref_line_size}{num，指定参考线的粗细，默认为0.5}

\item{ref_line_color}{num，指定参考线的颜色，默认为black}

\item{col_name_face}{chr，指定纵标目是否加粗，默认为bold，其他为plain/italic}

\item{col_line_type}{chr，指定标目线的类型，默认为solid}

\item{col_line_color}{chr，指定标目线的颜色，默认为black}

\item{col_line_size}{num，指定标目线的粗细，默认为0.5}

\item{low_line_type}{chr，指定x轴线的类型，默认为solid}

\item{low_line_color}{chr，指定x轴线的颜色，默认为black}

\item{low_line_size}{num，指定x轴线的粗细，默认为0.5}

\item{text_size}{num，指定全局文字的大小，默认为4.5}

\item{text_color}{chr，指定全局文字的颜色，默认为black}

\item{text_family}{chr，指定全局文字的字体，默认为Arial}

\item{p_value_col}{chr，指定P值列的列名用于对P值进行round，默认为NULL，若指定则p_value_round不能为空}

\item{p_value_round}{num，指定P值列round保留小数位数，，默认为NULL，若指定则p_value_col不能为空}

\item{round_cols}{chr_vector，字符串向量，指定需要round的列名，默认为NULL，若指定则round_digit不能为空}

\item{round_digit}{num，指定round列保留小数位数，，默认为NULL，若指定则round_cols不能为空}
}
\value{
A ggplot2 object
}
\description{
基于 tidyverse 创建一个绘制森林图的函数
这是在赶DDL的时候制作的R包，因此诸多部分在后续会被再次更新
由于语法简单，强烈建议在必要的时候，直接对本函数进行修改以及重编
复杂需求并不支持，复杂需求请自行使用tidyverse绘制
本函数的主要目的是，结合我们常用的，需要在PPT中完成后续的进一步调整的需求之上，快速稳定合理地输出森林图
并尽最大可能减少后续在PPT中的调整门槛和步骤
}
