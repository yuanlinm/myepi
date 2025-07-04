# `myepi`: 队列研究基础分析整合包

**这个R项目包含了一系列用于数据分析和可视化的函数，主要围绕生存分析中的Cox比例风险模型展开。以下是每个函数的详细说明和使用示例，欢迎使用与反馈。**
```
1. count_na(缺失值报告)
2. cross_tb(交叉表)
3. cox_run(cox模型拟合)
4. cox_run_sub(cox亚组分析)
5. cox_run_q(cox分位数分析)
6. cox_het(亚组异质性检验)
7. plot_forest(森林图绘制)
```

---

## 1. `count_na`

### 功能介绍

`count_na` 函数用于检查数据框（data frame）中的缺失值（NA）。它可以对整个数据集进行检查，也可以根据指定的分组变量进行分组检查。函数会返回一个包含变量名、变量类型、缺失值数量和缺失率的数据框，并按缺失率从高到低排序。

### 使用示例

```R
# 整体数据NA检查
count_na(dat = my_data)
# 按 'group' 分组检查
count_na(dat = my_data, group_var = "group")
```

## 2. `cross_tb`

### 功能介绍

`cross_tb` 函数用于生成交叉表或分组统计摘要。对于分类变量，它计算每个组的频数和百分比；对于连续变量，它计算每个组的均值和中位数。该函数支持多个分组变量。

### 使用示例

```R
# 运行函数，连续变量交叉汇总
cross_tb(dat = my_data, var = "age", by = "group")
# 运行函数，分类变量交叉汇总
cross_tb(dat = my_data, var = "gender", by = "group")
```

## 3. `cox_run`

### 功能介绍

`cox_run` 是一个核心函数，用于执行Cox比例风险回归分析。它支持标准的生存数据格式（开始时间、结束时间、事件状态）和简单的生存数据格式（生存时间、事件状态）。该函数可以包含协变量，并能处理`strata()`和`cluster()`等特殊项进行基线风险宽容或时间依赖变量分析。

### 参数说明

- `data`: 数据框。
- `time1`: (可选) 字符串，随访开始时间的变量名。
- `time2`: (可选) 字符串，随访结束时间的变量名。
- `timediff`: (可选) 字符串，生存时间的变量名。如果提供了`timediff`，则忽略`time1`和`time2`。
- `event`: 字符串，结局事件的变量名（通常为0或1）。
- `mainvar`: 字符串，要分析的主要关注变量。
- `covars`: (可选) 字符串向量，协变量的名称，可纳入交互项。
- `extra_vars`: (可选) 字符串向量，除了`mainvar`外，还希望在结果中展示的其他变量（例如交互项）。

### 使用示例

```R
# run，函数还会不可见地返回一个列表，包含模型对象 (cox_result$model) 和结果数据框 (cox_result$result)。
cox_result <- cox_run(data = lung, timediff = "time", event = "status", mainvar = "sex", covars = "age")
```

## 4. `cox_run_sub`

### 功能介绍

`cox_run_sub` 函数在`cox_run`的基础上，实现了亚组分析。它会根据指定的分组变量，对每个亚组分别运行Cox回归，并将结果汇总。这对于评估不同人群中某个变量的效应是否一致非常有用。

### 参数说明

- `data`: 数据框。
- `group_var`: 字符串，用于划分亚组的变量名。
- 其他参数与 `cox_run` 相同 (`time1`, `time2`, `timediff`, `event`, `mainvar`, `covars`, ...)。

### 使用示例

```R
# run，函数还会不可见地返回一个列表，包含模型对象 (cox_result$model) 和结果数据框 (cox_result$result)。
# 注意：结果中可能包含警告或NA，因为某些亚组的样本量太小无法建模。
cox_result <- cox_run(data = lung,group_var="sex", timediff = "time", event = "status", mainvar = "sex", covars = "age")
```

## 5. `cox_run_q`

### 功能介绍

`cox_run_q` 函数用于处理连续型变量。它首先将指定的连续变量按分位数（如三分位数、四分位数）进行分组，然后将这个新生成的分组变量作为主要变量进行Cox回归分析。这有助于探究变量的非线性效应。

### 参数说明

- `data`: 数据框。
- `mainvar`: 字符串，需要进行分位数转换的连续变量名。
- `q`: 整数，指定要划分的组数（例如，3表示三分位数）。
- 其他参数与 `cox_run` 相同 (`time1`, `time2`, `timediff`, `event`, `covars`, ...)。

### 使用示例

```R
# run，函数还会不可见地返回一个列表，包含模型对象 (cox_result$model) 和结果数据框 (cox_result$result)。
# 注意：由于时间依赖变量的长数据特性，本函数不支持基于时依变量的分位数Cox建模
cox_result <- cox_run(data = lung,q=3, timediff = "time", event = "status", mainvar = "BMI", covars = "age")
```

## 6. `cox_het`

### 功能介绍

`cox_het` 函数用于检验亚组间的异质性。它通常接收 `cox_run_sub` 的输出结果，利用 `metafor` 包的功能，计算Cochran's Q统计量、I²统计量和对应的p值，以判断亚组间的效应是否存在显著差异。

### 参数说明

- `df`: 一个数据框，通常是 `cox_run_sub` 的输出结果。必须包含 `Subgroup`, `beta`, `se` 这几列。
- `method`: 字符串，`metafor::rma` 函数的异质性检验方法，默认为 "REML"。
- `digits`: 数字，结果保留的小数位数。

### 使用示例

```R
# 可直接对 cox_run_sub 的结果进行异质性检验。
het_result <- cox_het(df = subgroup_result_clean)
```

## 7. `plot_forest`

### 功能介绍

`plot_forest` 是一个强大的、基于 `ggplot2` 的森林图绘制函数。它专门设计用于可视化Cox回归的结果（或其他类似效应量的分析结果），能够将效应量（如HR）及其置信区间以图形方式展示，并在图形两侧附上相关的文本信息（如变量名、P值等）。该函数提供了极为丰富的参数来自定义图形的几乎每一个细节。

### 参数说明

该函数参数众多，这里仅列出核心参数：

- `df`: 数据框，用于绘图的数据。
- `left_side_cols`: 数值向量，指定显示在森林图左侧的列号。
- `right_side_cols`: 数值向量，指定显示在森林图右侧的列号。
- `estimate`: 字符串，效应量（如HR）的列名。
- `lower`: 字符串，置信区间下限的列名。
- `upper`: 字符串，置信区间上限的列名。
- `x_limit`: 数值向量，`c(min, max)`，设置X轴范围。
- `ref_line_value`: 数值，参考线的位置（通常为1）。
- `p_value_col`: (可选) 字符串，P值列的列名，用于自动格式化。
- ... 以及大量用于调整颜色、大小、字体、形状等的参数。

### 使用示例

```R
# 我们可以直接使用 cox_run_sub 的结果来绘制森林图，并进一步利用topptx输出
forest_plot <- plot_forest(
  df = plot_data,
  left_side_cols = c(2, 3), 
  right_side_cols = c(11, 8), 
  estimate = "HR",
  lower = "HR_lower",
  upper = "HR_upper",
  x_limit = c(0.1, 2.5),
  ref_line_value = 1,
  p_value_col = "P",
  p_value_round = 3
)
topptx(forest_plot, filename = 'dir/forest_plot.pptx')
```
