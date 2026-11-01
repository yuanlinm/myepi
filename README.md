# myepi

队列与病例对照研究常用分析：缺失概览、Cox 回归（主效应、分位数、亚组与交互、异质性）、非线性剂量反应（RCS / PSP）、逻辑回归、条件逻辑回归与线性回归，以及发表级森林图与剂量反应曲线图。

反馈与需求：关注公众号 **epi solution** 后留言。

## 安装

```r
if (!requireNamespace("devtools", quietly = TRUE)) install.packages("devtools")
devtools::install_github("yuanlinm/myepi")
library(myepi)
```

可选依赖：`rms`（`cox_run_rcs`）、`sandwich`（`logit_run` / `lm_run` 的稳健方差）、`systemfonts`（绘图前检查字体）；系统工具 poppler（绘图后校验 PDF 的嵌入字体与文本）。

## 通用约定

- 分析函数不自动打印，返回结果对象：`res <- cox_run(...)`，用 `print(res)` 查看，`res$result` 为结果表，`res$model` 为模型。
- 结果表保存原始精度，仅在打印时格式化。
- 拟合前按完整病例剔除模型变量缺失，并以 message 报告；事件数、人数、人年、发病率均基于同一分析样本。
- 随访时间：`timediff`（右删失），或 `time1` + `time2`（计数过程长数据）；长数据用 `idL` 指定个体 ID，人数与病例数按个体去重。
- `event` 编码同 `survival::Surv`：0/1、TRUE/FALSE 或 1/2。
- `covars` 可写列名或表达式，如 `"strata(center)"`、`"cluster(id)"`、`"a*b"`。
- `robust = FALSE`（默认）使用模型方差，忽略 `cluster()`；`robust = TRUE` 使用稳健方差（按 `cluster()` 或 `idL` 聚类）。CI、P 值与各项检验均基于所选方差。
- `t_unit`：随访时间单位 `"d"` / `"y"` / `"m"`；`incidence_scale`：发病率尺度，默认每 10 万人年。

## 函数

### count_na：缺失概览

```r
count_na(df)                         # 各变量缺失数与缺失率，按缺失率降序
count_na(df, group_var = "center")   # 按组统计
```

### cox_run：Cox 回归

```r
res <- cox_run(df, timediff = "time", event = "status", mainvar = "exposure",
               covars = c("age", "sex", "strata(center)"))
res$result   # 各水平的事件数/人数、人年、发病率及 95% CI、HR、95% CI、P
```

分类暴露自动输出参照行；`extra_vars` 可额外输出协变量或交互项（如 `"x:z"`）的效应；`...` 传给 `survival::coxph`。

### cox_run_q：分位数分组

```r
res <- cox_run_q(df, mainvar = "bmi", q = 4, timediff = "time", event = "status",
                 covars = c("age", "sex"), trend = TRUE)
res <- cox_run_q(df, mainvar = "bmi", q = c(10, 18.5, 24, 28, 60), ...)   # 自定义断点
```

分位点在分析样本中计算（指定 `idL` 时按个体）；分位点重复时自动合并并警告。趋势检验得分可选组内中位数或序号（`trend_method`）。`res$breaks` 为实际断点。

### cox_run_sub：亚组分析与交互检验

```r
res <- cox_run_sub(df, group_var = "sex", timediff = "time", event = "status",
                   mainvar = "exposure", covars = c("age", "bmi"))
res$result        # 各亚组结果，含 P_interaction
res$interaction   # 交互检验（模型方差：似然比检验；稳健方差：Wald 检验）
```

含分组变量的协变量项在亚组模型中自动剔除；`group_na` 控制分组变量缺失的处理（剔除或单列一组）。

### cox_het：亚组间异质性

```r
cox_het(res)   # res 为 cox_run_sub 的结果；metafor::rma，默认 method = "EE"
```

输出 Cochran's Q、P 值、I²、τ²；参照行自动排除。亚组差异建议以 `P_interaction` 为主要指标。

### cox_run_rcs：限制性立方样条

```r
res <- cox_run_rcs(df, timediff = "time", event = "status", mainvar = "pm25",
                   covars = c("age", "sex"), nk = 3)
res$result   # 500 点曲线：x, hr, ll, ul（相对参照值）
res$tests    # 总体关联与非线性 Wald 检验
```

基于 `rms::cph` + `rcs()` + `Predict()` + `anova()`。节点：`nk`（默认 3，Harrell 分位数）、`knots`（绝对值）或 `knot_probs`（分位数）；参照值 `ref` 默认中位数；`range_probs` 可截取预测范围。

### cox_run_psp：惩罚样条

```r
res <- cox_run_psp(df, timediff = "time", event = "status", mainvar = "pm25",
                   covars = c("age", "sex"), df = 4)
res$result   # 500 点曲线：x, hr, ll, ul
res$tests    # 线性与非线性 Wald 检验
```

基于 `survival::coxph` + `pspline()`，曲线同 `termplot()`（按 survival 默认中心化，无参照值）。`df = 0` 按 AIC 选择；也可用 `theta`、`nterm`。

两个样条函数都不允许协变量中出现含 `mainvar` 的项。

### logit_run：逻辑回归

```r
res <- logit_run(df, outcome = "case", mainvar = "exposure", covars = c("age", "sex"))
res$result   # 各水平的病例数/总人数、OR、95% CI、P
```

基于 `glm(family = binomial)`。结局可为 0/1、TRUE/FALSE 或二水平因子（第二个水平为事件）。检出（准）完全分离时给出警告。`ci_method = "wald"`（默认）或 `"profile"`（剖面似然）；`robust = TRUE` 时用 `sandwich` 稳健方差（有 `cluster()` 时按其聚类）。

### clogit_run：条件逻辑回归

```r
res <- clogit_run(df, outcome = "case", mainvar = "exposure", match_id = "set_id", covars = "bmi")
res$result   # 各水平的病例数与对照数、OR、95% CI、P
```

基于 `survival::clogit`（默认精确条件似然）。报告匹配组数与有效匹配组数（同时含病例与对照）。稳健方差需同时设置 `method = "approximate"` 或 `"efron"`。

### lm_run：线性回归

```r
res <- lm_run(df, outcome = "sbp", mainvar = "exposure", covars = c("age", "sex"))
res$result   # 各水平的人数、结局均值与标准差、beta、95% CI、P
```

基于 `lm`，推断采用 t 分布；`res$info` 含 R² 与调整 R²。

### logit_run_rcs / clogit_run_rcs / lm_run_rcs：回归模型的限制性立方样条

```r
res <- logit_run_rcs(df, outcome = "case", mainvar = "pm25", covars = c("age", "sex"))
res <- clogit_run_rcs(df, outcome = "case", mainvar = "pm25", match_id = "set_id")
res <- lm_run_rcs(df, outcome = "sbp", mainvar = "pm25", covars = c("age", "sex"))
res$result   # 500 点曲线：x 与 or（或 beta）、ll、ul（相对参照值）
res$tests    # 总体关联与非线性检验
```

节点、参照值与预测范围的参数同 `cox_run_rcs`。`logit_run_rcs` 与 `lm_run_rcs` 基于 `rms::lrm` / `rms::ols` + `Predict()` + `anova()`（线性回归为 t 分布 CI 与 F 检验）；`clogit_run_rcs` 基于 `survival::clogit` 与相同的样条基函数，对比与检验的定义同 `Predict()` / `anova()`。匹配变量在匹配组内无变异，不能作为 `clogit_run_rcs` 的暴露。

### plot_forest：森林图（表格式）

```r
sub_sex <- cox_run_sub(df, group_var = "sex", ...)
sub_age <- cox_run_sub(df, group_var = "age_group", ...)
plot_forest(list(sub_sex, sub_age), file = "forest.pdf",
            labels = c(sex = "Sex", age_group = "Age"))
plot_forest(list(PM2.5 = list(sub_sex, sub_age), O3 = list(sub_sex2, sub_age2)), file = "forest2.pdf")
```

接收 `cox_run_sub`、`cox_run_q`、`cox_run`、`logit_run`、`clogit_run`、`lm_run` 的结果或数据框；效应量（HR / OR / beta）、轴标题与无效值（1 或 0）自动识别。具名列表为多个面板（1–3 个）。输出矢量 PDF，`lancet_style = TRUE` 为 Lancet 排版。

### plot_forest_landscape：森林图（横向）

```r
plot_forest_landscape(
  list(Ozone = list(Concurrent = r0, "1-year lag" = r1),
       PM2.5 = list(Concurrent = p0, "1-year lag" = p1)),
  file = "lag.pdf")
```

类别沿 x 轴排列，面板自上而下堆叠，适用于滞后或暴露窗口比较。

### plot_crf：剂量反应曲线

```r
plot_crf(res, file = "crf.pdf", xlab = expression(PM[2.5]~"(µg/m"^3*")"),
         ci_fill = TRUE, ci_border = FALSE)
```

接收 `cox_run_rcs`、`cox_run_psp`、`logit_run_rcs`、`clogit_run_rcs`、`lm_run_rcs` 的结果；y 轴标题与参照线（HR、OR 为 1，beta 为 0）自动确定。可调整曲线、CI 阴影与 CI 边线的颜色、线宽与透明度。

### 绘图通用参数

`font_family`（默认 `"Helvetica"`，未安装时报错，不做替换）、`width_mm` / `height_mm`、`overwrite`（默认不覆盖已有文件）。返回运行摘要，可用 `print()` 查看。

## 引用

"Statistical analyses were performed in R using the myepi package (GitHub: yuanlinm/myepi)."
