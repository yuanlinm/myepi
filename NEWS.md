# myepi 0.2.0 (2026-11-03)

## 新增

- 加性交互：`cox_run_add()`、`logit_run_add()`、`clogit_run_add()`。两个因素组成联合分组（参照为两者的低风险水平），对每个非参照组合报告 RERI、AP、S，联合超额风险的两因素贡献分解（A 单独、B 单独、交互，合计为 1；VanderWeele & Tchetgen Tchetgen 2014），乘法交互比值与分层效应；联合效应表可直接用于 `plot_forest()`。
- 因素分组与顺序：连续变量按界值（`cut_*`）或分位数（`q_*`，默认三分位）在分析样本中分组；`risk_*` 声明风险方向，自动排序使第一个水平为低风险参照；分类变量用 `levels_*` 指定顺序。单独暴露组效应 < 1 时警告（Knol 等 2011）。
- CI：delta 法（默认，解析梯度，与所选方差一致）或 bootstrap 百分位数（分组界值固定，按个体、聚类或匹配组重抽样，`seed` 可复现）。
- 逻辑回归结局比例 > 10% 时警告；条件逻辑回归中不可估计（如匹配变量）的因素直接报错。

## 文档

- README 新增加性交互说明；速查表更新为 v0.2.0。

# myepi 0.1.0 (2026-11-01)

## 新增

- 逻辑回归 `logit_run()`、条件逻辑回归 `clogit_run()`、线性回归 `lm_run()`，及其亚组分析（`*_run_sub()`，含交互检验）与分位数分组分析（`*_run_q()`，含趋势检验）。
- 非线性剂量反应：`cox_run_rcs()`、`cox_run_psp()`、`logit_run_rcs()`、`clogit_run_rcs()`、`lm_run_rcs()`，均输出 500 点曲线与总体/非线性检验。
- 发表级矢量绘图：`plot_forest()`（表格式）、`plot_forest_landscape()`（横向）、`plot_crf()`（剂量反应曲线）；字体嵌入、文本提取与线宽校验，任一校验失败不生成文件。
- `cox_run_sub()` 新增交互检验（`P_interaction`）与 `group_na`；`logit_run()` 新增（准）完全分离检测。

## 变更（不兼容）

- 所有分析函数不再自动打印，返回 `myepi_result` 对象（`model`、`result`、`info`），用 `print()` 查看；结果表保存原始精度。
- 拟合前按完整病例生成分析样本并报告缺失处理；事件数、人数、人年与发病率均基于同一分析样本。
- `robust` 默认 `FALSE`：仅在用户指定 `robust = TRUE` 时使用稳健方差，未指定时 `cluster()` 被忽略。
- `cox_run_q()` 与 `cox_het()` 改为返回 `myepi_result` 对象；`cox_het()` 默认 `method = "EE"`，排除参照行，删除 `digits` 参数。
- `plot_forest()` 改为 grid 实现并直接输出 PDF；不再依赖 ggplot2。
- 删除 `cross_tb()`。

## 修复

- 数值型标签的因子水平与系数错位；交互项被误判为暴露水平；1/2 结局编码下计数错误。
- `cox_run_sub()` 分组变量缺失时分母被污染、分类暴露各水平计数被亚组合计覆盖。
- `cox_run_q()` 稳健方差下 CI 与 P 值口径不一致、分位点重复时报错。
- `count_na()` 分组变量含缺失时报错。

# myepi 0.0.2 (2026-09-05)

- 修复 `cox_run` / `cox_run_sub` / `cox_run_q` 在 incidence 人时量纲上的错误。
- 新增参数 `t_unit`（`d`/`y`/`m`）与 `incidence_scale`，默认保持向后兼容（`d`, `1e5`）。
- 修复 `cox_run_sub` 与 `cox_run_q` 的参数透传与结果一致性问题。
- 更新文档（man）与测试，包含 `d` 与 `y` 的一致性校验。
