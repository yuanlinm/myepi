# myepi v0.0.2

发布日期：2026-09-05

## Highlights

- 修复 `cox_run` / `cox_run_sub` / `cox_run_q` 在 incidence 计算中的人时量纲问题。
- 新增 `t_unit` 参数（`d`/`y`/`m`），用于按天/年/月解释随访时间并统一换算为人年。
- 新增 `incidence_scale` 参数，支持自定义发病率展示尺度（默认每 10 万人年）。
- 修复 `cox_run_sub` 与 `cox_run_q` 对新增参数的透传与行为一致性。
- 同步更新 man 文档与相关测试（含 `d` 与 `y` 一致性测试）。

## Breaking changes

None

## Migration guide

- 默认行为不变：若不传参，仍按 `t_unit = "d"` 与 `incidence_scale = 1e5` 运行。
- 若 `time` 字段本身以“年”记录，请设置 `t_unit = "y"`；若以“月”记录，请设置 `t_unit = "m"`，以获得正确的人年换算与 incidence 数值。

## Verification

- 建议使用既有分析脚本复跑关键模型。
- 对比升级前后 incidence 的数量级，确认在人时单位与业务预期上保持一致。
