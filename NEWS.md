# myepi 0.0.2 (2026-09-05)

- 修复 `cox_run` / `cox_run_sub` / `cox_run_q` 在 incidence 人时量纲上的错误。
- 新增参数 `t_unit`（`d`/`y`/`m`）与 `incidence_scale`，默认保持向后兼容（`d`, `1e5`）。
- 修复 `cox_run_sub` 与 `cox_run_q` 的参数透传与结果一致性问题。
- 更新文档（man）与测试，包含 `d` 与 `y` 的一致性校验。
