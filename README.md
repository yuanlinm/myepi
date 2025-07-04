# myepi

这是一个R包项目的起始模板。

## 目录结构

- DESCRIPTION: 包的元数据
- NAMESPACE: 包的命名空间
- R/: 存放R函数脚本
- man/: 存放文档
- tests/: 存放测试脚本

## 如何开发

1. 在R/目录下添加你的R脚本。
2. 使用roxygen2注释生成文档。
3. 使用 `devtools::load_all()`进行本地开发。
4. 使用 `devtools::test()`运行测试。

## 依赖建议

建议安装 `devtools`、`roxygen2`、`testthat`等R包以便开发。
