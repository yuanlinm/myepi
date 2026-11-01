# 生成 README 中的速查表：Rscript cheatsheets/cheatsheet.R cheatsheets/cheatsheet_v0.1.0.png（需 WenQuanYi Zen Hei 字体）
# myepi 0.1.0 速查表（RStudio cheatsheet 风格）
args <- commandArgs(trailingOnly = TRUE)
out <- args[1]
library(grid)

CJK <- "WenQuanYi Zen Hei"
MONO <- "WenQuanYi Zen Hei Mono"
INK <- "#1F2933"
GREY <- "#5F6B76"
COL <- c(prep = "#3B6EA8", model = "#2E8B73", het = "#B5651D", plot = "#7A4FA0", flow = "#4A5560")
LIGHT <- c(prep = "#EAF1F9", model = "#E7F4EF", het = "#FBF0E6", plot = "#F2ECF8", flow = "#EEF0F2")

W <- 16
H <- 10
png(out, width = W, height = H, units = "in", res = 220, type = "cairo", bg = "white")
grid.newpage()
pushViewport(viewport(xscale = c(0, W), yscale = c(0, H)))

txt <- function(label, x, y, size = 10, col = INK, face = "plain", just = c("left", "centre"), family = CJK, rot = 0) {
  grid.text(label, x = unit(x, "native"), y = unit(y, "native"), just = just, rot = rot,
            gp = gpar(fontfamily = family, fontsize = size, col = col, fontface = face, lineheight = 1.05))
}
code <- function(label, x, y, size = 9.5, col = INK, just = c("left", "centre")) txt(label, x, y, size, col, "plain", just, MONO)
box <- function(x, y, w, h, fill = NA, col = NA, lwd = 1, r = 0.08) {
  grid.roundrect(x = unit(x, "native"), y = unit(y, "native"), width = unit(w, "native"), height = unit(h, "native"),
                 just = c("left", "bottom"), r = unit(r, "native"), gp = gpar(fill = fill, col = col, lwd = lwd))
}
section <- function(x, y, w, h, title, key) {
  box(x, y, w, h, fill = LIGHT[[key]], col = NA)
  box(x, y + h - 0.42, w, 0.42, fill = COL[[key]], col = NA, r = 0.08)
  grid.rect(x = unit(x, "native"), y = unit(y + h - 0.42, "native"), width = unit(w, "native"), height = unit(0.1, "native"),
            just = c("left", "bottom"), gp = gpar(fill = COL[[key]], col = NA))
  txt(title, x + 0.18, y + h - 0.21, 13, "white", "bold")
}
arrow_line <- function(x0, y0, x1, y1, col = GREY, lwd = 1.4) {
  grid.lines(x = unit(c(x0, x1), "native"), y = unit(c(y0, y1), "native"),
             arrow = arrow(length = unit(0.09, "inches"), type = "closed"), gp = gpar(col = col, fill = col, lwd = lwd))
}

# ---- 标题 ----
grid.rect(x = unit(0, "native"), y = unit(H - 0.95, "native"), width = unit(W, "native"), height = unit(0.95, "native"),
          just = c("left", "bottom"), gp = gpar(fill = INK, col = NA))
txt("myepi 速查表", 0.35, H - 0.45, 26, "white", "bold")
txt("队列与病例对照研究：回归分析流程与发表级图形", 3.15, H - 0.47, 14, "#D7DEE5")
txt("v0.1.0", W - 0.35, H - 0.36, 14, "white", "bold", c("right", "centre"))
txt("github.com/yuanlinm/myepi", W - 0.35, H - 0.66, 10.5, "#D7DEE5", just = c("right", "centre"))

# ---- 1 数据检查与通用约定 ----
x1 <- 0.3; y1 <- 4.45; w1 <- 4.3; h1 <- 4.45
section(x1, y1, w1, h1, "1  数据检查与通用约定", "prep")
yy <- y1 + h1 - 0.75
code("count_na(df, group_var = NULL)", x1 + 0.2, yy, 10.5, COL[["prep"]]); yy <- yy - 0.3
txt("各变量缺失数与缺失率，按缺失率降序", x1 + 0.2, yy, 9.5, GREY); yy <- yy - 0.4
txt("结果对象（myepi_result）", x1 + 0.2, yy, 11, face = "bold"); yy <- yy - 0.3
code("res <- cox_run(...)   # 不自动打印", x1 + 0.2, yy); yy <- yy - 0.27
code("print(res)            # 格式化显示", x1 + 0.2, yy); yy <- yy - 0.27
code("res$result  res$model  res$info", x1 + 0.2, yy); yy <- yy - 0.4
txt("通用约定", x1 + 0.2, yy, 11, face = "bold"); yy <- yy - 0.3
bul <- c("完整病例分析，缺失处理以 message 报告",
         "描述统计与模型基于同一分析样本",
         "covars: \"age\"  \"a*b\"  \"strata(center)\"",
         "robust = FALSE（默认）；TRUE 时用稳健方差",
         "Cox 长数据：time1 + time2 + idL",
         "结果保存原始精度，仅打印时格式化")
for (b in bul) { txt(paste0("•  ", b), x1 + 0.2, yy, 9.6); yy <- yy - 0.265 }

# ---- 2 模型 × 分析矩阵 ----
x2 <- 4.8; y2 <- 4.45; w2 <- 10.9; h2 <- 4.45
section(x2, y2, w2, h2, "2  回归建模：模型 × 分析", "model")
cols <- c("Cox 回归（HR）", "逻辑回归（OR）", "条件逻辑回归（OR）", "线性回归（β）")
rows <- c("主效应", "分位数 + 趋势", "亚组 + 交互", "RCS 样条", "惩罚样条")
fns <- rbind(c("cox_run", "logit_run", "clogit_run", "lm_run"),
             c("cox_run_q", "logit_run_q", "clogit_run_q", "lm_run_q"),
             c("cox_run_sub", "logit_run_sub", "clogit_run_sub", "lm_run_sub"),
             c("cox_run_rcs", "logit_run_rcs", "clogit_run_rcs", "lm_run_rcs"),
             c("cox_run_psp", "—", "—", "—"))
tx <- x2 + 0.2; ty <- y2 + h2 - 0.7; lab_w <- 1.55; cw <- 1.95; rh <- 0.38
for (j in seq_along(cols)) {
  box(tx + lab_w + (j - 1) * cw + 0.03, ty - 0.17, cw - 0.06, 0.34, fill = COL[["model"]], col = NA, r = 0.05)
  txt(cols[j], tx + lab_w + (j - 0.5) * cw, ty, 10, "white", "bold", c("centre", "centre"))
}
for (i in seq_along(rows)) {
  yc <- ty - i * rh - 0.05
  txt(rows[i], tx, yc, 10.5, face = "bold")
  for (j in seq_along(cols)) {
    fill <- if (fns[i, j] == "—") "#F4F6F7" else "white"
    box(tx + lab_w + (j - 1) * cw + 0.03, yc - 0.155, cw - 0.06, 0.31, fill = fill, col = "#C9D6D0", lwd = 0.8, r = 0.04)
    code(fns[i, j], tx + lab_w + (j - 0.5) * cw, yc, 10, if (fns[i, j] == "—") "#9AA5AE" else INK, c("centre", "centre"))
  }
}
ny <- ty - 6 * rh - 0.12
notes <- c(
  "分位数：q = 4 等分位或自定义断点；trend = TRUE 给出 P for trend（组内中位数或序号得分）",
  "亚组：P_interaction —— 模型方差下 LRT（线性回归为 F 检验），robust = TRUE 时为 Wald；clogit 须按匹配因素分组",
  "RCS：nk = 3（Harrell 分位数）/ knots / knot_probs；参照值默认中位数；rms::Predict + anova，500 点曲线",
  "惩罚样条：survival::pspline，df（0 为 AIC）/ theta / nterm；曲线同 termplot()；线性与非线性检验")
for (n in notes) { txt(paste0("•  ", n), tx, ny, 9.4, GREY); ny <- ny - 0.27 }

# ---- 3 异质性 ----
x3 <- 0.3; y3 <- 0.35; w3 <- 3.4; h3 <- 4.0
section(x3, y3, w3, h3, "3  亚组异质性", "het")
yy <- y3 + h3 - 0.75
code("cox_het(sub_res)", x3 + 0.2, yy, 10.5, COL[["het"]]); yy <- yy - 0.32
for (b in c("接收任一 *_run_sub 结果", "metafor::rma，默认 EE", "输出 Q、P、I²、τ²", "自动排除参照行",
            "主要指标仍为 P_interaction")) {
  txt(paste0("•  ", b), x3 + 0.2, yy, 9.6); yy <- yy - 0.29
}
# 示意：两亚组估计与合并
yy <- yy - 0.25
bx <- x3 + 1.0; bw <- 2.0
grid.lines(unit(c(bx + bw * 0.45, bx + bw * 0.45), "native"), unit(c(yy - 0.85, yy + 0.05), "native"), gp = gpar(col = GREY, lty = 2))
for (k in 1:2) {
  yk <- yy - (k - 1) * 0.32
  est <- c(0.62, 0.75)[k]
  grid.lines(unit(bx + bw * c(est - 0.18, est + 0.18), "native"), unit(rep(yk, 2), "native"), gp = gpar(col = INK, lwd = 1.6))
  grid.points(unit(bx + bw * est, "native"), unit(yk, "native"), pch = 15, size = unit(0.09, "inches"), gp = gpar(col = INK))
  txt(c("sex: M", "sex: F")[k], bx - 0.05, yk, 8.5, GREY, just = c("right", "centre"))
}
grid.polygon(unit(bx + bw * c(0.56, 0.68, 0.80, 0.68), "native"), unit(yy - 0.66 + c(0, 0.09, 0, -0.09), "native"),
             gp = gpar(fill = COL[["het"]], col = NA))
txt("Q, I²", bx - 0.05, yy - 0.66, 8.5, GREY, just = c("right", "centre"))

# ---- 4 发表级图形 ----
x4 <- 3.9; y4 <- 0.35; w4 <- 7.25; h4 <- 4.0
section(x4, y4, w4, h4, "4  发表级图形（矢量 PDF）", "plot")
pw <- 2.25
panels_x <- x4 + 0.2 + (0:2) * (pw + 0.13)
heads <- c("plot_forest()", "plot_forest_landscape()", "plot_crf()")
descs <- list(c("表格式森林图", "多个 *_sub 结果堆叠为亚组块", "具名列表 = 1–3 个面板"),
              c("横向森林图", "滞后 / 暴露窗口比较", "面板自上而下堆叠"),
              c("剂量反应曲线", "接收 *_rcs / cox_run_psp", "CI 阴影与边线可选"))
for (k in 1:3) {
  px <- panels_x[k]; py <- y4 + 0.25
  box(px, py, pw, h4 - 0.95, fill = "white", col = "#D6CCE3", lwd = 0.8)
  code(heads[k], px + pw / 2, py + h4 - 1.2, if (k == 2) 8.8 else 9.8, COL[["plot"]], c("centre", "centre"))
  for (m in 1:3) txt(descs[[k]][m], px + pw / 2, py + 0.72 - (m - 1) * 0.22, 8.6, if (m == 1) INK else GREY,
                     if (m == 1) "bold" else "plain", c("centre", "centre"))
  # 示意图区域
  gx <- px + 0.2; gy <- py + 0.98; gw <- pw - 0.4; gh <- 1.5
  if (k == 1) {
    labs <- c("Sex", "  Male", "  Female", "Age", "  <65", "  65+")
    est <- c(NA, 0.55, 0.42, NA, 0.47, 0.66)
    for (r in seq_along(labs)) {
      yr <- gy + gh - 0.12 - (r - 1) * 0.24
      txt(labs[r], gx, yr, 7.5, INK, if (is.na(est[r])) "bold" else "plain")
      if (!is.na(est[r])) {
        cx <- gx + 0.75 + est[r] * 0.8
        grid.lines(unit(c(cx - 0.17, cx + 0.17), "native"), unit(rep(yr, 2), "native"), gp = gpar(col = INK, lwd = 1.3))
        grid.points(unit(cx, "native"), unit(yr, "native"), pch = 15, size = unit(0.06, "inches"), gp = gpar(col = INK))
      }
    }
    grid.lines(unit(rep(gx + 0.75 + 0.5 * 0.8, 2), "native"), unit(c(gy, gy + gh), "native"), gp = gpar(col = INK, lwd = 0.8))
  } else if (k == 2) {
    for (pnl in 1:2) {
      base <- gy + (2 - pnl) * 0.78
      grid.lines(unit(c(gx, gx + gw), "native"), unit(rep(base + 0.3, 2), "native"), gp = gpar(col = GREY, lty = 2, lwd = 0.8))
      grid.lines(unit(c(gx, gx), "native"), unit(c(base, base + 0.68), "native"), gp = gpar(col = INK, lwd = 0.8))
      for (c in 1:4) {
        cx <- gx + 0.2 + (c - 0.5) * (gw - 0.25) / 4
        e <- base + 0.3 + c(0.12, 0.18, 0.1, 0.05)[c] * (if (pnl == 1) 1.4 else 0.8)
        grid.lines(unit(rep(cx, 2), "native"), unit(c(e - 0.15, e + 0.15), "native"), gp = gpar(col = INK, lwd = 1.2))
        for (cap in c(e - 0.15, e + 0.15)) grid.lines(unit(cx + c(-0.05, 0.05), "native"), unit(rep(cap, 2), "native"), gp = gpar(col = INK, lwd = 1.2))
        grid.points(unit(cx, "native"), unit(e, "native"), pch = 15, size = unit(0.055, "inches"), gp = gpar(col = INK))
      }
    }
  } else {
    xs <- seq(0, 1, length.out = 100)
    f <- 0.25 + 0.5 * xs^1.6
    se <- 0.08 + 0.18 * abs(xs - 0.35)^1.3
    grid.polygon(unit(gx + gw * c(xs, rev(xs)), "native"), unit(gy + gh * c(f - se, rev(f + se)), "native"),
                 gp = gpar(fill = "#D9D9D9", col = NA))
    grid.lines(unit(c(gx, gx + gw), "native"), unit(rep(gy + gh * 0.32, 2), "native"), gp = gpar(col = GREY, lty = 2, lwd = 0.8))
    grid.lines(unit(gx + gw * xs, "native"), unit(gy + gh * f, "native"), gp = gpar(col = INK, lwd = 1.5))
    grid.lines(unit(c(gx, gx, gx + gw), "native"), unit(c(gy + gh, gy, gy), "native"), gp = gpar(col = INK, lwd = 0.8))
  }
}
txt("共同特性：最终印刷尺寸 · 单色 · 字体嵌入（默认 Helvetica）· HR / OR / β 与无效值自动识别 · 校验失败不生成文件",
    x4 + 0.2, y4 + 0.13, 8.4, GREY)

# ---- 5 流转 ----
x5 <- 11.35; y5 <- 0.35; w5 <- 4.35; h5 <- 4.0
section(x5, y5, w5, h5, "5  函数流转", "flow")
node <- function(label, x, y, w, key, size = 9.2) {
  box(x - w / 2, y - 0.17, w, 0.34, fill = "white", col = COL[[key]], lwd = 1.3, r = 0.06)
  code(label, x, y, size, COL[[key]], c("centre", "centre"))
}
cx <- x5 + w5 / 2
ax <- x5 + 1.45; bxn <- x5 + 3.55
node("count_na", cx, y5 + 3.2, 1.5, "prep")
node("*_run · *_run_q · *_run_sub", ax, y5 + 2.3, 2.6, "model", 8.4)
node("*_rcs · *_psp", bxn, y5 + 2.3, 1.4, "model", 8.4)
node("cox_het", x5 + 0.8, y5 + 1.35, 1.1, "het")
node("plot_forest(_landscape)", x5 + 1.5, y5 + 0.5, 2.45, "plot", 8.6)
node("plot_crf", bxn, y5 + 0.5, 1.3, "plot")
arrow_line(cx - 0.2, y5 + 3.03, ax + 0.3, y5 + 2.48)
arrow_line(cx + 0.25, y5 + 3.03, bxn - 0.2, y5 + 2.48)
arrow_line(ax - 0.5, y5 + 2.13, x5 + 0.8, y5 + 1.53)
txt("*_sub", x5 + 0.55, y5 + 1.85, 8.2, GREY, just = c("right", "centre"))
arrow_line(ax + 0.45, y5 + 2.13, ax + 0.45, y5 + 0.68)
arrow_line(bxn, y5 + 2.13, bxn, y5 + 0.68)
txt("HR / OR / β", ax + 0.55, y5 + 1.4, 8.2, GREY, just = c("left", "centre"))
txt("hr / or / beta", bxn - 0.1, y5 + 1.1, 8.2, GREY, just = c("right", "centre"))

popViewport()
invisible(dev.off())
