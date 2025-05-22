
# RCS曲线计算

# 将数据标记
dd_data_dii <- datadist(data_dii)
options(datadist = "dd_data_dii")
dd_data_table2_3 <- datadist(data_table2_3)
options(datadist = "dd_data_table2_3")

# 打开优化过程的详细追踪
options(digits = 5)
options(warn = 1)


# 高尿酸血症发病率
# 柱状图
# 创建分组 步长为1
data_dii <- data_dii %>%
  mutate(DII_Group = cut(Total_DII_Adjusted, breaks = seq(min(Total_DII_Adjusted), max(Total_DII_Adjusted), by = 1)))

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(hyperuricemia_count, na.rm = TRUE)  # 组内高尿酸比例
  )

# 高尿酸血症发病率RCS
fit <- lrm(hyperuricemia_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
# 转化y轴为概率
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
# 转换为数据框
pred_df <- as.data.frame(pred) 
# plot(pred, xlab = "DII", ylab = "hyperuricemia", main = "")

# 提取 p 值
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 绘制
ggplot() +
  # 柱状图
  geom_bar(data = bar_data, aes(x = Mean_DII, y = Prevalence), 
           stat = "identity", fill = "steelblue", alpha = 0.8, width = 0.7) +
  # RCS 曲线
  geom_line(data = pred_df, aes(x = Total_DII_Adjusted, y = yhat), 
            color = "red", size = 1) +
  # 置信区间
  geom_ribbon(data = pred_df, aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
              fill = "#b0171f", alpha = 0.3) +
  # 图例和标签
  labs(
    x = "DII Score", 
    y = "Hyperuricemia Prevalence",
    title = ""
  ) +
  # 添加到图中
  annotate("text", x = -5, y = 0.35, label = paste("P-overall =", p_overall), size = 4) +
  annotate("text", x = -5, y = 0.3, label = paste("P-nonlinear =", p_nonlinear), size = 4)+
  theme_minimal() + 
  theme(panel.grid = element_blank())

# 查看p
anova(fit)
















# 高尿酸第二版
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(hyperuricemia_count, na.rm = TRUE),  # 组内高血压
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(hyperuricemia_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Hyperuricemia Prevalence"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))






























# 高尿酸细分版
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 0.1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(hyperuricemia_count, na.rm = TRUE),  # 组内高血压
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(hyperuricemia_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.08
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Hyperuricemia Prevalence"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))






























# SII
# 柱状图
# 创建分组 步长为1
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(SII, na.rm = TRUE),  # 组内SII
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

bar_data_numeric


# SII的RCS
sii_fit <- ols(SII ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
# 生成预测值
pred <- Predict(sii_fit, Total_DII_Adjusted, fun = identity)
# 转换为数据框
pred_df <- as.data.frame(pred) 
plot(pred, xlab = "DII", ylab = "SII", main = "")

# 提取 p 值
anova_res <- anova(sii_fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 绘图
ggplot() +
  # 柱状图 (注意这里使用 numeric 的 midpoint 做 x)
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    # 如果每段区间正好宽1，你可以设 width=0.8~0.9；或者直接用全宽
    width = 0.8 * bar_data_numeric$width  
  ) +
  
  # spline 曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  
  # 置信区间
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f", alpha = 0.3
  ) +
  
  # 一些标签和注释
  annotate(
    "text", 
    x = min(pred_df$Total_DII_Adjusted), 
    y = max(bar_data_numeric$Prevalence) * 1.1, 
    label = paste("P-overall =", p_overall), 
    size = 4,
    hjust = 0
  ) +
  annotate(
    "text", 
    x = min(pred_df$Total_DII_Adjusted), 
    y = max(bar_data_numeric$Prevalence) * 1.05, 
    label = paste("P-nonlinear =", p_nonlinear), 
    size = 4,
    hjust = 0
  ) +
  
  # 坐标轴标签
  labs(
    x = "DII Score",
    y = "SII"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )




























# SIRI
# 柱状图
# 创建分组 步长为1
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(SIRI, na.rm = TRUE),  # 组内SII
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

bar_data_numeric


# SIRI的RCS
siri_fit <- ols(SIRI ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
# 生成预测值
pred <- Predict(siri_fit, Total_DII_Adjusted, fun = identity)
# 转换为数据框
pred_df <- as.data.frame(pred) 
plot(pred, xlab = "DII", ylab = "SIRI", main = "")

# 提取 p 值
anova_res <- anova(siri_fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 绘图
ggplot() +
  # 柱状图 (注意这里使用 numeric 的 midpoint 做 x)
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    # 如果每段区间正好宽1，你可以设 width=0.8~0.9；或者直接用全宽
    width = 0.8 * bar_data_numeric$width  
  ) +
  
  # spline 曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  
  # 置信区间
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f", alpha = 0.3
  ) +
  
  # 一些标签和注释
  annotate(
    "text", 
    x = min(pred_df$Total_DII_Adjusted), 
    y = max(bar_data_numeric$Prevalence) * 1.1, 
    label = paste("P-overall =", p_overall), 
    size = 4,
    hjust = 0
  ) +
  annotate(
    "text", 
    x = min(pred_df$Total_DII_Adjusted), 
    y = max(bar_data_numeric$Prevalence) * 1.05, 
    label = paste("P-nonlinear =", p_nonlinear), 
    size = 4,
    hjust = 0
  ) +
  
  # 坐标轴标签
  labs(
    x = "DII Score",
    y = "SIRI"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )
































# GFR
# 柱状图
# 创建分组 步长为1
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(eGFR, na.rm = TRUE),  # 组内GFR
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

bar_data_numeric


# GFR的RCS
gfr_fit <- ols(eGFR ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
# 生成预测值
pred <- Predict(gfr_fit, Total_DII_Adjusted, fun = identity)
# 转换为数据框
pred_df <- as.data.frame(pred) 
plot(pred, xlab = "DII", ylab = "GFR", main = "")

# 提取 p 值
anova_res <- anova(gfr_fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 绘图
ggplot() +
  # 柱状图 (注意这里使用 numeric 的 midpoint 做 x)
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    # 如果每段区间正好宽1，你可以设 width=0.8~0.9；或者直接用全宽
    width = 0.8 * bar_data_numeric$width  
  ) +
  
  # spline 曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  
  # 置信区间
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f", alpha = 0.3
  ) +
  
  # 一些标签和注释
  annotate(
    "text", 
    x = min(pred_df$Total_DII_Adjusted), 
    y = max(bar_data_numeric$Prevalence) * 1.1, 
    label = paste("P-overall =", p_overall), 
    size = 4,
    hjust = 0
  ) +
  annotate(
    "text", 
    x = min(pred_df$Total_DII_Adjusted), 
    y = max(bar_data_numeric$Prevalence) * 1.05, 
    label = paste("P-nonlinear =", p_nonlinear), 
    size = 4,
    hjust = 0
  ) +
  
  # 坐标轴标签
  labs(
    x = "DII Score",
    y = "GFR"
  ) +
  
  theme_minimal() +
  theme(
    panel.grid = element_blank()
  )






























# 冠心病（第一版）
# 柱状图
# 创建分组 步长为1
data_dii <- data_dii %>%
  mutate(DII_Group = cut(Total_DII_Adjusted, breaks = seq(min(Total_DII_Adjusted), max(Total_DII_Adjusted), by = 1)))

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(heartdisease_count, na.rm = TRUE)  # 组内发病率比例
  )

# 冠心病发病率RCS
fit <- lrm(heartdisease_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
# 转化y轴为概率
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
# 转换为数据框
pred_df <- as.data.frame(pred) 
# plot(pred, xlab = "DII", ylab = "hyperuricemia", main = "")

# 提取 p 值
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 绘制
ggplot() +
  # 柱状图
  geom_bar(data = bar_data, aes(x = Mean_DII, y = Prevalence), 
           stat = "identity", fill = "steelblue", alpha = 0.8, width = 0.7) +
  # RCS 曲线
  geom_line(data = pred_df, aes(x = Total_DII_Adjusted, y = yhat), 
            color = "red", size = 1) +
  # 置信区间
  geom_ribbon(data = pred_df, aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
              fill = "#b0171f", alpha = 0.3) +
  # 图例和标签
  labs(
    x = "DII Score", 
    y = "Heart Disease",
    title = ""
  ) +
  # 添加到图中
  annotate("text", x = -5, y = max(bar_data$Prevalence) * 1.1,, label = paste("P-overall =", p_overall), size = 4) +
  annotate("text", x = -5, y = max(bar_data$Prevalence) * 1.05, label = paste("P-nonlinear =", p_nonlinear), size = 4)+
  theme_minimal() + 
  theme(panel.grid = element_blank())

# 查看p
anova(fit)




# 第二版
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(heartdisease_count, na.rm = TRUE),  # 组内冠心病
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(heartdisease_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Heart Disease"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))






























# 高血压
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(hypertension_count, na.rm = TRUE),  # 组内高血压
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(hypertension_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Hypertension"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))



































# 糖尿病
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(diabetes_count, na.rm = TRUE),  # 组内糖尿病
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(diabetes_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    # y = max(bar_data_numeric$Prevalence) * 1.1,
    y = 0.18,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    # y = max(bar_data_numeric$Prevalence) * 1.05,
    0.19,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Diabetes"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))





































# 高胆固醇血症
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(hypercholesterolemia_count, na.rm = TRUE),  # 组内高脂血症
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(hypercholesterolemia_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Hypercholesterolemia"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))






































# 是否吸烟
data_dii <- data_dii %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),  # 组内平均 DII
    Prevalence = mean(smoke_count, na.rm = TRUE),  # 组内高脂血症
    .groups    = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    # 因子转字符
    DII_Group_Chr = as.character(DII_Group),
    # 去掉括号/方括号
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  # separate()会直接把" -10, -9 "这样的字符串分割成两列
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  # 去掉空格并转数值
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  # 计算中点、区间宽度
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 5. 拟合 RCS，转化为概率
fit <- lrm(smoke_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii)
pred <- Predict(fit, Total_DII_Adjusted, fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 6. 绘图
ggplot() +
  # (a) 柱状图：x 为中点，y 为每组的“患病率(比例)”
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Smoke"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02))




































# 用于验证的散点图
ggplot(data_dii, aes(x = Total_DII_Adjusted, y = LBDSUASImg)) +
  geom_point(
    color = "#2E86C1",    # 设置点颜色为矢车菊蓝
    alpha = 0.7,          # 设置透明度（0-1）
    size = 3,             # 点的大小
    shape = 19            # 实心圆点形状
  ) +
  geom_smooth(
    method = "lm",        # 添加线性回归趋势线
    color = "#E74C3C",    # 趋势线颜色
    se = TRUE,            # 显示置信区间
    linetype = "solid"    # 实线类型
  ) +
  labs(
    x = "DII",
    y = "uric acid",
    title = "Relationship",
    caption = "Nhanes"
  ) +
  theme_bw(base_size = 12) +  # 白底主题，基础字号12
  theme(
    plot.title = element_text(hjust = 0.5, face = "bold"),  # 标题居中加粗
    axis.title = element_text(face = "bold"),        # 坐标轴标题加粗
    panel.grid.major = element_line(color = "grey90")# 网格线颜色
  )








ref_lines <- data.frame(
  cutoff = c(6,7),
  gender = c("Female (≥6 mg/dL)","Male (≥7 mg/dL)")
)

# ---- 高级散点图代码 ----
ggplot(data_dii, aes(Total_DII_Adjusted, LBDSUASImg)) +
  # 主散点层
  geom_jitter(
    width = 0.12,
    height = 0.1,
    color = "#27AE60",    # 主题色调整为翡翠绿
    alpha = 0.55,         # STEM期刊推荐透明度
    size = 2.5,
    shape = 18            # 菱形符号提升辨识度
  ) +
  # 双趋势线处理
  geom_smooth(
    aes(group = Sex),     # 分离性别趋势
    method = "lm",
    formula = y ~ poly(x,2),
    color = "grey40",
    se = FALSE,           # 关闭置信区间增强可读性
    linewidth = 0.8
  ) +
  # 双向参考线系统
  geom_hline(
    data = ref_lines,
    aes(yintercept = cutoff, linetype = gender),
    color = c("#8E44AD","#3498DB"),  # 紫色 VS 蓝色
    linewidth = 1.1
  ) +
  # 动态标注系统
  geom_text(
    data = ref_lines,
    aes(x = max(data_dii$Total_DII_Adjusted)*0.95,
        y = cutoff,
        label = gender),
    hjust = 1, vjust = -0.5,
    color = c("#8E44AD","#3498DB"),
    fontface = "bold",
    size = 4
  ) +
  # 坐标轴与标签系统
  labs(
    x = expression(bold("Dietary Inflammatory Index")),
    y = expression(bold("Serum Uric Acid (mg/dL)")),
    title = "膳食炎症指数与血尿酸水平总体分布",
    caption = "数据来源：NHANES | 趋势线按性别分层展示"
  ) +
  # 标尺系统优化
  scale_y_continuous(
    breaks = seq(floor(min(data_dii$LBDSUASImg)),
                 ceiling(max(data_dii$LBDSUASImg)),
                 by=0.5),
    minor_breaks = seq(floor(min(data_dii$LBDSUASImg)),
                       ceiling(max(data_dii$LBDSUASImg)),
                       by=0.25),
    expand = expansion(mult = c(0.02, 0.05))
  ) +
  # 主题系统
  theme_classic(base_size = 13) +
  theme(
    plot.title = element_text(hjust = 0.5, size=16, margin=margin(b=15)),
    axis.line = element_line(linewidth=0.8, color="black"),
    axis.ticks = element_line(color="black", linewidth=0.6),
    legend.position = "top",
    legend.title = element_blank(),
    legend.text = element_text(color="#2C3E50"),
    panel.background = element_rect(fill = "grey98") 
  ) +
  # 科学注释
  annotate(
    "text",
    x = min(data_dii$Total_DII_Adjusted),
    y = max(data_dii$LBDSUASImg)*0.95,
    label = paste("总样本量：", nrow(data_dii)),
    hjust = 0, color = "#34495E", size=4.5
  )






# 对数据集进行男女分类
data_dii_male <- subset(data_dii, Sex == "Male")
data_dii_female <- subset(data_dii, Sex == "Female")







# ---- Male数据可视化增强 ----
ggplot(data_dii_male, aes(x = Total_DII_Adjusted, y = LBDSUASImg)) +
  geom_jitter(  # 使用jitter代替point解决重叠
    width = 0.1,     # x轴抖动范围（DII的10%标准差）
    height = 0.15,   # y轴抖动范围（尿酸测量误差范围）
    color = "#2980B9",
    alpha = 0.4,     # 更低透明度
    size = 2,        # 更小的点尺寸
    shape = 16
  ) +
  geom_smooth(
    method = "gam",  # 改用广义加性模型捕捉非线性关系
    formula = y ~ s(x, bs = "tp"),
    color = "#C0392B",
    se = TRUE,
    linewidth = 1.5
  ) +
  geom_hline(
    yintercept = 7,
    color = "#34495E",
    linetype = "dotdash",
    linewidth = 1.2
  ) +
  labs(
    x = "Dietary Inflammatory Index",
    y = "Serum Uric Acid (mg/dL)\n[Reference: ≥7 mg/dL = Hyperuricemia]",
    title = expression(bold("男性：膳食炎症指数与血尿酸关系"))
  ) +
  scale_y_continuous(
    breaks = seq(2, 12, 0.5),  # y轴每0.5单位细分刻度
    minor_breaks = seq(2, 12, 0.25),  # 次要刻度
    limits = c(quantile(data_dii_male$LBDSUASImg,0.01),
               quantile(data_dii_male$LBDSUASImg,0.99)) # 动态范围
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.2),
    axis.ticks = element_line(color = "grey40"),
    plot.title = element_text(hjust = 0.5, margin = margin(b=15)),
    axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r=10))
  )


















# ---- Female数据可视化增强 ----
ggplot(data_dii_female, aes(x = Total_DII_Adjusted, y = LBDSUASImg)) +
  geom_jitter(
    width = 0.1,
    height = 0.15,
    color = "#D81B60",
    alpha = 0.4,
    size = 2,
    shape = 17       # 三角符号区分性别
  ) +
  geom_smooth(
    method = "gam",
    formula = y ~ s(x, bs = "tp"),
    color = "#00897B",
    se = TRUE,
    linewidth = 1.5
  ) +
  geom_hline(
    yintercept = 6,
    color = "#34495E",
    linetype = "dotdash",
    linewidth = 1.2
  ) +
  labs(
    x = "Dietary Inflammatory Index",
    y = "Serum Uric Acid (mg/dL)\n[Reference: ≥6 mg/dL = Hyperuricemia]",
    title = expression(bold("女性：膳食炎症指数与血尿酸关系"))
  ) +
  scale_y_continuous(
    breaks = seq(2, 10, 0.5),
    minor_breaks = seq(2, 10, 0.25),
    limits = c(quantile(data_dii_female$LBDSUASImg,0.01),
               quantile(data_dii_female$LBDSUASImg,0.99))
  ) +
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.major = element_line(color = "grey90", linewidth = 0.3),
    panel.grid.minor = element_line(color = "grey95", linewidth = 0.2),
    axis.ticks = element_line(color = "grey40"),
    plot.title = element_text(hjust = 0.5, margin = margin(b=15)),
    axis.title.y = element_text(angle = 0, vjust = 0.5, margin = margin(r=10))
  )























# 颜色体系（NEJM标准配色）
nejm_colors <- c(
  "Male" = "#BC3C29",     # 深红
  "Female" = "#0072B5",   # 深蓝
  "Total" = "#E18727",     # 橙色
  "Smooth" = "#6F99AD",     # 趋势线
  "RefLine" = "#20854E"    # 草绿
)

# 共享主题设置
nejm_theme <- theme(
  text = element_text(family = "sans", color = "#2F4F4F"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  axis.title = element_text(face = "bold", size = 12),
  axis.text = element_text(color = "black", size = 10),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
  panel.grid = element_blank(),
  legend.position = "top",
  legend.justification = c(1,0)
)

# y轴统一范围（基于所有数据）
y_limits <- c(
  floor(min(data_dii$LBDSUASImg, na.rm = TRUE)),
  ceiling(max(data_dii$LBDSUASImg, na.rm = TRUE))
)




# ----- 总数据集绘图 -----
total_plot <- ggplot(data_dii, aes(Total_DII_Adjusted, LBDSUASImg)) +
  geom_point(
    color = nejm_colors["Total"], 
    alpha = 0.6, 
    size = 0.1,
    position = position_jitter(width = 0.15, height = 0.1)
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x,
    color = nejm_colors["Smooth"],   # NEJM用辅助色
    linewidth = 0.8,
    se = FALSE
  ) +
  geom_hline(
    yintercept = c(6,7),
    color = c(nejm_colors["Female"], nejm_colors["Male"]),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    limits = c(-12,12),
    breaks = seq(-12,12,4)
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(y_limits[1], y_limits[2],1),
    expand = expansion(add = c(0.2,0.5))
  ) +
  labs(
    x = "Dietary Inflammatory Index (DII)",
    y = "Uric Acid (mg/dL)",
    title = "Dose-Response Relationship Between DII and Uric Acid"
  ) +
  nejm_theme +
  annotate(
    "text", 
    x = -10, y = 6.5, 
    # label = "Female cutoff: 6 mg/dL", 
    label = "",
    color = nejm_colors["Female"], 
    hjust = 0, size = 3.5
  ) +
  annotate(
    "text", 
    x = -10, y = 7.5, 
    # label = "Male cutoff: 7 mg/dL", 
    label = "",
    color = nejm_colors["Male"], 
    hjust = 0, size = 3.5
  )











# ----- 男性数据集绘图 -----
male_plot <- ggplot(data_dii_male, aes(Total_DII_Adjusted, LBDSUASImg)) +
  geom_jitter(
    color = nejm_colors["Male"], 
    alpha = 0.7, 
    size = 0.1,
    width = 0.15, 
    height = 0.1
  ) +
  geom_smooth(
    method = "lm", 
    color = nejm_colors["Smooth"],
    linewidth = 0.8,
    se = FALSE
  ) +
  geom_hline(
    yintercept = 7, 
    color = nejm_colors["RefLine"],
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    limits = c(-12,12),
    breaks = seq(-12,12,4)
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(y_limits[1], y_limits[2],1)
  ) +
  labs(
    x = "Dietary Inflammatory Index (DII)",
    y = "Uric Acid (mg/dL)",
    title = "Dose-Response Relationship Between DII and Uric Acid - Male"
  ) +
  nejm_theme +
  annotate(
    "text",
    x = -10, y = 7.3,
    # label = "Hyperuricemia cutoff: 7 mg/dL",
    label = "",
    color = nejm_colors["RefLine"],
    size = 3.5,
    hjust = 0
  )















# ----- 女性数据集绘图 -----
female_plot <- ggplot(data_dii_female, aes(Total_DII_Adjusted, LBDSUASImg)) +
  geom_jitter(
    color = nejm_colors["Female"], 
    alpha = 0.7, 
    size = 0.1,
    width = 0.15, 
    height = 0.1
  ) +
  geom_smooth(
    method = "lm", 
    color = nejm_colors["Smooth"],
    linewidth = 0.8,
    se = FALSE
  ) +
  geom_hline(
    yintercept = 6, 
    color = nejm_colors["RefLine"],
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    limits = c(-12,12),
    breaks = seq(-12,12,4)
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(y_limits[1], y_limits[2],1)
  ) +
  labs(
    x = "Dietary Inflammatory Index (DII)",
    y = "Uric Acid (mg/dL)",
    title = "Dose-Response Relationship Between DII and Uric Acid - Female"
  ) +
  nejm_theme +
  annotate(
    "text",
    x = -10, y = 6.3,
    # label = "Hyperuricemia cutoff: 6 mg/dL",
    label = "",
    color = nejm_colors["RefLine"],
    size = 3.5,
    hjust = 0
  )






# 标准化图像输出参数
ggsave_fix <- function(filename, plot, width=6.5, height=5.5){
  ggsave(filename, plot, 
         device = cairo_pdf, 
         units = "in", 
         dpi = 600,
         width = width,
         height = height)
}

ggsave_fix("D:/data_analyse/fig/2/99_20_dii_uric_Total.pdf", total_plot) 
ggsave_fix("D:/data_analyse/fig/2/99_20_dii_uric_Male.pdf", male_plot) 
ggsave_fix("D:/data_analyse/fig/2/99_20_dii_uric_Female.pdf", female_plot)









rbind(
  data_dii_male %>% 
    lm(LBDSUASImg ~ Total_DII_Adjusted, data = .) %>% 
    broom::tidy() %>% 
    mutate(Sex = "Male"),
  data_dii_female %>% 
    lm(LBDSUASImg ~ Total_DII_Adjusted, data = .) %>% 
    broom::tidy() %>% 
    mutate(Sex = "Female")
) %>% 
  filter(term == "Total_DII_Adjusted")




































# 从此开始为同数据集下的第二篇论文绘图，为gout绘图


# 使用散点图看清dii与高尿酸的关系
# 对数据集进行男女分类
data_dii_male <- subset(data_dii_07_18, Sex == "Male")
data_dii_female <- subset(data_dii_07_18, Sex == "Female")

# 颜色体系（NEJM标准配色）
nejm_colors <- c(
  "Male" = "#BC3C29",     # 深红
  "Female" = "#0072B5",   # 深蓝
  "Total" = "#E18727",     # 橙色
  "Smooth" = "#6F99AD",     # 趋势线
  "RefLine" = "#20854E"    # 草绿
)

# 共享主题设置
nejm_theme <- theme(
  text = element_text(family = "sans", color = "#2F4F4F"),
  plot.title = element_text(hjust = 0.5, face = "bold", size = 14),
  axis.title = element_text(face = "bold", size = 12),
  axis.text = element_text(color = "black", size = 10),
  panel.background = element_rect(fill = "white"),
  panel.border = element_rect(color = "black", fill = NA, linewidth = 0.8),
  panel.grid = element_blank(),
  legend.position = "top",
  legend.justification = c(1,0)
)

# y轴统一范围（基于所有数据）
y_limits <- c(
  floor(min(data_dii_07_18$LBDSUASImg, na.rm = TRUE)),
  ceiling(max(data_dii_07_18$LBDSUASImg, na.rm = TRUE))
)




# ----- 总数据集绘图 -----
total_plot <- ggplot(data_dii_07_18, aes(Total_DII_Adjusted, LBDSUASImg)) +
  geom_point(
    color = nejm_colors["Total"], 
    alpha = 0.6, 
    size = 0.1,
    position = position_jitter(width = 0.15, height = 0.1)
  ) +
  geom_smooth(
    method = "lm", 
    formula = y ~ x,
    color = nejm_colors["Smooth"],   # NEJM用辅助色
    linewidth = 0.8,
    se = FALSE
  ) +
  geom_hline(
    yintercept = c(6,7),
    color = c(nejm_colors["Female"], nejm_colors["Male"]),
    linetype = "dashed",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    breaks = seq(-12,12,4)
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(y_limits[1], y_limits[2],1),
    expand = expansion(add = c(0.2,0.5))
  ) +
  labs(
    x = "Dietary Inflammatory Index (DII)",
    y = "Uric Acid (mg/dL)",
    title = "Dose-Response Relationship Between DII and Uric Acid"
  ) +
  nejm_theme +
  annotate(
    "text", 
    x = -10, y = 6.5, 
    # label = "Female cutoff: 6 mg/dL", 
    label = "",
    color = nejm_colors["Female"], 
    hjust = 0, size = 3.5
  ) +
  annotate(
    "text", 
    x = -10, y = 7.5, 
    # label = "Male cutoff: 7 mg/dL", 
    label = "",
    color = nejm_colors["Male"], 
    hjust = 0, size = 3.5
  )











# ----- 男性数据集绘图 -----
male_plot <- ggplot(data_dii_male, aes(Total_DII_Adjusted, LBDSUASImg)) +
  geom_jitter(
    color = nejm_colors["Male"], 
    alpha = 0.7, 
    size = 0.1,
    width = 0.15, 
    height = 0.1
  ) +
  geom_smooth(
    method = "lm", 
    color = nejm_colors["Smooth"],
    linewidth = 0.8,
    se = FALSE
  ) +
  geom_hline(
    yintercept = 7, 
    color = nejm_colors["RefLine"],
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    breaks = seq(-12,12,4)
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(y_limits[1], y_limits[2],1)
  ) +
  labs(
    x = "Dietary Inflammatory Index (DII)",
    y = "Uric Acid (mg/dL)",
    title = "Dose-Response Relationship Between DII and Uric Acid - Male"
  ) +
  nejm_theme +
  annotate(
    "text",
    x = -10, y = 7.3,
    # label = "Hyperuricemia cutoff: 7 mg/dL",
    label = "",
    color = nejm_colors["RefLine"],
    size = 3.5,
    hjust = 0
  )















# ----- 女性数据集绘图 -----
female_plot <- ggplot(data_dii_female, aes(Total_DII_Adjusted, LBDSUASImg)) +
  geom_jitter(
    color = nejm_colors["Female"], 
    alpha = 0.7, 
    size = 0.1,
    width = 0.15, 
    height = 0.1
  ) +
  geom_smooth(
    method = "lm", 
    color = nejm_colors["Smooth"],
    linewidth = 0.8,
    se = FALSE
  ) +
  geom_hline(
    yintercept = 6, 
    color = nejm_colors["RefLine"],
    linetype = "dotdash",
    linewidth = 0.8
  ) +
  scale_x_continuous(
    breaks = seq(-12,12,4)
  ) +
  scale_y_continuous(
    limits = y_limits,
    breaks = seq(y_limits[1], y_limits[2],1)
  ) +
  labs(
    x = "Dietary Inflammatory Index (DII)",
    y = "Uric Acid (mg/dL)",
    title = "Dose-Response Relationship Between DII and Uric Acid - Female"
  ) +
  nejm_theme +
  annotate(
    "text",
    x = -10, y = 6.3,
    # label = "Hyperuricemia cutoff: 6 mg/dL",
    label = "",
    color = nejm_colors["RefLine"],
    size = 3.5,
    hjust = 0
  )






# 标准化图像输出参数
ggsave_fix <- function(filename, plot, width=6.5, height=5.5){
  ggsave(filename, plot, 
         device = cairo_pdf, 
         units = "in", 
         dpi = 600,
         width = width,
         height = height)
}

ggsave_fix("D:/data_analyse/fig/2/07_16_dii_gout_Total.pdf", total_plot) 
ggsave_fix("D:/data_analyse/fig/2/07_16_dii_gout_Male.pdf", male_plot) 
ggsave_fix("D:/data_analyse/fig/2/07_16_dii_gout_Female.pdf", female_plot)









rbind(
  data_dii_male %>% 
    lm(LBDSUASImg ~ Total_DII_Adjusted, data = .) %>% 
    broom::tidy() %>% 
    mutate(Sex = "Male"),
  data_dii_female %>% 
    lm(LBDSUASImg ~ Total_DII_Adjusted, data = .) %>% 
    broom::tidy() %>% 
    mutate(Sex = "Female")
) %>% 
  filter(term == "Total_DII_Adjusted")










































# 以下为同数据集的gout绘图

# 标记数据集
dd_data_dii_07_18 <- datadist(data_dii_07_18)
options(datadist = "dd_data_dii_07_18")


# 柱状图+RCS ，采用高尿酸第二版进行改动
# 高尿酸第二版
data_dii_07_18_filtered <- data_dii_07_18 %>%
  filter(Total_DII_Adjusted >= -12)

# 对过滤后的数据进行分组
data_dii_07_18_filtered <- data_dii_07_18_filtered %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data <- data_dii_07_18_filtered %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),
    Prevalence = mean(gout_count, na.rm = TRUE),
    .groups = "drop"
  )

# 确保 DII_Group 是因子
bar_data <- bar_data %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组
bar_data_numeric <- bar_data %>%
  mutate(
    DII_Group_Chr = as.character(DII_Group),
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 在过滤后的数据上拟合 RCS
fit <- lrm(gout_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii_07_18_filtered)

min_dii <- min(data_dii_07_18_filtered$Total_DII_Adjusted)
max_dii <- max(data_dii_07_18_filtered$Total_DII_Adjusted)
pred <- Predict(fit, Total_DII_Adjusted = seq(min_dii, max_dii, length.out = 100), fun = plogis)
pred_df <- as.data.frame(pred)

# 提取 p-value
anova_res <- anova(fit)
p_overall <- format(anova_res["TOTAL", "P"], digits = 3)
p_nonlinear <- format(anova_res[" Nonlinear", "P"], digits = 3)

# 绘图
ggplot() +
  # (a) 柱状图
  geom_bar(
    data = bar_data_numeric,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric$midpoint) + 3,
    y = max(bar_data_numeric$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear),
    size = 4
  ) +
  # 主题与坐标
  labs(
    x = "DII Score",
    y = "Gout Prevalence"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  coord_cartesian(xlim = c(min_dii, max_dii))

# 检查最大值
max(data_dii_07_18$Total_DII_Adjusted)

# 检查接近10的值分布
table(cut(data_dii_07_18$Total_DII_Adjusted,
          breaks = seq(9, 10.1, by = 0.1),
          include.lowest = TRUE))


































#  痛风只计算男性
# 筛选男性
data_dii_07_18_male <- data_dii_07_18 %>%
  filter(Sex == "Male" & Total_DII_Adjusted >= -12)

# 对筛选后的男性数据进行分组
data_dii_07_18_male <- data_dii_07_18_male %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data_male <- data_dii_07_18_male %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),
    Prevalence = mean(gout_count, na.rm = TRUE),
    .groups = "drop"
  )

# 确保 DII_Group 是因子
bar_data_male <- bar_data_male %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组数据处理
bar_data_numeric_male <- bar_data_male %>%
  mutate(
    DII_Group_Chr = as.character(DII_Group),
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 在筛选后的男性数据上拟合 RCS
fit_male <- lrm(gout_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii_07_18_male)

# 指定预测的 DII 范围
min_dii_male <- min(data_dii_07_18_male$Total_DII_Adjusted)
max_dii_male <- max(data_dii_07_18_male$Total_DII_Adjusted)
pred_male <- Predict(fit_male, Total_DII_Adjusted = seq(min_dii_male, max_dii_male, length.out = 100), fun = plogis)
pred_df_male <- as.data.frame(pred_male)

# 提取 p-value
anova_res_male <- anova(fit_male)
p_overall_male <- format(anova_res_male["TOTAL", "P"], digits = 3)
p_nonlinear_male <- format(anova_res_male[" Nonlinear", "P"], digits = 3)

# 男性绘图
plot_male <- ggplot() +
  # (a) 柱状图
  geom_bar(
    data = bar_data_numeric_male,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "steelblue",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df_male,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df_male,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric_male$midpoint) + 3,
    y = max(bar_data_numeric_male$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall_male),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric_male$midpoint) + 3,
    y = max(bar_data_numeric_male$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear_male),
    size = 4
  ) +
  # 主题与坐标
  labs(
    title = "",
    x = "DII Score",
    y = "Gout Prevalence"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  # 限制 x 轴的范围
  coord_cartesian(xlim = c(min_dii_male, max_dii_male))

# 显示男性图表
print(plot_male)

# 检查最大值
max(data_dii_07_18_male$Total_DII_Adjusted)

# 检查接近10的值分布
table(cut(data_dii_07_18_male$Total_DII_Adjusted,
          breaks = seq(9, 10.1, by = 0.1),
          include.lowest = TRUE))


# 设置图表尺寸
ggsave("D:/data_analyse/fig/2/gout/07_16_dii_gout_male.pdf", plot_male, width = 10, height = 6, dpi = 600)





























# 痛风，只计算女性
# 筛选女性
data_dii_07_18_female <- data_dii_07_18 %>%
  filter(Sex == "Female" & Total_DII_Adjusted >= -12)

# 对筛选后的女性数据进行分组
data_dii_07_18_female <- data_dii_07_18_female %>%
  mutate(
    DII_Group = cut(
      Total_DII_Adjusted, 
      breaks = seq(floor(min(Total_DII_Adjusted)), ceiling(max(Total_DII_Adjusted)), by = 1),
      include.lowest = TRUE,
      right = FALSE
    )
  )

# 计算每组的患病率
bar_data_female <- data_dii_07_18_female %>%
  group_by(DII_Group) %>%
  summarise(
    Mean_DII = mean(Total_DII_Adjusted, na.rm = TRUE),
    Prevalence = mean(gout_count, na.rm = TRUE),
    .groups = "drop"
  )

# 确保 DII_Group 是因子
bar_data_female <- bar_data_female %>%
  mutate(DII_Group = factor(DII_Group, levels = unique(DII_Group)))

# 分组数据处理
bar_data_numeric_female <- bar_data_female %>%
  mutate(
    DII_Group_Chr = as.character(DII_Group),
    interval_clean = str_replace_all(DII_Group_Chr, "\\[|\\]|\\(|\\)", "")
  ) %>%
  separate(interval_clean, into = c("x_low_chr", "x_high_chr"), sep = ",") %>%
  mutate(
    x_low  = as.numeric(str_trim(x_low_chr)),
    x_high = as.numeric(str_trim(x_high_chr))
  ) %>%
  mutate(
    midpoint = (x_low + x_high)/2,
    width    = x_high - x_low
  )

# 在筛选后的女性数据上拟合 RCS
fit_female <- lrm(gout_count ~ rcs(Total_DII_Adjusted, 4), data = data_dii_07_18_female)

# 指定预测的 DII 范围
min_dii_female <- min(data_dii_07_18_female$Total_DII_Adjusted)
max_dii_female <- max(data_dii_07_18_female$Total_DII_Adjusted)
pred_female <- Predict(fit_female, Total_DII_Adjusted = seq(min_dii_female, max_dii_female, length.out = 100), fun = plogis)
pred_df_female <- as.data.frame(pred_female)

# 提取 p-value
anova_res_female <- anova(fit_female)
p_overall_female <- format(anova_res_female["TOTAL", "P"], digits = 3)
p_nonlinear_female <- format(anova_res_female[" Nonlinear", "P"], digits = 3)

# 女性绘图
plot_female <- ggplot() +
  # (a) 柱状图
  geom_bar(
    data = bar_data_numeric_female,
    aes(x = midpoint, y = Prevalence),
    stat = "identity",
    fill = "lightpink",
    alpha = 0.8,
    width = 0.7
  ) +
  # (b) RCS 拟合曲线
  geom_line(
    data = pred_df_female,
    aes(x = Total_DII_Adjusted, y = yhat),
    color = "red",
    size = 1
  ) +
  # (c) 置信区间带
  geom_ribbon(
    data = pred_df_female,
    aes(x = Total_DII_Adjusted, ymin = lower, ymax = upper),
    fill = "#b0171f",
    alpha = 0.3
  ) +
  # (d) 文本标注 P 值
  annotate(
    "text",
    x = min(bar_data_numeric_female$midpoint) + 3,
    y = max(bar_data_numeric_female$Prevalence) * 1.1,
    label = paste("P-overall =", p_overall_female),
    size = 4
  ) +
  annotate(
    "text",
    x = min(bar_data_numeric_female$midpoint) + 3,
    y = max(bar_data_numeric_female$Prevalence) * 1.05,
    label = paste("P-nonlinear =", p_nonlinear_female),
    size = 4
  ) +
  # 主题与坐标
  labs(
    title = "",
    x = "DII Score",
    y = "Gout Prevalence"
  ) +
  theme_minimal() +
  theme(panel.grid = element_blank()) +
  # 在 x 轴两端留出一点空隙
  scale_x_continuous(expand = expansion(mult = 0.02)) +
  # 限制 x 轴的范围
  coord_cartesian(xlim = c(min_dii_female, max_dii_female))

# 显示女性图表
print(plot_female)


# 检查最大值
max(data_dii_07_18_female$Total_DII_Adjusted)

# 检查接近10的值分布
table(cut(data_dii_07_18_female$Total_DII_Adjusted,
          breaks = seq(9, 10.1, by = 0.1),
          include.lowest = TRUE))


