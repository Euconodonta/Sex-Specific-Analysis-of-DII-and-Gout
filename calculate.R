
# 计算DII所需列
# 读取DII参考表格
dii_reference <- read.csv("D:/data_analyse/data/20241115/DII_reference.csv")

# 创建计算DII的函数
calculate_dii <- function(data_row, dii_ref) {
  dii_scores <- list()  # 创建一个列表存储每个食物参数的DII得分
  total_dii <- 0        # 初始化总DII得分
  
  for (i in 1:nrow(dii_ref)) {
    col_name <- dii_ref$name[i]
    col_parameter <- dii_ref$food_parameter[i]
    prefixed_col_name <- paste0("DII_", col_parameter)  # 给列名加上前缀
    effect_score <- dii_ref$effect_score[i]
    mean_intake <- dii_ref$mean_intake[i]
    sd <- dii_ref$sd[i]
    
    # 检查数据是否有对应的列
    if (col_name %in% names(data_row)) {
      intake_value <- data_row[[col_name]]
      
      # 如果值不是缺失，计算标准化值和贡献的DII得分
      if (!is.na(intake_value)) {
        z_score <- (intake_value - mean_intake) / sd
        contribution <- z_score * effect_score
        dii_scores[[prefixed_col_name]] <- contribution
        total_dii <- total_dii + contribution
      } else {
        dii_scores[[prefixed_col_name]] <- NA  # 缺失值处理
      }
    } else {
      dii_scores[[prefixed_col_name]] <- NA  # 列不存在时处理
    }
  }
  
  dii_scores[["Total_DII"]] <- total_dii  # 添加总DII得分
  return(dii_scores)
}

# 逐行计算DII
data_dii <- datav4
data_dii <- data_dii %>%
  rowwise() %>%
  mutate(across(everything(), list())) %>%  # 确保所有列能够参与计算
  mutate(DII_results = list(calculate_dii(pick(everything()), dii_reference))) %>%
  unnest_wider(DII_results) %>%  # 展开计算结果为多列
  ungroup()  # 移除rowwise标记

# 对每一项进行规整
data_dii <- data_dii %>%
  mutate(across(starts_with("DII_"), 
                ~ {
                  col_mean <- mean(., na.rm = TRUE)
                  col_sd <- sd(., na.rm = TRUE)
                  lower_limit <- col_mean - 3 * col_sd
                  upper_limit <- col_mean + 3 * col_sd
                  pmin(pmax(., lower_limit), upper_limit)
                }, 
                .names = "{.col}_Adjusted"),
         across(starts_with("DII_"), 
                ~ {
                  col_mean <- mean(., na.rm = TRUE)
                  col_sd <- sd(., na.rm = TRUE)
                  lower_limit <- col_mean - 3 * col_sd
                  upper_limit <- col_mean + 3 * col_sd
                  ifelse(. < lower_limit | . > upper_limit, TRUE, FALSE)
                },
                .names = "{.col}_Is_Truncated"))

# 基于每一小项重新计算Total_DII
data_dii <- data_dii %>%
  rowwise() %>%
  mutate(Total_DII = sum(c_across(ends_with("_Adjusted")), na.rm = TRUE)) %>%
  ungroup()

# 对Total_DII进行规整
mean_total <- mean(data_dii$Total_DII, na.rm = TRUE)
sd_total <- sd(data_dii$Total_DII, na.rm = TRUE)
lower_limit <- mean_total - 3 * sd_total
upper_limit <- mean_total + 3 * sd_total
data_dii <- data_dii %>%
  mutate(
    Total_DII_Adjusted = pmin(pmax(Total_DII, lower_limit), upper_limit),  # 动态截尾
    Total_DII_Is_Truncated = ifelse(Total_DII < lower_limit | Total_DII > upper_limit, TRUE, FALSE)  # 标记被截尾数据
  )

# 保存结果
write.csv(data_dii, "D:/data_analyse/data/20241115/data_dii.csv", row.names = FALSE, na = "")
colSums(is.na(data_dii))


# 对gout数据再进行一次计算
# 逐行计算DII
data_dii_07_18 <- datav4_gout
data_dii_07_18 <- data_dii_07_18 %>%
  rowwise() %>%
  mutate(across(everything(), list())) %>%  # 确保所有列能够参与计算
  mutate(DII_results = list(calculate_dii(pick(everything()), dii_reference))) %>%
  unnest_wider(DII_results) %>%  # 展开计算结果为多列
  ungroup()  # 移除rowwise标记

# 对每一项进行规整
data_dii_07_18 <- data_dii_07_18 %>%
  mutate(across(starts_with("DII_"), 
                ~ {
                  col_mean <- mean(., na.rm = TRUE)
                  col_sd <- sd(., na.rm = TRUE)
                  lower_limit <- col_mean - 3 * col_sd
                  upper_limit <- col_mean + 3 * col_sd
                  pmin(pmax(., lower_limit), upper_limit)
                }, 
                .names = "{.col}_Adjusted"),
         across(starts_with("DII_"), 
                ~ {
                  col_mean <- mean(., na.rm = TRUE)
                  col_sd <- sd(., na.rm = TRUE)
                  lower_limit <- col_mean - 3 * col_sd
                  upper_limit <- col_mean + 3 * col_sd
                  ifelse(. < lower_limit | . > upper_limit, TRUE, FALSE)
                },
                .names = "{.col}_Is_Truncated"))

# 基于每一小项重新计算Total_DII
data_dii_07_18 <- data_dii_07_18 %>%
  rowwise() %>%
  mutate(Total_DII = sum(c_across(ends_with("_Adjusted")), na.rm = TRUE)) %>%
  ungroup()

# 对Total_DII进行规整
mean_total <- mean(data_dii_07_18$Total_DII, na.rm = TRUE)
sd_total <- sd(data_dii_07_18$Total_DII, na.rm = TRUE)
lower_limit <- mean_total - 3 * sd_total
upper_limit <- mean_total + 3 * sd_total
data_dii_07_18 <- data_dii_07_18 %>%
  mutate(
    Total_DII_Adjusted = pmin(pmax(Total_DII, lower_limit), upper_limit),  # 动态截尾
    Total_DII_Is_Truncated = ifelse(Total_DII < lower_limit | Total_DII > upper_limit, TRUE, FALSE)  # 标记被截尾数据
  )

# 保存结果
write.csv(data_dii_07_18, "D:/data_analyse/data/20241115/data_dii_07_18.csv", row.names = FALSE, na = "")
colSums(is.na(data_dii_07_18))















# 计算Table 1 均值和置信区间
# 数据筛选
data_table1 <- data_dii
data_table1_07_18 <- data_dii_07_18
DII_hyperuricemia_data <- data_table1[data_table1$Hyperuricemia == "Yes", ]
DII_non_hyperuricemia_data <- data_table1[data_table1$Hyperuricemia == "No", ]
DII_gout_data_07_18 <- data_dii_07_18[data_dii_07_18$Gout == "Yes", ]
DII_non_gout_data_07_18 <- data_dii_07_18[data_dii_07_18$Gout == "No", ]
DII_hyperuricemia_data_07_18 <- data_dii_07_18[data_dii_07_18$Hyperuricemia == "Yes", ]
DII_non_hyperuricemia_data_07_18 <- data_dii_07_18[data_dii_07_18$Hyperuricemia == "No", ]

# 定义计算均值和置信区间的函数
calculate_dii_stats <- function(data_table1) {
  
  # 定义计算均值和置信区间的辅助函数
  calculate_mean_ci <- function(data, value_col) {
    mean_value <- mean(data[[value_col]], na.rm = TRUE)  # 计算均值
    sd_value <- sd(data[[value_col]], na.rm = TRUE)  # 计算标准差
    n_value <- sum(!is.na(data[[value_col]]))  # 计算样本量
    
    # 计算 95% 置信区间
    lower_ci <- mean_value - 1.96 * (sd_value / sqrt(n_value))
    upper_ci <- mean_value + 1.96 * (sd_value / sqrt(n_value))
    
    # 返回均值和置信区间
    return(c(Mean = mean_value, Lower_CI = lower_ci, Upper_CI = upper_ci))
  }
  
  # 筛选所有小项 DII 和总 DII 列
  dii_cols <- colnames(data_table1)[grepl("^DII_.*_Adjusted$", colnames(data_table1))]  # 匹配前缀是 "DII_" 且以 "_Adjusted" 结尾的 
  # 将总 DII 列添加到列表中
  dii_cols <- c(dii_cols, "Total_DII_Adjusted")  # 假设总 DII 列名是 "Total_DII_Adjusted"
  
  # 对每个 DII 列计算均值和 95% 置信区间
  dii_stats <- sapply(dii_cols, function(col) {
    calculate_mean_ci(data_table1, col)
  })
  
  # 转置结果，以便按行显示
  dii_stats <- t(dii_stats)
  
  # 创建一个更易读的表格，并添加 "Result" 列
  dii_stats_df <- as.data.frame(dii_stats)
  dii_stats_df$DII_Name <- dii_cols
  
  # 计算 完整 "Result" 列：均值 (置信区间)
  dii_stats_df$FullResult <- paste0(dii_stats_df$Mean, " (", dii_stats_df$Lower_CI, ", ",dii_stats_df$Upper_CI, ")")
  # 计算 简化 "Result" 列：均值 (置信区间)
  dii_stats_df$result <- paste0(round(dii_stats_df$Mean, 3), " (", round(dii_stats_df$Lower_CI, 3), ", ", round(dii_stats_df$Upper_CI, 3), ")")
  
  # 重新排列列的顺序，使其更清晰
  dii_stats_df <- dii_stats_df[, c("DII_Name", "Mean", "Lower_CI", "Upper_CI", "FullResult", "result")]
  
  # 输出
  write.csv(dii_stats_df, file.path("D:/data_analyse/csv/2/table1/", paste0(deparse(substitute(data_table1)), ".csv")), row.names = FALSE)
  
  # 返回结果
  return(dii_stats_df)
}

# 调用函数
calculate_dii_stats(data_table1)
calculate_dii_stats(data_table1_07_18)
calculate_dii_stats(DII_hyperuricemia_data)
calculate_dii_stats(DII_non_hyperuricemia_data)
calculate_dii_stats(DII_gout_data_07_18)
calculate_dii_stats(DII_non_gout_data_07_18)
calculate_dii_stats(DII_hyperuricemia_data_07_18)
calculate_dii_stats(DII_non_hyperuricemia_data_07_18)

# 计算table1 p值
# 定义函数进行 t 检验
perform_t_tests <- function(data, group_column) {
  # 筛选所有以 DII 开头并以 Adjusted 结尾的列名以及 Total_DII_Adjusted
  dii_columns <- grep("^DII.*Adjusted$", names(data), value = TRUE)
  dii_columns <- c(dii_columns, "Total_DII_Adjusted")
  
  # 初始化一个数据框来存储列名和 p 值
  result_df <- data.frame(
    Column = character(0),  # 存储列名
    P_value = numeric(0)     # 存储 p 值
  )
  
  # 对每一列进行 t 检验并提取列名和 p 值
  for (col in dii_columns) {
    # 执行 t 检验，依据参数选择 group_column
    t_test_result <- t.test(data[[col]] ~ data[[group_column]])
    
    # 获取 p 值并保留三位小数
    # p_value <- round(t_test_result$p.value, 3)
    # 获取完整 p 值
    p_value <- t_test_result$p.value
    
    # 将列名和 p 值添加到结果数据框
    result_df <- rbind(result_df, data.frame(Column = col, P_value = p_value))
  }
  
  # 返回最终的结果数据框
  return(result_df)
}

# 调用函数进行 t 检验（基于 gout_count）
data_table1_07_18_gout_p <- perform_t_tests(data_table1_07_18, "gout_count")
data_table1_07_18_hyperuricemia_p <- perform_t_tests(data_table1_07_18, "hyperuricemia_count")
data_table1_hyperuricemia_p <- perform_t_tests(data_table1, "hyperuricemia_count")

# 打印结果
print(data_table1_07_18_gout_p)
print(data_table1_07_18_hyperuricemia_p)
print(data_table1_hyperuricemia_p)

# 导出
write.csv(data_table1_07_18_gout_p, "D:/data_analyse/csv/2/table1/data_table1_07_18_gout_p.csv", row.names = FALSE, na = "")
write.csv(data_table1_07_18_hyperuricemia_p, "D:/data_analyse/csv/2/table1/data_table1_07_18_hyperuricemia_p.csv", row.names = FALSE, na = "")
write.csv(data_table1_hyperuricemia_p, "D:/data_analyse/csv/2/table1/data_table1_hyperuricemia_p.csv", row.names = FALSE, na = "")























































# 计算table2、3    此时原始数据为 data_dii 以及 data_dii_07_18

# 07-18的table2
data_table2_3 <- data_dii_07_18
# 按尿酸值从小到大排序
sorted_data <- data_table2_3[order(data_table2_3$Total_DII_Adjusted), ]
# 计算总行数和每组大小
n <- nrow(sorted_data)
split_size <- ceiling(n / 4)  # 向上取整，保证4组总行数 = n
# 构造一个与排序后数据相同长度的“分组标签”向量
quartile_vec <- c(
  rep("Q1", split_size),             # 前 split_size 行标记为 Q1
  rep("Q2", split_size),             # 接下来的 split_size 行标记为 Q2
  rep("Q3", split_size),             # 第三段标记为 Q3
  rep("Q4", n - 3 * split_size)      # 剩余的行都标记为 Q4
)
# 将“分组标签”写入 sorted_data 的新列 UA_quartile
sorted_data$UA_quartile <- quartile_vec
# 分别取出四个子数据集
data_table2_3_part1 <- subset(sorted_data, UA_quartile == "Q1")
data_table2_3_part2 <- subset(sorted_data, UA_quartile == "Q2")
data_table2_3_part3 <- subset(sorted_data, UA_quartile == "Q3")
data_table2_3_part4 <- subset(sorted_data, UA_quartile == "Q4")
# 转换为因子
sorted_data$UA_quartile_factor <- factor(
  sorted_data$UA_quartile, 
  levels = c("Q1","Q2","Q3","Q4")
)
# 再将因子转为数值
sorted_data$UA_quartile_num <- as.numeric(sorted_data$UA_quartile_factor)


# 计算分类变量的人数占比
# 定义一个函数，用于计算人数和占比
calculate_proportion <- function(df, variable) {
  df %>%
    group_by(!!sym(variable)) %>%      # 按分类变量分组
    summarise(n = n(),                 # 计算人数
              percent = round(n / nrow(df) * 100, 1)) %>%  # 计算占比，保留一位小数
    mutate(category = as.character(!!sym(variable)),
           percent = format(percent, nsmall = 1)) %>%     # 强制保留一位小数
    select(category, n, percent)       # 输出分类标签、人数、占比
}
# 定义分类变量列表（排除数值型变量）
categorical_vars <- c("Sex", "AgeGroup", "Race", "Education", "Marital", 
                      "Income", "BMI", "Obesity", "DSD", "Alchohol","Hyperuricemia", "Gout", "Diabetes", 
                      "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")


# 07-18 Q1
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part1, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t2_Q1.csv", row.names = FALSE)


# 07-18 Q2
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part2, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t2_Q2.csv", row.names = FALSE)


# 07-18 Q3
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part3, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t2_Q3.csv", row.names = FALSE)


# 07-18 Q4
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part4, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序 
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果 
print(final_results)
# 保存为单个CSV文件 
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t2_Q4.csv", row.names = FALSE)

# 计算 BMI 四分位数
bmi_part1_quartiles <- quantile(data_table2_3_part1$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part2_quartiles <- quantile(data_table2_3_part2$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part3_quartiles <- quantile(data_table2_3_part3$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part4_quartiles <- quantile(data_table2_3_part4$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value_part1 <- mean(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
sd_value_part1 <- sd(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
mean_value_part2 <- mean(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
sd_value_part2 <- sd(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
mean_value_part3 <- mean(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
sd_value_part3 <- sd(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
mean_value_part4 <- mean(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
sd_value_part4 <- sd(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
# 计算高尿酸四个部分四分位数
hyperuricemia_part1_quartiles <- quantile(data_table2_3_part1$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part2_quartiles <- quantile(data_table2_3_part2$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part3_quartiles <- quantile(data_table2_3_part3$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part4_quartiles <- quantile(data_table2_3_part4$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 合并结果为一个数据框
quartiles_df <- data.frame(
  Metric = c("BMI1", "BMI2", "BMI3", "BMI4", "CaloriesMean", "CaloriesSD", "Hyperuricemia1", "Hyperuricemia2", "Hyperuricemia3", "Hyperuricemia4"),
  Q1 = c(bmi_part1_quartiles["Q1"], bmi_part2_quartiles["Q1"], bmi_part3_quartiles["Q1"], bmi_part4_quartiles["Q1"], mean_value_part1, sd_value_part1, hyperuricemia_part1_quartiles["Q1"], hyperuricemia_part2_quartiles["Q1"], hyperuricemia_part3_quartiles["Q1"], hyperuricemia_part4_quartiles["Q1"]),
  Median = c(bmi_part1_quartiles["Median"], bmi_part2_quartiles["Median"], bmi_part3_quartiles["Median"], bmi_part4_quartiles["Median"],mean_value_part2, sd_value_part2, hyperuricemia_part1_quartiles["Median"], hyperuricemia_part2_quartiles["Median"], hyperuricemia_part3_quartiles["Median"], hyperuricemia_part4_quartiles["Median"]),
  Q3 = c(bmi_part1_quartiles["Q3"], bmi_part2_quartiles["Q3"], bmi_part3_quartiles["Q3"], bmi_part4_quartiles["Q3"], mean_value_part3, sd_value_part3, hyperuricemia_part1_quartiles["Q3"], hyperuricemia_part2_quartiles["Q3"], hyperuricemia_part3_quartiles["Q3"], hyperuricemia_part4_quartiles["Q3"]),
  Q4 = c(bmi_part1_quartiles["Q4"], bmi_part2_quartiles["Q4"], bmi_part3_quartiles["Q4"], bmi_part4_quartiles["Q4"], mean_value_part4, sd_value_part4, hyperuricemia_part1_quartiles["Q4"], hyperuricemia_part2_quartiles["Q4"], hyperuricemia_part3_quartiles["Q4"], hyperuricemia_part4_quartiles["Q4"])
)
# 将结果保存为 CSV 文件
write.csv(quartiles_df, "D:/data_analyse/csv/2/table2/07_18_t2_quartiles_summary.csv", row.names = FALSE)
# 显示数据框
print(quartiles_df)



































# 99-20的table2
data_table2_3 <- data_dii
# 按DII从小到大排序
sorted_data <- data_table2_3[order(data_table2_3$Total_DII_Adjusted), ]
# 计算总行数和每组大小
n <- nrow(sorted_data)
split_size <- ceiling(n / 4)  # 向上取整，保证4组总行数 = n
# 构造一个与排序后数据相同长度的“分组标签”向量
quartile_vec <- c(
  rep("Q1", split_size),             # 前 split_size 行标记为 Q1
  rep("Q2", split_size),             # 接下来的 split_size 行标记为 Q2
  rep("Q3", split_size),             # 第三段标记为 Q3
  rep("Q4", n - 3 * split_size)      # 剩余的行都标记为 Q4
)
# 将“分组标签”写入 sorted_data 的新列 UA_quartile
sorted_data$UA_quartile <- quartile_vec
# 分别取出四个子数据集
data_table2_3_part1 <- subset(sorted_data, UA_quartile == "Q1")
data_table2_3_part2 <- subset(sorted_data, UA_quartile == "Q2")
data_table2_3_part3 <- subset(sorted_data, UA_quartile == "Q3")
data_table2_3_part4 <- subset(sorted_data, UA_quartile == "Q4")
# 转换为因子
sorted_data$UA_quartile_factor <- factor(
  sorted_data$UA_quartile, 
  levels = c("Q1","Q2","Q3","Q4")
)
# 再将因子转为数值
sorted_data$UA_quartile_num <- as.numeric(sorted_data$UA_quartile_factor)


# 计算分类变量的人数占比
# 定义一个函数，用于计算人数和占比
calculate_proportion <- function(df, variable) {
  df %>%
    group_by(!!sym(variable)) %>%      # 按分类变量分组
    summarise(n = n(),                 # 计算人数
              percent = round(n / nrow(df) * 100, 1)) %>%  # 计算占比，保留一位小数
    mutate(category = as.character(!!sym(variable)),
           percent = format(percent, nsmall = 1)) %>%     # 强制保留一位小数
    select(category, n, percent)       # 输出分类标签、人数、占比
}
# 定义分类变量列表（排除数值型变量）
categorical_vars <- c("Sex", "AgeGroup", "Race", "Education", "Marital", 
                      "Income", "BMI", "Obesity", "DSD", "Alchohol","Hyperuricemia", "Diabetes", 
                      "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")


# 99-20 Q1
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part1, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t2_Q1.csv", row.names = FALSE)


# 99-20 Q2
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part2, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t2_Q2.csv", row.names = FALSE)


# 99-20 Q3
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part3, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t2_Q3.csv", row.names = FALSE)


# 99-20 Q4
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part4, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序 
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果 
print(final_results)
# 保存为单个CSV文件 
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t2_Q4.csv", row.names = FALSE)

# 计算 BMI 四分位数
bmi_part1_quartiles <- quantile(data_table2_3_part1$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part2_quartiles <- quantile(data_table2_3_part2$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part3_quartiles <- quantile(data_table2_3_part3$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part4_quartiles <- quantile(data_table2_3_part4$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value_part1 <- mean(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
sd_value_part1 <- sd(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
mean_value_part2 <- mean(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
sd_value_part2 <- sd(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
mean_value_part3 <- mean(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
sd_value_part3 <- sd(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
mean_value_part4 <- mean(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
sd_value_part4 <- sd(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
# 计算高尿酸四个部分四分位数
hyperuricemia_part1_quartiles <- quantile(data_table2_3_part1$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part2_quartiles <- quantile(data_table2_3_part2$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part3_quartiles <- quantile(data_table2_3_part3$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part4_quartiles <- quantile(data_table2_3_part4$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 合并结果为一个数据框
quartiles_df <- data.frame(
  Metric = c("BMI1", "BMI2", "BMI3", "BMI4", "CaloriesMean", "CaloriesSD", "Hyperuricemia1", "Hyperuricemia2", "Hyperuricemia3", "Hyperuricemia4"),
  Q1 = c(bmi_part1_quartiles["Q1"], bmi_part2_quartiles["Q1"], bmi_part3_quartiles["Q1"], bmi_part4_quartiles["Q1"], mean_value_part1, sd_value_part1, hyperuricemia_part1_quartiles["Q1"], hyperuricemia_part2_quartiles["Q1"], hyperuricemia_part3_quartiles["Q1"], hyperuricemia_part4_quartiles["Q1"]),
  Median = c(bmi_part1_quartiles["Median"], bmi_part2_quartiles["Median"], bmi_part3_quartiles["Median"], bmi_part4_quartiles["Median"],mean_value_part2, sd_value_part2, hyperuricemia_part1_quartiles["Median"], hyperuricemia_part2_quartiles["Median"], hyperuricemia_part3_quartiles["Median"], hyperuricemia_part4_quartiles["Median"]),
  Q3 = c(bmi_part1_quartiles["Q3"], bmi_part2_quartiles["Q3"], bmi_part3_quartiles["Q3"], bmi_part4_quartiles["Q3"], mean_value_part3, sd_value_part3, hyperuricemia_part1_quartiles["Q3"], hyperuricemia_part2_quartiles["Q3"], hyperuricemia_part3_quartiles["Q3"], hyperuricemia_part4_quartiles["Q3"]),
  Q4 = c(bmi_part1_quartiles["Q4"], bmi_part2_quartiles["Q4"], bmi_part3_quartiles["Q4"], bmi_part4_quartiles["Q4"], mean_value_part4, sd_value_part4, hyperuricemia_part1_quartiles["Q4"], hyperuricemia_part2_quartiles["Q4"], hyperuricemia_part3_quartiles["Q4"], hyperuricemia_part4_quartiles["Q4"])
)
# 将结果保存为 CSV 文件
write.csv(quartiles_df, "D:/data_analyse/csv/2/table2/99_20_t2_quartiles_summary.csv", row.names = FALSE)
# 显示数据框
print(quartiles_df)










































# 07-18的table3
data_table2_3 <- data_dii_07_18
# 按尿酸值从小到大排序
sorted_data <- data_table2_3[order(data_table2_3$LBDSUASI), ]
# 计算总行数和每组大小
n <- nrow(sorted_data)
split_size <- ceiling(n / 4)  # 向上取整，保证4组总行数 = n
# 构造一个与排序后数据相同长度的“分组标签”向量
quartile_vec <- c(
  rep("Q1", split_size),             # 前 split_size 行标记为 Q1
  rep("Q2", split_size),             # 接下来的 split_size 行标记为 Q2
  rep("Q3", split_size),             # 第三段标记为 Q3
  rep("Q4", n - 3 * split_size)      # 剩余的行都标记为 Q4
)
# 将“分组标签”写入 sorted_data 的新列 UA_quartile
sorted_data$UA_quartile <- quartile_vec
# 分别取出四个子数据集
data_table2_3_part1 <- subset(sorted_data, UA_quartile == "Q1")
data_table2_3_part2 <- subset(sorted_data, UA_quartile == "Q2")
data_table2_3_part3 <- subset(sorted_data, UA_quartile == "Q3")
data_table2_3_part4 <- subset(sorted_data, UA_quartile == "Q4")
# 转换为因子
sorted_data$UA_quartile_factor <- factor(
  sorted_data$UA_quartile, 
  levels = c("Q1","Q2","Q3","Q4")
)
# 再将因子转为数值
sorted_data$UA_quartile_num <- as.numeric(sorted_data$UA_quartile_factor)


# 计算分类变量的人数占比
# 定义一个函数，用于计算人数和占比
calculate_proportion <- function(df, variable) {
  df %>%
    group_by(!!sym(variable)) %>%      # 按分类变量分组
    summarise(n = n(),                 # 计算人数
              percent = round(n / nrow(df) * 100, 1)) %>%  # 计算占比，保留一位小数
    mutate(category = as.character(!!sym(variable)),
           percent = format(percent, nsmall = 1)) %>%     # 强制保留一位小数
    select(category, n, percent)       # 输出分类标签、人数、占比
}
# 定义分类变量列表（排除数值型变量）
categorical_vars <- c("Sex", "AgeGroup", "Race", "Education", "Marital", 
                      "Income", "BMI", "Obesity", "DSD", "Alchohol", "Gout", "Diabetes", 
                      "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")


# 07-18 Q1
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part1, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t3_Q1.csv", row.names = FALSE)

# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value <- mean(data_table2_3$DRXTKCAL, na.rm = TRUE)  # 计算均值
sd_value <- sd(data_table2_3$DRXTKCAL, na.rm = TRUE)      # 计算标准差
# 输出结果
cat("均值 (Mean): ", round(mean_value, 2), "\n")
cat("标准差 (SD): ", round(sd_value, 2), "\n")


# 07-18 Q2
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part2, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t3_Q2.csv", row.names = FALSE)


# 07-18 Q3
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part3, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t3_Q3.csv", row.names = FALSE)


# 07-18 Q4
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part4, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序 
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果 
print(final_results)
# 保存为单个CSV文件 
write.csv(final_results, "D:/data_analyse/csv/2/table2/07_18_seperate_t3_Q4.csv", row.names = FALSE)


# 计算 BMI 四分位数
bmi_part1_quartiles <- quantile(data_table2_3_part1$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part2_quartiles <- quantile(data_table2_3_part2$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part3_quartiles <- quantile(data_table2_3_part3$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part4_quartiles <- quantile(data_table2_3_part4$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value_part1 <- mean(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
sd_value_part1 <- sd(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
mean_value_part2 <- mean(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
sd_value_part2 <- sd(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
mean_value_part3 <- mean(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
sd_value_part3 <- sd(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
mean_value_part4 <- mean(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
sd_value_part4 <- sd(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
# 计算DII四个部分四分位数
DII_part1_quartiles <- quantile(data_table2_3_part1$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part2_quartiles <- quantile(data_table2_3_part2$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part3_quartiles <- quantile(data_table2_3_part3$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part4_quartiles <- quantile(data_table2_3_part4$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 合并结果为一个数据框
quartiles_df <- data.frame(
  Metric = c("BMI1", "BMI2", "BMI3", "BMI4", "CaloriesMean", "CaloriesSD", "DII1", "DII2", "DII3", "DII4"),
  Q1 = c(bmi_part1_quartiles["Q1"], bmi_part2_quartiles["Q1"], bmi_part3_quartiles["Q1"], bmi_part4_quartiles["Q1"], mean_value_part1, sd_value_part1, DII_part1_quartiles["Q1"], DII_part2_quartiles["Q1"], DII_part3_quartiles["Q1"], DII_part4_quartiles["Q1"]),
  Median = c(bmi_part1_quartiles["Median"], bmi_part2_quartiles["Median"], bmi_part3_quartiles["Median"], bmi_part4_quartiles["Median"],mean_value_part2, sd_value_part2, DII_part1_quartiles["Median"], DII_part2_quartiles["Median"], DII_part3_quartiles["Median"], DII_part4_quartiles["Median"]),
  Q3 = c(bmi_part1_quartiles["Q3"], bmi_part2_quartiles["Q3"], bmi_part3_quartiles["Q3"], bmi_part4_quartiles["Q3"], mean_value_part3, sd_value_part3, DII_part1_quartiles["Q3"], DII_part2_quartiles["Q3"], DII_part3_quartiles["Q3"], DII_part4_quartiles["Q3"]),
  Q4 = c(bmi_part1_quartiles["Q4"], bmi_part2_quartiles["Q4"], bmi_part3_quartiles["Q4"], bmi_part4_quartiles["Q4"], mean_value_part4, sd_value_part4, DII_part1_quartiles["Q4"], DII_part2_quartiles["Q4"], DII_part3_quartiles["Q4"], DII_part4_quartiles["Q4"])
)
# 将结果保存为 CSV 文件
write.csv(quartiles_df, "D:/data_analyse/csv/2/table2/07_18_t3_quartiles_summary.csv", row.names = FALSE)
# 显示数据框
print(quartiles_df)





































# 99-20的table3
data_table2_3 <- data_dii
# 按尿酸值从小到大排序
sorted_data <- data_table2_3[order(data_table2_3$LBDSUASI), ]
# 计算总行数和每组大小
n <- nrow(sorted_data)
split_size <- ceiling(n / 4)  # 向上取整，保证4组总行数 = n
# 构造一个与排序后数据相同长度的“分组标签”向量
quartile_vec <- c(
  rep("Q1", split_size),             # 前 split_size 行标记为 Q1
  rep("Q2", split_size),             # 接下来的 split_size 行标记为 Q2
  rep("Q3", split_size),             # 第三段标记为 Q3
  rep("Q4", n - 3 * split_size)      # 剩余的行都标记为 Q4
)
# 将“分组标签”写入 sorted_data 的新列 UA_quartile
sorted_data$UA_quartile <- quartile_vec
# 分别取出四个子数据集
data_table2_3_part1 <- subset(sorted_data, UA_quartile == "Q1")
data_table2_3_part2 <- subset(sorted_data, UA_quartile == "Q2")
data_table2_3_part3 <- subset(sorted_data, UA_quartile == "Q3")
data_table2_3_part4 <- subset(sorted_data, UA_quartile == "Q4")
# 转换为因子
sorted_data$UA_quartile_factor <- factor(
  sorted_data$UA_quartile, 
  levels = c("Q1","Q2","Q3","Q4")
)
# 再将因子转为数值
sorted_data$UA_quartile_num <- as.numeric(sorted_data$UA_quartile_factor)


# 计算分类变量的人数占比
# 定义一个函数，用于计算人数和占比
calculate_proportion <- function(df, variable) {
  df %>%
    group_by(!!sym(variable)) %>%      # 按分类变量分组
    summarise(n = n(),                 # 计算人数
              percent = round(n / nrow(df) * 100, 1)) %>%  # 计算占比，保留一位小数
    mutate(category = as.character(!!sym(variable)),
           percent = format(percent, nsmall = 1)) %>%     # 强制保留一位小数
    select(category, n, percent)       # 输出分类标签、人数、占比
}
# 定义分类变量列表（排除数值型变量）
categorical_vars <- c("Sex", "AgeGroup", "Race", "Education", "Marital", 
                      "Income", "BMI", "Obesity", "DSD", "Alchohol", "Diabetes", 
                      "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")


# 99-20 Q1
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part1, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t3_Q1.csv", row.names = FALSE)

# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value <- mean(data_table2_3$DRXTKCAL, na.rm = TRUE)  # 计算均值
sd_value <- sd(data_table2_3$DRXTKCAL, na.rm = TRUE)      # 计算标准差
# 输出结果
cat("均值 (Mean): ", round(mean_value, 2), "\n")
cat("标准差 (SD): ", round(sd_value, 2), "\n")


# 99-20 Q2
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part2, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t3_Q2.csv", row.names = FALSE)


# 99-20 Q3
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part3, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t3_Q3.csv", row.names = FALSE)


# 99-20 Q4
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part4, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序 
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果 
print(final_results)
# 保存为单个CSV文件 
write.csv(final_results, "D:/data_analyse/csv/2/table2/99_20_seperate_t3_Q4.csv", row.names = FALSE)


# 计算 BMI 四分位数
bmi_part1_quartiles <- quantile(data_table2_3_part1$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part2_quartiles <- quantile(data_table2_3_part2$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part3_quartiles <- quantile(data_table2_3_part3$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part4_quartiles <- quantile(data_table2_3_part4$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value_part1 <- mean(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
sd_value_part1 <- sd(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
mean_value_part2 <- mean(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
sd_value_part2 <- sd(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
mean_value_part3 <- mean(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
sd_value_part3 <- sd(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
mean_value_part4 <- mean(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
sd_value_part4 <- sd(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
# 计算DII四个部分四分位数
DII_part1_quartiles <- quantile(data_table2_3_part1$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part2_quartiles <- quantile(data_table2_3_part2$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part3_quartiles <- quantile(data_table2_3_part3$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part4_quartiles <- quantile(data_table2_3_part4$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 合并结果为一个数据框
quartiles_df <- data.frame(
  Metric = c("BMI1", "BMI2", "BMI3", "BMI4", "CaloriesMean", "CaloriesSD", "DII1", "DII2", "DII3", "DII4"),
  Q1 = c(bmi_part1_quartiles["Q1"], bmi_part2_quartiles["Q1"], bmi_part3_quartiles["Q1"], bmi_part4_quartiles["Q1"], mean_value_part1, sd_value_part1, DII_part1_quartiles["Q1"], DII_part2_quartiles["Q1"], DII_part3_quartiles["Q1"], DII_part4_quartiles["Q1"]),
  Median = c(bmi_part1_quartiles["Median"], bmi_part2_quartiles["Median"], bmi_part3_quartiles["Median"], bmi_part4_quartiles["Median"],mean_value_part2, sd_value_part2, DII_part1_quartiles["Median"], DII_part2_quartiles["Median"], DII_part3_quartiles["Median"], DII_part4_quartiles["Median"]),
  Q3 = c(bmi_part1_quartiles["Q3"], bmi_part2_quartiles["Q3"], bmi_part3_quartiles["Q3"], bmi_part4_quartiles["Q3"], mean_value_part3, sd_value_part3, DII_part1_quartiles["Q3"], DII_part2_quartiles["Q3"], DII_part3_quartiles["Q3"], DII_part4_quartiles["Q3"]),
  Q4 = c(bmi_part1_quartiles["Q4"], bmi_part2_quartiles["Q4"], bmi_part3_quartiles["Q4"], bmi_part4_quartiles["Q4"], mean_value_part4, sd_value_part4, DII_part1_quartiles["Q4"], DII_part2_quartiles["Q4"], DII_part3_quartiles["Q4"], DII_part4_quartiles["Q4"])
)
# 将结果保存为 CSV 文件
write.csv(quartiles_df, "D:/data_analyse/csv/2/table2/99_20_t3_quartiles_summary.csv", row.names = FALSE)
# 显示数据框
print(quartiles_df)





































# 计算P值


# 新p值计算
# 2.1 提取 t 检验的 p 值
get_p_ttest <- function(formula, data) {
  fit <- t.test(formula, data = data)
  round(fit$p.value, 3)
}
# 2.2 提取单因素 ANOVA 的 p 值
# 注意：如果只有一个因子，summary(aov())的结果表中，
# “Pr(>F)”的第一个因子对应行[1]，最后一行通常是Residuals
get_p_aov <- function(formula, data) {
  fit <- aov(formula, data = data)
  pval <- summary(fit)[[1]]["Pr(>F)"][1]
  round(pval, 3)
}
# 2.3 提取 Cochran–Armitage 趋势检验的 p 值
get_p_cochran_armitage <- function(tab) {
  # tab 通常是 table(sorted_data$UA_quartile, sorted_data$Sex) 这类
  fit <- CochranArmitageTest(tab)
  round(fit$p.value, 3)
}
# 2.4 提取线性回归的 p 值
# 对于多分类自变量，如果你想获取“整体”p值，要用 Type II/III Anova;
# 这里示例只取回归系数第 2 行的 p 值(假设只有 1 个主要自变量)
get_p_lm <- function(formula, data) {
  fit <- lm(formula, data = data)
  # 系数矩阵 [2,4] 列是 Pr(>|t|)
  round(summary(fit)$coefficients[2, 4], 3)
}


# 表3 p-overall
# 二分类
p_overall_A_bin <- list()
# 性别
p_overall_A_bin[["Sex"]] <- get_p_ttest(LBDSUASI ~ Sex, data = sorted_data)
# 婚姻
p_overall_A_bin[["Marital"]] <- get_p_ttest(LBDSUASI ~ Marital, data = sorted_data)
# 肥胖
p_overall_A_bin[["Obesity"]] <- get_p_ttest(LBDSUASI ~ Obesity, data = sorted_data)
# 膳食补充
p_overall_A_bin[["DSD"]] <- get_p_ttest(LBDSUASI ~ DSD, data = sorted_data)
# 酒精
p_overall_A_bin[["Alchohol"]] <- get_p_ttest(LBDSUASI ~ Alchohol, data = sorted_data)
# 痛风
# p_overall_A_bin[["Gout"]] <- get_p_ttest(LBDSUASI ~ Gout, data = sorted_data)
# 糖尿病
p_overall_A_bin[["Diabetes"]] <- get_p_ttest(LBDSUASI ~ Diabetes, data = sorted_data)
# 高脂血症
p_overall_A_bin[["Hypercholesterolemia"]] <- get_p_ttest(LBDSUASI ~ Hypercholesterolemia, data = sorted_data)
# 高血压
p_overall_A_bin[["Hypertension"]] <- get_p_ttest(LBDSUASI ~ Hypertension, data = sorted_data)
# 冠心病
p_overall_A_bin[["HeartDisease"]] <- get_p_ttest(LBDSUASI ~ HeartDisease, data = sorted_data)
p_overall_A_bin_df <- data.frame(
  Variable = names(p_overall_A_bin),
  P_value  = unlist(p_overall_A_bin),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_A_bin_df

# 多分类
p_overall_A_multi <- list()
# 年龄
p_overall_A_multi[["AgeGroup"]] <- get_p_aov(LBDSUASI ~ AgeGroup, data = sorted_data)
# 种族
p_overall_A_multi[["Race"]] <- get_p_aov(LBDSUASI ~ Race, data = sorted_data)
# 教育
p_overall_A_multi[["Education"]] <- get_p_aov(LBDSUASI ~ Education, data = sorted_data)
# 贫困
p_overall_A_multi[["Income"]] <- get_p_aov(LBDSUASI ~ Income, data = sorted_data)
# BMI
p_overall_A_multi[["BMI"]] <- get_p_aov(LBDSUASI ~ BMI, data = sorted_data)
# GFR
p_overall_A_multi[["GFR"]] <- get_p_aov(LBDSUASI ~ GFR, data = sorted_data)
p_overall_A_multi_df <- data.frame(
  Variable = names(p_overall_A_multi),
  P_value  = unlist(p_overall_A_multi),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_A_multi_df

# 连续变量
p_overall_A_cont <- list()
# BMI
p_overall_A_cont[["BMXBMI_factor"]] <- get_p_aov(LBDSUASI ~ factor(BMXBMI), data = sorted_data)
# 卡路里
p_overall_A_cont[["DRXTKCAL_factor"]] <- get_p_aov(LBDSUASI ~ factor(DRXTKCAL), data = sorted_data)
# DII
p_overall_A_cont[["Total_DII_Adjusted_factor"]] <- get_p_aov(LBDSUASI ~ factor(Total_DII_Adjusted), data = sorted_data)
p_overall_A_cont_df <- data.frame(
  Variable = names(p_overall_A_cont),
  P_value  = unlist(p_overall_A_cont),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_A_cont_df



# 表2 p-overall
# 二分类
p_overall_B_bin <- list()
# 性别
p_overall_B_bin[["Sex"]] <- get_p_ttest(Total_DII_Adjusted ~ Sex, data = sorted_data)
# 婚姻
p_overall_B_bin[["Marital"]] <- get_p_ttest(Total_DII_Adjusted ~ Marital, data = sorted_data)
# 肥胖
p_overall_B_bin[["Obesity"]] <- get_p_ttest(Total_DII_Adjusted ~ Obesity, data = sorted_data)
# 膳食补充
p_overall_B_bin[["DSD"]] <- get_p_ttest(Total_DII_Adjusted ~ DSD, data = sorted_data)
# 酒精
p_overall_B_bin[["Alchohol"]] <- get_p_ttest(Total_DII_Adjusted ~ Alchohol, data = sorted_data)
# 痛风
# p_overall_B_bin[["Gout"]] <- get_p_ttest(Total_DII_Adjusted ~ Gout, data = sorted_data)
# 糖尿病
p_overall_B_bin[["Diabetes"]] <- get_p_ttest(Total_DII_Adjusted ~ Diabetes, data = sorted_data)
# 高脂血症
p_overall_B_bin[["Hypercholesterolemia"]] <- get_p_ttest(Total_DII_Adjusted ~ Hypercholesterolemia, data = sorted_data)
# 高血压
p_overall_B_bin[["Hypertension"]] <- get_p_ttest(Total_DII_Adjusted ~ Hypertension, data = sorted_data)
# 冠心病
p_overall_B_bin[["HeartDisease"]] <- get_p_ttest(Total_DII_Adjusted ~ HeartDisease, data = sorted_data)
p_overall_B_bin_df <- data.frame(
  Variable = names(p_overall_B_bin),
  P_value  = unlist(p_overall_B_bin),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_B_bin_df

# 多分类
p_overall_B_multi <- list()
# 年龄
p_overall_B_multi[["AgeGroup"]] <- get_p_aov(Total_DII_Adjusted ~ AgeGroup, data = sorted_data)
# 种族
p_overall_B_multi[["Race"]] <- get_p_aov(Total_DII_Adjusted ~ Race, data = sorted_data)
# 教育
p_overall_B_multi[["Education"]] <- get_p_aov(Total_DII_Adjusted ~ Education, data = sorted_data)
# 贫困
p_overall_B_multi[["Income"]] <- get_p_aov(Total_DII_Adjusted ~ Income, data = sorted_data)
# BMI
p_overall_B_multi[["BMI"]] <- get_p_aov(Total_DII_Adjusted ~ BMI, data = sorted_data)
# GFR
p_overall_B_multi[["GFR"]] <- get_p_aov(Total_DII_Adjusted ~ GFR, data = sorted_data)
p_overall_B_multi_df <- data.frame(
  Variable = names(p_overall_B_multi),
  P_value  = unlist(p_overall_B_multi),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_B_multi_df

# 连续变量
p_overall_B_cont <- list()
# BMI
p_overall_B_cont[["BMXBMI_factor"]] <- get_p_aov(Total_DII_Adjusted ~ factor(BMXBMI), data = sorted_data)
# 卡路里
p_overall_B_cont[["DRXTKCAL_factor"]] <- get_p_aov(Total_DII_Adjusted ~ factor(DRXTKCAL), data = sorted_data)
# 高尿酸
p_overall_B_cont[["LBDSUASI_factor"]] <- get_p_aov(Total_DII_Adjusted ~ factor(LBDSUASI), data = sorted_data)
p_overall_B_cont_df <- data.frame(
  Variable = names(p_overall_B_cont),
  P_value  = unlist(p_overall_B_cont),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_B_cont_df



# 表2、3通用p-trend
# 二分类
p_trend_bin <- list()
# 性别
tab_sex <- table(sorted_data$UA_quartile, sorted_data$Sex)
p_trend_bin[["Sex"]] <- get_p_cochran_armitage(tab_sex)
# 婚姻
tab_marital <- table(sorted_data$UA_quartile, sorted_data$Marital)
p_trend_bin[["Marital"]] <- get_p_cochran_armitage(tab_marital)
# 肥胖
tab_obesity <- table(sorted_data$UA_quartile, sorted_data$Obesity)
p_trend_bin[["Obesity"]] <- get_p_cochran_armitage(tab_obesity)
# 膳食补充
tab_dsd <- table(sorted_data$UA_quartile, sorted_data$DSD)
p_trend_bin[["DSD"]] <- get_p_cochran_armitage(tab_dsd)
# 酒精
tab_alchohol <- table(sorted_data$UA_quartile, sorted_data$Alchohol)
p_trend_bin[["Alchohol"]] <- get_p_cochran_armitage(tab_alchohol)
# 痛风
# tab_gout <- table(sorted_data$UA_quartile, sorted_data$Gout)
# p_trend_bin[["Gout"]] <- get_p_cochran_armitage(tab_gout)
# 糖尿病
tab_diabetes <- table(sorted_data$UA_quartile, sorted_data$Diabetes)
p_trend_bin[["Diabetes"]] <- get_p_cochran_armitage(tab_diabetes)
# 高脂血症
tab_hyperchol <- table(sorted_data$UA_quartile, sorted_data$Hypercholesterolemia)
p_trend_bin[["Hypercholesterolemia"]] <- get_p_cochran_armitage(tab_hyperchol)
# 高血压
tab_hypertension <- table(sorted_data$UA_quartile, sorted_data$Hypertension)
p_trend_bin[["Hypertension"]] <- get_p_cochran_armitage(tab_hypertension)
# 冠心病
tab_heartdisease <- table(sorted_data$UA_quartile, sorted_data$HeartDisease)
p_trend_bin[["HeartDisease"]] <- get_p_cochran_armitage(tab_heartdisease)
p_trend_bin_df <- data.frame(
  Variable = names(p_trend_bin),
  P_value  = unlist(p_trend_bin),
  stringsAsFactors = FALSE
)
# 查看结果
p_trend_bin_df

# 多分类和连续变量
p_trend_multi <- list()
# 年龄
p_trend_multi[["AgeGroup"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ AgeGroup, data = sorted_data)
# 种族
p_trend_multi[["Race"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Race, data = sorted_data)
# 教育
p_trend_multi[["Education"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Education, data = sorted_data)
# 贫困
p_trend_multi[["Income"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Income, data = sorted_data)
# BMI
p_trend_multi[["BMI"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ BMI, data = sorted_data)
# GFR
p_trend_multi[["GFR"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ GFR, data = sorted_data)
# 卡路里
p_trend_multi[["DRXTKCAL"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ DRXTKCAL, data = sorted_data)
# BMI
p_trend_multi[["BMXBMI"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ BMXBMI, data = sorted_data)
# 高尿酸
p_trend_multi[["LBDSUASI"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ LBDSUASI, data = sorted_data)
# DII
p_trend_multi[["Total_DII_Adjusted"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Total_DII_Adjusted, data = sorted_data)
p_trend_multi_df <- data.frame(
  Variable = names(p_trend_multi),
  P_value  = unlist(p_trend_multi),
  stringsAsFactors = FALSE
)
# 查看结果
p_trend_multi_df
# 合并p-trend
p_trend_df <- bind_rows(
  p_trend_bin_df %>% mutate(TestType = "CochranArmitage"),
  p_trend_multi_df %>% mutate(TestType = "LinearRegression")
)
# 查看合并后的结果
p_trend_df



# 输出
# 07-18 t3
write.csv(p_overall_A_bin_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t3_bin.csv", row.names = FALSE)
write.csv(p_overall_A_multi_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t3_multi.csv", row.names = FALSE)
write.csv(p_overall_A_cont_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t3_cont.csv", row.names = FALSE)
write.csv(p_trend_df, "D:/data_analyse/csv/2/table2/p_trend_07_18_t3.csv", row.names = FALSE)
# 99-20 t3
write.csv(p_overall_A_bin_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t3_bin.csv", row.names = FALSE)
write.csv(p_overall_A_multi_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t3_multi.csv", row.names = FALSE)
write.csv(p_overall_A_cont_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t3_cont.csv", row.names = FALSE)
write.csv(p_trend_df, "D:/data_analyse/csv/2/table2/p_trend_99_20_t3.csv", row.names = FALSE)
# 07-18 t2
write.csv(p_overall_B_bin_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t2_bin.csv", row.names = FALSE)
write.csv(p_overall_B_multi_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t2_multi.csv", row.names = FALSE)
write.csv(p_overall_B_cont_df, "D:/data_analyse/csv/2/table2/p_overall_07_18_t2_cont.csv", row.names = FALSE)
write.csv(p_trend_df, "D:/data_analyse/csv/2/table2/p_trend_07_18_t2.csv", row.names = FALSE)
# 99-20 t2
write.csv(p_overall_B_bin_df, "D:/data_analyse/csv/2/table2/p_overall_99_20_t2_bin.csv", row.names = FALSE)
write.csv(p_overall_B_multi_df, "D:/data_analyse/csv/2/table2/p_overall_99_20_t2_multi.csv", row.names = FALSE)
write.csv(p_overall_B_cont_df, "D:/data_analyse/csv/2/table2/p_overall_99_20_t2_cont.csv", row.names = FALSE)
write.csv(p_trend_df, "D:/data_analyse/csv/2/table2/p_trend_99_20_t2.csv", row.names = FALSE)




# 新输出
# 07-18 t3
df_07_18_t3_bin   <- p_overall_A_bin_df   %>% mutate(AnalysisType = "Binary")
df_07_18_t3_multi <- p_overall_A_multi_df %>% mutate(AnalysisType = "Multi")
df_07_18_t3_cont  <- p_overall_A_cont_df  %>% mutate(AnalysisType = "Continuous")
df_07_18_t3_trend <- p_trend_df           %>% mutate(AnalysisType = "Trend")
# 合并为一个数据框
df_07_18_t3_all <- dplyr::bind_rows(
  df_07_18_t3_bin,
  df_07_18_t3_multi,
  df_07_18_t3_cont,
  df_07_18_t3_trend
)
# 一次性输出到 CSV
write.csv(df_07_18_t3_all, 
          "D:/data_analyse/csv/2/table2/p_07_18_t3.csv", 
          row.names = FALSE)

# 99-20 t3
df_99_20_t3_bin   <- p_overall_A_bin_df   %>% mutate(AnalysisType = "Binary")
df_99_20_t3_multi <- p_overall_A_multi_df %>% mutate(AnalysisType = "Multi")
df_99_20_t3_cont  <- p_overall_A_cont_df  %>% mutate(AnalysisType = "Continuous")
df_99_20_t3_trend <- p_trend_df           %>% mutate(AnalysisType = "Trend")
df_99_20_t3_all <- dplyr::bind_rows(
  df_99_20_t3_bin,
  df_99_20_t3_multi,
  df_99_20_t3_cont,
  df_99_20_t3_trend
)
write.csv(df_99_20_t3_all, 
          "D:/data_analyse/csv/2/table2/p_99_20_t3.csv", 
          row.names = FALSE)


# 07-18 t2
df_07_18_t2_bin   <- p_overall_B_bin_df   %>% mutate(AnalysisType = "Binary")
df_07_18_t2_multi <- p_overall_B_multi_df %>% mutate(AnalysisType = "Multi")
df_07_18_t2_cont  <- p_overall_B_cont_df  %>% mutate(AnalysisType = "Continuous")
df_07_18_t2_trend <- p_trend_df           %>% mutate(AnalysisType = "Trend")
df_07_18_t2_all <- dplyr::bind_rows(
  df_07_18_t2_bin,
  df_07_18_t2_multi,
  df_07_18_t2_cont,
  df_07_18_t2_trend
)
write.csv(df_07_18_t2_all, 
          "D:/data_analyse/csv/2/table2/p_07_18_t2.csv", 
          row.names = FALSE)



# 99-20 t2
df_99_20_t2_bin   <- p_overall_B_bin_df   %>% mutate(AnalysisType = "Binary")
df_99_20_t2_multi <- p_overall_B_multi_df %>% mutate(AnalysisType = "Multi")
df_99_20_t2_cont  <- p_overall_B_cont_df  %>% mutate(AnalysisType = "Continuous")
df_99_20_t2_trend <- p_trend_df           %>% mutate(AnalysisType = "Trend")
df_99_20_t2_all <- dplyr::bind_rows(
  df_99_20_t2_bin,
  df_99_20_t2_multi,
  df_99_20_t2_cont,
  df_99_20_t2_trend
)
write.csv(df_99_20_t2_all, 
          "D:/data_analyse/csv/2/table2/p_99_20_t2.csv", 
          row.names = FALSE)






























# 计算发病率95CI
# 以高尿酸血症为例
cases <- sum(datav4$hyperuricemia_count == 1)  # 发病人数
rate <- cases / n  # 发病率

# 计算95% CI
Z <- 1.96  # 95%置信水平
SE <- sqrt(rate * (1 - rate) / n)  # 标准误差
CI_lower <- rate - Z * SE
CI_upper <- rate + Z * SE

# 确保区间不超出[0,1]
CI_lower <- max(0, CI_lower)
CI_upper <- min(1, CI_upper)

# 输出结果
cat("发病率:", rate, "\n")
cat("95% CI:", CI_lower, "-", CI_upper, "\n")









# 计算SE 以DII为例
# 计算均值
mean_value <- mean(data_dii$Total_DII_Adjusted)
# 计算标准差 (SD)
sd_value <- sd(data_dii$Total_DII_Adjusted)
# 计算标准误差 (SE)
n <- length(data_dii$Total_DII_Adjusted)
se_value <- sd_value / sqrt(n)
# 输出结果
cat("Mean ± SE:", mean_value, "±", se_value, "\n")







# 计算年龄SE
mean_value <- mean(data_dii$RIDAGEYR)
# 计算标准差 (SD)
sd_value <- sd(data_dii$RIDAGEYR)
# 计算标准误差 (SE)
n <- length(data_dii$RIDAGEYR)
se_value <- sd_value / sqrt(n)
# 输出结果
cat("Mean ± SE:", mean_value, "±", se_value, "\n")















# 计算简易基本信息对于高尿酸分布表
selected_data <- datav4[, c("Sex", "AgeGroup", "BMI", "Hyperuricemia")]






















# 计算22年间患病率曲线
# 使用cut()函数创建分组列
selected_data <- data_dii[, c("SEQN", "RIAGENDR", "hyperuricemia_count")]
breaks <- c(1, 9965, 21004, 31126, 41474, 51623, 62160, 71916, 83731, 93702, 124822)  # 定义分组区间
labels <- c("1999-2000", "2001-2002", "2003-2004", "2005-2006", "2007-2008", "2009-2010", "2011-2012", "2013-2014", "2015-2016", "2017-2020")  # 定义分组标签
selected_data$group <- cut(selected_data$SEQN, breaks = breaks, labels = labels, include.lowest = TRUE)

# 使用split()函数将数据框按分组列拆分为多个数据集
grouped_datasets <- split(selected_data, selected_data$group)

# 查看分组后的数据集
names(grouped_datasets)  # 查看分组名称
head(grouped_datasets[[1]])  # 查看第一个分组的数据集
grouped_datasets[["1999-2000"]]
str(grouped_datasets)

# 定义一个计算患病率与置信区间的函数
calculate_prevalence <- function(data, group_name) {
  # 计算总体患病率
  total_ci <- binom.confint(
    x = sum(data$hyperuricemia_count==1),
    n = nrow(data),
    methods = "wilson"
  )
  
  # 计算男性患病率
  male_data <- data %>% filter(RIAGENDR==1)
  male_ci <- if (nrow(male_data) > 0) {
    binom.confint(
      x = sum(male_data$hyperuricemia_count == 1),
      n = nrow(male_data),
      methods = "wilson"
    )
  } else {
    data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  # 计算女性患病率
  female_data <- data %>% filter(RIAGENDR==2)
  female_ci <- if (nrow(female_data) > 0) {
    binom.confint(
      x = sum(female_data$hyperuricemia_count==1),
      n = nrow(female_data),
      methods = "wilson"
    )
  } else {
    data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  # 构建结果数据框
  data.frame(
    Group = group_name,
    Total_Prev = sprintf("%.1f%% (%.1f-%.1f)", 
                         total_ci$mean * 100,
                         total_ci$lower * 100,
                         total_ci$upper * 100),
    Male_Prev = sprintf("%.1f%% (%.1f-%.1f)", 
                        male_ci$mean * 100,
                        male_ci$lower * 100,
                        male_ci$upper * 100),
    Female_Prev = sprintf("%.1f%% (%.1f-%.1f)", 
                          female_ci$mean * 100,
                          female_ci$lower * 100,
                          female_ci$upper * 100)
  )
}

# 对每个分组进行计算
result <- imap_dfr(
  grouped_datasets,
  ~ calculate_prevalence(.x, .y)
) 

# 查看结果
print(result, right = FALSE)
write.csv(result, file = "D:/data_analyse/csv/2/prevalence_results.csv", row.names = FALSE)



































# OR计算
# 整理数据
data_or <- data_dii
data_or$Total_DII_Group <- ifelse(data_or$Total_DII_Adjusted > median(data_or$Total_DII_Adjusted), "High", "Low")


# 总人群分析 15项
# 性别：Sex 1 2
# 年龄：AgeGroup 20-39 40-59 60-79 80+
# 种族：Race 1 2 3 4 5 mexicanAmerican others non-hispanicWhite non-hispanicBlack others
# 教育：Education 1 2 3 4 5 someHigh someHigh high someCollege college
# 婚姻：Marital 1 6 married 2 3 4 5 alone
# 收入：Inacome <1 1-2 >2
# 吸烟：smoke_count
# BMI：BMI <18.5 18.5-24 >24
# 肥胖：Obesity Yes No
# 膳食：DSD 0 No 不为0 Yes
# 酒精：Alchohol 1 Yes 2 No
# 糖尿病：Diabetes 1 2
# 高胆固醇：Hypercholesterolemia <5.2 No >-5.2 Yes
# 高血压：Hypertension 1 Yes 2 No
# 冠心病：HeartDisease 1 Yes 2 No
# GFR： <30 30-60 60-90 >90


# 总体
model_total <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = data_or, 
                   family = binomial(link = "logit"))
summary(model_total)
anova(model_total, test="LRT")
exp(coef(model_total))    
exp(confint(model_total))     
exp(confint(model_total, parm="Total_DII_Adjusted"))



# 性别
# 男性亚组
model_male <- glm(hyperuricemia_count ~ Total_DII_Adjusted + AgeGroup + Race + Education + Marital + Income + BMI + 
                    Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                  data = subset(data_or, Sex == "Male"), 
                  family = binomial)
summary(model_male)
anova(model_male, test="LRT")
exp(coef(model_male))      
exp(confint(model_male, parm="Total_DII_Adjusted"))

# 女性亚组
model_female <- glm(hyperuricemia_count ~ Total_DII_Adjusted + AgeGroup + Race + Education + Marital + Income + BMI + 
                      Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                    data = subset(data_or, Sex == "Female"), 
                    family = binomial)
summary(model_female)
anova(model_female, test="LRT")
exp(coef(model_female))
exp(confint(model_female, parm="Total_DII_Adjusted"))
# 性别交互验证
model_sex <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Sex + AgeGroup + Race + Education + Marital + Income + BMI + 
                           Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                         data = data_or, 
                         family = binomial(link = "logit"))
anova(model_sex, test="LRT")



# 年龄
# 20-39亚组
model_20_39 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + Race + Education + Marital + Income + BMI + 
                    Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                  data = subset(data_or, AgeGroup %in% c('20-29', '30-39')), 
                  family = binomial)
summary(model_20_39)
anova(model_20_39, test="LRT")
exp(coef(model_20_39))      
exp(confint(model_20_39, parm="Total_DII_Adjusted"))
# 40-59亚组
model_40_59 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + Race + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, AgeGroup %in% c('40-49', '50-59')), 
                   family = binomial)
summary(model_40_59)
anova(model_40_59, test="LRT")
exp(coef(model_40_59))      
exp(confint(model_40_59, parm="Total_DII_Adjusted"))
# 60-79亚组
model_60_79 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + Race + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, AgeGroup %in% c('60-69', '70-79')), 
                   family = binomial)
summary(model_60_79)
anova(model_60_79, test="LRT")
exp(coef(model_60_79))      
exp(confint(model_60_79, parm="Total_DII_Adjusted"))
# 80+亚组
model_80 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + Race + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, AgeGroup == '80+'), 
                   family = binomial)
summary(model_80)
anova(model_80, test="LRT")
exp(coef(model_80))      
exp(confint(model_80, parm="Total_DII_Adjusted"))
# 年龄交叉验证
model_age <- glm(hyperuricemia_count ~ Total_DII_Adjusted * AgeGroup + Sex + Race + Education + Marital + Income + BMI + 
                   Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                 data = data_or, 
                 family = binomial(link = "logit"))
summary(model_age)
anova(model_age, test="LRT")



# 种族
# Non-Hispanic white亚组
model_white <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, Race == 'Non-Hispanic white'), 
                   family = binomial)
summary(model_white)
anova(model_white, test="LRT")
exp(coef(model_white))      
exp(confint(model_white, parm="Total_DII_Adjusted"))
# Non-Hispanic black亚组
model_black <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, Race == 'Non-Hispanic black'), 
                   family = binomial)

summary(model_black)
anova(model_black, test="LRT")
exp(coef(model_black))      
exp(confint(model_black, parm="Total_DII_Adjusted"))
# Mexican American亚组
model_mexican <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, Race == 'Mexican American'), 
                   family = binomial)
summary(model_mexican)
anova(model_mexican, test="LRT")
exp(coef(model_mexican))      
exp(confint(model_mexican, parm="Total_DII_Adjusted"))
# Others亚组
model_race_others <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Education + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, Race == 'Others'), 
                   family = binomial)
summary(model_race_others)
anova(model_race_others, test="LRT")
exp(coef(model_race_others))      
exp(confint(model_race_others, parm="Total_DII_Adjusted"))
# 种族交叉验证
model_race <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Race + Sex + AgeGroup + Education + Marital + Income + BMI + 
                   Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                 data = data_or, 
                 family = binomial(link = "logit"))
anova(model_race, test="LRT")



# 教育
# Some high school亚组
model_some_high <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Marital + Income + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, Education == 'Some high school'), 
                   family = binomial)
summary(model_some_high)
anova(model_some_high, test="LRT")
exp(coef(model_some_high))      
exp(confint(model_some_high, parm="Total_DII_Adjusted"))
# High school or GED亚组
model_high <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Marital + Income + BMI + 
                         Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                       data = subset(data_or, Education == 'High school or GED'), 
                       family = binomial)
summary(model_high)
anova(model_high, test="LRT")
exp(coef(model_high))      
exp(confint(model_high, parm="Total_DII_Adjusted"))
# College graduate亚组
model_some_college <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Marital + Income + BMI + 
                         Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                       data = subset(data_or, Education == 'Some college'), 
                       family = binomial)
summary(model_some_college)
anova(model_some_college, test="LRT")
exp(coef(model_some_college))      
exp(confint(model_some_college, parm="Total_DII_Adjusted"))
# College graduate亚组
model_college <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Marital + Income + BMI + 
                         Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                       data = subset(data_or, Education == 'College graduate'), 
                       family = binomial)
summary(model_college)
anova(model_college, test="LRT")
exp(coef(model_college))      
exp(confint(model_college, parm="Total_DII_Adjusted"))
# 教育交叉验证
model_education <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Education + Sex + AgeGroup + Race + Marital + Income + BMI + 
                    Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                  data = data_or, 
                  family = binomial(link = "logit"))
anova(model_education, test="LRT")



# 婚姻
# Married or living with a partner亚组
model_married <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Income + BMI + 
                       Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                     data = subset(data_or, Marital == 'Married or living with a partner'), 
                     family = binomial)
summary(model_married)
anova(model_married, test="LRT")
exp(coef(model_married))      
exp(confint(model_married, parm="Total_DII_Adjusted"))
# Living alone亚组
model_alone <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Income + BMI + 
                       Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                     data = subset(data_or, Marital == 'Living alone'), 
                     family = binomial)
summary(model_alone)
anova(model_alone, test="LRT")
exp(coef(model_alone))      
exp(confint(model_alone, parm="Total_DII_Adjusted"))
# 婚姻交叉验证
model_marital <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Marital + Sex + AgeGroup + Race + Education + Income + BMI + 
                         Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                       data = data_or, 
                       family = binomial(link = "logit"))
anova(model_marital, test="LRT")



# 收入
# <1亚组
model_less1 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, INDFMPIR <= 1), 
                   family = binomial)
summary(model_less1)
anova(model_less1, test="LRT")
exp(coef(model_less1))      
exp(confint(model_less1, parm="Total_DII_Adjusted"))
# 1 to 2亚组
model_1to2 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, INDFMPIR > 1 & INDFMPIR < 2), 
                   family = binomial)
summary(model_1to2)
anova(model_1to2, test="LRT")
exp(coef(model_1to2))      
exp(confint(model_1to2, parm="Total_DII_Adjusted"))
# >2亚组
model_more2 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + BMI + 
                     Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, INDFMPIR >= 2), 
                   family = binomial)
summary(model_more2)
anova(model_more2, test="LRT")
exp(coef(model_more2))      
exp(confint(model_more2, parm="Total_DII_Adjusted"))
# 收入交叉验证
model_income <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Income + Sex + AgeGroup + Race + Education + Marital + BMI + 
                       Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                     data = data_or, 
                     family = binomial(link = "logit"))
anova(model_income, test="LRT")



# BMI
# Underweight亚组
model_underweight <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + 
                    Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                  data = subset(data_or, BMI == 'Underweight'), 
                  family = binomial)
summary(model_underweight)
anova(model_underweight, test="LRT")
exp(coef(model_underweight))      
exp(confint(model_underweight, parm="Total_DII_Adjusted"))
# Normal weight亚组
model_normalweight <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + 
                           Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                         data = subset(data_or, BMI == 'Normal weight'), 
                         family = binomial)
summary(model_normalweight)
anova(model_normalweight, test="LRT")
exp(coef(model_normalweight))      
exp(confint(model_normalweight, parm="Total_DII_Adjusted"))
# Overweight亚组
model_overweight <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + 
                           Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                         data = subset(data_or, BMI == 'Overweight'), 
                         family = binomial)
summary(model_overweight)
anova(model_overweight, test="LRT")
exp(coef(model_overweight))      
exp(confint(model_overweight, parm="Total_DII_Adjusted"))
# Obesity亚组
model_obesity <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + 
                           Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                         data = subset(data_or, BMI == 'Obesity'), 
                         family = binomial)
summary(model_obesity)
anova(model_obesity, test="LRT")
exp(coef(model_obesity))      
exp(confint(model_obesity, parm="Total_DII_Adjusted"))
# BMI交叉验证
model_BMI <- glm(hyperuricemia_count ~ Total_DII_Adjusted * BMI + Sex + AgeGroup + Race + Education + Marital + Income + 
                      Obesity + smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                    data = data_or, 
                    family = binomial(link = "logit"))
anova(model_BMI, test="LRT")



# 吸烟
# 0亚组
model_nosmoke <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                       Obesity + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                     data = subset(data_or, smoke_count == 0), 
                     family = binomial)
summary(model_nosmoke)
anova(model_nosmoke, test="LRT")
exp(coef(model_nosmoke))      
exp(confint(model_nosmoke, parm="Total_DII_Adjusted"))
# 1亚组
model_yessmoke <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                     Obesity + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                     data = subset(data_or, smoke_count == 1), 
                     family = binomial)
summary(model_yessmoke)
anova(model_yessmoke, test="LRT")
exp(coef(model_yessmoke))      
exp(confint(model_yessmoke, parm="Total_DII_Adjusted"))
# 吸烟交叉验证
model_smoke <- glm(hyperuricemia_count ~ Total_DII_Adjusted * smoke_count + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                   Obesity + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                 data = data_or, 
                 family = binomial(link = "logit"))
anova(model_smoke, test="LRT")



# 肥胖
# yes亚组
model_yesobesity <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                        smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                      data = subset(data_or, Obesity == 'Yes'), 
                      family = binomial)
summary(model_yesobesity)
anova(model_yesobesity, test="LRT")
exp(coef(model_yesobesity))      
exp(confint(model_yesobesity, parm="Total_DII_Adjusted"))
# no亚组
model_noobesity <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                          smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                        data = subset(data_or, Obesity == 'No'), 
                        family = binomial)
summary(model_noobesity)
anova(model_noobesity, test="LRT")
exp(coef(model_noobesity))      
exp(confint(model_noobesity, parm="Total_DII_Adjusted"))
# 肥胖交叉验证
model_obesity <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Obesity + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                     smoke_count + DSD + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = data_or, 
                   family = binomial(link = "logit"))
anova(model_obesity, test="LRT")



# 膳食
# NO亚组
model_nodsd <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                         smoke_count + Obesity + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                       data = subset(data_or, DSD == 'NO'), 
                       family = binomial)
summary(model_nodsd)
anova(model_nodsd, test="LRT")
exp(coef(model_nodsd))      
exp(confint(model_nodsd, parm="Total_DII_Adjusted"))
# YES亚组
model_yesdsd <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                     smoke_count + Obesity + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                   data = subset(data_or, DSD == 'YES'), 
                   family = binomial)
summary(model_yesdsd)
anova(model_yesdsd, test="LRT")
exp(coef(model_yesdsd))
exp(confint(model_yesdsd, parm="Total_DII_Adjusted"))
# 膳食交叉验证
model_dsd <- glm(hyperuricemia_count ~ Total_DII_Adjusted * DSD + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                       smoke_count + Obesity + Alchohol + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                     data = data_or, 
                     family = binomial(link = "logit"))
anova(model_dsd, test="LRT")



# 酒精
# YES亚组
model_yesalchohol <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                      smoke_count + Obesity + DSD + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                    data = subset(data_or, Alchohol == 'YES'), 
                    family = binomial)
summary(model_yesalchohol)
anova(model_yesalchohol, test="LRT")
exp(coef(model_yesalchohol))
exp(confint(model_yesalchohol, parm="Total_DII_Adjusted"))
# NO亚组
model_noalchohol <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                           smoke_count + Obesity + DSD + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                         data = subset(data_or, Alchohol == 'NO'), 
                         family = binomial)
summary(model_noalchohol)
anova(model_noalchohol, test="LRT")
exp(coef(model_noalchohol))
exp(confint(model_noalchohol, parm="Total_DII_Adjusted"))
# 酒精交叉验证
model_alchohol <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Alchohol + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                   smoke_count + Obesity + DSD + Diabetes + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                 data = data_or, 
                 family = binomial(link = "logit"))
anova(model_alchohol, test="LRT")



# 糖尿病
# No亚组
model_nodiabetes <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                          smoke_count + Obesity + DSD + Alchohol + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                        data = subset(data_or, Diabetes == 'No'), 
                        family = binomial)
summary(model_nodiabetes)
anova(model_nodiabetes, test="LRT")
exp(coef(model_nodiabetes))
exp(confint(model_nodiabetes, parm="Total_DII_Adjusted"))
# Yes亚组
model_yesdiabetes <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                          smoke_count + Obesity + DSD + Alchohol + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                        data = subset(data_or, Diabetes == 'Yes'), 
                        family = binomial)
summary(model_yesdiabetes)
anova(model_yesdiabetes, test="LRT")
exp(coef(model_yesdiabetes))
exp(confint(model_yesdiabetes, parm="Total_DII_Adjusted"))
# 糖尿病交叉验证
model_diabetes <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Diabetes + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                        smoke_count + Obesity + DSD + Alchohol + Hypercholesterolemia + Hypertension + HeartDisease + GFR,
                      data = data_or, 
                      family = binomial(link = "logit"))
anova(model_diabetes, test="LRT")



# 高胆固醇
# Yes亚组
model_yeshypercholesterolemia <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                           smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + HeartDisease + GFR,
                         data = subset(data_or, Hypercholesterolemia == 'Yes'), 
                         family = binomial)
summary(model_yeshypercholesterolemia)
anova(model_yeshypercholesterolemia, test="LRT")
exp(coef(model_yeshypercholesterolemia))
exp(confint(model_yeshypercholesterolemia, parm="Total_DII_Adjusted"))
# No亚组
model_nohypercholesterolemia <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                                       smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + HeartDisease + GFR,
                                     data = subset(data_or, Hypercholesterolemia == 'No'), 
                                     family = binomial)
summary(model_nohypercholesterolemia)
anova(model_nohypercholesterolemia, test="LRT")
exp(coef(model_nohypercholesterolemia))
exp(confint(model_nohypercholesterolemia, parm="Total_DII_Adjusted"))
# 高胆固醇交叉验证
model_hypercholesterolemia <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Hypercholesterolemia + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                        smoke_count + Obesity + DSD + Diabetes + Hypertension + HeartDisease + GFR,
                      data = data_or, 
                      family = binomial(link = "logit"))
anova(model_hypercholesterolemia, test="LRT")



# 高血压
# No亚组
model_nohypertension <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                                      smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypercholesterolemia + HeartDisease + GFR,
                                    data = subset(data_or, Hypertension == 'No'), 
                                    family = binomial)
summary(model_nohypertension)
anova(model_nohypertension, test="LRT")
exp(coef(model_nohypertension))
exp(confint(model_nohypertension, parm="Total_DII_Adjusted"))
# Yes亚组
model_yeshypertension <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                              smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypercholesterolemia + HeartDisease + GFR,
                            data = subset(data_or, Hypertension == 'Yes'), 
                            family = binomial)
summary(model_nohypertension)
anova(model_nohypertension, test="LRT")
exp(coef(model_nohypertension))
exp(confint(model_nohypertension, parm="Total_DII_Adjusted"))
# 高血压交叉验证
model_hypertension <- glm(hyperuricemia_count ~ Total_DII_Adjusted * Hypertension + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                                    smoke_count + Obesity + DSD + Diabetes + Hypercholesterolemia + HeartDisease + GFR,
                                  data = data_or, 
                                  family = binomial(link = "logit"))
anova(model_hypertension, test="LRT")



# 冠心病
# No亚组
model_noheartdisease <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                                      smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + Hypercholesterolemia + GFR,
                                    data = subset(data_or, HeartDisease == 'No'), 
                                    family = binomial)
summary(model_noheartdisease)
anova(model_noheartdisease, test="LRT")
exp(coef(model_noheartdisease))
exp(confint(model_noheartdisease, parm="Total_DII_Adjusted"))
# Yes亚组
model_yesheartdisease <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                              smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + Hypercholesterolemia + GFR,
                            data = subset(data_or, HeartDisease == 'Yes'), 
                            family = binomial)
summary(model_yesheartdisease)
anova(model_yesheartdisease, test="LRT")
exp(coef(model_yesheartdisease))
exp(confint(model_yesheartdisease, parm="Total_DII_Adjusted"))
# 冠心病交叉验证
model_heartdisease <- glm(hyperuricemia_count ~ Total_DII_Adjusted * HeartDisease + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                                    smoke_count + Obesity + DSD + Diabetes + Diabetes + Hypertension + Hypercholesterolemia + GFR,
                                  data = data_or, 
                                  family = binomial(link = "logit"))
anova(model_heartdisease, test="LRT")



# GFR
# <30亚组
model_less30 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                               smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + Hypercholesterolemia + HeartDisease,
                             data = subset(data_or, eGFR < 30), 
                             family = binomial)
summary(model_less30)
anova(model_less30, test="LRT")
exp(coef(model_less30))
exp(confint(model_less30, parm="Total_DII_Adjusted"))
# 30-60亚组
model_30to60 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                      smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + Hypercholesterolemia + HeartDisease,
                    data = subset(data_or, eGFR >= 30 & eGFR < 60), 
                    family = binomial)
summary(model_30to60)
anova(model_30to60, test="LRT")
exp(coef(model_30to60))
exp(confint(model_30to60, parm="Total_DII_Adjusted"))
# 60-90亚组
model_60to90 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                      smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + Hypercholesterolemia + HeartDisease,
                    data = subset(data_or, eGFR >= 60 & eGFR < 90), 
                    family = binomial)
summary(model_60to90)
anova(model_60to90, test="LRT")
exp(coef(model_60to90))
exp(confint(model_60to90, parm="Total_DII_Adjusted"))
# >90亚组
model_more90 <- glm(hyperuricemia_count ~ Total_DII_Adjusted + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                      smoke_count + Obesity + DSD + Alchohol + Diabetes + Hypertension + Hypercholesterolemia + HeartDisease,
                    data = subset(data_or, eGFR > 90), 
                    family = binomial)
summary(model_more90)
anova(model_more90, test="LRT")
exp(coef(model_more90))
exp(confint(model_more90, parm="Total_DII_Adjusted"))
# gfr交叉验证
model_gfr <- glm(hyperuricemia_count ~ Total_DII_Adjusted * eGFR + Sex + AgeGroup + Race + Education + Marital + Income + BMI +
                            smoke_count + Obesity + DSD + Diabetes + Diabetes + Hypertension + Hypercholesterolemia + HeartDisease,
                          data = data_or, 
                          family = binomial(link = "logit"))
anova(model_gfr, test="LRT")






















# 数据导出
# 自定义函数：提取模型结果和置信区间
extract_model_results <- function(model, model_name) {
  tryCatch({
    # 提取系数、OR、p值等
    coef_df <- tidy(model, exponentiate = FALSE)
    coef_df$OR <- exp(coef_df$estimate)
    conf_int <- exp(confint.default(model))
    coef_df$OR_conf_low <- conf_int[,1]
    coef_df$OR_conf_high <- conf_int[,2]
    coef_df$model <- model_name
    coef_df$term_type <- "coefficient"
    
    # 提取ANOVA LRT结果
    anova_df <- tidy(anova(model, test = "LRT"))
    anova_df$model <- model_name
    anova_df$term_type <- "anova_LRT"
    
    # 合并结果
    list(coef = coef_df, anova = anova_df)
  }, error = function(e) {
    message(paste("Error in", model_name, ":", e$message))
    return(NULL)
  })
}

# 所有模型的名称列表（需要根据实际定义的模型补充完整）
models <- list(
  model_total,
  model_male, model_female, model_sex,
  model_20_39, model_40_59, model_60_79, model_80, model_age,
  model_white, model_black, model_mexican, model_race_others, model_race,
  model_some_high, model_high, model_some_college, model_college, model_education,
  model_married, model_alone, model_marital,
  model_less1, model_1to2, model_more2, model_income,
  model_underweight, model_normalweight, model_overweight, model_obesity, model_BMI,
  model_nosmoke, model_yessmoke, model_smoke,
  model_yesobesity, model_noobesity, model_obesity,
  model_nodsd, model_yesdsd, model_dsd,
  model_yesalchohol, model_noalchohol, model_alchohol,
  model_nodiabetes, model_yesdiabetes, model_diabetes,
  model_yeshypercholesterolemia, model_nohypercholesterolemia, model_hypercholesterolemia,
  model_nohypertension, model_yeshypertension, model_hypertension,
  model_noheartdisease, model_yesheartdisease, model_heartdisease,
  model_less30, model_30to60, model_60to90, model_more90, model_gfr
)

# 模型名称向量（必须与上述模型顺序完全一致）
model_names <- c(
  "model_total",
  "model_male", "model_female", "model_sex",
  "model_20_39", "model_40_59", "model_60_79", "model_80", "model_age",
  "model_white", "model_black", "model_mexican", "model_race_others", "model_race",
  "model_some_high", "model_high", "model_some_college", "model_college", "model_education",
  "model_married", "model_alone", "model_marital",
  "model_less1", "model_1to2", "model_more2", "model_income",
  "model_underweight", "model_normalweight", "model_overweight", "model_obesity", "model_BMI",
  "model_nosmoke", "model_yessmoke", "model_smoke",
  "model_yesobesity", "model_noobesity", "model_obesity",
  "model_nodsd", "model_yesdsd", "model_dsd",
  "model_yesalchohol", "model_noalchohol", "model_alchohol",
  "model_nodiabetes", "model_yesdiabetes", "model_diabetes",
  "model_yeshypercholesterolemia", "model_nohypercholesterolemia", "model_hypercholesterolemia",
  "model_nohypertension", "model_yeshypertension", "model_hypertension",
  "model_noheartdisease", "model_yesheartdisease", "model_heartdisease",
  "model_less30", "model_30to60", "model_60to90", "model_more90", "model_gfr"
)

# 确保数量和顺序匹配
stopifnot(length(models) == length(model_names))

# 提取所有结果
all_results <- map2(models, model_names, extract_model_results)

# 分离系数和ANOVA结果
coef_list <- map(all_results, ~ .x$coef)
anova_list <- map(all_results, ~ .x$anova)

# 合并为两个数据框
final_coef <- bind_rows(coef_list)
final_anova <- bind_rows(anova_list)

# 保存为CSV
write.csv(final_coef, "D:/data_analyse/csv/2/model_coefficients_results.csv", row.names = FALSE)
write.csv(final_anova, "D:/data_analyse/csv/2/model_anova_results.csv", row.names = FALSE)




# 方便算数节选表格
selected_or_data <- data_dii[, c("SEQN", "hyperuricemia_count", "Sex", "AgeGroup", "Race", "Education", "Marital", "Income",
                              "smoke_count", "BMI", "Obesity", "DSD", "Alchohol", "Diabetes", "Hypercholesterolemia",
                              "Hypertension", "HeartDisease", "GFR")]
write.csv(selected_or_data, "D:/data_analyse/csv/2/selected_or_data.csv", row.names = FALSE)


































# table5 逻辑回归 表5 计算OR最终森林图
# 先计算DII四分位
quantiles <- quantile(data_dii$Total_DII_Adjusted,
                      probs = c(0, 0.25, 0.5, 0.75, 1),
                      na.rm = TRUE)

# 创建分组（包含区间端点的最小值）
data_dii$DII_Group <- cut(
  data_dii$Total_DII_Adjusted,
  breaks = quantiles,
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,  # 确保包含最小值
  right = FALSE           # 左闭右开区间 [a,b)
)

# 验证分组效果
cat("各分组样本数：\n")
print(table(data_dii$DII_Group))

cat("\n实际分割阈值（原始标度）：\n")
print(quantiles)
# > print(quantiles)
# 0%          25%          50%          75%         100% 
#　-11.14163711  -0.06123187   3.12543536   5.54722241  12.80802243




# 开始计算OR
# 筛选男性且UA分组为Q1/Q3的数据
data_male <- subset(data_dii, 
                    Sex == "Male" & 
                      DII_Group %in% c("Q1", "Q3"))

# 创建2x2列联表
table_or <- table(
  DII_Group = data_male$DII_Group,  # 行：Q1/Q3（对照组/暴露组）
  Hyperuricemia = data_male$hyperuricemia_count  # 列：无/有（0/1）
)

# 检查表格方向（确保Q1在第一行，病例在第二列）
print(table_or)

# 方法一：直接计算OR（推荐）
or_result <- oddsratio(table_or, method = "wald")
cat("\n--- 基于交叉表的结果 ---\n")
print(or_result$measure[3, ])  # 提取Q3组的OR和CI






# 函数化  （修改为计算total）
calc_or <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii, 
    DII_Group %in% c("Q1", "Q3")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$hyperuricemia_count
  )
  
 
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  final_measure <- or_result$measure[3, ]
  final_p       <- or_result$p.value
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  return(result_df[3, ])
}
calc_or("Sex", "Male")
# 结果如下，没有写入csv
#     OR        lower    upper     p.midp.exact   p.fisher.exact   p.chi.square
#　3  1.109606  1.03126  1.193905  0.005362722    0.005415939      0.005359708



# 函数化
calc_or <- function(var_name, var_value) {

  data_subset <- subset(
    data_dii, 
    get(var_name) == var_value &  DII_Group %in% c("Q1", "Q3")
  )
  

  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$hyperuricemia_count
  )
  
  or_result <- oddsratio(table_or, method = "wald")
  
  final_measure <- or_result$measure[3, ]
  final_p       <- or_result$p.value
  
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/or_result_0221.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[3, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(output_row)
}



calc_or("Sex", "Male")
calc_or("Sex", "Female")
calc_or_23("AgeGroup", "20-29")
calc_or_45("AgeGroup", "40-49")
calc_or_67("AgeGroup", "60-69")
calc_or("AgeGroup", "80+")
calc_or("Race", "Non-Hispanic white")
calc_or("Race", "Non-Hispanic black")
calc_or("Race", "Mexican American")
calc_or("Race", "Others")
calc_or("Education", "Some high school")
calc_or("Education", "High school or GED")
calc_or("Education", "Some college")
calc_or("Education", "College graduate")
calc_or("Marital", "Married or living with a partner")
calc_or("Marital", "Living alone")
calc_or("Income", "≤ 1.0")
calc_or("Income", "1.0 to 2.0")
calc_or("Income", "＞ 2.0")
calc_or("BMI", "Underweight")
calc_or("BMI", "Normal weight")
calc_or("BMI", "Overweight")
calc_or("BMI", "Obesity")
calc_or("smoke_count", "0")
calc_or("smoke_count", "1")
calc_or("Alchohol", "NO")
calc_or("Alchohol", "YES")
calc_or("DSD", "NO")
calc_or("DSD", "YES")
calc_or("Diabetes", "No")
calc_or("Diabetes", "Yes")
calc_or("Obesity", "No")
calc_or("Obesity", "Yes")
calc_or("Hypercholesterolemia", "No")
calc_or("Hypercholesterolemia", "Yes")
calc_or("Hypertension", "No")
calc_or("Hypertension", "Yes")
calc_or("HeartDisease", "No")
calc_or("HeartDisease", "Yes")
calc_or("GFR", "GFR ≥ 90mL/min")
calc_or("GFR", "GFR 60 to 89mL/min")
calc_or("GFR", "GFR 30 to 59mL/min")
calc_or("GFR", "GFR < 30mL/min")





# 为年龄组修改的函数化
calc_or_23 <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii,
    data_dii[[var_name]] %in% c(var_value, "30-39") & DII_Group %in% c("Q1", "Q3")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$hyperuricemia_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[3, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/or_result_0221.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[3, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}

# 为年龄组修改的函数化
calc_or_45 <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii,
    data_dii[[var_name]] %in% c(var_value, "50-59") & DII_Group %in% c("Q1", "Q3")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$hyperuricemia_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[3, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/or_result_0221.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[3, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}

# 为年龄组修改的函数化
calc_or_67 <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii,
    data_dii[[var_name]] %in% c(var_value, "70-79") & DII_Group %in% c("Q1", "Q3")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$hyperuricemia_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[3, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/or_result_0221.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[3, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}






































# 按照高尿酸分类计算膳食摄入
# 目标营养素列
all_target_cols <- c(
  "DRXTKCAL",    # 总能量（不调整）
  "DRXTPROT",    # 蛋白质
  "DRXTTFAT",    # 总脂肪
  "DRXTSFAT",    # 饱和脂肪
  "DRXTCARB",    # 碳水化合物
  "DRXTFIBE",    # 纤维
  "DRXTCHOL",    # 胆固醇
  "DRXTVARE",    # 维生素A
  "DRXTVB1",     # 维生素B1（硫胺素）
  "DRXTVB2",     # 维生素B2（核黄素）
  "DRXTVB6",     # 维生素B6
  "DRXTVB12",    # 维生素B12
  "DRXTFOLA",    # 叶酸
  "DRXTNIAC",    # 烟酸
  "DRXTVC",      # 维生素C
  "DRXTVE",      # 维生素E
  "DRXTMAGN",    # 镁
  "DRXTIRON",    # 铁
  "DRXTZINC",    # 锌
  "DRXTSELE",    # 硒
  "DRXTCAFF",    # 咖啡因
  "DRXTALCO",    # 酒精
  "DRXTMFAT",    # 单不饱和脂肪酸
  "DRXTPFAT"     # 多不饱和脂肪酸
)

# ----------------------------------
# 步骤1: 预处理数据（能量调整其他列，保留原始总能量）
# ----------------------------------
data_adjusted <- data_dii %>%
  mutate(
    across(
      .cols = all_of(all_target_cols[all_target_cols != "DRXTKCAL"]),  # 排除总能量列进行单独处理
      .fns = ~ (. / DRXTKCAL) * 1000, 
      .names = "{.col}_per_1e3kcal"
    )
  )

# 获取所有调整后的列名和新加入的总能量原始列
selected_cols <- c(
  "DRXTKCAL",                        # 原始列
  paste0(
    all_target_cols[all_target_cols != "DRXTKCAL"], 
    "_per_1e3kcal"                   # 调整后列
  )
)

# 清除所有卡路里为0的项 
data_adjusted <- data_adjusted %>%
  filter(DRXTKCAL > 0, !is.na(DRXTKCAL))

# ----------------------------------
# 步骤2: 分组计算Mean±SD（包含总能量）并为所有变量添加p值
# ----------------------------------
result <- data_adjusted %>%
  filter(hyperuricemia_count %in% c(0, 1)) %>%
  group_by(hyperuricemia_count) %>%
  summarise(
    across(
      all_of(selected_cols),         # 含总能量和调整后的营养素
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -hyperuricemia_count,
    names_to = c("variable", "stat"),
    names_sep = "\\."
  ) %>%
  pivot_wider(
    names_from = c(hyperuricemia_count, stat),
    values_from = value,
    names_glue = "Group_{hyperuricemia_count}_{stat}"
  ) %>%
  mutate(
    Non_Hyperuricemia = ifelse(
      grepl("_per_1e3kcal", variable), 
      sprintf("%.3f±%.3f", Group_0_mean, Group_0_sd),     # 调整后的列：2位小数
      sprintf("%.3f±%.3f", Group_0_mean, Group_0_sd)      # 原始总能量：整数格式
    ),
    Hyperuricemia = ifelse(
      grepl("_per_1e3kcal", variable), 
      sprintf("%.3f±%.3f", Group_1_mean, Group_1_sd),
      sprintf("%.3f±%.3f", Group_1_mean, Group_1_sd)
    )
  ) 

# ----------------------------------
# 步骤3: 添加p值列（需独立处理总能量与其他变量）
# ----------------------------------
calculate_p_value <- function(var) {
  formula <- as.formula(paste(var, "~ hyperuricemia_count"))
  if (var == "DRXTKCAL") {
    t.test(formula, data = data_dii %>% filter(hyperuricemia_count %in% c(0, 1)))$p.value
  } else {
    t.test(formula, data = data_adjusted %>% filter(hyperuricemia_count %in% c(0, 1)))$p.value
  }
}

result$p_value <- sapply(selected_cols, calculate_p_value) %>% sprintf("%.3f", .)

# ----------------------------------
# 步骤4: 格式化最终结果（添加中英文名称和单位）
# ----------------------------------

# 创建营养素名称和单位的映射表（需根据实际数据补充完整）
nutrient_mapping <- tibble::tribble(
  ~Original_Name,             ~Chinese_Name,        ~English_Name_Unit,
  "DRXTKCAL",                 "总能量",             "Total Energy (kcal)",
  "DRXTPROT_per_1e3kcal",     "蛋白质",             "Protein (g)",
  "DRXTTFAT_per_1e3kcal",     "总脂肪",             "Total Fat (g)",
  "DRXTSFAT_per_1e3kcal",     "饱和脂肪酸",         "Saturated Fat (g)",
  "DRXTCARB_per_1e3kcal",     "碳水化合物",         "Carbohydrate (g)",
  "DRXTFIBE_per_1e3kcal",     "膳食纤维",           "Fiber (g)",
  "DRXTCHOL_per_1e3kcal",     "胆固醇",             "Cholesterol (mg)",
  "DRXTVARE_per_1e3kcal",     "维生素A",            "Vitamin A (mcg RAE)",
  "DRXTVB1_per_1e3kcal",      "维生素B1（硫胺素）", "Vitamin B1 (mg)",
  "DRXTVB2_per_1e3kcal",      "维生素B2（核黄素）", "Vitamin B2 (mg)",
  "DRXTVB6_per_1e3kcal",      "维生素B6",           "Vitamin B6 (mg)",
  "DRXTVB12_per_1e3kcal",     "维生素B12",          "Vitamin B12 (mcg)",
  "DRXTFOLA_per_1e3kcal",     "叶酸",               "Folate (mcg)",
  "DRXTNIAC_per_1e3kcal",     "烟酸",               "Niacin (mg)",
  "DRXTVC_per_1e3kcal",       "维生素C",            "Vitamin C (mg)",
  "DRXTVE_per_1e3kcal",       "维生素E",            "Vitamin E (mg)",
  "DRXTMAGN_per_1e3kcal",     "镁",                 "Magnesium (mg)",
  "DRXTIRON_per_1e3kcal",     "铁",                 "Iron (mg)",
  "DRXTZINC_per_1e3kcal",     "锌",                 "Zinc (mg)",
  "DRXTSELE_per_1e3kcal",     "硒",                 "Selenium (mcg)",
  "DRXTCAFF_per_1e3kcal",     "咖啡因",             "Caffeine (mg)",
  "DRXTALCO_per_1e3kcal",     "酒精",               "Alcohol (g)",
  "DRXTMFAT_per_1e3kcal",     "单不饱和脂肪酸",     "MUFAs (g)",
  "DRXTPFAT_per_1e3kcal",     "多不饱和脂肪酸",     "PUFAs (g)"
)

# 合并映射信息到结果中
final_result <- result %>%
  dplyr::select(
    Variable = variable,
    Non_Hyperuricemia,
    Hyperuricemia,
    p_value
  ) %>%
  dplyr::left_join(nutrient_mapping, by = c("Variable" = "Original_Name")) %>%  # 联立映射表
  mutate(
    Variable = ifelse(  # 格式化原始变量名
      Variable == "DRXTKCAL",
      "Total_Energy (kcal)",
      gsub("_per_1e3kcal", "", Variable)
    )
  ) %>%
  dplyr::select(
    Original_Variable = Variable,
    Chinese_Name,
    English_Name_Unit,
    Non_Hyperuricemia,
    Hyperuricemia,
    p_value
  )  # 调整列顺序


print(final_result)
write.csv(final_result, "D:/data_analyse/csv/2/dietary_intakes.csv", fileEncoding = "GBK", row.names = FALSE)












































# 从此开始是使用同样数据集的第二篇论文，主要关于痛风

# 计算痛风发病率 95CI
cases <- sum(data_dii_07_18$gout_count == 1)  # 发病人数
rate <- cases / n  # 发病率

# 计算95% CI
Z <- 1.96  # 95%置信水平
SE <- sqrt(rate * (1 - rate) / n)  # 标准误差
CI_lower <- rate - Z * SE
CI_upper <- rate + Z * SE

# 确保区间不超出[0,1]
CI_lower <- max(0, CI_lower)
CI_upper <- min(1, CI_upper)

# 输出结果
cat("发病率:", rate, "\n")
cat("95% CI:", CI_lower, "-", CI_upper, "\n")
# 发病率: 0.02323846 
# 95% CI: 0.02174045 - 0.02473646 









# 计算SE 以DII为例
# 计算均值
mean_value <- mean(data_dii_07_18$Total_DII_Adjusted)
# 计算标准差 (SD)
sd_value <- sd(data_dii_07_18$Total_DII_Adjusted)
# 计算标准误差 (SE)
n <- length(data_dii_07_18$Total_DII_Adjusted)
se_value <- sd_value / sqrt(n)
# 输出结果
cat("Mean ± SE:", mean_value, "±", se_value, "\n")
# Mean ± SE: -2.027475 ± 0.0406385 






# 计算年龄SE
mean_value <- mean(data_dii_07_18$RIDAGEYR)
# 计算标准差 (SD)
sd_value <- sd(data_dii_07_18$RIDAGEYR)
# 计算标准误差 (SE)
n <- length(data_dii_07_18$RIDAGEYR)
se_value <- sd_value / sqrt(n)
# 输出结果
cat("Mean ± SE:", mean_value, "±", se_value, "\n")
# Mean ± SE: 49.2351 ± 0.1227127 























# 计算10年间（2007-2016）患病率曲线
# 使用cut()函数创建分组列
selected_data <- data_dii_07_18[, c("SEQN", "RIAGENDR", "gout_count")]
breaks <- c(41474, 51623, 62160, 71916, 83731, 93702)  # 定义分组区间
labels <- c("2007-2008", "2009-2010", "2011-2012", "2013-2014", "2015-2016")  # 定义分组标签
selected_data$group <- cut(selected_data$SEQN, breaks = breaks, labels = labels, include.lowest = TRUE)

# 使用split()函数将数据框按分组列拆分为多个数据集
grouped_datasets <- split(selected_data, selected_data$group)

# 查看分组后的数据集
names(grouped_datasets)  # 查看分组名称
head(grouped_datasets[[1]])  # 查看第一个分组的数据集
grouped_datasets[["2007-2008"]]
str(grouped_datasets)

# 定义一个计算患病率与置信区间的函数
calculate_prevalence <- function(data, group_name) {
  # 计算总体患病率
  total_ci <- binom.confint(
    x = sum(data$gout_count==1),
    n = nrow(data),
    methods = "wilson"
  )
  
  # 计算男性患病率
  male_data <- data %>% filter(RIAGENDR==1)
  male_ci <- if (nrow(male_data) > 0) {
    binom.confint(
      x = sum(male_data$gout_count == 1),
      n = nrow(male_data),
      methods = "wilson"
    )
  } else {
    data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  # 计算女性患病率
  female_data <- data %>% filter(RIAGENDR==2)
  female_ci <- if (nrow(female_data) > 0) {
    binom.confint(
      x = sum(female_data$gout_count==1),
      n = nrow(female_data),
      methods = "wilson"
    )
  } else {
    data.frame(mean = NA, lower = NA, upper = NA)
  }
  
  # 构建结果数据框
  data.frame(
    Group = group_name,
    Total_Prev = sprintf("%.1f%% (%.1f-%.1f)", 
                         total_ci$mean * 100,
                         total_ci$lower * 100,
                         total_ci$upper * 100),
    Male_Prev = sprintf("%.1f%% (%.1f-%.1f)", 
                        male_ci$mean * 100,
                        male_ci$lower * 100,
                        male_ci$upper * 100),
    Female_Prev = sprintf("%.1f%% (%.1f-%.1f)", 
                          female_ci$mean * 100,
                          female_ci$lower * 100,
                          female_ci$upper * 100)
  )
}

# 对每个分组进行计算
result <- imap_dfr(
  grouped_datasets,
  ~ calculate_prevalence(.x, .y)
) 

# 查看结果
print(result, right = FALSE)
write.csv(result, file = "D:/data_analyse/csv/2/gout/sheet2/prevalence_results.csv", row.names = FALSE)






































# 计算OR最终森林图
# 先计算DII四分位
quantiles <- quantile(data_dii_07_18$Total_DII_Adjusted,
                      probs = c(0, 0.25, 0.5, 0.75, 1),
                      na.rm = TRUE)

# 创建分组（包含区间端点的最小值）
data_dii_07_18$DII_Group <- cut(
  data_dii_07_18$Total_DII_Adjusted,
  breaks = quantiles,
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,  # 确保包含最小值
  right = FALSE           # 左闭右开区间 [a,b)
)

# 未经数据整合版 创建分组（包含区间端点的最小值）
quantiles <- quantile(data_dii_07_18$Total_DII,
                      probs = c(0, 0.25, 0.5, 0.75, 1),
                      na.rm = TRUE)
data_dii_07_18$DII_Group <- cut(
  data_dii_07_18$Total_DII,
  breaks = quantiles,
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,  # 确保包含最小值
  right = FALSE           # 左闭右开区间 [a,b)
)

# 验证分组效果
cat("各分组样本数：\n")
print(table(data_dii_07_18$DII_Group))

cat("\n实际分割阈值（原始标度）：\n")
print(quantiles)
# > print(quantiles)
# 0%        25%        50%        75%       100% 
# -19.808896  -5.416226  -1.064525   2.264095   9.987097 



# 开始计算OR
# 筛选男性且UA分组为Q1/Q3的数据
data_male <- subset(data_dii_07_18, 
                    Sex == "Male" & 
                      DII_Group %in% c("Q1", "Q4"))

# 创建2x2列联表
table_or <- table(
  DII_Group = data_male$DII_Group,  # 行：Q1/Q3（对照组/暴露组）
  Hyperuricemia = data_male$hyperuricemia_count  # 列：无/有（0/1）
)

# 检查表格方向（确保Q1在第一行，病例在第二列）
print(table_or)

# 方法一：直接计算OR（推荐）
or_result <- oddsratio(table_or, method = "wald")
cat("\n--- 基于交叉表的结果 ---\n")
print(or_result$measure[4, ])  # 提取Q3组的OR和CI






# 函数化  （修改为计算total）
calc_or <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18, 
    DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  # final_measure <- or_result$measure[3, ]
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  return(result_df[4, ])
  # return(result_df)
}
calc_or("Sex", "Male")
# 结果如下，没有写入csv
# OR         lower      upper     p.midp.exact   p.fisher.exact  p.chi.square
# 0.8930827  0.7352032  1.084866  0.2549248      0.2755873       0.2543671



# 函数化
calc_or <- function(var_name, var_value) {
  
  data_subset <- subset(
    data_dii_07_18, 
    get(var_name) == var_value &  DII_Group %in% c("Q1", "Q4")
  )
  
  
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  or_result <- oddsratio(table_or, method = "wald")
  
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0306.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(output_row)
}


# 插入一条，筛选并计算各亚组发病率
selected_data <- data_dii_07_18[, c("Gout", "Sex", "AgeGroup", "Race", "Education", "Marital", "Income", "BMI", "smoke_count", "Alchohol", "DSD", "hyperuricemia_count", "Diabetes", "Obesity", "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")]
write.csv(selected_data, "D:/data_analyse/csv/2/gout/sheet3/prevalence.csv", fileEncoding = "GBK", row.names = FALSE)


calc_or("Sex", "Male")
calc_or("Sex", "Female")
calc_or_23("AgeGroup", "20-29")
calc_or_45("AgeGroup", "40-49")
calc_or_67("AgeGroup", "60-69")
calc_or("AgeGroup", "80+")
calc_or("Race", "Non-Hispanic white")
calc_or("Race", "Non-Hispanic black")
calc_or("Race", "Mexican American")
calc_or("Race", "Others")
calc_or("Education", "Some high school")
calc_or("Education", "High school or GED")
calc_or("Education", "Some college")
calc_or("Education", "College graduate")
calc_or("Marital", "Married or living with a partner")
calc_or("Marital", "Living alone")
calc_or("Income", "≤ 1.0")
calc_or("Income", "1.0 to 2.0")
calc_or("Income", "＞ 2.0")
calc_or("BMI", "Underweight")
calc_or("BMI", "Normal weight")
calc_or("BMI", "Overweight")
calc_or("BMI", "Obesity")
calc_or("smoke_count", "0")
calc_or("smoke_count", "1")
calc_or("Alchohol", "NO")
calc_or("Alchohol", "YES")
calc_or("DSD", "NO")
calc_or("DSD", "YES")
calc_or("hyperuricemia_count", "0")
calc_or("hyperuricemia_count", "1")
calc_or("Diabetes", "No")
calc_or("Diabetes", "Yes")
calc_or("Obesity", "No")
calc_or("Obesity", "Yes")
calc_or("Hypercholesterolemia", "No")
calc_or("Hypercholesterolemia", "Yes")
calc_or("Hypertension", "No")
calc_or("Hypertension", "Yes")
calc_or("HeartDisease", "No")
calc_or("HeartDisease", "Yes")
calc_or("GFR", "GFR ≥ 90mL/min")
calc_or("GFR", "GFR 60 to 89mL/min")
calc_or("GFR", "GFR 30 to 59mL/min")
calc_or("GFR", "GFR < 30mL/min")





# 为年龄组修改的函数化
calc_or_23 <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18,
    data_dii_07_18[[var_name]] %in% c(var_value, "30-39") & DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$gout_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0306.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}

# 为年龄组修改的函数化
calc_or_45 <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18,
    data_dii_07_18[[var_name]] %in% c(var_value, "50-59") & DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0306.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}

# 为年龄组修改的函数化
calc_or_67 <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18,
    data_dii_07_18[[var_name]] %in% c(var_value, "70-79") & DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0306.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}






































# 按照痛风分类计算膳食摄入
# 目标营养素列
all_target_cols <- c(
  "DRXTKCAL",    # 总能量（不调整）
  "DRXTPROT",    # 蛋白质
  "DRXTTFAT",    # 总脂肪
  "DRXTSFAT",    # 饱和脂肪
  "DRXTCARB",    # 碳水化合物
  "DRXTFIBE",    # 纤维
  "DRXTCHOL",    # 胆固醇
  "DRXTVARE",    # 维生素A
  "DRXTVB1",     # 维生素B1（硫胺素）
  "DRXTVB2",     # 维生素B2（核黄素）
  "DRXTVB6",     # 维生素B6
  "DRXTVB12",    # 维生素B12
  "DRXTFOLA",    # 叶酸
  "DRXTNIAC",    # 烟酸
  "DRXTVC",      # 维生素C
  "DRXTVE",      # 维生素E
  "DRXTMAGN",    # 镁
  "DRXTIRON",    # 铁
  "DRXTZINC",    # 锌
  "DRXTSELE",    # 硒
  "DRXTCAFF",    # 咖啡因
  "DRXTALCO",    # 酒精
  "DRXTMFAT",    # 单不饱和脂肪酸
  "DRXTPFAT"     # 多不饱和脂肪酸
)

# ----------------------------------
# 步骤1: 预处理数据（能量调整其他列，保留原始总能量）
# ----------------------------------
data_adjusted <- data_dii_07_18 %>%
  mutate(
    across(
      .cols = all_of(all_target_cols[all_target_cols != "DRXTKCAL"]),  # 排除总能量列进行单独处理
      .fns = ~ (. / DRXTKCAL) * 1000, 
      .names = "{.col}_per_1e3kcal"
    )
  )

# 获取所有调整后的列名和新加入的总能量原始列
selected_cols <- c(
  "DRXTKCAL",                        # 原始列
  paste0(
    all_target_cols[all_target_cols != "DRXTKCAL"], 
    "_per_1e3kcal"                   # 调整后列
  )
)

# 清除所有卡路里为0的项 
data_adjusted <- data_adjusted %>%
  filter(DRXTKCAL > 0, !is.na(DRXTKCAL))

# ----------------------------------
# 步骤2: 分组计算Mean±SD（包含总能量）并为所有变量添加p值
# ----------------------------------
result <- data_adjusted %>%
  filter(gout_count %in% c(0, 1)) %>%
  group_by(gout_count) %>%
  summarise(
    across(
      all_of(selected_cols),         # 含总能量和调整后的营养素
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -gout_count,
    names_to = c("variable", "stat"),
    names_sep = "\\."
  ) %>%
  pivot_wider(
    names_from = c(gout_count, stat),
    values_from = value,
    names_glue = "Group_{gout_count}_{stat}"
  ) %>%
  mutate(
    Non_Gout = ifelse(
      grepl("_per_1e3kcal", variable), 
      sprintf("%.3f±%.3f", Group_0_mean, Group_0_sd),     # 调整后的列：2位小数
      sprintf("%.3f±%.3f", Group_0_mean, Group_0_sd)      # 原始总能量：整数格式
    ),
    Gout = ifelse(
      grepl("_per_1e3kcal", variable), 
      sprintf("%.3f±%.3f", Group_1_mean, Group_1_sd),
      sprintf("%.3f±%.3f", Group_1_mean, Group_1_sd)
    )
  ) 

# ----------------------------------
# 步骤3: 添加p值列（需独立处理总能量与其他变量）
# ----------------------------------
calculate_p_value <- function(var) {
  formula <- as.formula(paste(var, "~ gout_count"))
  if (var == "DRXTKCAL") {
    t.test(formula, data = data_dii_07_18 %>% filter(gout_count %in% c(0, 1)))$p.value
  } else {
    t.test(formula, data = data_adjusted %>% filter(gout_count %in% c(0, 1)))$p.value
  }
}

result$p_value <- sapply(selected_cols, calculate_p_value) %>% sprintf("%.3f", .)

# ----------------------------------
# 步骤4: 格式化最终结果（添加中英文名称和单位）
# ----------------------------------

# 创建营养素名称和单位的映射表（需根据实际数据补充完整）
nutrient_mapping <- tibble::tribble(
  ~Original_Name,             ~Chinese_Name,        ~English_Name_Unit,
  "DRXTKCAL",                 "总能量",             "Total Energy (kcal)",
  "DRXTPROT_per_1e3kcal",     "蛋白质",             "Protein (g)",
  "DRXTTFAT_per_1e3kcal",     "总脂肪",             "Total Fat (g)",
  "DRXTSFAT_per_1e3kcal",     "饱和脂肪酸",         "Saturated Fat (g)",
  "DRXTCARB_per_1e3kcal",     "碳水化合物",         "Carbohydrate (g)",
  "DRXTFIBE_per_1e3kcal",     "膳食纤维",           "Fiber (g)",
  "DRXTCHOL_per_1e3kcal",     "胆固醇",             "Cholesterol (mg)",
  "DRXTVARE_per_1e3kcal",     "维生素A",            "Vitamin A (mcg RAE)",
  "DRXTVB1_per_1e3kcal",      "维生素B1（硫胺素）", "Vitamin B1 (mg)",
  "DRXTVB2_per_1e3kcal",      "维生素B2（核黄素）", "Vitamin B2 (mg)",
  "DRXTVB6_per_1e3kcal",      "维生素B6",           "Vitamin B6 (mg)",
  "DRXTVB12_per_1e3kcal",     "维生素B12",          "Vitamin B12 (mcg)",
  "DRXTFOLA_per_1e3kcal",     "叶酸",               "Folate (mcg)",
  "DRXTNIAC_per_1e3kcal",     "烟酸",               "Niacin (mg)",
  "DRXTVC_per_1e3kcal",       "维生素C",            "Vitamin C (mg)",
  "DRXTVE_per_1e3kcal",       "维生素E",            "Vitamin E (mg)",
  "DRXTMAGN_per_1e3kcal",     "镁",                 "Magnesium (mg)",
  "DRXTIRON_per_1e3kcal",     "铁",                 "Iron (mg)",
  "DRXTZINC_per_1e3kcal",     "锌",                 "Zinc (mg)",
  "DRXTSELE_per_1e3kcal",     "硒",                 "Selenium (mcg)",
  "DRXTCAFF_per_1e3kcal",     "咖啡因",             "Caffeine (mg)",
  "DRXTALCO_per_1e3kcal",     "酒精",               "Alcohol (g)",
  "DRXTMFAT_per_1e3kcal",     "单不饱和脂肪酸",     "MUFAs (g)",
  "DRXTPFAT_per_1e3kcal",     "多不饱和脂肪酸",     "PUFAs (g)"
)
print(head(result))


# 合并映射信息到结果中
final_result <- result %>%
  dplyr::select(
    Variable = variable,
    Non_Gout,
    Gout,
    p_value
  ) %>%
  dplyr::left_join(nutrient_mapping, by = c("Variable" = "Original_Name")) %>%  # 联立映射表
  mutate(
    Variable = ifelse(  # 格式化原始变量名
      Variable == "DRXTKCAL",
      "Total_Energy (kcal)",
      gsub("_per_1e3kcal", "", Variable)
    )
  ) %>%
  dplyr::select(
    Original_Variable = Variable,
    Chinese_Name,
    English_Name_Unit,
    Non_Gout,
    Gout,
    p_value
  )  # 调整列顺序


print(final_result)
write.csv(final_result, "D:/data_analyse/csv/2/gout/sheet4/dietary_intakes.csv", fileEncoding = "GBK", row.names = FALSE)





































# 按照高尿酸分类计算膳食摄入，相当于是增加高尿酸版本
# 目标营养素列
all_target_cols <- c(
  "DRXTKCAL",    # 总能量（不调整）
  "DRXTPROT",    # 蛋白质
  "DRXTTFAT",    # 总脂肪
  "DRXTSFAT",    # 饱和脂肪
  "DRXTCARB",    # 碳水化合物
  "DRXTFIBE",    # 纤维
  "DRXTCHOL",    # 胆固醇
  "DRXTVARE",    # 维生素A
  "DRXTVB1",     # 维生素B1（硫胺素）
  "DRXTVB2",     # 维生素B2（核黄素）
  "DRXTVB6",     # 维生素B6
  "DRXTVB12",    # 维生素B12
  "DRXTFOLA",    # 叶酸
  "DRXTNIAC",    # 烟酸
  "DRXTVC",      # 维生素C
  "DRXTVE",      # 维生素E
  "DRXTMAGN",    # 镁
  "DRXTIRON",    # 铁
  "DRXTZINC",    # 锌
  "DRXTSELE",    # 硒
  "DRXTCAFF",    # 咖啡因
  "DRXTALCO",    # 酒精
  "DRXTMFAT",    # 单不饱和脂肪酸
  "DRXTPFAT"     # 多不饱和脂肪酸
)

# ----------------------------------
# 步骤1: 预处理数据（能量调整其他列，保留原始总能量）
# ----------------------------------
data_adjusted <- data_dii_07_18 %>%
  mutate(
    across(
      .cols = all_of(all_target_cols[all_target_cols != "DRXTKCAL"]),  # 排除总能量列进行单独处理
      .fns = ~ (. / DRXTKCAL) * 1000, 
      .names = "{.col}_per_1e3kcal"
    )
  )

# 获取所有调整后的列名和新加入的总能量原始列
selected_cols <- c(
  "DRXTKCAL",                        # 原始列
  paste0(
    all_target_cols[all_target_cols != "DRXTKCAL"], 
    "_per_1e3kcal"                   # 调整后列
  )
)

# 清除所有卡路里为0的项 
data_adjusted <- data_adjusted %>%
  filter(DRXTKCAL > 0, !is.na(DRXTKCAL))

# ----------------------------------
# 步骤2: 分组计算Mean±SD（包含总能量）并为所有变量添加p值
# ----------------------------------
result <- data_adjusted %>%
  filter(hyperuricemia_count %in% c(0, 1)) %>%
  group_by(hyperuricemia_count) %>%
  summarise(
    across(
      all_of(selected_cols),         # 含总能量和调整后的营养素
      list(
        mean = ~ mean(., na.rm = TRUE),
        sd = ~ sd(., na.rm = TRUE)
      ),
      .names = "{.col}.{.fn}"
    )
  ) %>%
  pivot_longer(
    cols = -hyperuricemia_count,
    names_to = c("variable", "stat"),
    names_sep = "\\."
  ) %>%
  pivot_wider(
    names_from = c(hyperuricemia_count, stat),
    values_from = value,
    names_glue = "Group_{hyperuricemia_count}_{stat}"
  ) %>%
  mutate(
    Non_Hyperuricemia = ifelse(
      grepl("_per_1e3kcal", variable), 
      sprintf("%.3f±%.3f", Group_0_mean, Group_0_sd),     # 调整后的列：2位小数
      sprintf("%.3f±%.3f", Group_0_mean, Group_0_sd)      # 原始总能量：整数格式
    ),
    Hyperuricemia = ifelse(
      grepl("_per_1e3kcal", variable), 
      sprintf("%.3f±%.3f", Group_1_mean, Group_1_sd),
      sprintf("%.3f±%.3f", Group_1_mean, Group_1_sd)
    )
  ) 

# ----------------------------------
# 步骤3: 添加p值列（需独立处理总能量与其他变量）
# ----------------------------------
calculate_p_value <- function(var) {
  formula <- as.formula(paste(var, "~ hyperuricemia_count"))
  if (var == "DRXTKCAL") {
    t.test(formula, data = data_dii_07_18 %>% filter(hyperuricemia_count %in% c(0, 1)))$p.value
  } else {
    t.test(formula, data = data_adjusted %>% filter(hyperuricemia_count %in% c(0, 1)))$p.value
  }
}

result$p_value <- sapply(selected_cols, calculate_p_value) %>% sprintf("%.3f", .)

# ----------------------------------
# 步骤4: 格式化最终结果（添加中英文名称和单位）
# ----------------------------------

# 创建营养素名称和单位的映射表（需根据实际数据补充完整）
nutrient_mapping <- tibble::tribble(
  ~Original_Name,             ~Chinese_Name,        ~English_Name_Unit,
  "DRXTKCAL",                 "总能量",             "Total Energy (kcal)",
  "DRXTPROT_per_1e3kcal",     "蛋白质",             "Protein (g)",
  "DRXTTFAT_per_1e3kcal",     "总脂肪",             "Total Fat (g)",
  "DRXTSFAT_per_1e3kcal",     "饱和脂肪酸",         "Saturated Fat (g)",
  "DRXTCARB_per_1e3kcal",     "碳水化合物",         "Carbohydrate (g)",
  "DRXTFIBE_per_1e3kcal",     "膳食纤维",           "Fiber (g)",
  "DRXTCHOL_per_1e3kcal",     "胆固醇",             "Cholesterol (mg)",
  "DRXTVARE_per_1e3kcal",     "维生素A",            "Vitamin A (mcg RAE)",
  "DRXTVB1_per_1e3kcal",      "维生素B1（硫胺素）", "Vitamin B1 (mg)",
  "DRXTVB2_per_1e3kcal",      "维生素B2（核黄素）", "Vitamin B2 (mg)",
  "DRXTVB6_per_1e3kcal",      "维生素B6",           "Vitamin B6 (mg)",
  "DRXTVB12_per_1e3kcal",     "维生素B12",          "Vitamin B12 (mcg)",
  "DRXTFOLA_per_1e3kcal",     "叶酸",               "Folate (mcg)",
  "DRXTNIAC_per_1e3kcal",     "烟酸",               "Niacin (mg)",
  "DRXTVC_per_1e3kcal",       "维生素C",            "Vitamin C (mg)",
  "DRXTVE_per_1e3kcal",       "维生素E",            "Vitamin E (mg)",
  "DRXTMAGN_per_1e3kcal",     "镁",                 "Magnesium (mg)",
  "DRXTIRON_per_1e3kcal",     "铁",                 "Iron (mg)",
  "DRXTZINC_per_1e3kcal",     "锌",                 "Zinc (mg)",
  "DRXTSELE_per_1e3kcal",     "硒",                 "Selenium (mcg)",
  "DRXTCAFF_per_1e3kcal",     "咖啡因",             "Caffeine (mg)",
  "DRXTALCO_per_1e3kcal",     "酒精",               "Alcohol (g)",
  "DRXTMFAT_per_1e3kcal",     "单不饱和脂肪酸",     "MUFAs (g)",
  "DRXTPFAT_per_1e3kcal",     "多不饱和脂肪酸",     "PUFAs (g)"
)
print(head(result))


# 合并映射信息到结果中
final_result <- result %>%
  dplyr::select(
    Variable = variable,
    Non_Hyperuricemia,
    Hyperuricemia,
    p_value
  ) %>%
  dplyr::left_join(nutrient_mapping, by = c("Variable" = "Original_Name")) %>%  # 联立映射表
  mutate(
    Variable = ifelse(  # 格式化原始变量名
      Variable == "DRXTKCAL",
      "Total_Energy (kcal)",
      gsub("_per_1e3kcal", "", Variable)
    )
  ) %>%
  dplyr::select(
    Original_Variable = Variable,
    Chinese_Name,
    English_Name_Unit,
    Non_Hyperuricemia,
    Hyperuricemia,
    p_value
  )  # 调整列顺序


print(final_result)
write.csv(final_result, "D:/data_analyse/csv/2/gout/sheet4/dietary_intakes_hyperuricemia.csv", fileEncoding = "GBK", row.names = FALSE)










































# 只计算男性版本
data_dii_07_18_male <- data_dii_07_18 %>% filter(Sex == "Male")
# 计算OR最终森林图
# 先计算DII四分位
quantiles <- quantile(data_dii_07_18_male$Total_DII_Adjusted,
                      probs = c(0, 0.25, 0.5, 0.75, 1),
                      na.rm = TRUE)

# 创建分组（包含区间端点的最小值）
data_dii_07_18_male$DII_Group <- cut(
  data_dii_07_18_male$Total_DII_Adjusted,
  breaks = quantiles,
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,  # 确保包含最小值
  right = FALSE           # 左闭右开区间 [a,b)
)

# 未经数据整合版 创建分组（包含区间端点的最小值）
quantiles <- quantile(data_dii_07_18_male$Total_DII,
                      probs = c(0, 0.25, 0.5, 0.75, 1),
                      na.rm = TRUE)
data_dii_07_18_male$DII_Group <- cut(
  data_dii_07_18_male$Total_DII,
  breaks = quantiles,
  labels = c("Q1", "Q2", "Q3", "Q4"),
  include.lowest = TRUE,  # 确保包含最小值
  right = FALSE           # 左闭右开区间 [a,b)
)

# 验证分组效果
cat("各分组样本数：\n")
print(table(data_dii_07_18_male$DII_Group))

cat("\n实际分割阈值（原始标度）：\n")
print(quantiles)
# > print(quantiles)
# 0%        25%        50%        75%       100% 
# -19.808896  -5.416226  -1.064525   2.264095   9.987097 





# 函数化  （修改为计算total）
calc_or_male <- function() {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18_male, 
    DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  # final_measure <- or_result$measure[3, ]
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  return(result_df[4, ])
  # return(result_df)
}
calc_or_male()
# 结果如下，没有写入csv
# OR         lower      upper     p.midp.exact   p.fisher.exact  p.chi.square
# 1.626971   1.306159   2.026579  1.718434e-05   1.883125e-05    1.195231e-05



# 函数化
calc_or_male <- function(var_name, var_value) {
  
  data_subset <- subset(
    data_dii_07_18_male, 
    get(var_name) == var_value &  DII_Group %in% c("Q1", "Q4")
  )
  
  
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  or_result <- oddsratio(table_or, method = "wald")
  
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0313_male.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(output_row)
}


# 插入一条，筛选并计算各亚组发病率
selected_data <- data_dii_07_18_male[, c("Gout", "AgeGroup", "Race", "Education", "Marital", "Income", "BMI", "smoke_count", "Alchohol", "DSD", "hyperuricemia_count", "Diabetes", "Obesity", "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")]
write.csv(selected_data, "D:/data_analyse/csv/2/gout/sheet3/prevalence_male.csv", fileEncoding = "GBK", row.names = FALSE)


calc_or_male("Sex", "Male")
calc_or_23_male("AgeGroup", "20-29")
calc_or_45_male("AgeGroup", "40-49")
calc_or_67_male("AgeGroup", "60-69")
calc_or_male("AgeGroup", "80+")
calc_or_male("Race", "Non-Hispanic white")
calc_or_male("Race", "Non-Hispanic black")
calc_or_male("Race", "Mexican American")
calc_or_male("Race", "Others")
calc_or_male("Education", "Some high school")
calc_or_male("Education", "High school or GED")
calc_or_male("Education", "Some college")
calc_or_male("Education", "College graduate")
calc_or_male("Marital", "Married or living with a partner")
calc_or_male("Marital", "Living alone")
calc_or_male("Income", "≤ 1.0")
calc_or_male("Income", "1.0 to 2.0")
calc_or_male("Income", "＞ 2.0")
calc_or_male("BMI", "Underweight")
calc_or_male("BMI", "Normal weight")
calc_or_male("BMI", "Overweight")
calc_or_male("BMI", "Obesity")
calc_or_male("smoke_count", "0")
calc_or_male("smoke_count", "1")
calc_or_male("Alchohol", "NO")
calc_or_male("Alchohol", "YES")
calc_or_male("DSD", "NO")
calc_or_male("DSD", "YES")
calc_or_male("hyperuricemia_count", "0")
calc_or_male("hyperuricemia_count", "1")
calc_or_male("Diabetes", "No")
calc_or_male("Diabetes", "Yes")
calc_or_male("Obesity", "No")
calc_or_male("Obesity", "Yes")
calc_or_male("Hypercholesterolemia", "No")
calc_or_male("Hypercholesterolemia", "Yes")
calc_or_male("Hypertension", "No")
calc_or_male("Hypertension", "Yes")
calc_or_male("HeartDisease", "No")
calc_or_male("HeartDisease", "Yes")
calc_or_male("GFR", "GFR ≥ 90mL/min")
calc_or_male("GFR", "GFR 60 to 89mL/min")
calc_or_male("GFR", "GFR 30 to 59mL/min")
calc_or_male("GFR", "GFR < 30mL/min")





# 为年龄组修改的函数化
calc_or_23_male <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18_male,
    data_dii_07_18_male[[var_name]] %in% c(var_value, "30-39") & DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Hyperuricemia = data_subset$gout_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0313_male.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}

# 为年龄组修改的函数化
calc_or_45_male <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18_male,
    data_dii_07_18_male[[var_name]] %in% c(var_value, "50-59") & DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0313_male.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}

# 为年龄组修改的函数化
calc_or_67_male <- function(var_name, var_value) {
  # 1. 筛选数据：只保留 var_name == var_value 且 DII_Group 为 Q1 或 Q3 的行
  data_subset <- subset(
    data_dii_07_18_male,
    data_dii_07_18_male[[var_name]] %in% c(var_value, "70-79") & DII_Group %in% c("Q1", "Q4")
  )
  
  # 2. 构建 2x2 列联表
  table_or <- table(
    DII_Group = data_subset$DII_Group, 
    Gout = data_subset$gout_count
  )
  
  # 3. 查看交叉表（便于确认行列）
  # cat("\n--- 2x2 列联表 ---\n")
  # print(table_or)
  
  # 4. 使用 oddsratio() 计算 OR（默认 Q1 为对照组、Q3 为暴露组）
  or_result <- oddsratio(table_or, method = "wald")
  
  # 5. 提取 Q3 对应的 OR、CI，以及 p-value
  #   - or_result$measure[3, ]：第 3 行通常对应 Q3 对比 Q1 的结果
  #   - or_result$p.value[3, "p.value"]：第 3 行的 p 值（对比 OR=1 的检验）
  final_measure <- or_result$measure[4, ]
  final_p       <- or_result$p.value
  
  # 6. 打印结果
  # cat("\n--- 基于交叉表的结果（Q3 相对于 Q1） ---\n")
  # print(final_measure)
  # cat("\n--- 对应的 p-value ---\n")
  # cat(final_p, "\n")
  
  # 7. 同时将结果作为对象返回（便于后续处理）
  # 例如：可组合在一起返回一个 data.frame
  result_df <- data.frame(
    var   = var_name, 
    value = var_value,
    OR    = final_measure["estimate"],
    lower = final_measure["lower"],
    upper = final_measure["upper"],
    p     = final_p
  )
  
  
  # 安全写入CSV（增加多层级保护）
  file_path <- "D:/data_analyse/csv/2/gout/sheet3/or_result_0313_male.csv"
  need_header <- !file.exists(file_path)
  output_row <- result_df[4, ]
  
  tryCatch({
    write.table(
      output_row,
      file = file_path,
      sep = ",", 
      append = TRUE,       # 固定追加模式
      col.names = need_header,  # 第一次写表头
      row.names = FALSE,
      quote = FALSE
    )
  }, error = function(e) {
    message("写入失败:", e$message)
  })
  
  
  return(result_df[3, ])
}





































# 为男性专门计算四分位 table1 table2 table3


# 计算Table 1 均值和置信区间
# 数据筛选
data_table1_07_18_male <- data_dii_07_18_male
DII_gout_data_07_18_male <- data_dii_07_18_male[data_dii_07_18_male$Gout == "Yes", ]
DII_non_gout_data_07_18_male <- data_dii_07_18_male[data_dii_07_18_male$Gout == "No", ]
DII_hyperuricemia_data_07_18_male <- data_dii_07_18_male[data_dii_07_18_male$Hyperuricemia == "Yes", ]
DII_non_hyperuricemia_data_07_18_male <- data_dii_07_18_male[data_dii_07_18_male$Hyperuricemia == "No", ]

# 定义计算均值和置信区间的函数
calculate_dii_stats_gout <- function(data_table1) {
  
  # 定义计算均值和置信区间的辅助函数
  calculate_mean_ci <- function(data, value_col) {
    mean_value <- mean(data[[value_col]], na.rm = TRUE)  # 计算均值
    sd_value <- sd(data[[value_col]], na.rm = TRUE)  # 计算标准差
    n_value <- sum(!is.na(data[[value_col]]))  # 计算样本量
    
    # 计算 95% 置信区间
    lower_ci <- mean_value - 1.96 * (sd_value / sqrt(n_value))
    upper_ci <- mean_value + 1.96 * (sd_value / sqrt(n_value))
    
    # 返回均值和置信区间
    return(c(Mean = mean_value, Lower_CI = lower_ci, Upper_CI = upper_ci))
  }
  
  # 筛选所有小项 DII 和总 DII 列
  dii_cols <- colnames(data_table1)[grepl("^DII_.*_Adjusted$", colnames(data_table1))]  # 匹配前缀是 "DII_" 且以 "_Adjusted" 结尾的 
  # 将总 DII 列添加到列表中
  dii_cols <- c(dii_cols, "Total_DII_Adjusted")  # 假设总 DII 列名是 "Total_DII_Adjusted"
  
  # 对每个 DII 列计算均值和 95% 置信区间
  dii_stats <- sapply(dii_cols, function(col) {
    calculate_mean_ci(data_table1, col)
  })
  
  # 转置结果，以便按行显示
  dii_stats <- t(dii_stats)
  
  # 创建一个更易读的表格，并添加 "Result" 列
  dii_stats_df <- as.data.frame(dii_stats)
  dii_stats_df$DII_Name <- dii_cols
  
  # 计算 完整 "Result" 列：均值 (置信区间)
  dii_stats_df$FullResult <- paste0(dii_stats_df$Mean, " (", dii_stats_df$Lower_CI, ", ",dii_stats_df$Upper_CI, ")")
  # 计算 简化 "Result" 列：均值 (置信区间)
  dii_stats_df$result <- paste0(round(dii_stats_df$Mean, 3), " (", round(dii_stats_df$Lower_CI, 3), ", ", round(dii_stats_df$Upper_CI, 3), ")")
  
  # 重新排列列的顺序，使其更清晰
  dii_stats_df <- dii_stats_df[, c("DII_Name", "Mean", "Lower_CI", "Upper_CI", "FullResult", "result")]
  
  # 输出
  write.csv(dii_stats_df, file.path("D:/data_analyse/csv/2/gout/table1/", paste0(deparse(substitute(data_table1)), ".csv")), row.names = FALSE)
  
  # 返回结果
  return(dii_stats_df)
}

# 调用函数
calculate_dii_stats_gout(data_table1_07_18_male)
calculate_dii_stats_gout(DII_gout_data_07_18_male)
calculate_dii_stats_gout(DII_non_gout_data_07_18_male)
calculate_dii_stats_gout(DII_hyperuricemia_data_07_18_male)
calculate_dii_stats_gout(DII_non_hyperuricemia_data_07_18_male)

# 计算table1 p值
# 定义函数进行 t 检验
perform_t_tests <- function(data, group_column) {
  # 筛选所有以 DII 开头并以 Adjusted 结尾的列名以及 Total_DII_Adjusted
  dii_columns <- grep("^DII.*Adjusted$", names(data), value = TRUE)
  dii_columns <- c(dii_columns, "Total_DII_Adjusted")
  
  # 初始化一个数据框来存储列名和 p 值
  result_df <- data.frame(
    Column = character(0),  # 存储列名
    P_value = numeric(0)     # 存储 p 值
  )
  
  # 对每一列进行 t 检验并提取列名和 p 值
  for (col in dii_columns) {
    # 执行 t 检验，依据参数选择 group_column
    t_test_result <- t.test(data[[col]] ~ data[[group_column]])
    
    # 获取 p 值并保留三位小数
    # p_value <- round(t_test_result$p.value, 3)
    # 获取完整 p 值
    p_value <- t_test_result$p.value
    
    # 将列名和 p 值添加到结果数据框
    result_df <- rbind(result_df, data.frame(Column = col, P_value = p_value))
  }
  
  # 返回最终的结果数据框
  return(result_df)
}

# 调用函数进行 t 检验（基于 gout_count）
data_table1_07_18_gout_male_p <- perform_t_tests(data_table1_07_18_male, "gout_count")
data_table1_07_18_hyperuricemia_male_p <- perform_t_tests(data_table1_07_18_male, "hyperuricemia_count")

# 打印结果
print(data_table1_07_18_gout_male_p)
print(data_table1_07_18_hyperuricemia_male_p)

# 导出
write.csv(data_table1_07_18_gout_male_p, "D:/data_analyse/csv/2/gout/table1/data_table1_07_18_gout_male_p.csv", row.names = FALSE, na = "")
write.csv(data_table1_07_18_hyperuricemia_male_p, "D:/data_analyse/csv/2/gout/table1/data_table1_07_18_hyperuricemia_male_p.csv", row.names = FALSE, na = "")























































# 计算table2、3    此时原始数据为 data_dii_07_18_male

# 07-18的table2
data_table2_3_male <- data_dii_07_18_male
# 按尿酸值从小到大排序
sorted_data <- data_table2_3_male[order(data_table2_3_male$Total_DII_Adjusted), ]
# 计算总行数和每组大小
n <- nrow(sorted_data)
split_size <- ceiling(n / 4)  # 向上取整，保证4组总行数 = n
# 构造一个与排序后数据相同长度的“分组标签”向量
quartile_vec <- c(
  rep("Q1", split_size),             # 前 split_size 行标记为 Q1
  rep("Q2", split_size),             # 接下来的 split_size 行标记为 Q2
  rep("Q3", split_size),             # 第三段标记为 Q3
  rep("Q4", n - 3 * split_size)      # 剩余的行都标记为 Q4
)
# 将“分组标签”写入 sorted_data 的新列 UA_quartile
sorted_data$UA_quartile <- quartile_vec
# 分别取出四个子数据集
data_table2_3_part1 <- subset(sorted_data, UA_quartile == "Q1")
data_table2_3_part2 <- subset(sorted_data, UA_quartile == "Q2")
data_table2_3_part3 <- subset(sorted_data, UA_quartile == "Q3")
data_table2_3_part4 <- subset(sorted_data, UA_quartile == "Q4")
# 转换为因子
sorted_data$UA_quartile_factor <- factor(
  sorted_data$UA_quartile, 
  levels = c("Q1","Q2","Q3","Q4")
)
# 再将因子转为数值
sorted_data$UA_quartile_num <- as.numeric(sorted_data$UA_quartile_factor)


# 计算分类变量的人数占比
# 定义一个函数，用于计算人数和占比
calculate_proportion <- function(df, variable) {
  df %>%
    group_by(!!sym(variable)) %>%      # 按分类变量分组
    summarise(n = n(),                 # 计算人数
              percent = round(n / nrow(df) * 100, 1)) %>%  # 计算占比，保留一位小数
    mutate(category = as.character(!!sym(variable)),
           percent = format(percent, nsmall = 1)) %>%     # 强制保留一位小数
    select(category, n, percent)       # 输出分类标签、人数、占比
}
# 定义分类变量列表（排除数值型变量）
categorical_vars <- c("AgeGroup", "Race", "Education", "Marital", "Income", 
                      "BMI", "smoke_count", "Alchohol", "DSD", "Diabetes", "Obesity", 
                      "Hyperuricemia", "Gout", "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")


# 07-18 Q1
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part1, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/gout/table2/07_18_seperate_t2_Q1.csv", row.names = FALSE)


# 07-18 Q2
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part2, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/gout/table2/07_18_seperate_t2_Q2.csv", row.names = FALSE)


# 07-18 Q3
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part3, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/gout/table2/07_18_seperate_t2_Q3.csv", row.names = FALSE)


# 07-18 Q4
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part4, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序 
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果 
print(final_results)
# 保存为单个CSV文件 
write.csv(final_results, "D:/data_analyse/csv/2/gout/table2/07_18_seperate_t2_Q4.csv", row.names = FALSE)

# 计算 BMI 四分位数
bmi_part1_quartiles <- quantile(data_table2_3_part1$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part2_quartiles <- quantile(data_table2_3_part2$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part3_quartiles <- quantile(data_table2_3_part3$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part4_quartiles <- quantile(data_table2_3_part4$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value_part1 <- mean(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
sd_value_part1 <- sd(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
mean_value_part2 <- mean(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
sd_value_part2 <- sd(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
mean_value_part3 <- mean(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
sd_value_part3 <- sd(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
mean_value_part4 <- mean(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
sd_value_part4 <- sd(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
# 计算高尿酸四个部分四分位数
hyperuricemia_part1_quartiles <- quantile(data_table2_3_part1$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part2_quartiles <- quantile(data_table2_3_part2$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part3_quartiles <- quantile(data_table2_3_part3$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
hyperuricemia_part4_quartiles <- quantile(data_table2_3_part4$LBDSUASImg, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(hyperuricemia_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 合并结果为一个数据框
quartiles_df <- data.frame(
  Metric = c("BMI1", "BMI2", "BMI3", "BMI4", "CaloriesMean", "CaloriesSD", "Hyperuricemia1", "Hyperuricemia2", "Hyperuricemia3", "Hyperuricemia4"),
  Q1 = c(bmi_part1_quartiles["Q1"], bmi_part2_quartiles["Q1"], bmi_part3_quartiles["Q1"], bmi_part4_quartiles["Q1"], mean_value_part1, sd_value_part1, hyperuricemia_part1_quartiles["Q1"], hyperuricemia_part2_quartiles["Q1"], hyperuricemia_part3_quartiles["Q1"], hyperuricemia_part4_quartiles["Q1"]),
  Median = c(bmi_part1_quartiles["Median"], bmi_part2_quartiles["Median"], bmi_part3_quartiles["Median"], bmi_part4_quartiles["Median"],mean_value_part2, sd_value_part2, hyperuricemia_part1_quartiles["Median"], hyperuricemia_part2_quartiles["Median"], hyperuricemia_part3_quartiles["Median"], hyperuricemia_part4_quartiles["Median"]),
  Q3 = c(bmi_part1_quartiles["Q3"], bmi_part2_quartiles["Q3"], bmi_part3_quartiles["Q3"], bmi_part4_quartiles["Q3"], mean_value_part3, sd_value_part3, hyperuricemia_part1_quartiles["Q3"], hyperuricemia_part2_quartiles["Q3"], hyperuricemia_part3_quartiles["Q3"], hyperuricemia_part4_quartiles["Q3"]),
  Q4 = c(bmi_part1_quartiles["Q4"], bmi_part2_quartiles["Q4"], bmi_part3_quartiles["Q4"], bmi_part4_quartiles["Q4"], mean_value_part4, sd_value_part4, hyperuricemia_part1_quartiles["Q4"], hyperuricemia_part2_quartiles["Q4"], hyperuricemia_part3_quartiles["Q4"], hyperuricemia_part4_quartiles["Q4"])
)
# 将结果保存为 CSV 文件
write.csv(quartiles_df, "D:/data_analyse/csv/2/gout/table2/07_18_t2_quartiles_summary.csv", row.names = FALSE)
# 显示数据框
print(quartiles_df)









































# 07-18的table3
data_table2_3_male <- data_dii_07_18_male
# 按尿酸值从小到大排序
sorted_data <- data_table2_3_male[order(data_table2_3_male$LBDSUASI), ]
# 计算总行数和每组大小
n <- nrow(sorted_data)
split_size <- ceiling(n / 4)  # 向上取整，保证4组总行数 = n
# 构造一个与排序后数据相同长度的“分组标签”向量
quartile_vec <- c(
  rep("Q1", split_size),             # 前 split_size 行标记为 Q1
  rep("Q2", split_size),             # 接下来的 split_size 行标记为 Q2
  rep("Q3", split_size),             # 第三段标记为 Q3
  rep("Q4", n - 3 * split_size)      # 剩余的行都标记为 Q4
)
# 将“分组标签”写入 sorted_data 的新列 UA_quartile
sorted_data$UA_quartile <- quartile_vec
# 分别取出四个子数据集
data_table2_3_part1 <- subset(sorted_data, UA_quartile == "Q1")
data_table2_3_part2 <- subset(sorted_data, UA_quartile == "Q2")
data_table2_3_part3 <- subset(sorted_data, UA_quartile == "Q3")
data_table2_3_part4 <- subset(sorted_data, UA_quartile == "Q4")
# 转换为因子
sorted_data$UA_quartile_factor <- factor(
  sorted_data$UA_quartile, 
  levels = c("Q1","Q2","Q3","Q4")
)
# 再将因子转为数值
sorted_data$UA_quartile_num <- as.numeric(sorted_data$UA_quartile_factor)


# 计算分类变量的人数占比
# 定义一个函数，用于计算人数和占比
calculate_proportion <- function(df, variable) {
  df %>%
    group_by(!!sym(variable)) %>%      # 按分类变量分组
    summarise(n = n(),                 # 计算人数
              percent = round(n / nrow(df) * 100, 1)) %>%  # 计算占比，保留一位小数
    mutate(category = as.character(!!sym(variable)),
           percent = format(percent, nsmall = 1)) %>%     # 强制保留一位小数
    select(category, n, percent)       # 输出分类标签、人数、占比
}
# 定义分类变量列表（排除数值型变量）
categorical_vars <- c("AgeGroup", "Race", "Education", "Marital", "Income", 
                      "BMI", "smoke_count", "Alchohol", "DSD", "Diabetes", "Obesity", 
                      "Hyperuricemia", "Gout", "Hypercholesterolemia", "Hypertension", "HeartDisease", "GFR")


# 07-18 Q1
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part1, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/gout/table3/07_18_seperate_t3_Q1.csv", row.names = FALSE)

# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value <- mean(data_table2_3$DRXTKCAL, na.rm = TRUE)  # 计算均值
sd_value <- sd(data_table2_3$DRXTKCAL, na.rm = TRUE)      # 计算标准差
# 输出结果
cat("均值 (Mean): ", round(mean_value, 2), "\n")
cat("标准差 (SD): ", round(sd_value, 2), "\n")


# 07-18 Q2
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part2, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/gout/table3/07_18_seperate_t3_Q2.csv", row.names = FALSE)


# 07-18 Q3
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part3, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果
print(final_results)
# 保存为单个CSV文件
write.csv(final_results, "D:/data_analyse/csv/2/gout/table3/07_18_seperate_t3_Q3.csv", row.names = FALSE)


# 07-18 Q4
# 初始化一个空数据框，用于存放所有结果
final_results <- data.frame()
# 遍历所有分类变量，计算人数和占比，并合并到一个数据框
for (var in categorical_vars) {
  temp_result <- calculate_proportion(data_table2_3_part4, var)
  temp_result$Variable <- var  # 添加一个标识列，标记当前变量名称
  final_results <- rbind(final_results, temp_result)  # 将结果逐步合并
}
# 调整列顺序 
final_results <- final_results[, c("Variable", "category", "n", "percent")]
# 格式化结果 
final_results <- final_results %>% mutate(
  result = paste0(n, " (", percent, ")")
)
# 显示合并后的结果 
print(final_results)
# 保存为单个CSV文件 
write.csv(final_results, "D:/data_analyse/csv/2/gout/table3/07_18_seperate_t3_Q4.csv", row.names = FALSE)


# 计算 BMI 四分位数
bmi_part1_quartiles <- quantile(data_table2_3_part1$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part2_quartiles <- quantile(data_table2_3_part2$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part3_quartiles <- quantile(data_table2_3_part3$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
bmi_part4_quartiles <- quantile(data_table2_3_part4$BMXBMI, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(bmi_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 计算卡路里均值（mean）和标准差（sd），同时忽略缺失值NA
mean_value_part1 <- mean(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
sd_value_part1 <- sd(data_table2_3_part1$DRXTKCAL, na.rm = TRUE) 
mean_value_part2 <- mean(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
sd_value_part2 <- sd(data_table2_3_part2$DRXTKCAL, na.rm = TRUE) 
mean_value_part3 <- mean(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
sd_value_part3 <- sd(data_table2_3_part3$DRXTKCAL, na.rm = TRUE) 
mean_value_part4 <- mean(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
sd_value_part4 <- sd(data_table2_3_part4$DRXTKCAL, na.rm = TRUE) 
# 计算DII四个部分四分位数
DII_part1_quartiles <- quantile(data_table2_3_part1$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part1_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part2_quartiles <- quantile(data_table2_3_part2$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part2_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part3_quartiles <- quantile(data_table2_3_part3$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part3_quartiles) <- c("Q1", "Median", "Q3", "Q4")
DII_part4_quartiles <- quantile(data_table2_3_part4$Total_DII_Adjusted, probs = c(0.25, 0.5, 0.75, 1), na.rm = TRUE)
names(DII_part4_quartiles) <- c("Q1", "Median", "Q3", "Q4")
# 合并结果为一个数据框
quartiles_df <- data.frame(
  Metric = c("BMI1", "BMI2", "BMI3", "BMI4", "CaloriesMean", "CaloriesSD", "DII1", "DII2", "DII3", "DII4"),
  Q1 = c(bmi_part1_quartiles["Q1"], bmi_part2_quartiles["Q1"], bmi_part3_quartiles["Q1"], bmi_part4_quartiles["Q1"], mean_value_part1, sd_value_part1, DII_part1_quartiles["Q1"], DII_part2_quartiles["Q1"], DII_part3_quartiles["Q1"], DII_part4_quartiles["Q1"]),
  Median = c(bmi_part1_quartiles["Median"], bmi_part2_quartiles["Median"], bmi_part3_quartiles["Median"], bmi_part4_quartiles["Median"],mean_value_part2, sd_value_part2, DII_part1_quartiles["Median"], DII_part2_quartiles["Median"], DII_part3_quartiles["Median"], DII_part4_quartiles["Median"]),
  Q3 = c(bmi_part1_quartiles["Q3"], bmi_part2_quartiles["Q3"], bmi_part3_quartiles["Q3"], bmi_part4_quartiles["Q3"], mean_value_part3, sd_value_part3, DII_part1_quartiles["Q3"], DII_part2_quartiles["Q3"], DII_part3_quartiles["Q3"], DII_part4_quartiles["Q3"]),
  Q4 = c(bmi_part1_quartiles["Q4"], bmi_part2_quartiles["Q4"], bmi_part3_quartiles["Q4"], bmi_part4_quartiles["Q4"], mean_value_part4, sd_value_part4, DII_part1_quartiles["Q4"], DII_part2_quartiles["Q4"], DII_part3_quartiles["Q4"], DII_part4_quartiles["Q4"])
)
# 将结果保存为 CSV 文件
write.csv(quartiles_df, "D:/data_analyse/csv/2/gout/table3/07_18_t3_quartiles_summary.csv", row.names = FALSE)
# 显示数据框
print(quartiles_df)







# 计算P值
# p值计算
# usage：上面有两个表格，每个表格都可以生成一个sorted_data，运行了哪个表格的所有函数，就可以用下面的方法计算该表格的p值
#        下面分为几个部分，第一个是计算p的函数，接下来分别是：表2的p-overall、表3的p-overall、表23通用的p-trend。也就是说，如果要计算表2，则要运行 表2的p-overall、表23通用的p-trend两个部分，之后运行表2的输出部分即可，表3同理。    

# 2.1 提取 t 检验的 p 值
get_p_ttest <- function(formula, data) {
  fit <- t.test(formula, data = data)
  round(fit$p.value, 3)
}
# 2.2 提取单因素 ANOVA 的 p 值
# 注意：如果只有一个因子，summary(aov())的结果表中，
# “Pr(>F)”的第一个因子对应行[1]，最后一行通常是Residuals
get_p_aov <- function(formula, data) {
  fit <- aov(formula, data = data)
  pval <- summary(fit)[[1]]["Pr(>F)"][1]
  round(pval, 3)
}
# 2.3 提取 Cochran–Armitage 趋势检验的 p 值
get_p_cochran_armitage <- function(tab) {
  # tab 通常是 table(sorted_data$UA_quartile, sorted_data$Sex) 这类
  fit <- CochranArmitageTest(tab)
  round(fit$p.value, 3)
}
# 2.4 提取线性回归的 p 值
# 对于多分类自变量，如果你想获取“整体”p值，要用 Type II/III Anova;
# 这里示例只取回归系数第 2 行的 p 值(假设只有 1 个主要自变量)
get_p_lm <- function(formula, data) {
  fit <- lm(formula, data = data)
  # 系数矩阵 [2,4] 列是 Pr(>|t|)
  round(summary(fit)$coefficients[2, 4], 3)
}





# 表2 p-overall
# 二分类
p_overall_B_bin <- list()
# 婚姻
p_overall_B_bin[["Marital"]] <- get_p_ttest(Total_DII_Adjusted ~ Marital, data = sorted_data)
# 肥胖
p_overall_B_bin[["Obesity"]] <- get_p_ttest(Total_DII_Adjusted ~ Obesity, data = sorted_data)
# 膳食补充
p_overall_B_bin[["DSD"]] <- get_p_ttest(Total_DII_Adjusted ~ DSD, data = sorted_data)
# 酒精
p_overall_B_bin[["Alchohol"]] <- get_p_ttest(Total_DII_Adjusted ~ Alchohol, data = sorted_data)
# 痛风
p_overall_B_bin[["Gout"]] <- get_p_ttest(Total_DII_Adjusted ~ Gout, data = sorted_data)
# 糖尿病
p_overall_B_bin[["Diabetes"]] <- get_p_ttest(Total_DII_Adjusted ~ Diabetes, data = sorted_data)
# 高脂血症
p_overall_B_bin[["Hypercholesterolemia"]] <- get_p_ttest(Total_DII_Adjusted ~ Hypercholesterolemia, data = sorted_data)
# 高血压
p_overall_B_bin[["Hypertension"]] <- get_p_ttest(Total_DII_Adjusted ~ Hypertension, data = sorted_data)
# 冠心病
p_overall_B_bin[["HeartDisease"]] <- get_p_ttest(Total_DII_Adjusted ~ HeartDisease, data = sorted_data)
p_overall_B_bin_df <- data.frame(
  Variable = names(p_overall_B_bin),
  P_value  = unlist(p_overall_B_bin),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_B_bin_df

# 多分类
p_overall_B_multi <- list()
# 年龄
p_overall_B_multi[["AgeGroup"]] <- get_p_aov(Total_DII_Adjusted ~ AgeGroup, data = sorted_data)
# 种族
p_overall_B_multi[["Race"]] <- get_p_aov(Total_DII_Adjusted ~ Race, data = sorted_data)
# 教育
p_overall_B_multi[["Education"]] <- get_p_aov(Total_DII_Adjusted ~ Education, data = sorted_data)
# 贫困
p_overall_B_multi[["Income"]] <- get_p_aov(Total_DII_Adjusted ~ Income, data = sorted_data)
# BMI
p_overall_B_multi[["BMI"]] <- get_p_aov(Total_DII_Adjusted ~ BMI, data = sorted_data)
# GFR
p_overall_B_multi[["GFR"]] <- get_p_aov(Total_DII_Adjusted ~ GFR, data = sorted_data)
p_overall_B_multi_df <- data.frame(
  Variable = names(p_overall_B_multi),
  P_value  = unlist(p_overall_B_multi),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_B_multi_df

# 连续变量
p_overall_B_cont <- list()
# BMI
p_overall_B_cont[["BMXBMI_factor"]] <- get_p_aov(Total_DII_Adjusted ~ factor(BMXBMI), data = sorted_data)
# 卡路里
p_overall_B_cont[["DRXTKCAL_factor"]] <- get_p_aov(Total_DII_Adjusted ~ factor(DRXTKCAL), data = sorted_data)
# 高尿酸
p_overall_B_cont[["LBDSUASI_factor"]] <- get_p_aov(Total_DII_Adjusted ~ factor(LBDSUASI), data = sorted_data)
p_overall_B_cont_df <- data.frame(
  Variable = names(p_overall_B_cont),
  P_value  = unlist(p_overall_B_cont),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_B_cont_df







# 表3 p-overall
# 二分类
p_overall_A_bin <- list()
# 婚姻
p_overall_A_bin[["Marital"]] <- get_p_ttest(LBDSUASI ~ Marital, data = sorted_data)
# 肥胖
p_overall_A_bin[["Obesity"]] <- get_p_ttest(LBDSUASI ~ Obesity, data = sorted_data)
# 膳食补充
p_overall_A_bin[["DSD"]] <- get_p_ttest(LBDSUASI ~ DSD, data = sorted_data)
# 酒精
p_overall_A_bin[["Alchohol"]] <- get_p_ttest(LBDSUASI ~ Alchohol, data = sorted_data)
# 痛风
p_overall_A_bin[["Gout"]] <- get_p_ttest(LBDSUASI ~ Gout, data = sorted_data)
# 糖尿病
p_overall_A_bin[["Diabetes"]] <- get_p_ttest(LBDSUASI ~ Diabetes, data = sorted_data)
# 高脂血症
p_overall_A_bin[["Hypercholesterolemia"]] <- get_p_ttest(LBDSUASI ~ Hypercholesterolemia, data = sorted_data)
# 高血压
p_overall_A_bin[["Hypertension"]] <- get_p_ttest(LBDSUASI ~ Hypertension, data = sorted_data)
# 冠心病
p_overall_A_bin[["HeartDisease"]] <- get_p_ttest(LBDSUASI ~ HeartDisease, data = sorted_data)
p_overall_A_bin_df <- data.frame(
  Variable = names(p_overall_A_bin),
  P_value  = unlist(p_overall_A_bin),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_A_bin_df

# 多分类
p_overall_A_multi <- list()
# 年龄
p_overall_A_multi[["AgeGroup"]] <- get_p_aov(LBDSUASI ~ AgeGroup, data = sorted_data)
# 种族
p_overall_A_multi[["Race"]] <- get_p_aov(LBDSUASI ~ Race, data = sorted_data)
# 教育
p_overall_A_multi[["Education"]] <- get_p_aov(LBDSUASI ~ Education, data = sorted_data)
# 贫困
p_overall_A_multi[["Income"]] <- get_p_aov(LBDSUASI ~ Income, data = sorted_data)
# BMI
p_overall_A_multi[["BMI"]] <- get_p_aov(LBDSUASI ~ BMI, data = sorted_data)
# GFR
p_overall_A_multi[["GFR"]] <- get_p_aov(LBDSUASI ~ GFR, data = sorted_data)
p_overall_A_multi_df <- data.frame(
  Variable = names(p_overall_A_multi),
  P_value  = unlist(p_overall_A_multi),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_A_multi_df

# 连续变量
p_overall_A_cont <- list()
# BMI
p_overall_A_cont[["BMXBMI_factor"]] <- get_p_aov(LBDSUASI ~ factor(BMXBMI), data = sorted_data)
# 卡路里
p_overall_A_cont[["DRXTKCAL_factor"]] <- get_p_aov(LBDSUASI ~ factor(DRXTKCAL), data = sorted_data)
# DII
p_overall_A_cont[["Total_DII_Adjusted_factor"]] <- get_p_aov(LBDSUASI ~ factor(Total_DII_Adjusted), data = sorted_data)
p_overall_A_cont_df <- data.frame(
  Variable = names(p_overall_A_cont),
  P_value  = unlist(p_overall_A_cont),
  stringsAsFactors = FALSE
)
# 查看结果
p_overall_A_cont_df







# 表2、3通用p-trend
# 二分类
p_trend_bin <- list()
# 婚姻
tab_marital <- table(sorted_data$UA_quartile, sorted_data$Marital)
p_trend_bin[["Marital"]] <- get_p_cochran_armitage(tab_marital)
# 肥胖
tab_obesity <- table(sorted_data$UA_quartile, sorted_data$Obesity)
p_trend_bin[["Obesity"]] <- get_p_cochran_armitage(tab_obesity)
# 膳食补充
tab_dsd <- table(sorted_data$UA_quartile, sorted_data$DSD)
p_trend_bin[["DSD"]] <- get_p_cochran_armitage(tab_dsd)
# 酒精
tab_alchohol <- table(sorted_data$UA_quartile, sorted_data$Alchohol)
p_trend_bin[["Alchohol"]] <- get_p_cochran_armitage(tab_alchohol)
# 痛风
tab_gout <- table(sorted_data$UA_quartile, sorted_data$Gout)
p_trend_bin[["Gout"]] <- get_p_cochran_armitage(tab_gout)
# 糖尿病
tab_diabetes <- table(sorted_data$UA_quartile, sorted_data$Diabetes)
p_trend_bin[["Diabetes"]] <- get_p_cochran_armitage(tab_diabetes)
# 高脂血症
tab_hyperchol <- table(sorted_data$UA_quartile, sorted_data$Hypercholesterolemia)
p_trend_bin[["Hypercholesterolemia"]] <- get_p_cochran_armitage(tab_hyperchol)
# 高血压
tab_hypertension <- table(sorted_data$UA_quartile, sorted_data$Hypertension)
p_trend_bin[["Hypertension"]] <- get_p_cochran_armitage(tab_hypertension)
# 冠心病
tab_heartdisease <- table(sorted_data$UA_quartile, sorted_data$HeartDisease)
p_trend_bin[["HeartDisease"]] <- get_p_cochran_armitage(tab_heartdisease)
p_trend_bin_df <- data.frame(
  Variable = names(p_trend_bin),
  P_value  = unlist(p_trend_bin),
  stringsAsFactors = FALSE
)
# 查看结果
p_trend_bin_df

# 多分类和连续变量
p_trend_multi <- list()
# 年龄
p_trend_multi[["AgeGroup"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ AgeGroup, data = sorted_data)
# 种族
p_trend_multi[["Race"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Race, data = sorted_data)
# 教育
p_trend_multi[["Education"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Education, data = sorted_data)
# 贫困
p_trend_multi[["Income"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Income, data = sorted_data)
# BMI
p_trend_multi[["BMI"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ BMI, data = sorted_data)
# GFR
p_trend_multi[["GFR"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ GFR, data = sorted_data)
# 卡路里
p_trend_multi[["DRXTKCAL"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ DRXTKCAL, data = sorted_data)
# BMI
p_trend_multi[["BMXBMI"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ BMXBMI, data = sorted_data)
# 高尿酸
p_trend_multi[["LBDSUASI"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ LBDSUASI, data = sorted_data)
# DII
p_trend_multi[["Total_DII_Adjusted"]] <- get_p_lm(as.numeric(UA_quartile_num) ~ Total_DII_Adjusted, data = sorted_data)
p_trend_multi_df <- data.frame(
  Variable = names(p_trend_multi),
  P_value  = unlist(p_trend_multi),
  stringsAsFactors = FALSE
)
# 查看结果
p_trend_multi_df
# 合并p-trend
p_trend_df <- bind_rows(
  p_trend_bin_df %>% mutate(TestType = "CochranArmitage"),
  p_trend_multi_df %>% mutate(TestType = "LinearRegression")
)
# 查看合并后的结果
p_trend_df





# 输出
# 07-18 t2
df_07_18_t2_bin   <- p_overall_B_bin_df   %>% mutate(AnalysisType = "Binary")
df_07_18_t2_multi <- p_overall_B_multi_df %>% mutate(AnalysisType = "Multi")
df_07_18_t2_cont  <- p_overall_B_cont_df  %>% mutate(AnalysisType = "Continuous")
df_07_18_t2_trend <- p_trend_df           %>% mutate(AnalysisType = "Trend")
df_07_18_t2_all <- dplyr::bind_rows(
  df_07_18_t2_bin,
  df_07_18_t2_multi,
  df_07_18_t2_cont,
  df_07_18_t2_trend
)
write.csv(df_07_18_t2_all, 
          "D:/data_analyse/csv/2/gout/table2/p_07_18_t2.csv", 
          row.names = FALSE)



# 07-18 t3
df_07_18_t3_bin   <- p_overall_A_bin_df   %>% mutate(AnalysisType = "Binary")
df_07_18_t3_multi <- p_overall_A_multi_df %>% mutate(AnalysisType = "Multi")
df_07_18_t3_cont  <- p_overall_A_cont_df  %>% mutate(AnalysisType = "Continuous")
df_07_18_t3_trend <- p_trend_df           %>% mutate(AnalysisType = "Trend")
# 合并为一个数据框
df_07_18_t3_all <- dplyr::bind_rows(
  df_07_18_t3_bin,
  df_07_18_t3_multi,
  df_07_18_t3_cont,
  df_07_18_t3_trend
)
# 一次性输出到 CSV
write.csv(df_07_18_t3_all, 
          "D:/data_analyse/csv/2/gout/table3/p_07_18_t3.csv", 
          row.names = FALSE)


