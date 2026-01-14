############# ASR计算--地区 ###############
# 设置工作目录为数据所在文件夹
setwd("/Users/fqq/Documents/GBD口腔老年人/分解分析")
# 第1步: 加载必要的库
library(dplyr)
library(data.table)

# 第2步: 读取数据文件
data <- fread('Global_region_70-four.csv') # 读取负担数据
weights_data <- fread("/Users/fqq/Documents/GBD口腔老年人/GBD2021-70以上人口权重.csv") # 读取权重数据

data$age_name<-gsub(" years","",data$age_name)
data$age_name <- factor(data$age_name, 
                               levels = c("70-74",
                                          "75-79", "80-84", "85-89", "90-94", "95+"))


# 确保权重数据中的年龄组与负担数据中的年龄组一致
# 如果负担数据中的年龄组有重复，需要先处理

# 第3步: 筛选负担数据中需要计算ASR的年龄组（如果有必要，比如只对特定年龄组标准化）
# 这里假设我们使用权重数据中的年龄组来筛选负担数据
selected_ages <- weights_data$age_name
selected_data <- data %>% filter(age_name %in% selected_ages)

selected_data <- selected_data %>% filter(metric_name == "Rate")


# 第4步: 权重匹配
merged_data <- selected_data %>%
  left_join(weights_data, by = "age_name")

# 检查是否有年龄组匹配失败
if(any(is.na(merged_data$weight))) {
  warning("Some age groups in the burden data do not have weights. They will be removed.")
  merged_data <- merged_data %>% filter(!is.na(weight))
}

# 第5步: 计算年龄标准化率
ASR_results <- merged_data %>%
  group_by(measure_id, measure_name, location_id, location_name,
           sex_id, sex_name, cause_id, cause_name,
           metric_id, metric_name, year) %>%
  summarise(Age_standardized = sum(val * weight, na.rm = TRUE),
            upper = sum(upper * weight, na.rm = TRUE),
            lower = sum(lower * weight, na.rm = TRUE),
            .groups = 'drop')

# 第6步: 修改列名
colnames(ASR_results)[colnames(ASR_results) == "Age_standardized"] <- "val"

# 第7步: 增加列名并保持一致
ASR_results <- ASR_results %>%
  mutate(age_id = 27, 
         age_name = "Age-standardized") %>%
  select(measure_id, measure_name, location_id, location_name,
         sex_id, sex_name, age_id, age_name, cause_id, cause_name,
         metric_id, metric_name, year, val, upper, lower)

# 第8步: 与原始数据合并
Age_standardized_data <- bind_rows(data, ASR_results)

# 第9步: 输出合并后的数据
fwrite(Age_standardized_data, "Age_standardized_data.csv", row.names = FALSE)



############# ASR计算--国家 ###############
# 计算年龄标准化率----------------------------------------------------------------------------
# 设置工作目录（替换为你的实际路径）
setwd("/Users/fqq/Documents/GBD口腔老年人/全球地区国家三指标数据")

# 获取所有CSV文件的路径
csv_files <- list.files(pattern = "\\.csv$", full.names = TRUE)

# 使用lapply读取所有文件
data_list <- lapply(csv_files, function(file) {
  # 读取CSV文件（可根据需要调整参数）
  df <- read.csv(file, stringsAsFactors = FALSE, fileEncoding = "UTF-8")
  return(df)
})

# 合并所有数据框
combined_data <- do.call(rbind, data_list)

# 查看合并后的数据框结构
data <- combined_data
library(data.table)
library(dplyr)
# 读取权重数据
weights_data <- fread("/Users/fqq/Documents/GBD口腔老年人/GBD2021-70以上人口权重.csv")

# 处理年龄组名称
data$age_name <- gsub(" years", "", data$age_name)
data$age_name <- factor(data$age_name, 
                        levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))

# 筛选用于计算年龄标准化率的数据
selected_ages <- weights_data$age_name
selected_data <- data %>% filter(age_name %in% selected_ages)
selected_data <- selected_data %>% filter(metric_name == "Rate")

# 权重匹配并计算年龄标准化率
merged_data <- selected_data %>%
  left_join(weights_data, by = "age_name") %>%
  filter(!is.na(weight))  # 移除无权重的年龄组

ASR_results <- merged_data %>%
  group_by(measure_id, measure_name, location_id, location_name,
           sex_id, sex_name, cause_id, cause_name,
           metric_id, metric_name, year) %>%
  summarise(val = sum(val * weight, na.rm = TRUE),  # 年龄标准化率
            upper = sum(upper * weight, na.rm = TRUE),
            lower = sum(lower * weight, na.rm = TRUE),
            .groups = 'drop') %>%
  mutate(age_id = 27, age_name = "Age-standardized") %>%
  dplyr::select(measure_id, measure_name, location_id, location_name,
                sex_id, sex_name, age_id, age_name, cause_id, cause_name,
                metric_id, metric_name, year, val, upper, lower)

# ----------------------新增：计算All ages的Number指标----------------------
# 筛选metric_name为"Number"的数据
number_data <- data %>%
  filter(metric_name == "Number",
         age_name %in% selected_ages)  # 确保只汇总目标年龄组

# 按分组求和，得到各分组的总人数（All ages）
all_ages_number <- number_data %>%
  group_by(measure_id, measure_name, location_id, location_name,
           sex_id, sex_name, cause_id, cause_name,
           metric_id, metric_name, year) %>%
  summarise(val = sum(val, na.rm = TRUE),  # 各年龄组Number之和
            upper = sum(upper, na.rm = TRUE),  #  upper值求和
            lower = sum(lower, na.rm = TRUE),  #  lower值求和
            .groups = 'drop') %>%
  # 添加All ages对应的标识（根据实际数据的age_id规则设置，这里假设为22）
  mutate(age_id = 22,  # 通常GBD中All ages的age_id为22，可根据实际调整
         age_name = "All ages") %>%
  dplyr::select(measure_id, measure_name, location_id, location_name,
                sex_id, sex_name, age_id, age_name, cause_id, cause_name,
                metric_id, metric_name, year, val, upper, lower)

# ----------------------合并所有结果----------------------
# 合并原始数据、年龄标准化率数据、All ages的Number数据
final_data <- bind_rows(
  data,          # 原始数据
  ASR_results,   # 年龄标准化率数据
  all_ages_number  # All ages的Number数据
)

# 输出最终结果
fwrite(final_data, "Final_combined_data.csv", row.names = FALSE)