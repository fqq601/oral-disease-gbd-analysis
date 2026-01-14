# table456----------------------------------------------------------------------------
# 设置工作目录（替换为你的实际路径）
setwd("/Users/fqq/Documents/GBD口腔老年人/全球地区国家三指标数据")


###Table S1######
final_data<-read.csv("Final_combined_data.csv")

new_data <- final_data %>%
  filter(year %in% c(2021) & sex_name == "Both"& cause_name == "Oral disorders"&age_name == "Age-standardized")


new_data <- new_data %>%
  mutate(
    # 除以1000并保留两位小数
    val = round(val, 1),
    lower = round(lower, 1),
    upper = round(upper, 1))


# 创建一个新的列all_ages_number，将val、lower和upper拼接成格式化的字符串
new_data$all_ages_number <- paste0(new_data$val," (",new_data$lower,"-",new_data$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间



# 调整列顺序：将year和Cases列放在location_name后面
new_data <- new_data %>%
  relocate(all_ages_number, .after = location_name)


new_data <- new_data %>% 
  filter(
    !location_name %in% c("Global",
                          # 排除的SDI类别
                          "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI",
                          # 排除的地区列表
                          "East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", 
                          "Eastern Europe", "High-income Asia Pacific", "Australasia", "Western Europe", 
                          "Southern Latin America", "High-income North America", "Caribbean",
                          "Andean Latin America", "Central Latin America", "Tropical Latin America",
                          "North Africa and Middle East", "South Asia", "Central Sub-Saharan Africa", 
                          "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"
    ))


# 将分解结果保存为CSV文件
write.csv(both_de, "ASIR-both.csv")










###Table S1######
final_data<-read.csv("Final_combined_data.csv")

new_data <- final_data %>%
  filter(year %in% c(2021)&cause_name == "Periodontal diseases"&metric_name == "Rate"&age_name == "Age-standardized")


new_data <- new_data %>%
  mutate(
    # 除以1000并保留两位小数
    val = round(val/1000, 2),
    lower = round(lower/1000, 2),
    upper = round(upper/1000, 2))


# 创建一个新的列all_ages_number，将val、lower和upper拼接成格式化的字符串
new_data$all_ages_number <- paste0(new_data$val," (",new_data$lower,"-",new_data$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间



# 调整列顺序：将year和Cases列放在location_name后面
new_data <- new_data %>%
  relocate(all_ages_number, .after = location_name)


new_data <- new_data %>% 
  filter(
    location_name %in% c("Global",
                          # 排除的SDI类别
                          "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI",
                          # 排除的地区列表
                          "East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", 
                          "Eastern Europe", "High-income Asia Pacific", "Australasia", "Western Europe", 
                          "Southern Latin America", "High-income North America", "Caribbean",
                          "Andean Latin America", "Central Latin America", "Tropical Latin America",
                          "North Africa and Middle East", "South Asia", "Central Sub-Saharan Africa", 
                          "Eastern Sub-Saharan Africa", "Southern Sub-Saharan Africa", "Western Sub-Saharan Africa"
    ))




