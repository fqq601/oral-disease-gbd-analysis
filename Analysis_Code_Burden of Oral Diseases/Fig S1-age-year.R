# 设置工作目录为数据所在文件夹
setwd("/Users/fqq/Documents/GBD口腔老年人/分解分析")
##### 第一步 Incidence ######
# 读取CSV文件中的中国IBD数据
Age_standardized_data <- read.csv('Age_standardized_data.csv',header = T)


# 查看数据中的所有年龄组信息，了解数据分布
unique(Age_standardized_data$age_name)
,
panel.grid.minor = element_blank()

# 隐藏x轴标题
axis.title.x = element_blank()

# 将Number指标的数值除以100,000
Age_standardized_data <- Age_standardized_data %>%
  mutate(
    val = ifelse(metric_name == "Number", val / 100000, val),
    upper = ifelse(metric_name == "Number", upper / 100000, upper),
    lower = ifelse(metric_name == "Number", lower / 100000, lower)
  )



# 加载所需包
library(ggplot2)
library(dplyr)


##### 第一步 Incidence ######
##### 第一步 Both ######
# 数据预处理
plot_Incidence_Both <- Age_standardized_data %>%
  filter(
    measure_name == "Incidence",
    metric_name == "Number",
    sex_name == "Both"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_Incidence_Both <- ggplot(plot_Incidence_Both, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Incidence (x100000)"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_Incidence_Both$year), max(plot_Incidence_Both$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  # 关键修改：固定纵坐标范围为 50,000,000 到 250,000,000
  scale_y_continuous(
    limits = c(0, 3000),  # 明确设置上下限
    breaks = seq(0, 3000, by = 1000),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_Incidence_Both)




##### 第一步 Incidence ######
##### 第一步 Female ######
# 数据预处理
plot_Incidence_Female <- Age_standardized_data %>%
  filter(
    measure_name == "Incidence",
    metric_name == "Number",
    sex_name == "Female"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_Incidence_Female <- ggplot(plot_Incidence_Female, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Incidence in Global"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_Incidence_Female$year), max(plot_Incidence_Female$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  # 关键修改：固定纵坐标范围为 50,000,000 到 250,000,000
  scale_y_continuous(
    limits = c(0, 3000),  # 明确设置上下限
    breaks = seq(0, 3000, by = 1000),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    legend.position = "right"
  )

print(p_Incidence_Female)




##### 第一步 Incidence ######
##### 第一步 Male ######
# 数据预处理
plot_Incidence_Male <- Age_standardized_data %>%
  filter(
    measure_name == "Incidence",
    metric_name == "Number",
    sex_name == "Male"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_Incidence_Male <- ggplot(plot_Incidence_Male, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Incidence in Global"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_Incidence_Male$year), max(plot_Incidence_Male$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  # 关键修改：固定纵坐标范围为 50,000,000 到 250,000,000
  scale_y_continuous(
    limits = c(0, 3000),  # 明确设置上下限
    breaks = seq(0, 3000, by = 1000),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 8),
    legend.position = "right"
  )

print(p_Incidence_Male)



# 加载所需包
library(patchwork)
library(ggplot2)

# 假设已有的三张图为 p1、p2、p3

# 调整每张图的主题，只保留左侧图的纵轴
p_Incidence_Both <- p_Incidence_Both + 
  theme(
    axis.title = element_text(size = 20),  # 坐标轴标题字体大小
    axis.text = element_text(size = 16)
  )

p_Incidence_Female <- p_Incidence_Female + 
  theme(
    # 隐藏中间图的纵轴标题和刻度
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # 隐藏中间图纵轴刻度线
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 16)    # 坐标轴刻度文字字体大小
  )

p_Incidence_Male <- p_Incidence_Male + 
  theme(
    # 隐藏右侧图的纵轴标题和刻度
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # 隐藏中间图纵轴刻度线
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 16)
  )

# 不使用cowplot的替代方案
combined_plot_Incidence <- p_Incidence_Both + p_Incidence_Female + p_Incidence_Male +
  plot_layout(widths = c(1, 1, 1), guides = "collect") +
  plot_annotation(
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 8)))
  ) &
  theme(legend.position = "right",
        legend.text = element_text(size = 14),  # 图例文字字体大小
        legend.title = element_text(size = 16),# 图例标题字体大小，如果有标题的话
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.03, 0.03, 0.03, 0.03), "cm"))
# 显示组合图
print(combined_plot_Incidence)






##### 第一步 Prevalence ######
##### 第一步 Both ######
# 数据预处理
plot_Prevalence_Both <- Age_standardized_data %>%
  filter(
    measure_name == "Prevalence",
    metric_name == "Number",
    sex_name == "Both"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_Prevalence_Both <- ggplot(plot_Prevalence_Both, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Prevalence (x100000)"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_Prevalence_Both$year), max(plot_Prevalence_Both$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_Prevalence_Both)





##### 第一步 Prevalence ######
##### 第一步 Female ######
# 数据预处理
plot_Prevalence_Female <- Age_standardized_data %>%
  filter(
    measure_name == "Prevalence",
    metric_name == "Number",
    sex_name == "Female"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_Prevalence_Female <- ggplot(plot_Prevalence_Female, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Incidence in Global"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_Prevalence_Female$year), max(plot_Prevalence_Female$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    limits = c(0, 7500),  # 明确设置上下限
    breaks = seq(0, 7500, by = 2500),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_Prevalence_Female)




##### 第一步 Incidence ######
##### 第一步 Male ######
# 数据预处理
plot_Prevalence_Male <- Age_standardized_data %>%
  filter(
    measure_name == "Prevalence",
    metric_name == "Number",
    sex_name == "Male"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_Prevalence_Male <- ggplot(plot_Prevalence_Male, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Prevalence in Global"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_Prevalence_Male$year), max(plot_Prevalence_Male$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    limits = c(0, 7500),  # 明确设置上下限
    breaks = seq(0, 7500, by = 2500),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_Prevalence_Male)




# 加载所需包
library(patchwork)
library(ggplot2)

# 假设已有的三张图为 p1、p2、p3

# 调整每张图的主题，只保留左侧图的纵轴
p_Prevalence_Both <- p_Prevalence_Both + 
  theme(
    axis.title = element_text(size = 20),  # 坐标轴标题字体大小
    axis.text = element_text(size = 16)
  )

p_Prevalence_Female <- p_Prevalence_Female + 
  theme(
    # 隐藏中间图的纵轴标题和刻度
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # 隐藏中间图纵轴刻度线
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 16)    # 坐标轴刻度文字字体大小
  )

p_Prevalence_Male <- p_Prevalence_Male + 
  theme(
    # 隐藏右侧图的纵轴标题和刻度
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # 隐藏中间图纵轴刻度线
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 16)
  )

# 不使用cowplot的替代方案
combined_plot_Prevalence <- p_Prevalence_Both + p_Prevalence_Female + p_Prevalence_Male +
  plot_layout(widths = c(1, 1, 1), guides = "collect") +
  plot_annotation(
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 8)))
  ) &
  theme(legend.position = "right",
        legend.text = element_text(size = 14),  # 图例文字字体大小
        legend.title = element_text(size = 16),# 图例标题字体大小，如果有标题的话
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.03, 0.03, 0.03, 0.03), "cm"))
# 显示组合图
print(combined_plot_Prevalence)









##### 第一步 DALYs ######
##### 第一步 Both ######
# 数据预处理
plot_DALYs_Both <- Age_standardized_data %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name == "Number",
    sex_name == "Both"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_DALYs_Both <- ggplot(plot_DALYs_Both, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of DALYs (x100000)"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_DALYs_Both$year), max(plot_DALYs_Both$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_DALYs_Both)





##### 第一步 DALYs ######
##### 第一步 Female ######
# 数据预处理
plot_DALYs_Female <- Age_standardized_data %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name == "Number",
    sex_name == "Female"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_DALYs_Female <- ggplot(plot_DALYs_Female, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Incidence in Global"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_DALYs_Female$year), max(plot_DALYs_Female$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    limits = c(0, 150),  # 明确设置上下限
    breaks = seq(0, 150, by = 50),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_DALYs_Female)




##### 第一步 Incidence ######
##### 第一步 Male ######
# 数据预处理
plot_DALYs_Male <- Age_standardized_data %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)",
    metric_name == "Number",
    sex_name == "Male"
  ) %>%
  group_by(year, age_name) %>%
  summarise(val = sum(val), .groups = "drop") %>%
  mutate(
    age_name = factor(age_name, 
                      levels = c("70-74", "75-79", "80-84", "85-89", "90-94", "95+"))
  ) %>%
  arrange(year, age_name)

# 定义更浅的蓝色彩渐变（从浅蓝到更浅的蓝，保持顺序）
light_blue_gradient <- c(
  "70-74" = "#1971C2",    # 基础浅蓝色
  "75-79" = "#1c7ED6",
  "80-84" = "#228BE6",
  "85-89" = "#339AF0",
  "90-94" = "#00BFFF",
  "95+"   = "#00FFFF"     # 最浅蓝
)



# 绘制无间隙堆叠面积图
p_DALYs_Male <- ggplot(plot_DALYs_Male, aes(x = year, y = val, fill = age_name)) +
  geom_area(
    position = position_stack(),  # 严格堆叠
    alpha = 0.9, 
    color = NA,                   # 移除分隔线（消除缝隙）
    linewidth = 0,                # 线条宽度设为0
    linejoin = "round"
  ) +
  
  # 使用浅色系渐变
  scale_fill_manual(
    values = light_blue_gradient,
    name = "Age Group"
  ) +
  
  labs(
    x = "Year",
    y = "Number of Prevalence in Global"
  ) +
  
  scale_x_continuous(
    breaks = seq(min(plot_DALYs_Male$year), max(plot_DALYs_Male$year), by = 10),
    expand = c(0.02, 0.02)
  ) +
  
  scale_y_continuous(
    limits = c(0, 150),  # 明确设置上下限
    breaks = seq(0, 150, by = 50),  # 刻度间隔为 50,000,000
    expand = c(0.02, 0.02),
    labels = scales::comma
  ) +
  
  theme_bw() +
  theme(
    plot.title = element_text(hjust = 0.5, size = 14, face = "bold"),
    axis.title = element_text(size = 12),
    axis.text = element_text(size = 10),
    legend.position = "right"
  )

print(p_DALYs_Male)




# 加载所需包
library(patchwork)
library(ggplot2)

# 假设已有的三张图为 p1、p2、p3

# 调整每张图的主题，只保留左侧图的纵轴
p_DALYs_Both <- p_DALYs_Both + 
  theme(
    axis.title = element_text(size = 20),  # 坐标轴标题字体大小
    axis.text = element_text(size = 16)
  )

p_DALYs_Female <- p_DALYs_Female + 
  theme(
    # 隐藏中间图的纵轴标题和刻度
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # 隐藏中间图纵轴刻度线
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 16)    # 坐标轴刻度文字字体大小
  )

p_DALYs_Male <- p_DALYs_Male + 
  theme(
    # 隐藏右侧图的纵轴标题和刻度
    axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    # 隐藏中间图纵轴刻度线
    axis.ticks.y = element_blank(),
    axis.text = element_text(size = 16)
  )

# 不使用cowplot的替代方案
combined_plot_DALYs <- p_DALYs_Both + p_DALYs_Female + p_DALYs_Male +
  plot_layout(widths = c(1, 1, 1), guides = "collect") +
  plot_annotation(
    theme = theme(plot.caption = element_text(hjust = 0.5, size = 16, margin = margin(t = 8)))
  ) &
  theme(legend.position = "right",
        legend.text = element_text(size = 14),  # 图例文字字体大小
        legend.title = element_text(size = 16),# 图例标题字体大小，如果有标题的话
        axis.ticks = element_blank(),
        plot.margin = unit(c(0.03, 0.03, 0.03, 0.03), "cm"))
# 显示组合图
print(combined_plot_DALYs)







#拼图咯----------------------------------------------------------------------
# 加载所需包
# 加载包
library(ggplot2)
library(patchwork)
library(cowplot)





# 使用 plot_layout 进行更精确的控制，调大间隔可以通过调整 heights 参数里的值来实现
# 例如适当减小每个图所占高度比例，增加间隔比例，这里设置为 c(1, 0.2, 1, 0.2, 1)，即图与图之间间隔占比为 0.2
vertical_plot <- combined_plot_Incidence / 
  plot_spacer() /  # 添加间隔空白图
  combined_plot_Prevalence / 
  plot_spacer() /  # 添加间隔空白图
  combined_plot_DALYs +
  plot_layout(nrow = 5, heights = c(1, 0.2, 1, 0.2, 1)) 

# 显示最终图表
print(vertical_plot)
