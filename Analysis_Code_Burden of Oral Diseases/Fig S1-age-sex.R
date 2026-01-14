# 设置工作目录为数据所在文件夹
setwd("/Users/fqq/Documents/GBD口腔老年人/分解分析")


# 加载必要的R包，用于数据可视化和处理
install.packages("reshape2")
library(ggplot2)    # ggplot2包用于绘制图形
library(reshape2)   # reshape2包用于数据重塑，将宽表转为长表
library(dplyr)      # dplyr包用于数据处理，如筛选、分组、汇总
# install.packages('readxl')  # 如果没有安装readxl包，可以用此命令安装
library(readxl)   # readxl包用于读取Excel文件


##### 第一步 Incidence ######
# 读取CSV文件中的中国IBD数据
IBD_china <- read.csv('Age_standardized_data.csv',header = T)




# 查看数据中的所有年龄组信息，了解数据分布
unique(IBD_china$age_name)



# 筛选需要的数据，包括性别、地区、度量指标和患病率
data1_Incidence <- subset(IBD_china,
                
                
                IBD_china$sex_name != "Both" &  # 去除两性数据
                  
                  
                  
                  IBD_china$location_name == 'Global' &  # 只提取中国的数据
                  
                  IBD_china$year == '2021' &  # 只提取中国的数据
                  
                  (IBD_china$metric_name %in% c('Number', 'Rate')) &  # 提取人数和每10万人患病率数据
                  
                  (IBD_china$age_name %in% c("70-74","75-79", "80-84", "85-89", "90-94", "95+")) &  # 提取人数和每10万人患病率数据
                  
                  IBD_china$measure_name == 'Incidence')  # 只提取患病率数据





# 查看数据中的所有度量指标
unique(data1_Incidence$metric_name)

# 查看数据结构，了解数据类型和内容
str(data1_Incidence)

# 筛选需要的列，保留性别、年龄、度量指标、年份、数值和置信区间
data2_Incidence <- data1_Incidence[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")]




# 创建患病率柱状图和折线图的 ggplot 对象

# 首先确保age_name是有序因子
data2_Incidence$age_name <- factor(data2_Incidence$age_name, 
                         levels = c("70-74",
                                    "75-79", "80-84", "85-89", "90-94", "95+"),
                         ordered = TRUE)

# 计算合适的转换因子（如果需要双y轴）
# 假设Rate是以每10万人为单位，Number是实际人数
max_number_Incidence <- max(subset(data2_Incidence, metric_name == "Number")$val, na.rm = TRUE)
max_rate_Incidence <- max(subset(data2_Incidence, metric_name == "Rate")$val, na.rm = TRUE)
conversion_factor_Incidence <- max_number_Incidence / max_rate_Incidence

p1 <- ggplot() +
  # 柱状图 - 患病人数
  geom_bar(
    data = subset(data2_Incidence, metric_name == "Number"),
    aes(x = age_name, y = val, fill = sex_name),
    stat = "identity",
    position = position_dodge(width = 0.8),
    width = 0.7
  ) +
  # 误差条 - 患病人数
  geom_errorbar(
    data = subset(data2_Incidence, metric_name == "Number"),
    aes(x = age_name, ymin = lower, ymax = upper, group = sex_name),
    position = position_dodge(width = 0.8),
    width = 0.25,
    color = "black"
  ) +
  # 折线图 - 患病率（使用右侧y轴）
  geom_line(
    data = subset(data2_Incidence, metric_name == "Rate"),
    aes(x = age_name, y = val * conversion_factor_Incidence, color = sex_name, group = sex_name),
    size = 1
  ) +
  # 点图 - 在折线上添加点:cite[2]
  geom_point(
    data = subset(data2_Incidence, metric_name == "Rate"),
    aes(x = age_name, y = val * conversion_factor_Incidence, color = sex_name, group = sex_name),
    size = 2  # 可以调整点的大小
  ) +
  # 置信区间带状图 - 患病率
  geom_ribbon(
    data = subset(data2_Incidence, metric_name == "Rate"),
    aes(x = age_name, 
        ymin = lower * conversion_factor_Incidence, 
        ymax = upper * conversion_factor_Incidence, 
        fill = sex_name, 
        group = sex_name),
    alpha = 0.2
  ) +
  # 双y轴设置
  scale_y_continuous(
    name = "2021 Incidence cases",
    sec.axis = sec_axis(~ . / conversion_factor_Incidence, 
                        name = "Incident rate (per 100,000)")
  ) +
  
  scale_fill_manual(values = c("#6B58A6", "#FCAF17", "blue", "red"), name = "Number") +  # 自定义柱状图颜色. scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") 用于自定义柱状图的填充颜色，将不同的填充颜色分配给图例中的类别，并设置图例标题为“Number”。
  
  
  scale_color_manual(values = c("#6B58A6", "#FCAF17"), name = "Rate") +  # 自定义折线图颜色
  
  
  
  labs(x = "Age Group") +  # 设置x轴标签为“年份”
  
  theme_bw() +
  theme(
    legend.position = 'right',
    legend.title = element_text(size = 16),
    # 增大图例文字大小（修正为合理数值）
    legend.text = element_text(size = 14),  # 原size=1调整为12
    axis.text = element_text(size = 16),    # 末尾添加了逗号
    axis.text.x = element_text(vjust = 1, hjust = 1),
    axis.text.y.left = element_text(margin = margin(r = 0)),
    axis.text.y.right = element_text(margin = margin(r = 0)),
    axis.title.x = element_text(size = 16),
    axis.title.y = element_text(size = 20),
    axis.ticks = element_blank()
  )

  print(p1)

  
  
  
  
  ##### 第二步 Prevalence ######

  # 筛选需要的数据，包括性别、地区、度量指标和患病率
  data1_Prevalence <- subset(IBD_china,
                            
                            
                            IBD_china$sex_name != "Both" &  # 去除两性数据
                              
                              
                              
                              IBD_china$location_name == 'Global' &  # 只提取中国的数据
                              
                              IBD_china$year == '2021' &  # 只提取中国的数据
                              
                              (IBD_china$metric_name %in% c('Number', 'Rate')) &  # 提取人数和每10万人患病率数据
                              
                              (IBD_china$age_name %in% c("70-74","75-79", "80-84", "85-89", "90-94", "95+")) &  # 提取人数和每10万人患病率数据
                              
                              IBD_china$measure_name == 'Prevalence')  # 只提取患病率数据
  
  
  
  
  
  # 查看数据中的所有度量指标
  unique(data1_Prevalence$metric_name)
  
  # 查看数据结构，了解数据类型和内容
  str(data1_Prevalence)
  
  # 筛选需要的列，保留性别、年龄、度量指标、年份、数值和置信区间
  data2_Prevalence <- data1_Prevalence[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")]
  
  
  
  
  # 创建患病率柱状图和折线图的 ggplot 对象
  
  # 首先确保age_name是有序因子
  data2_Prevalence$age_name <- factor(data2_Prevalence$age_name, 
                                     levels = c("70-74",
                                                "75-79", "80-84", "85-89", "90-94", "95+"),
                                     ordered = TRUE)
  
  # 计算合适的转换因子（如果需要双y轴）
  # 假设Rate是以每10万人为单位，Number是实际人数
  max_number_Prevalence <- max(subset(data2_Prevalence, metric_name == "Number")$val, na.rm = TRUE)
  max_rate_Prevalence <- max(subset(data2_Prevalence, metric_name == "Rate")$val, na.rm = TRUE)
  conversion_factor_Prevalence <- max_number_Prevalence / max_rate_Prevalence
  
  p2 <- ggplot() +
    # 柱状图 - 患病人数
    geom_bar(
      data = subset(data2_Prevalence, metric_name == "Number"),
      aes(x = age_name, y = val, fill = sex_name),
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    # 误差条 - 患病人数
    geom_errorbar(
      data = subset(data2_Prevalence, metric_name == "Number"),
      aes(x = age_name, ymin = lower, ymax = upper, group = sex_name),
      position = position_dodge(width = 0.8),
      width = 0.25,
      color = "black"
    ) +
    # 折线图 - 患病率（使用右侧y轴）
    geom_line(
      data = subset(data2_Prevalence, metric_name == "Rate"),
      aes(x = age_name, y = val * conversion_factor_Prevalence, color = sex_name, group = sex_name),
      size = 1
    ) +
    # 点图 - 在折线上添加点:cite[2]
    geom_point(
      data = subset(data2_Prevalence, metric_name == "Rate"),
      aes(x = age_name, y = val * conversion_factor_Prevalence, color = sex_name, group = sex_name),
      size = 2  # 可以调整点的大小
    ) +
    # 置信区间带状图 - 患病率
    geom_ribbon(
      data = subset(data2_Prevalence, metric_name == "Rate"),
      aes(x = age_name, 
          ymin = lower * conversion_factor_Prevalence, 
          ymax = upper * conversion_factor_Prevalence, 
          fill = sex_name, 
          group = sex_name),
      alpha = 0.2
    ) +
    # 双y轴设置
    scale_y_continuous(
      name = "2021 Prevalence cases",
      sec.axis = sec_axis(~ . / conversion_factor_Incidence, 
                          name = "Prevalent rate (per 100,000)")
    ) +
    
    scale_fill_manual(values = c("#6B58A6", "#FCAF17", "blue", "red"), name = "Number") +  # 自定义柱状图颜色. scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") 用于自定义柱状图的填充颜色，将不同的填充颜色分配给图例中的类别，并设置图例标题为“Number”。
    
    
    scale_color_manual(values = c("#6B58A6", "#FCAF17"), name = "Rate") +  # 自定义折线图颜色
    
    
    
    labs(x = "Age Group") +  # 设置x轴标签为“年份”
    
    theme_bw() +
    theme(
      legend.position = 'right',
      legend.title = element_text(size = 16),
      # 增大图例文字大小（修正为合理数值）
      legend.text = element_text(size = 14),  # 原size=1调整为12
      axis.text = element_text(size = 16),    # 末尾添加了逗号
      axis.text.x = element_text(vjust = 1, hjust = 1),
      axis.text.y.left = element_text(margin = margin(r = 0)),
      axis.text.y.right = element_text(margin = margin(r = 0)),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      axis.ticks = element_blank()
    )
  
  print(p2)
  
  
  
  
  
  
 
  
  ##### 第四步 DALYs ######
  
  # 筛选需要的数据，包括性别、地区、度量指标和患病率
  data1_DALYs <- subset(IBD_china,
                         
                         
                         IBD_china$sex_name != "Both" &  # 去除两性数据
                           
                           
                           
                           IBD_china$location_name == 'Global' &  # 只提取中国的数据
                           
                           IBD_china$year == '2021' &  # 只提取中国的数据
                           
                           (IBD_china$metric_name %in% c('Number', 'Rate')) &  # 提取人数和每10万人患病率数据
                           
                           (IBD_china$age_name %in% c("70-74","75-79", "80-84", "85-89", "90-94", "95+")) &  # 提取人数和每10万人患病率数据
                           
                           IBD_china$measure_name == 'DALYs (Disability-Adjusted Life Years)')  # 只提取患病率数据
  
  
  
  
  
  # 查看数据中的所有度量指标
  unique(data1_DALYs$metric_name)
  
  # 查看数据结构，了解数据类型和内容
  str(data1_DALYs)
  
  # 筛选需要的列，保留性别、年龄、度量指标、年份、数值和置信区间
  data2_DALYs <- data1_DALYs[, c("sex_name", "age_name", "metric_name", "year", "val", "upper", "lower")]
  
  
  
  
  # 创建患病率柱状图和折线图的 ggplot 对象
  
  # 首先确保age_name是有序因子
  data2_DALYs$age_name <- factor(data2_DALYs$age_name, 
                                  levels = c("70-74",
                                             "75-79", "80-84", "85-89", "90-94", "95+"),
                                  ordered = TRUE)
  
  # 计算合适的转换因子（如果需要双y轴）
  # 假设Rate是以每10万人为单位，Number是实际人数
  max_number_DALYs <- max(subset(data2_DALYs, metric_name == "Number")$val, na.rm = TRUE)
  max_rate_DALYs <- max(subset(data2_DALYs, metric_name == "Rate")$val, na.rm = TRUE)
  conversion_factor_DALYs <- max_number_DALYs / max_rate_DALYs
  
  p3 <- ggplot() +
    # 柱状图 - 患病人数
    geom_bar(
      data = subset(data2_DALYs, metric_name == "Number"),
      aes(x = age_name, y = val, fill = sex_name),
      stat = "identity",
      position = position_dodge(width = 0.8),
      width = 0.7
    ) +
    # 误差条 - 患病人数
    geom_errorbar(
      data = subset(data2_DALYs, metric_name == "Number"),
      aes(x = age_name, ymin = lower, ymax = upper, group = sex_name),
      position = position_dodge(width = 0.8),
      width = 0.25,
      color = "black"
    ) +
    # 折线图 - 患病率（使用右侧y轴）
    geom_line(
      data = subset(data2_DALYs, metric_name == "Rate"),
      aes(x = age_name, y = val * conversion_factor_DALYs, color = sex_name, group = sex_name),
      size = 1
    ) +
    # 点图 - 在折线上添加点:cite[2]
    geom_point(
      data = subset(data2_DALYs, metric_name == "Rate"),
      aes(x = age_name, y = val * conversion_factor_DALYs, color = sex_name, group = sex_name),
      size = 2  # 可以调整点的大小
    ) +
    # 置信区间带状图 - 患病率
    geom_ribbon(
      data = subset(data2_DALYs, metric_name == "Rate"),
      aes(x = age_name, 
          ymin = lower * conversion_factor_DALYs, 
          ymax = upper * conversion_factor_DALYs, 
          fill = sex_name, 
          group = sex_name),
      alpha = 0.2
    ) +
    # 双y轴设置
    scale_y_continuous(
      name = "2021 DALYs cases",
      sec.axis = sec_axis(~ . / conversion_factor_DALYs, 
                          name = "DALYs rate (per 100,000)")
    ) +
    
    scale_fill_manual(values = c("#6B58A6", "#FCAF17", "blue", "red"), name = "Number") +  # 自定义柱状图颜色. scale_fill_manual(values = c("#ffaa00", "#1240ab", "blue", "red"), name = "Number") 用于自定义柱状图的填充颜色，将不同的填充颜色分配给图例中的类别，并设置图例标题为“Number”。
    
    
    scale_color_manual(values = c("#6B58A6", "#FCAF17"), name = "Rate") +  # 自定义折线图颜色
    
    
    
    labs(x = "Age Group") +  # 设置x轴标签为“年份”
    
    theme_bw() +
    theme(
      legend.position = 'right',
      legend.title = element_text(size = 16),
      # 增大图例文字大小（修正为合理数值）
      legend.text = element_text(size = 14),  # 原size=1调整为12
      axis.text = element_text(size = 16),    # 末尾添加了逗号
      axis.text.x = element_text(vjust = 1, hjust = 1),
      axis.text.y.left = element_text(margin = margin(r = 0)),
      axis.text.y.right = element_text(margin = margin(r = 0)),
      axis.title.x = element_text(size = 16),
      axis.title.y = element_text(size = 20),
      axis.ticks = element_blank()
    )
  
  print(p3)
  
  
  
  
  #拼图咯----------------------------------------------------------------------
  # 加载所需包
  # 加载包
  library(ggplot2)
  library(patchwork)
  library(cowplot)
  
  
  
  
  
  # 使用 plot_layout 进行更精确的控制，调大间隔可以通过调整 heights 参数里的值来实现
  # 例如适当减小每个图所占高度比例，增加间隔比例，这里设置为 c(1, 0.2, 1, 0.2, 1)，即图与图之间间隔占比为 0.2
  vertical_plot <- p1 / 
    plot_spacer() /  # 添加间隔空白图
    p2 / 
    plot_spacer() /  # 添加间隔空白图
    p3 +
    plot_layout(nrow = 5, heights = c(1, 0.2, 1, 0.2, 1)) 
  
  # 显示最终图表
  print(vertical_plot)
  
  