#"Correlation_SDI_国家层面"


#1.安装并加载相关R包

#install.packages("readxl")
#install.packages("tidyverse")
#install.packages("ggplot2")
library(readxl)
library(tidyverse)
library(ggplot2)




##获取204个国家的COPD疾病负担数据（我们直接利用TableS4的数据）
#Global_21Regions_204counties_1990to2021 <- read.csv('/Users/fqq/Documents/GBD口腔老年人/全球地区国家三指标数据/Final_combined_data.csv', header=T)

#Global_21Regions_204counties_1990to2021 <- Global_21Regions_204counties_1990to2021 %>%
filter(sex_name == "Both",
       age_name == "Age-standardized",
       measure_name == "Prevalence",
       metric_name == "Rate",
       cause_name == "Oral disorders") 

# 输出最终结果
#fwrite(Global_21Regions_204counties_1990to2021, "/Users/fqq/Documents/GBD口腔老年人/全球地区国家三指标数据/全球地区国家-口腔疾病-ASPR.csv", row.names = FALSE)






#2.加载数据

##获取SDI_value文件(这个文件是location × year的SDI矩阵)
SDI_value <- readxl::read_xlsx('/Users/fqq/Documents/小论文/肝胆外/定观老师/FigureS1/SDI_value_GBD2021_1990_2021_modified.XLSX',sheet=1) #读入xlsx文件的第二个sheet,跳过第一行


##获取204个国家的list文件（我们直接利用TableS4的数据）
list_global_21regions_153countries <- read_xlsx("/Users/fqq/Documents/小论文/肝胆外/定观老师/FigureS1/list_global_21regions_204countries.xlsx", sheet=1)


##获取204个国家的COPD疾病负担数据（我们直接利用TableS4的数据）
Global_21Regions_204counties_1990to2021 <- read.csv('/Users/fqq/Documents/GBD口腔老年人/全球地区国家三指标数据/全球地区国家-口腔疾病-ASDR.csv', header=T)



#3.数据清洗（即需要整理出3列数据，分别是204个国家的location list，SDI值，疾病负担指标DALYs ASR） 

##3.1 制作第一列数据：204个国家的location list
list_153countries <- list_global_21regions_153countries %>%
  filter(!subregion %in% c("Global","region21")) %>% #匹配符号，常常与向量一起使用
  rename("Location" = "Country")

##3.2 制作第二列数据：204个国家的在2019年的SDI数据
SDI_value_153countries <- SDI_value %>%
  select("Location","2021") %>%
  filter(Location %in% list_153countries$Location) %>%
  rename("SDI_2021"="2021")

##3.3 制作第三列数据：204个国家，2019年，年龄标准化的DALYs rate
Countries153_DALYs_ASR <- Global_21Regions_204counties_1990to2021 %>%
  select("location_name","year","sex_name","age_name","measure_name","metric_name","val") %>%
  filter(location_name %in% list_153countries$Location,
         year == "2021",
         sex_name == "Both",
         age_name == "Age-standardized",
         measure_name == "DALYs (Disability-Adjusted Life Years)",
         metric_name == "Rate") %>%
  rename("DALYs_ASR_2021" = "val",
         "Location" = "location_name") #列名重命名

##3.4 将上述数据框合并（基于location）
Countries153_DALYs_ASR_SDI_1 <- left_join(SDI_value_153countries,Countries153_DALYs_ASR, by = 'Location') 
Countries153_DALYs_ASR_SDI_2 <- left_join(Countries153_DALYs_ASR_SDI_1, list_153countries, by = 'Location') %>%
  mutate(SDI_2021 = as.numeric(str_replace(SDI_2021,'\\·','.')))





#4.计算相关系数并可视化

##4.1 计算相关系数
cor_test <- cor.test(Countries153_DALYs_ASR_SDI_2[,"SDI_2021",drop = T], Countries153_DALYs_ASR_SDI_2[, "DALYs_ASR_2021", drop = TRUE])

cor_r <- cor_test$estimate
cor_int<- cor_test$conf.int
cor_p <- cor_test$p.value

##4.2 基于ggplot函数进行可视化
Corr_153countries_SDI <- ggplot(Countries153_DALYs_ASR_SDI_2, # 数据框，用于绘图
                                aes(Countries153_DALYs_ASR_SDI_2[,"SDI_2021",drop = T],  # x轴变量，从数据框中获取"SDI"列
                                    Countries153_DALYs_ASR_SDI_2[,"DALYs_ASR_2021",drop = T])) + # y轴变量，从数据框中获取"DALYs_ASR"列
  geom_point(aes(color = subregion),size = 0.5)+ #添加图形几何图层以及该图层中颜色/形状映射
  scale_shape_manual(values = 1:22) +  #手动设置点的形状，一共22种
  geom_smooth(method = 'loess', se = FALSE, span = 0.5, color = 'black', linewidth = 0.8) +  # 添加平滑曲线，并设置曲线粗细) + #添加平滑曲线的图形几何图层
  geom_text(x = min(Countries153_DALYs_ASR_SDI_2$SDI_2021 + 0.25), #添加文本标签的图层，设置该文本的位置（x和y)、内容（label）、大小
            y = max(Countries153_DALYs_ASR_SDI_2[,"DALYs_ASR_2021",drop = T])*0.8,
            label = paste("R =", round(cor_r, 2),"(", round(cor_int[1],2), "to", round(cor_int[2],2), ")","\n", "p =", ifelse(cor_p<0.001,"< 0.001",round(cor_p,2))),
            hjust = 1, vjust = 0,
            size = 4)+
  geom_text(aes(label = Location,color = subregion),  # 将Location列的值作为标签
            hjust = 0.7, vjust = 0,
            size = 1) +
  ylab(paste0("ASPR (per 100 000)"))+ # 设置x和y轴的标签
  xlab("Socio-demographic index")+
  theme_bw(base_size = 8)+ #设置主题样式和基本字体大小，theme_bw 函数用于设置图表的主题样式为黑白风格。通过 base_size = 14 参数，设置图表的基本字体大小为 14
  theme(panel.background = element_rect(fill = "transparent"), # 设置绘图区域的背景为透明
        plot.background = element_rect(fill = "transparent"), # 设置整个绘图的背景为透明
        legend.position = 'top', # 设置legend的位置和大小
        legend.key.size = unit(0.3, "cm"),
        legend.title = element_blank(), 
        axis.text.x = element_text(face = "bold",size = 8), # 设置坐标轴文本的外观和大小
        axis.text.y = element_text(face = "bold",size = 8),
        axis.title.x = element_text(face = "bold",size = 10),# 设置坐标轴标题的外观和大小
        axis.title.y = element_text(face = "bold",size = 10)) 

Corr_153countries_SDI









#####不显示R和p#######
# 假设前面的包加载、数据准备及相关系数计算等步骤都已完成（同方式一开头部分）

# 绘制图形并调整 R 和 p 值文本标签的位置与大小
# 假设已完成数据准备和相关系数计算（cor_r、cor_int、cor_p 已存在）
Corr_153countries_SDI <- ggplot(Countries153_DALYs_ASR_SDI_2,
                                aes(SDI_2021,  # 简化 x 轴变量写法（直接用列名）
                                    DALYs_ASR_2021)) +  # 简化 y 轴变量写法
  geom_point(aes(color = subregion), size = 0.5) +
  scale_shape_manual(values = 1:22) +
  geom_smooth(method = 'loess', se = FALSE, span = 0.5, color = 'black', linewidth = 0.8) +
  
  # 核心：手动微调 R 和 p 值文本的位置（重点修改 x 和 y 的值）
  geom_text(
    # 手动设定 x 坐标（根据你的数据范围调整，例如 0.4、0.6 等）
    x = 0.5,  
    # 手动设定 y 坐标（根据你的数据范围调整，例如 50、100 等）
    y = 80,  
    label = paste("R =", round(cor_r, 2), 
                  "(", round(cor_int[1], 2), "to", round(cor_int[2], 2), ")", 
                  "\n", "p =", ifelse(cor_p < 0.001, "< 0.001", round(cor_p, 2))),
    hjust = 0.5,  # 水平居中对齐（0 左对齐，1 右对齐）
    vjust = 0.5,  # 垂直居中对齐（0 顶对齐，1 底对齐）
    size = 4  # 文本大小（可按需调整，如 5、6 等）
  ) +
  
  geom_text(aes(label = Location, color = subregion),
            hjust = 0.7, vjust = 0, size = 1) +
  ylab("ASDR (per 100 000)") +
  xlab("Socio-demographic index") +
  theme_bw(base_size = 8) +
  theme(
    panel.background = element_rect(fill = "transparent"),
    plot.background = element_rect(fill = "transparent"),
    legend.position = 'top',
    legend.key.size = unit(0.3, "cm"),
    legend.title = element_blank(),
    axis.text.x = element_text(face = "bold", size = 8),
    axis.text.y = element_text(face = "bold", size = 8),
    axis.title.x = element_text(face = "bold", size = 10),
    axis.title.y = element_text(face = "bold", size = 10)
  )

# 显示图形
print(Corr_153countries_SDI)

