#设置工作空间
setwd("/Users/fqq/Documents/GBD口腔老年人/分解分析")
##  Decomposition

install.packages("ggsci")
library(dplyr)
library(data.table)
library(purrr)
library(tidyr)
library(ggplot2)
library(ggsci)


#####SDI####SDI####SDI####
######################Incidence######################
## 读取疾病数据
IBD_china <- read.csv('Global_region_70-four.csv')

unique(IBD_china$age_name)





#age1对应IBD——china数据和age2对应总人口数据
age1 <- c("70-74 years","75-79 years","80-84 years","85-89 years","90-94 years","95+ years")   ###20个年龄组




age2 <- c("70-74 years","75-79 years","80-84 years","85-89 years","90-94 years","95+ years")   ###20个年龄


var_name <- c("location_name","sex_name","year","age_name","val") 


# 假设我们有多个地区的数据，先定义地区列表（这里示例包含中国、美国两个地区，可根据实际情况修改）
location_list <- c("Global", "High SDI", "High-middle SDI", "Middle SDI", "Low-middle SDI", "Low SDI") 

# 读取疾病数据时，筛选出特定地区（这里先以地区列表中的地区为例）的数据
IBD_in_both <- subset(IBD_china,
                      (IBD_china$age_name %in% age1 ) &
                        IBD_china$location_name %in% location_list)

# 后续对年龄相关处理保持不变，如转换年龄名称格式等操作
IBD_in_both$age_name<-gsub(" years","",IBD_in_both$age_name)
IBD_in_both$age_name <- factor(IBD_in_both$age_name, 
                               levels = c("70-74","75-79", "80-84", "85-89", "90-94", "95+"))

path = "/Users/fqq/Documents/10 BAPC_/GBD_population/GBD_population"


fileName = dir(path)


fileName



var_name <- c("location_name","sex_name","year","age_id","age_name","val") 

# 读取人口数据并筛选地区相关数据
population <- data.frame()
for(k in 1:length(fileName)){
  data = fread(file = paste(path,fileName[k],sep = "/"))
  population = rbind(population,data)
}

population<-population%>% dplyr::select("location_name","sex_name","year","age_name","val") %>% 
  filter(location_name %in% location_list &  # 这里按地区列表筛选地区数据
           age_name %in% age2)

# 同样对人口数据中年龄名称格式等做后续处理，保持和前面一致
population$age_name<-gsub(" years","",population$age_name)
population$age_name <- factor(population$age_name, 
                              levels = c("70-74", 
                                         "75-79", "80-84", "85-89", "90-94", "95+"))
population<-subset(population,
                   population$year==1990|
                     population$year==2021)




# 制作一个空表格，用于存储分解结果，包括地区、总体差异、年龄效应、人口效应、流行病学效应及其百分比
decomposition_name <- c('location_name',
                        'overll_difference',
                        'a_effect',
                        'p_effect',
                        'r_effect',
                        'a_percent',
                        'p_percent',
                        'r_percent')
decomposition_data_incidence <- as.data.frame(matrix(nrow=0,
                                                     ncol=length(decomposition_name)))
names(decomposition_data_incidence) <- decomposition_name

# 循环遍历每个地区，分别进行分解分析（这里以之前定义的地区列表为例）
for (loc in location_list) {
  # 筛选出1990年当前地区的人口数据
  Global_population_1990 <- population %>% 
    filter(location_name == loc  & 
             year == 1990) 
  
  # 计算1990年该地区的总人口数
  Global_1990 <- sum(Global_population_1990$val) 
  
  # 计算1990年不同年龄段的人口占比
  Global_population_1990$percent <- Global_population_1990$val / Global_1990 
  
  # 筛选出2021年当前地区的人口数据
  Global_population_2021 <- population %>% 
    filter(location_name == loc  & 
             year == 2021)
  
  # 计算2021年该地区的总人口数
  Global_2021 <- sum(Global_population_2021$val) 
  
  # 计算2021年不同年龄段的人口占比
  Global_population_2021$percent <- Global_population_2021$val / Global_2021 
  
  ### 提取1990年和2021年的年龄占比 (a) 和总人口 (p) 数据
  a_1990 <- Global_population_1990$percent  
  a_2021 <- Global_population_2021$percent  
  
  p_1990 <- Global_1990  
  p_2021 <- Global_2021  
  
  ## 获取发病率的数据
  case_1990_incidence <- IBD_in_both %>% filter(year == 1990 &
                                                  location_name == loc &
                                                  metric_name == 'Rate' &
                                                  measure_name == 'Incidence')
  
  case_2021_incidence <- IBD_in_both %>% filter(year == 2021 &
                                                  location_name == loc &
                                                  metric_name == 'Rate' &
                                                  measure_name == 'Incidence')
  
  # 将发病率单位转换为每10万人（除以10^5）
  r_1990_incidence <- as.numeric(case_1990_incidence$val) / 10^5  
  r_2021_incidence <- as.numeric(case_2021_incidence$val) / 10^5  
  
  ##### 根据公式计算三个效应 #####
  # 计算年龄效应
  a_effect_incidence <- round((sum(a_2021 * p_1990 * r_1990_incidence) + sum(a_2021 * p_2021 * r_2021_incidence)) / 3 + 
                                (sum(a_2021 * p_1990 * r_2021_incidence) + sum(a_2021 * p_2021 * r_1990_incidence)) / 6 -
                                (sum(a_1990 * p_1990 * r_1990_incidence) + sum(a_1990 * p_2021 * r_2021_incidence)) / 3 -
                                (sum(a_1990 * p_1990 * r_2021_incidence) + sum(a_1990 * p_2021 * r_1990_incidence)) / 6, 3)
  
  # 计算人口效应
  p_effect_incidence <- round((sum(a_1990 * p_2021 * r_1990_incidence) + sum(a_2021 * p_2021 * r_2021_incidence)) / 3 + 
                                (sum(a_1990 * p_2021 * r_2021_incidence) + sum(a_2021 * p_2021 * r_1990_incidence)) / 6 -
                                (sum(a_1990 * p_1990 * r_1990_incidence) + sum(a_2021 * p_1990 * r_2021_incidence)) / 3 -
                                (sum(a_1990 * p_1990 * r_2021_incidence) + sum(a_2021 * p_1990 * r_1990_incidence)) / 6, 3)
  
  # 计算流行病学效应
  r_effect_incidence <- round((sum(a_1990 * p_1990 * r_2021_incidence) + sum(a_2021 * p_2021 * r_2021_incidence)) / 3 + 
                                (sum(a_1990 * p_2021 * r_2021_incidence) + sum(a_2021 * p_1990 * r_2021_incidence)) / 6 -
                                (sum(a_1990 * p_1990 * r_1990_incidence) + sum(a_2021 * p_2021 * r_1990_incidence)) / 3 -
                                (sum(a_1990 * p_2021 * r_1990_incidence) + sum(a_2021 * p_1990 * r_1990_incidence)) / 6, 3)
  
  # 计算总体差异
  overll_differ_incidence <- round(a_effect_incidence + p_effect_incidence + r_effect_incidence, 2)
  
  # 计算每个效应占总体差异的百分比
  a_percent_incidence <- round(a_effect_incidence / overll_differ_incidence * 100, 2)
  p_percent_incidence <- round(p_effect_incidence / overll_differ_incidence * 100, 2)
  r_percent_incidence <- round(r_effect_incidence / overll_differ_incidence * 100, 2)
  
  # 将当前循环的地区和分解结果组合为一个行向量，转置后转换为数据框并追加到分解结果数据框中
  temp_incidence <- c(loc, 
                      overll_differ_incidence, 
                      a_effect_incidence,
                      p_effect_incidence, 
                      r_effect_incidence, 
                      a_percent_incidence, 
                      p_percent_incidence, 
                      r_percent_incidence) %>% 
    t() %>% as.data.frame()
  names(temp_incidence) <- decomposition_name 
  decomposition_data_incidence <- rbind(decomposition_data_incidence, temp_incidence) 
}



# 解决left_join多对多关系警告的问题

# 1. 确保发病数数据每个地区只保留一条记录
# 针对特定年龄段（70-74岁到90-94岁）汇总发病数
num_1990_incidence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &  # 注意这里已去除"years"后缀
           year == 1990 &
           metric_name == 'Number' &
           measure_name == 'Incidence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_1990 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

num_2021_incidence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &
           year == 2021 &
           metric_name == 'Number' &
           measure_name == 'Incidence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_2021 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

# 将发病数与按地区分类的分解结果合并
decomposition_data_incidence <- left_join(decomposition_data_incidence, num_1990_incidence, by = 'location_name') %>% 
  left_join(num_2021_incidence, by = 'location_name')

# 计算2021年与1990年发病数的差异
decomposition_data_incidence$diff1 <- decomposition_data_incidence$val_2021 - decomposition_data_incidence$val_1990

# 将数据框的数值列转换为数值型（这里确保列范围选择正确，对应按地区分类后的列情况）
decomposition_data_incidence[, 2:11] <- decomposition_data_incidence[, 2:11] %>% apply(c(1, 2), as.numeric)



# 将分解结果保存为CSV文件
write.csv(decomposition_data_incidence, "Incidence_SDI.csv")






# 重命名列名以便于绘图展示（这里和之前类似，只是分析主体是地区了）
names(decomposition_data_incidence)[2:5] <- c('Overll difference', 'Aging', 'Population', 'Epidemiological change')

# 将数据转换为适合绘图的长格式（操作逻辑不变，只是数据是按地区分类的了）
decomposition_plot_incidence <- decomposition_data_incidence[c(1:5)]
decomposition_plot_incidence <- decomposition_plot_incidence %>%
  pivot_longer(3:5, 
               names_to = "varname", 
               values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location_name = factor(location_name, levels = location_list, ordered = T))

# 自定义条形图的颜色（保持不变）
my_colors <- c("#FFFB73", "#33CCCC", "#FFC773")

# 绘制堆叠条形图，显示不同地区的分解效应（这里x轴变为地区名称了）
p_incidence <- ggplot(decomposition_plot_incidence, 
                      aes(x = location_name, 
                          y = value, 
                          fill = varname)) +
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = my_colors) + 
  theme_bw()
print(p_incidence)
# 在图中添加表示总体差异的点（这里同样基于地区分类的数据来添加）
decomposition_data_incidence$`Overll difference` <- as.numeric(decomposition_data_incidence$`Overll difference`)
plot_incidence <- p_incidence + 
  geom_point(data = decomposition_data_incidence, 
             mapping = aes(x = location_name, 
                           y = `Overll difference`), 
             fill = 'black', 
             color = 'black', 
             size = 3)

plot_incidence  # 显示最终的图表







######################Prevalence######################
decomposition_data_prevalence <- as.data.frame(matrix(nrow=0,
                                                      ncol=length(decomposition_name)))
names(decomposition_data_prevalence) <- decomposition_name

# 循环遍历每个地区，分别进行分解分析（这里以之前定义的地区列表为例）
for (loc in location_list) {
  # 筛选出1990年当前地区的人口数据
  Global_population_1990 <- population %>% 
    filter(location_name == loc  & 
             year == 1990) 
  
  # 计算1990年该地区的总人口数
  Global_1990 <- sum(Global_population_1990$val) 
  
  # 计算1990年不同年龄段的人口占比
  Global_population_1990$percent <- Global_population_1990$val / Global_1990 
  
  # 筛选出2021年当前地区的人口数据
  Global_population_2021 <- population %>% 
    filter(location_name == loc  & 
             year == 2021)
  
  # 计算2021年该地区的总人口数
  Global_2021 <- sum(Global_population_2021$val) 
  
  # 计算2021年不同年龄段的人口占比
  Global_population_2021$percent <- Global_population_2021$val / Global_2021 
  
  ### 提取1990年和2021年的年龄占比 (a) 和总人口 (p) 数据
  a_1990 <- Global_population_1990$percent  
  a_2021 <- Global_population_2021$percent  
  
  p_1990 <- Global_1990  
  p_2021 <- Global_2021  
  
  ## 获取发病率的数据
  case_1990_prevalence <- IBD_in_both %>% filter(year == 1990 &
                                                   location_name == loc &
                                                   metric_name == 'Rate' &
                                                   measure_name == 'Prevalence')
  
  case_2021_prevalence <- IBD_in_both %>% filter(year == 2021 &
                                                   location_name == loc &
                                                   metric_name == 'Rate' &
                                                   measure_name == 'Prevalence')
  
  # 将发病率单位转换为每10万人（除以10^5）
  r_1990_prevalence <- as.numeric(case_1990_prevalence$val) / 10^5  
  r_2021_prevalence <- as.numeric(case_2021_prevalence$val) / 10^5  
  
  ##### 根据公式计算三个效应 #####
  # 计算年龄效应
  a_effect_prevalence <- round((sum(a_2021 * p_1990 * r_1990_prevalence) + sum(a_2021 * p_2021 * r_2021_prevalence)) / 3 + 
                                 (sum(a_2021 * p_1990 * r_2021_prevalence) + sum(a_2021 * p_2021 * r_1990_prevalence)) / 6 -
                                 (sum(a_1990 * p_1990 * r_1990_prevalence) + sum(a_1990 * p_2021 * r_2021_prevalence)) / 3 -
                                 (sum(a_1990 * p_1990 * r_2021_prevalence) + sum(a_1990 * p_2021 * r_1990_prevalence)) / 6, 3)
  
  # 计算人口效应
  p_effect_prevalence <- round((sum(a_1990 * p_2021 * r_1990_prevalence) + sum(a_2021 * p_2021 * r_2021_prevalence)) / 3 + 
                                 (sum(a_1990 * p_2021 * r_2021_prevalence) + sum(a_2021 * p_2021 * r_1990_prevalence)) / 6 -
                                 (sum(a_1990 * p_1990 * r_1990_prevalence) + sum(a_2021 * p_1990 * r_2021_prevalence)) / 3 -
                                 (sum(a_1990 * p_1990 * r_2021_prevalence) + sum(a_2021 * p_1990 * r_1990_prevalence)) / 6, 3)
  
  # 计算流行病学效应
  r_effect_prevalence <- round((sum(a_1990 * p_1990 * r_2021_prevalence) + sum(a_2021 * p_2021 * r_2021_prevalence)) / 3 + 
                                 (sum(a_1990 * p_2021 * r_2021_prevalence) + sum(a_2021 * p_1990 * r_2021_prevalence)) / 6 -
                                 (sum(a_1990 * p_1990 * r_1990_prevalence) + sum(a_2021 * p_2021 * r_1990_prevalence)) / 3 -
                                 (sum(a_1990 * p_2021 * r_1990_prevalence) + sum(a_2021 * p_1990 * r_1990_prevalence)) / 6, 3)
  
  # 计算总体差异
  overll_differ_prevalence <- round(a_effect_prevalence + p_effect_prevalence + r_effect_prevalence, 2)
  
  # 计算每个效应占总体差异的百分比
  a_percent_prevalence <- round(a_effect_prevalence / overll_differ_prevalence * 100, 2)
  p_percent_prevalence <- round(p_effect_prevalence / overll_differ_prevalence * 100, 2)
  r_percent_prevalence <- round(r_effect_prevalence / overll_differ_prevalence * 100, 2)
  
  # 将当前循环的地区和分解结果组合为一个行向量，转置后转换为数据框并追加到分解结果数据框中
  temp_prevalence <- c(loc, 
                       overll_differ_prevalence, 
                       a_effect_prevalence,
                       p_effect_prevalence, 
                       r_effect_prevalence, 
                       a_percent_prevalence, 
                       p_percent_prevalence, 
                       r_percent_prevalence) %>% 
    t() %>% as.data.frame()
  names(temp_prevalence) <- decomposition_name 
  decomposition_data_prevalence <- rbind(decomposition_data_prevalence, temp_prevalence) 
}



# 解决left_join多对多关系警告的问题

# 1. 确保发病数数据每个地区只保留一条记录
# 针对特定年龄段（70-74岁到90-94岁）汇总发病数
num_1990_prevalence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &  # 注意这里已去除"years"后缀
           year == 1990 &
           metric_name == 'Number' &
           measure_name == 'Prevalence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_1990 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

num_2021_prevalence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &
           year == 2021 &
           metric_name == 'Number' &
           measure_name == 'Prevalence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_2021 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

# 将发病数与按地区分类的分解结果合并
decomposition_data_prevalence <- left_join(decomposition_data_prevalence, num_1990_prevalence, by = 'location_name') %>% 
  left_join(num_2021_prevalence, by = 'location_name')

# 计算2021年与1990年发病数的差异
decomposition_data_prevalence$diff1 <- decomposition_data_prevalence$val_2021 - decomposition_data_prevalence$val_1990

# 将数据框的数值列转换为数值型（这里确保列范围选择正确，对应按地区分类后的列情况）
decomposition_data_prevalence[, 2:11] <- decomposition_data_prevalence[, 2:11] %>% apply(c(1, 2), as.numeric)



# 将分解结果保存为CSV文件
write.csv(decomposition_data_prevalence, "Prevalence_SDI.csv")





# 重命名列名以便于绘图展示（这里和之前类似，只是分析主体是地区了）
names(decomposition_data_prevalence)[2:5] <- c('Overll difference', 'Aging', 'Population', 'Epidemiological change')

# 将数据转换为适合绘图的长格式（操作逻辑不变，只是数据是按地区分类的了）
decomposition_plot_prevalence <- decomposition_data_prevalence[c(1:5)]
decomposition_plot_prevalence <- decomposition_plot_prevalence %>%
  pivot_longer(3:5, 
               names_to = "varname", 
               values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location_name = factor(location_name, levels = location_list, ordered = T))

# 自定义条形图的颜色（保持不变）
my_colors <- c("#FFFB73", "#33CCCC", "#FFC773")

# 绘制堆叠条形图，显示不同地区的分解效应（这里x轴变为地区名称了）
p_prevalence <- ggplot(decomposition_plot_prevalence, 
                       aes(x = location_name, 
                           y = value, 
                           fill = varname)) +
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = my_colors) + 
  theme_bw()
print(p_prevalence)
# 在图中添加表示总体差异的点（这里同样基于地区分类的数据来添加）
decomposition_data_prevalence$`Overll difference` <- as.numeric(decomposition_data_prevalence$`Overll difference`)
plot_prevalence <- p_prevalence + 
  geom_point(data = decomposition_data_prevalence, 
             mapping = aes(x = location_name, 
                           y = `Overll difference`), 
             fill = 'black', 
             color = 'black', 
             size = 3)

plot_prevalence  # 显示最终的图表








######################DALYs######################
decomposition_data_DALYs <- as.data.frame(matrix(nrow=0,
                                                 ncol=length(decomposition_name)))
names(decomposition_data_DALYs) <- decomposition_name

# 循环遍历每个地区，分别进行分解分析（这里以之前定义的地区列表为例）
for (loc in location_list) {
  # 筛选出1990年当前地区的人口数据
  Global_population_1990 <- population %>% 
    filter(location_name == loc  & 
             year == 1990) 
  
  # 计算1990年该地区的总人口数
  Global_1990 <- sum(Global_population_1990$val) 
  
  # 计算1990年不同年龄段的人口占比
  Global_population_1990$percent <- Global_population_1990$val / Global_1990 
  
  # 筛选出2021年当前地区的人口数据
  Global_population_2021 <- population %>% 
    filter(location_name == loc  & 
             year == 2021)
  
  # 计算2021年该地区的总人口数
  Global_2021 <- sum(Global_population_2021$val) 
  
  # 计算2021年不同年龄段的人口占比
  Global_population_2021$percent <- Global_population_2021$val / Global_2021 
  
  ### 提取1990年和2021年的年龄占比 (a) 和总人口 (p) 数据
  a_1990 <- Global_population_1990$percent  
  a_2021 <- Global_population_2021$percent  
  
  p_1990 <- Global_1990  
  p_2021 <- Global_2021  
  
  ## 获取发病率的数据
  case_1990_DALYs <- IBD_in_both %>% filter(year == 1990 &
                                              location_name == loc &
                                              metric_name == 'Rate' &
                                              measure_name == 'DALYs (Disability-Adjusted Life Years)')
  
  case_2021_DALYs <- IBD_in_both %>% filter(year == 2021 &
                                              location_name == loc &
                                              metric_name == 'Rate' &
                                              measure_name == 'DALYs (Disability-Adjusted Life Years)')
  
  # 将发病率单位转换为每10万人（除以10^5）
  r_1990_DALYs <- as.numeric(case_1990_DALYs$val) / 10^5  
  r_2021_DALYs <- as.numeric(case_2021_DALYs$val) / 10^5  
  
  ##### 根据公式计算三个效应 #####
  # 计算年龄效应
  a_effect_DALYs <- round((sum(a_2021 * p_1990 * r_1990_DALYs) + sum(a_2021 * p_2021 * r_2021_DALYs)) / 3 + 
                            (sum(a_2021 * p_1990 * r_2021_DALYs) + sum(a_2021 * p_2021 * r_1990_DALYs)) / 6 -
                            (sum(a_1990 * p_1990 * r_1990_DALYs) + sum(a_1990 * p_2021 * r_2021_DALYs)) / 3 -
                            (sum(a_1990 * p_1990 * r_2021_DALYs) + sum(a_1990 * p_2021 * r_1990_DALYs)) / 6, 3)
  
  # 计算人口效应
  p_effect_DALYs <- round((sum(a_1990 * p_2021 * r_1990_DALYs) + sum(a_2021 * p_2021 * r_2021_DALYs)) / 3 + 
                            (sum(a_1990 * p_2021 * r_2021_DALYs) + sum(a_2021 * p_2021 * r_1990_DALYs)) / 6 -
                            (sum(a_1990 * p_1990 * r_1990_DALYs) + sum(a_2021 * p_1990 * r_2021_DALYs)) / 3 -
                            (sum(a_1990 * p_1990 * r_2021_DALYs) + sum(a_2021 * p_1990 * r_1990_DALYs)) / 6, 3)
  
  # 计算流行病学效应
  r_effect_DALYs <- round((sum(a_1990 * p_1990 * r_2021_DALYs) + sum(a_2021 * p_2021 * r_2021_DALYs)) / 3 + 
                            (sum(a_1990 * p_2021 * r_2021_DALYs) + sum(a_2021 * p_1990 * r_2021_DALYs)) / 6 -
                            (sum(a_1990 * p_1990 * r_1990_DALYs) + sum(a_2021 * p_2021 * r_1990_DALYs)) / 3 -
                            (sum(a_1990 * p_2021 * r_1990_DALYs) + sum(a_2021 * p_1990 * r_1990_DALYs)) / 6, 3)
  
  # 计算总体差异
  overll_differ_DALYs <- round(a_effect_DALYs + p_effect_DALYs + r_effect_DALYs, 2)
  
  # 计算每个效应占总体差异的百分比
  a_percent_DALYs <- round(a_effect_DALYs / overll_differ_DALYs * 100, 2)
  p_percent_DALYs <- round(p_effect_DALYs / overll_differ_DALYs * 100, 2)
  r_percent_DALYs <- round(r_effect_DALYs / overll_differ_DALYs * 100, 2)
  
  # 将当前循环的地区和分解结果组合为一个行向量，转置后转换为数据框并追加到分解结果数据框中
  temp_DALYs <- c(loc, 
                  overll_differ_DALYs, 
                  a_effect_DALYs,
                  p_effect_DALYs, 
                  r_effect_DALYs, 
                  a_percent_DALYs, 
                  p_percent_DALYs, 
                  r_percent_DALYs) %>% 
    t() %>% as.data.frame()
  names(temp_DALYs) <- decomposition_name 
  decomposition_data_DALYs <- rbind(decomposition_data_DALYs, temp_DALYs) 
}



# 解决left_join多对多关系警告的问题

# 1. 确保发病数数据每个地区只保留一条记录
# 针对特定年龄段（70-74岁到90-94岁）汇总发病数
num_1990_DALYs <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &  # 注意这里已去除"years"后缀
           year == 1990 &
           metric_name == 'Number' &
           measure_name == 'DALYs (Disability-Adjusted Life Years)') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_1990 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

num_2021_DALYs <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &
           year == 2021 &
           metric_name == 'Number' &
           measure_name == 'DALYs (Disability-Adjusted Life Years)') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_2021 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

# 将发病数与按地区分类的分解结果合并
decomposition_data_DALYs <- left_join(decomposition_data_DALYs, num_1990_DALYs, by = 'location_name') %>% 
  left_join(num_2021_DALYs, by = 'location_name')

# 计算2021年与1990年发病数的差异
decomposition_data_DALYs$diff1 <- decomposition_data_DALYs$val_2021 - decomposition_data_DALYs$val_1990

# 将数据框的数值列转换为数值型（这里确保列范围选择正确，对应按地区分类后的列情况）
decomposition_data_DALYs[, 2:11] <- decomposition_data_DALYs[, 2:11] %>% apply(c(1, 2), as.numeric)


# 将分解结果保存为CSV文件
write.csv(decomposition_data_DALYs, "DALYs_SDI.csv")



# 重命名列名以便于绘图展示（这里和之前类似，只是分析主体是地区了）
names(decomposition_data_DALYs)[2:5] <- c('Overll difference', 'Aging', 'Population', 'Epidemiological change')

# 将数据转换为适合绘图的长格式（操作逻辑不变，只是数据是按地区分类的了）
decomposition_plot_DALYs <- decomposition_data_DALYs[c(1:5)]
decomposition_plot_DALYs <- decomposition_plot_DALYs %>%
  pivot_longer(3:5, 
               names_to = "varname", 
               values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location_name = factor(location_name, levels = location_list, ordered = T))

# 自定义条形图的颜色（保持不变）
my_colors <- c("#FFFB73", "#33CCCC", "#FFC773")

# 绘制堆叠条形图，显示不同地区的分解效应（这里x轴变为地区名称了）
p_DALYs <- ggplot(decomposition_plot_DALYs, 
                  aes(x = location_name, 
                      y = value, 
                      fill = varname)) +
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = my_colors) + 
  theme_bw()
print(p_DALYs)
# 在图中添加表示总体差异的点（这里同样基于地区分类的数据来添加）
decomposition_data_DALYs$`Overll difference` <- as.numeric(decomposition_data_DALYs$`Overll difference`)
plot_DALYs <- p_DALYs + 
  geom_point(data = decomposition_data_DALYs, 
             mapping = aes(x = location_name, 
                           y = `Overll difference`), 
             fill = 'black', 
             color = 'black', 
             size = 3)

plot_DALYs  # 显示最终的图表


# 加载 patchwork 包
library(patchwork)

# 假设你的三张图分别名为 p1, p2, p3
# 使用 + 符号将它们并排组合
combined_plot <- plot_incidence + plot_prevalence + plot_DALYs +
  # 设置布局，让三张图平均分配宽度
  plot_layout(widths = c(1, 1, 1)) +
  # 合并图例并放在底部
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    # 可以根据需要调整轴标签大小
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16)
  )

# 显示组合图
print(combined_plot)


# 加载所需包
library(patchwork)
library(ggplot2)

# 假设已有的三张图为 p1、p2、p3

# 调整每张图的主题，只保留左侧图的纵轴
plot_incidence <- plot_incidence + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 18),  # 保留纵轴标题
    axis.text.y = element_text(size = 12),    # 保留纵轴刻度文字
    axis.ticks.y = element_line()           # 保留纵轴刻度线
  )

plot_prevalence <- plot_prevalence + 
  theme(axis.text.x = element_text(size = 12),
    axis.title.y = element_blank(),          # 隐藏纵轴标题
    axis.text.y = element_blank(),           # 隐藏纵轴刻度文字
    axis.ticks.y = element_blank(),          # 隐藏纵轴刻度线
    axis.line.y = element_blank(),           # 隐藏纵轴线（如果存在）
  )


plot_DALYs <- plot_DALYs + 
  theme(axis.text.x = element_text(size = 12),
    axis.title.y = element_blank(),          # 隐藏纵轴标题
    axis.text.y = element_blank(),           # 隐藏纵轴刻度文字
    axis.ticks.y = element_blank(),          # 隐藏纵轴刻度线
    axis.line.y = element_blank(),           # 隐藏纵轴线（如果存在）
  )


# 组合三张图，确保纵轴对齐
combined_plot <- plot_incidence + plot_prevalence + plot_DALYs +
  plot_layout(
    widths = c(1, 1, 1),  # 宽度相等
    guides = "collect"    # 合并图例
  ) &
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 12)
  )


# 显示组合图
print(combined_plot)










#####地区####地区####地区####
######################Incidence######################

# 假设我们有多个地区的数据，先定义地区列表（这里示例包含中国、美国两个地区，可根据实际情况修改）
location_list <- c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", "Eastern Europe",
                   "High-income Asia Pacific", "Australasia", "Western Europe", "Southern Latin America", "High-income North America", 
                   "Caribbean","Andean Latin America","Central Latin America","Tropical Latin America",
                   "North Africa and Middle East", "South Asia", "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa", 
                   "Southern Sub-Saharan Africa","Western Sub-Saharan Africa") 

# 读取疾病数据时，筛选出特定地区（这里先以地区列表中的地区为例）的数据
IBD_in_both <- subset(IBD_china,
                      (IBD_china$age_name %in% age1 ) &
                        IBD_china$location_name %in% location_list)

# 后续对年龄相关处理保持不变，如转换年龄名称格式等操作
IBD_in_both$age_name<-gsub(" years","",IBD_in_both$age_name)
IBD_in_both$age_name <- factor(IBD_in_both$age_name, 
                               levels = c("70-74",
                                          "75-79", "80-84", "85-89", "90-94", "95+"))

path = "/Users/fqq/Documents/10 BAPC_/GBD_population/GBD_population"


fileName = dir(path)


fileName



var_name <- c("location_name","sex_name","year","age_id","age_name","val") 

# 读取人口数据并筛选地区相关数据
population <- data.frame()
for(k in 1:length(fileName)){
  data = fread(file = paste(path,fileName[k],sep = "/"))
  population = rbind(population,data)
}

population<-population%>% dplyr::select("location_name","sex_name","year","age_name","val") %>% 
  filter(location_name %in% location_list &  # 这里按地区列表筛选地区数据
           age_name %in% age2)

# 同样对人口数据中年龄名称格式等做后续处理，保持和前面一致
population$age_name<-gsub(" years","",population$age_name)
population$age_name <- factor(population$age_name, 
                              levels = c("70-74", 
                                         "75-79", "80-84", "85-89", "90-94", "95+"))
population<-subset(population,
                   population$year==1990|
                     population$year==2021)




# 制作一个空表格，用于存储分解结果，包括地区、总体差异、年龄效应、人口效应、流行病学效应及其百分比
decomposition_name <- c('location_name',
                        'overll_difference',
                        'a_effect',
                        'p_effect',
                        'r_effect',
                        'a_percent',
                        'p_percent',
                        'r_percent')
decomposition_data_incidence <- as.data.frame(matrix(nrow=0,
                                                     ncol=length(decomposition_name)))
names(decomposition_data_incidence) <- decomposition_name

# 循环遍历每个地区，分别进行分解分析（这里以之前定义的地区列表为例）
for (loc in location_list) {
  # 筛选出1990年当前地区的人口数据
  Global_population_1990 <- population %>% 
    filter(location_name == loc  & 
             year == 1990) 
  
  # 计算1990年该地区的总人口数
  Global_1990 <- sum(Global_population_1990$val) 
  
  # 计算1990年不同年龄段的人口占比
  Global_population_1990$percent <- Global_population_1990$val / Global_1990 
  
  # 筛选出2021年当前地区的人口数据
  Global_population_2021 <- population %>% 
    filter(location_name == loc  & 
             year == 2021)
  
  # 计算2021年该地区的总人口数
  Global_2021 <- sum(Global_population_2021$val) 
  
  # 计算2021年不同年龄段的人口占比
  Global_population_2021$percent <- Global_population_2021$val / Global_2021 
  
  ### 提取1990年和2021年的年龄占比 (a) 和总人口 (p) 数据
  a_1990 <- Global_population_1990$percent  
  a_2021 <- Global_population_2021$percent  
  
  p_1990 <- Global_1990  
  p_2021 <- Global_2021  
  
  ## 获取发病率的数据
  case_1990_incidence <- IBD_in_both %>% filter(year == 1990 &
                                                  location_name == loc &
                                                  metric_name == 'Rate' &
                                                  measure_name == 'Incidence')
  
  case_2021_incidence <- IBD_in_both %>% filter(year == 2021 &
                                                  location_name == loc &
                                                  metric_name == 'Rate' &
                                                  measure_name == 'Incidence')
  
  # 将发病率单位转换为每10万人（除以10^5）
  r_1990_incidence <- as.numeric(case_1990_incidence$val) / 10^5  
  r_2021_incidence <- as.numeric(case_2021_incidence$val) / 10^5  
  
  ##### 根据公式计算三个效应 #####
  # 计算年龄效应
  a_effect_incidence <- round((sum(a_2021 * p_1990 * r_1990_incidence) + sum(a_2021 * p_2021 * r_2021_incidence)) / 3 + 
                                (sum(a_2021 * p_1990 * r_2021_incidence) + sum(a_2021 * p_2021 * r_1990_incidence)) / 6 -
                                (sum(a_1990 * p_1990 * r_1990_incidence) + sum(a_1990 * p_2021 * r_2021_incidence)) / 3 -
                                (sum(a_1990 * p_1990 * r_2021_incidence) + sum(a_1990 * p_2021 * r_1990_incidence)) / 6, 3)
  
  # 计算人口效应
  p_effect_incidence <- round((sum(a_1990 * p_2021 * r_1990_incidence) + sum(a_2021 * p_2021 * r_2021_incidence)) / 3 + 
                                (sum(a_1990 * p_2021 * r_2021_incidence) + sum(a_2021 * p_2021 * r_1990_incidence)) / 6 -
                                (sum(a_1990 * p_1990 * r_1990_incidence) + sum(a_2021 * p_1990 * r_2021_incidence)) / 3 -
                                (sum(a_1990 * p_1990 * r_2021_incidence) + sum(a_2021 * p_1990 * r_1990_incidence)) / 6, 3)
  
  # 计算流行病学效应
  r_effect_incidence <- round((sum(a_1990 * p_1990 * r_2021_incidence) + sum(a_2021 * p_2021 * r_2021_incidence)) / 3 + 
                                (sum(a_1990 * p_2021 * r_2021_incidence) + sum(a_2021 * p_1990 * r_2021_incidence)) / 6 -
                                (sum(a_1990 * p_1990 * r_1990_incidence) + sum(a_2021 * p_2021 * r_1990_incidence)) / 3 -
                                (sum(a_1990 * p_2021 * r_1990_incidence) + sum(a_2021 * p_1990 * r_1990_incidence)) / 6, 3)
  
  # 计算总体差异
  overll_differ_incidence <- round(a_effect_incidence + p_effect_incidence + r_effect_incidence, 2)
  
  # 计算每个效应占总体差异的百分比
  a_percent_incidence <- round(a_effect_incidence / overll_differ_incidence * 100, 2)
  p_percent_incidence <- round(p_effect_incidence / overll_differ_incidence * 100, 2)
  r_percent_incidence <- round(r_effect_incidence / overll_differ_incidence * 100, 2)
  
  # 将当前循环的地区和分解结果组合为一个行向量，转置后转换为数据框并追加到分解结果数据框中
  temp_incidence <- c(loc, 
                      overll_differ_incidence, 
                      a_effect_incidence,
                      p_effect_incidence, 
                      r_effect_incidence, 
                      a_percent_incidence, 
                      p_percent_incidence, 
                      r_percent_incidence) %>% 
    t() %>% as.data.frame()
  names(temp_incidence) <- decomposition_name 
  decomposition_data_incidence <- rbind(decomposition_data_incidence, temp_incidence) 
}



# 解决left_join多对多关系警告的问题

# 1. 确保发病数数据每个地区只保留一条记录
# 针对特定年龄段（70-74岁到90-94岁）汇总发病数
num_1990_incidence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &  # 注意这里已去除"years"后缀
           year == 1990 &
           metric_name == 'Number' &
           measure_name == 'Incidence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_1990 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

num_2021_incidence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &
           year == 2021 &
           metric_name == 'Number' &
           measure_name == 'Incidence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_2021 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

# 将发病数与按地区分类的分解结果合并
decomposition_data_incidence <- left_join(decomposition_data_incidence, num_1990_incidence, by = 'location_name') %>% 
  left_join(num_2021_incidence, by = 'location_name')

# 计算2021年与1990年发病数的差异
decomposition_data_incidence$diff1 <- decomposition_data_incidence$val_2021 - decomposition_data_incidence$val_1990

# 将数据框的数值列转换为数值型（这里确保列范围选择正确，对应按地区分类后的列情况）
decomposition_data_incidence[, 2:11] <- decomposition_data_incidence[, 2:11] %>% apply(c(1, 2), as.numeric)


# 将分解结果保存为CSV文件
write.csv(decomposition_data_incidence, "Incidence_region.csv")





# 重命名列名以便于绘图展示（这里和之前类似，只是分析主体是地区了）
names(decomposition_data_incidence)[2:5] <- c('Overll difference', 'Aging', 'Population', 'Epidemiological change')

# 将数据转换为适合绘图的长格式（操作逻辑不变，只是数据是按地区分类的了）
decomposition_plot_incidence <- decomposition_data_incidence[c(1:5)]
decomposition_plot_incidence <- decomposition_plot_incidence %>%
  pivot_longer(3:5, 
               names_to = "varname", 
               values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location_name = factor(location_name, levels = location_list, ordered = T))

# 自定义条形图的颜色（保持不变）
my_colors <- c("#FFFB73", "#33CCCC", "#FFC773")

# 绘制堆叠条形图，显示不同地区的分解效应（这里x轴变为地区名称了）
p_incidence <- ggplot(decomposition_plot_incidence, 
                      aes(x = location_name, 
                          y = value, 
                          fill = varname)) +
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = my_colors) + 
  theme_bw()
print(p_incidence)
# 在图中添加表示总体差异的点（这里同样基于地区分类的数据来添加）
decomposition_data_incidence$`Overll difference` <- as.numeric(decomposition_data_incidence$`Overll difference`)
plot_incidence <- p_incidence + 
  geom_point(data = decomposition_data_incidence, 
             mapping = aes(x = location_name, 
                           y = `Overll difference`), 
             fill = 'black', 
             color = 'black', 
             size = 3)

plot_incidence  # 显示最终的图表



######################Prevalence######################
decomposition_data_prevalence <- as.data.frame(matrix(nrow=0,
                                                      ncol=length(decomposition_name)))
names(decomposition_data_prevalence) <- decomposition_name

# 循环遍历每个地区，分别进行分解分析（这里以之前定义的地区列表为例）
for (loc in location_list) {
  # 筛选出1990年当前地区的人口数据
  Global_population_1990 <- population %>% 
    filter(location_name == loc  & 
             year == 1990) 
  
  # 计算1990年该地区的总人口数
  Global_1990 <- sum(Global_population_1990$val) 
  
  # 计算1990年不同年龄段的人口占比
  Global_population_1990$percent <- Global_population_1990$val / Global_1990 
  
  # 筛选出2021年当前地区的人口数据
  Global_population_2021 <- population %>% 
    filter(location_name == loc  & 
             year == 2021)
  
  # 计算2021年该地区的总人口数
  Global_2021 <- sum(Global_population_2021$val) 
  
  # 计算2021年不同年龄段的人口占比
  Global_population_2021$percent <- Global_population_2021$val / Global_2021 
  
  ### 提取1990年和2021年的年龄占比 (a) 和总人口 (p) 数据
  a_1990 <- Global_population_1990$percent  
  a_2021 <- Global_population_2021$percent  
  
  p_1990 <- Global_1990  
  p_2021 <- Global_2021  
  
  ## 获取发病率的数据
  case_1990_prevalence <- IBD_in_both %>% filter(year == 1990 &
                                                   location_name == loc &
                                                   metric_name == 'Rate' &
                                                   measure_name == 'Prevalence')
  
  case_2021_prevalence <- IBD_in_both %>% filter(year == 2021 &
                                                   location_name == loc &
                                                   metric_name == 'Rate' &
                                                   measure_name == 'Prevalence')
  
  # 将发病率单位转换为每10万人（除以10^5）
  r_1990_prevalence <- as.numeric(case_1990_prevalence$val) / 10^5  
  r_2021_prevalence <- as.numeric(case_2021_prevalence$val) / 10^5  
  
  ##### 根据公式计算三个效应 #####
  # 计算年龄效应
  a_effect_prevalence <- round((sum(a_2021 * p_1990 * r_1990_prevalence) + sum(a_2021 * p_2021 * r_2021_prevalence)) / 3 + 
                                 (sum(a_2021 * p_1990 * r_2021_prevalence) + sum(a_2021 * p_2021 * r_1990_prevalence)) / 6 -
                                 (sum(a_1990 * p_1990 * r_1990_prevalence) + sum(a_1990 * p_2021 * r_2021_prevalence)) / 3 -
                                 (sum(a_1990 * p_1990 * r_2021_prevalence) + sum(a_1990 * p_2021 * r_1990_prevalence)) / 6, 3)
  
  # 计算人口效应
  p_effect_prevalence <- round((sum(a_1990 * p_2021 * r_1990_prevalence) + sum(a_2021 * p_2021 * r_2021_prevalence)) / 3 + 
                                 (sum(a_1990 * p_2021 * r_2021_prevalence) + sum(a_2021 * p_2021 * r_1990_prevalence)) / 6 -
                                 (sum(a_1990 * p_1990 * r_1990_prevalence) + sum(a_2021 * p_1990 * r_2021_prevalence)) / 3 -
                                 (sum(a_1990 * p_1990 * r_2021_prevalence) + sum(a_2021 * p_1990 * r_1990_prevalence)) / 6, 3)
  
  # 计算流行病学效应
  r_effect_prevalence <- round((sum(a_1990 * p_1990 * r_2021_prevalence) + sum(a_2021 * p_2021 * r_2021_prevalence)) / 3 + 
                                 (sum(a_1990 * p_2021 * r_2021_prevalence) + sum(a_2021 * p_1990 * r_2021_prevalence)) / 6 -
                                 (sum(a_1990 * p_1990 * r_1990_prevalence) + sum(a_2021 * p_2021 * r_1990_prevalence)) / 3 -
                                 (sum(a_1990 * p_2021 * r_1990_prevalence) + sum(a_2021 * p_1990 * r_1990_prevalence)) / 6, 3)
  
  # 计算总体差异
  overll_differ_prevalence <- round(a_effect_prevalence + p_effect_prevalence + r_effect_prevalence, 2)
  
  # 计算每个效应占总体差异的百分比
  a_percent_prevalence <- round(a_effect_prevalence / overll_differ_prevalence * 100, 2)
  p_percent_prevalence <- round(p_effect_prevalence / overll_differ_prevalence * 100, 2)
  r_percent_prevalence <- round(r_effect_prevalence / overll_differ_prevalence * 100, 2)
  
  # 将当前循环的地区和分解结果组合为一个行向量，转置后转换为数据框并追加到分解结果数据框中
  temp_prevalence <- c(loc, 
                       overll_differ_prevalence, 
                       a_effect_prevalence,
                       p_effect_prevalence, 
                       r_effect_prevalence, 
                       a_percent_prevalence, 
                       p_percent_prevalence, 
                       r_percent_prevalence) %>% 
    t() %>% as.data.frame()
  names(temp_prevalence) <- decomposition_name 
  decomposition_data_prevalence <- rbind(decomposition_data_prevalence, temp_prevalence) 
}



# 解决left_join多对多关系警告的问题

# 1. 确保发病数数据每个地区只保留一条记录
# 针对特定年龄段（70-74岁到90-94岁）汇总发病数
num_1990_prevalence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &  # 注意这里已去除"years"后缀
           year == 1990 &
           metric_name == 'Number' &
           measure_name == 'Prevalence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_1990 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

num_2021_prevalence <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &
           year == 2021 &
           metric_name == 'Number' &
           measure_name == 'Prevalence') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_2021 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

# 将发病数与按地区分类的分解结果合并
decomposition_data_prevalence <- left_join(decomposition_data_prevalence, num_1990_prevalence, by = 'location_name') %>% 
  left_join(num_2021_prevalence, by = 'location_name')

# 计算2021年与1990年发病数的差异
decomposition_data_prevalence$diff1 <- decomposition_data_prevalence$val_2021 - decomposition_data_prevalence$val_1990

# 将数据框的数值列转换为数值型（这里确保列范围选择正确，对应按地区分类后的列情况）
decomposition_data_prevalence[, 2:11] <- decomposition_data_prevalence[, 2:11] %>% apply(c(1, 2), as.numeric)


# 将分解结果保存为CSV文件
write.csv(decomposition_data_prevalence, "Prevalence_region.csv")




# 重命名列名以便于绘图展示（这里和之前类似，只是分析主体是地区了）
names(decomposition_data_prevalence)[2:5] <- c('Overll difference', 'Aging', 'Population', 'Epidemiological change')

# 将数据转换为适合绘图的长格式（操作逻辑不变，只是数据是按地区分类的了）
decomposition_plot_prevalence <- decomposition_data_prevalence[c(1:5)]
decomposition_plot_prevalence <- decomposition_plot_prevalence %>%
  pivot_longer(3:5, 
               names_to = "varname", 
               values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location_name = factor(location_name, levels = location_list, ordered = T))

# 自定义条形图的颜色（保持不变）
my_colors <- c("#FFFB73", "#33CCCC", "#FFC773")

# 绘制堆叠条形图，显示不同地区的分解效应（这里x轴变为地区名称了）
p_prevalence <- ggplot(decomposition_plot_prevalence, 
                       aes(x = location_name, 
                           y = value, 
                           fill = varname)) +
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = my_colors) + 
  theme_bw()
print(p_prevalence)
# 在图中添加表示总体差异的点（这里同样基于地区分类的数据来添加）
decomposition_data_prevalence$`Overll difference` <- as.numeric(decomposition_data_prevalence$`Overll difference`)
plot_prevalence <- p_prevalence + 
  geom_point(data = decomposition_data_prevalence, 
             mapping = aes(x = location_name, 
                           y = `Overll difference`), 
             fill = 'black', 
             color = 'black', 
             size = 3)

plot_prevalence  # 显示最终的图表









######################DALYs######################
decomposition_data_DALYs <- as.data.frame(matrix(nrow=0,
                                                 ncol=length(decomposition_name)))
names(decomposition_data_DALYs) <- decomposition_name

# 循环遍历每个地区，分别进行分解分析（这里以之前定义的地区列表为例）
for (loc in location_list) {
  # 筛选出1990年当前地区的人口数据
  Global_population_1990 <- population %>% 
    filter(location_name == loc  & 
             year == 1990) 
  
  # 计算1990年该地区的总人口数
  Global_1990 <- sum(Global_population_1990$val) 
  
  # 计算1990年不同年龄段的人口占比
  Global_population_1990$percent <- Global_population_1990$val / Global_1990 
  
  # 筛选出2021年当前地区的人口数据
  Global_population_2021 <- population %>% 
    filter(location_name == loc  & 
             year == 2021)
  
  # 计算2021年该地区的总人口数
  Global_2021 <- sum(Global_population_2021$val) 
  
  # 计算2021年不同年龄段的人口占比
  Global_population_2021$percent <- Global_population_2021$val / Global_2021 
  
  ### 提取1990年和2021年的年龄占比 (a) 和总人口 (p) 数据
  a_1990 <- Global_population_1990$percent  
  a_2021 <- Global_population_2021$percent  
  
  p_1990 <- Global_1990  
  p_2021 <- Global_2021  
  
  ## 获取发病率的数据
  case_1990_DALYs <- IBD_in_both %>% filter(year == 1990 &
                                              location_name == loc &
                                              metric_name == 'Rate' &
                                              measure_name == 'DALYs (Disability-Adjusted Life Years)')
  
  case_2021_DALYs <- IBD_in_both %>% filter(year == 2021 &
                                              location_name == loc &
                                              metric_name == 'Rate' &
                                              measure_name == 'DALYs (Disability-Adjusted Life Years)')
  
  # 将发病率单位转换为每10万人（除以10^5）
  r_1990_DALYs <- as.numeric(case_1990_DALYs$val) / 10^5  
  r_2021_DALYs <- as.numeric(case_2021_DALYs$val) / 10^5  
  
  ##### 根据公式计算三个效应 #####
  # 计算年龄效应
  a_effect_DALYs <- round((sum(a_2021 * p_1990 * r_1990_DALYs) + sum(a_2021 * p_2021 * r_2021_DALYs)) / 3 + 
                            (sum(a_2021 * p_1990 * r_2021_DALYs) + sum(a_2021 * p_2021 * r_1990_DALYs)) / 6 -
                            (sum(a_1990 * p_1990 * r_1990_DALYs) + sum(a_1990 * p_2021 * r_2021_DALYs)) / 3 -
                            (sum(a_1990 * p_1990 * r_2021_DALYs) + sum(a_1990 * p_2021 * r_1990_DALYs)) / 6, 3)
  
  # 计算人口效应
  p_effect_DALYs <- round((sum(a_1990 * p_2021 * r_1990_DALYs) + sum(a_2021 * p_2021 * r_2021_DALYs)) / 3 + 
                            (sum(a_1990 * p_2021 * r_2021_DALYs) + sum(a_2021 * p_2021 * r_1990_DALYs)) / 6 -
                            (sum(a_1990 * p_1990 * r_1990_DALYs) + sum(a_2021 * p_1990 * r_2021_DALYs)) / 3 -
                            (sum(a_1990 * p_1990 * r_2021_DALYs) + sum(a_2021 * p_1990 * r_1990_DALYs)) / 6, 3)
  
  # 计算流行病学效应
  r_effect_DALYs <- round((sum(a_1990 * p_1990 * r_2021_DALYs) + sum(a_2021 * p_2021 * r_2021_DALYs)) / 3 + 
                            (sum(a_1990 * p_2021 * r_2021_DALYs) + sum(a_2021 * p_1990 * r_2021_DALYs)) / 6 -
                            (sum(a_1990 * p_1990 * r_1990_DALYs) + sum(a_2021 * p_2021 * r_1990_DALYs)) / 3 -
                            (sum(a_1990 * p_2021 * r_1990_DALYs) + sum(a_2021 * p_1990 * r_1990_DALYs)) / 6, 3)
  
  # 计算总体差异
  overll_differ_DALYs <- round(a_effect_DALYs + p_effect_DALYs + r_effect_DALYs, 2)
  
  # 计算每个效应占总体差异的百分比
  a_percent_DALYs <- round(a_effect_DALYs / overll_differ_DALYs * 100, 2)
  p_percent_DALYs <- round(p_effect_DALYs / overll_differ_DALYs * 100, 2)
  r_percent_DALYs <- round(r_effect_DALYs / overll_differ_DALYs * 100, 2)
  
  # 将当前循环的地区和分解结果组合为一个行向量，转置后转换为数据框并追加到分解结果数据框中
  temp_DALYs <- c(loc, 
                  overll_differ_DALYs, 
                  a_effect_DALYs,
                  p_effect_DALYs, 
                  r_effect_DALYs, 
                  a_percent_DALYs, 
                  p_percent_DALYs, 
                  r_percent_DALYs) %>% 
    t() %>% as.data.frame()
  names(temp_DALYs) <- decomposition_name 
  decomposition_data_DALYs <- rbind(decomposition_data_DALYs, temp_DALYs) 
}



# 解决left_join多对多关系警告的问题

# 1. 确保发病数数据每个地区只保留一条记录
# 针对特定年龄段（70-74岁到90-94岁）汇总发病数
num_1990_DALYs <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &  # 注意这里已去除"years"后缀
           year == 1990 &
           metric_name == 'Number' &
           measure_name == 'DALYs (Disability-Adjusted Life Years)') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_1990 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

num_2021_DALYs <- IBD_in_both %>% 
  filter(age_name %in% c("70-74", "75-79", "80-84", "85-89", "90-94", "95+") &
           year == 2021 &
           metric_name == 'Number' &
           measure_name == 'DALYs (Disability-Adjusted Life Years)') %>%
  group_by(location_name) %>%  # 按地区分组
  summarise(val_2021 = sum(val)) %>%  # 汇总各年龄段的发病数
  ungroup()

# 将发病数与按地区分类的分解结果合并
decomposition_data_DALYs <- left_join(decomposition_data_DALYs, num_1990_DALYs, by = 'location_name') %>% 
  left_join(num_2021_DALYs, by = 'location_name')

# 计算2021年与1990年发病数的差异
decomposition_data_DALYs$diff1 <- decomposition_data_DALYs$val_2021 - decomposition_data_DALYs$val_1990

# 将数据框的数值列转换为数值型（这里确保列范围选择正确，对应按地区分类后的列情况）
decomposition_data_DALYs[, 2:11] <- decomposition_data_DALYs[, 2:11] %>% apply(c(1, 2), as.numeric)



# 将分解结果保存为CSV文件
write.csv(decomposition_data_DALYs, "DALYs_region.csv")





# 重命名列名以便于绘图展示（这里和之前类似，只是分析主体是地区了）
names(decomposition_data_DALYs)[2:5] <- c('Overll difference', 'Aging', 'Population', 'Epidemiological change')

# 将数据转换为适合绘图的长格式（操作逻辑不变，只是数据是按地区分类的了）
decomposition_plot_DALYs <- decomposition_data_DALYs[c(1:5)]
decomposition_plot_DALYs <- decomposition_plot_DALYs %>%
  pivot_longer(3:5, 
               names_to = "varname", 
               values_to = "value") %>% 
  mutate(value = as.numeric(value)) %>% 
  mutate(location_name = factor(location_name, levels = location_list, ordered = T))

# 自定义条形图的颜色（保持不变）
my_colors <- c("#FFFB73", "#33CCCC", "#FFC773")

# 绘制堆叠条形图，显示不同地区的分解效应（这里x轴变为地区名称了）
p_DALYs <- ggplot(decomposition_plot_DALYs, 
                  aes(x = location_name, 
                      y = value, 
                      fill = varname)) +
  geom_bar(stat = "identity", 
           position = "stack") + 
  coord_flip() + 
  scale_fill_manual(values = my_colors) + 
  theme_bw()
print(p_DALYs)
# 在图中添加表示总体差异的点（这里同样基于地区分类的数据来添加）
decomposition_data_DALYs$`Overll difference` <- as.numeric(decomposition_data_DALYs$`Overll difference`)
plot_DALYs <- p_DALYs + 
  geom_point(data = decomposition_data_DALYs, 
             mapping = aes(x = location_name, 
                           y = `Overll difference`), 
             fill = 'black', 
             color = 'black', 
             size = 3)

plot_DALYs  # 显示最终的图表


# 加载 patchwork 包
library(patchwork)

# 假设你的三张图分别名为 p1, p2, p3
# 使用 + 符号将它们并排组合
combined_plot <- plot_incidence + plot_prevalence + plot_DALYs +
  # 设置布局，让三张图平均分配宽度
  plot_layout(widths = c(1, 1, 1)) +
  # 合并图例并放在底部
  plot_layout(guides = "collect") &
  theme(
    legend.position = "right",
    # 可以根据需要调整轴标签大小
    axis.text.x = element_text(size = 8),
    axis.text.y = element_text(size = 8)
  )

# 显示组合图
print(combined_plot)



# 加载所需包
library(patchwork)
library(ggplot2)

# 假设已有的三张图为 p1、p2、p3

# 调整每张图的主题，只保留左侧图的纵轴
plot_incidence <- plot_incidence + 
  theme(
    axis.title.y = element_text(size = 10),  # 保留纵轴标题
    axis.text.y = element_text(size = 8),    # 保留纵轴刻度文字
    axis.ticks.y = element_line()           # 保留纵轴刻度线
  )

plot_prevalence <- plot_prevalence + 
  theme(
    axis.title.y = element_blank(),          # 隐藏纵轴标题
    axis.text.y = element_blank(),           # 隐藏纵轴刻度文字
    axis.ticks.y = element_blank(),          # 隐藏纵轴刻度线
    axis.line.y = element_blank(),           # 隐藏纵轴线（如果存在）
  )

plot_DALYs <- plot_DALYs + 
  theme(
    axis.title.y = element_blank(),          # 隐藏纵轴标题
    axis.text.y = element_blank(),           # 隐藏纵轴刻度文字
    axis.ticks.y = element_blank(),          # 隐藏纵轴刻度线
    axis.line.y = element_blank(),           # 隐藏纵轴线（如果存在）
  )

# 组合三张图，确保纵轴对齐
combined_plot <- plot_incidence + plot_prevalence + plot_DALYs +
  plot_layout(
    widths = c(1, 1, 1),  # 宽度相等
    guides = "collect"    # 合并图例
  ) &
  theme(
    legend.position = "right",
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 8)
  )


# 显示组合图
print(combined_plot)




plot_incidence <- plot_incidence + 
  theme(
    axis.text.x = element_text(size = 12),
    axis.title.y = element_text(size = 18),  # 保留纵轴标题
    axis.text.y = element_text(size = 12),    # 保留纵轴刻度文字
    axis.ticks.y = element_line()           # 保留纵轴刻度线
  )

plot_prevalence <- plot_prevalence + 
  theme(axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),          # 隐藏纵轴标题
        axis.text.y = element_blank(),           # 隐藏纵轴刻度文字
        axis.ticks.y = element_blank(),          # 隐藏纵轴刻度线
        axis.line.y = element_blank(),           # 隐藏纵轴线（如果存在）
  )


plot_DALYs <- plot_DALYs + 
  theme(axis.text.x = element_text(size = 12),
        axis.title.y = element_blank(),          # 隐藏纵轴标题
        axis.text.y = element_blank(),           # 隐藏纵轴刻度文字
        axis.ticks.y = element_blank(),          # 隐藏纵轴刻度线
        axis.line.y = element_blank(),           # 隐藏纵轴线（如果存在）
  )


# 组合三张图，确保纵轴对齐
combined_plot <- plot_incidence + plot_prevalence + plot_DALYs +
  plot_layout(
    widths = c(1, 1, 1),  # 宽度相等
    guides = "collect"    # 合并图例
  ) &
  theme(
    legend.position = "bottom",
    legend.key.size = unit(0.8, "cm"),
    legend.text = element_text(size = 12)
  )


# 显示组合图
print(combined_plot)