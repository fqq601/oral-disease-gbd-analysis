# 设置工作目录（替换为你的实际路径）
setwd("/Users/fqq/Documents/GBD口腔老年人/ESI")
data <- fread("Age_standardized_data.csv") # 读取权重数据



# 显示数据集中age_name列的唯一值
unique(data$age_name)  # 使用unique()函数查看age_name列中的唯一值（即不重复的值）




#--------------------------------------Incidence--------------------------------------
#
# 筛选数据，选择发病率（measure_name为'Incidence'），年龄标准化（age_name为"Age-standardized"），以及度量名称为'Rate'的数据
ASIR <- subset(data, 
               
               
               data$measure_name=='Incidence' &
                 
                 
                 (data$age_name == "Age-standardized") &
                 
                 
                 data$metric_name== 'Rate')  # 使用subset()函数筛选出符合条件的数据，并保存为ASIR

View(ASIR)


# 计算发病率的标准误差，公式为(上限-下限)/(1.96*2)，这是基于95%置信区间的计算
ASIR$SE <- (ASIR$upper-ASIR$lower)/(1.96*2)  # 根据95%置信区间计算标准误差并添加到数据框的SE列中



#--------------------------------------sex--------------------------------------
#-----------ASIR_Total_Global_sex---------------

ASIR_Total <- subset(ASIR,
               location_name == "Global" &
                 cause_name == "Oral disorders")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_Total <- ASIR_Total[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_Total <- ASIR_Total[order(ASIR_Total$sex_name,ASIR_Total$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_Total_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASIR_Total, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-----------ASIR_CPT_Global_sex---------------

ASIR_CPT <- subset(ASIR,
                     location_name == "Global" &
                       cause_name == "Caries of permanent teeth")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_CPT <- ASIR_CPT[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_CPT <- ASIR_CPT[order(ASIR_CPT$sex_name,ASIR_CPT$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_CPT_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASIR_CPT, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件





#-----------ASIR_Edentulism_Global_sex---------------

ASIR_Edentulism <- subset(ASIR,
                   location_name == "Global" &
                     cause_name == "Edentulism")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_Edentulism <- ASIR_Edentulism[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_Edentulism <- ASIR_Edentulism[order(ASIR_Edentulism$sex_name,ASIR_Edentulism$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_Edentulism_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASIR_Edentulism, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-----------ASIR_PD_Global_sex---------------

ASIR_PD <- subset(ASIR,
                          location_name == "Global" &
                            cause_name == "Periodontal diseases")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_PD <- ASIR_PD[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_PD <- ASIR_PD[order(ASIR_PD$sex_name,ASIR_PD$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_PD_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASIR_PD, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#--------------------------------------SDI--------------------------------------
#-----------ASIR_SDI---------------

ASIR_SDI <- ASIR %>% 
  filter(cause_name == "Oral disorders",
         sex_name=="Both",
         location_name %in% c("High SDI", "High-middle SDI", "Middle SDI",
                              "Low-middle SDI", "Low SDI"))



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_SDI <- ASIR_SDI[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_SDI <- ASIR_SDI[order(ASIR_SDI$location_name,ASIR_SDI$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_SDI.csv"
# 写入 Excel 文件
write_csv(ASIR_SDI, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件





#--------------------------------------Region--------------------------------------
#-----------ASIR_Region---------------

ASIR_Region <- ASIR %>% 
  filter(cause_name == "Oral disorders",
         sex_name=="Both",
         location_name %in% c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", "Eastern Europe",
                              "High-income Asia Pacific", "Australasia", "Western Europe", "Southern Latin America", "High-income North America", 
                              "Caribbean","Andean Latin America","Central Latin America","Tropical Latin America",
                              "North Africa and Middle East", "South Asia", "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa", 
                              "Southern Sub-Saharan Africa","Western Sub-Saharan Africa"))



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_Region <- ASIR_Region[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_Region <- ASIR_Region[order(ASIR_Region$location_name,ASIR_Region$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_Region.csv"
# 写入 Excel 文件
write_csv(ASIR_Region, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-------------------------------------- location--------------------------------------
#-----------ASIR_location---------------
ASIR_location <- ASIR %>% 
  filter(cause_name == "Oral disorders",
         sex_name == "Both",
         !location_name %in% c(
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



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASIR_location <- ASIR_location[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASIR_location <- ASIR_location[order(ASIR_location$location_name,ASIR_location$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASIR_location.csv"
# 写入 Excel 文件
write_csv(ASIR_location, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件








#--------------------------------------Prevalence--------------------------------------
#
# 筛选数据，选择发病率（measure_name为'Incidence'），年龄标准化（age_name为"Age-standardized"），以及度量名称为'Rate'的数据
ASPR <- subset(data, 
               
               
               data$measure_name=='Prevalence' &
                 
                 
                 (data$age_name == "Age-standardized") &
                 
                 
                 data$metric_name== 'Rate')  # 使用subset()函数筛选出符合条件的数据，并保存为ASIR




# 计算发病率的标准误差，公式为(上限-下限)/(1.96*2)，这是基于95%置信区间的计算
ASPR$SE <- (ASPR$upper-ASPR$lower)/(1.96*2)  # 根据95%置信区间计算标准误差并添加到数据框的SE列中



#--------------------------------------sex--------------------------------------
#-----------ASPR_Total_Global_sex---------------

ASPR_Total <- subset(ASPR,
                     location_name == "Global" &
                       cause_name == "Oral disorders")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_Total <- ASPR_Total[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_Total <- ASPR_Total[order(ASPR_Total$sex_name,ASPR_Total$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_Total_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASPR_Total, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-----------ASPR_CPT_Global_sex---------------

ASPR_CPT <- subset(ASPR,
                   location_name == "Global" &
                     cause_name == "Caries of permanent teeth")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_CPT <- ASPR_CPT[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_CPT <- ASPR_CPT[order(ASPR_CPT$sex_name,ASPR_CPT$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_CPT_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASPR_CPT, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件





#-----------ASPR_Edentulism_Global_sex---------------

ASPR_Edentulism <- subset(ASPR,
                          location_name == "Global" &
                            cause_name == "Edentulism")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_Edentulism <- ASPR_Edentulism[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_Edentulism <- ASPR_Edentulism[order(ASPR_Edentulism$sex_name,ASPR_Edentulism$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_Edentulism_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASPR_Edentulism, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-----------ASPR_PD_Global_sex---------------

ASPR_PD <- subset(ASPR,
                  location_name == "Global" &
                    cause_name == "Periodontal diseases")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_PD <- ASPR_PD[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_PD <- ASPR_PD[order(ASPR_PD$sex_name,ASPR_PD$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_PD_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASPR_PD, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#--------------------------------------SDI--------------------------------------
#-----------ASPR_SDI---------------

ASPR_SDI <- ASPR %>% 
  filter(cause_name == "Oral disorders",
         sex_name=="Both",
         location_name %in% c("High SDI", "High-middle SDI", "Middle SDI",
                              "Low-middle SDI", "Low SDI"))



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_SDI <- ASPR_SDI[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_SDI <- ASPR_SDI[order(ASPR_SDI$location_name,ASPR_SDI$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_SDI.csv"
# 写入 Excel 文件
write_csv(ASPR_SDI, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件





#--------------------------------------Region--------------------------------------
#-----------ASPR_Region---------------

ASPR_Region <- ASPR %>% 
  filter(cause_name == "Oral disorders",
         sex_name=="Both",
         location_name %in% c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", "Eastern Europe",
                              "High-income Asia Pacific", "Australasia", "Western Europe", "Southern Latin America", "High-income North America", 
                              "Caribbean","Andean Latin America","Central Latin America","Tropical Latin America",
                              "North Africa and Middle East", "South Asia", "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa", 
                              "Southern Sub-Saharan Africa","Western Sub-Saharan Africa"))



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_Region <- ASPR_Region[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_Region <- ASPR_Region[order(ASPR_Region$location_name,ASPR_Region$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_Region.csv"
# 写入 Excel 文件
write_csv(ASPR_Region, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-------------------------------------- location--------------------------------------
#-----------ASIR_location---------------
ASPR_location <- ASPR %>% 
  filter(cause_name == "Oral disorders",
         sex_name == "Both",
         !location_name %in% c(
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



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASPR_location <- ASPR_location[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASPR_location <- ASPR_location[order(ASPR_location$location_name,ASPR_location$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASPR_location.csv"
# 写入 Excel 文件
write_csv(ASPR_location, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件









#--------------------------------------ASDR--------------------------------------
#
# 筛选数据，选择发病率（measure_name为'ASDR'），年龄标准化（age_name为"Age-standardized"），以及度量名称为'Rate'的数据
ASDR <- subset(data, 
               
               
               data$measure_name=='DALYs (Disability-Adjusted Life Years)' &
                 
                 
                 (data$age_name == "Age-standardized") &
                 
                 
                 data$metric_name== 'Rate')  # 使用subset()函数筛选出符合条件的数据，并保存为ASIR




# 计算发病率的标准误差，公式为(上限-下限)/(1.96*2)，这是基于95%置信区间的计算
ASDR$SE <- (ASDR$upper-ASDR$lower)/(1.96*2)  # 根据95%置信区间计算标准误差并添加到数据框的SE列中



#--------------------------------------sex--------------------------------------
#-----------ASIR_Total_Global_sex---------------

ASDR_Total <- subset(ASDR,
                     location_name == "Global" &
                       cause_name == "Oral disorders")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_Total <- ASDR_Total[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_Total <- ASDR_Total[order(ASDR_Total$sex_name,ASDR_Total$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_Total_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASDR_Total, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-----------ASIR_CPT_Global_sex---------------

ASDR_CPT <- subset(ASDR,
                   location_name == "Global" &
                     cause_name == "Caries of permanent teeth")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_CPT <- ASDR_CPT[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_CPT <- ASDR_CPT[order(ASDR_CPT$sex_name,ASDR_CPT$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_CPT_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASDR_CPT, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件





#-----------ASIR_Edentulism_Global_sex---------------

ASDR_Edentulism <- subset(ASDR,
                          location_name == "Global" &
                            cause_name == "Edentulism")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_Edentulism <- ASDR_Edentulism[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_Edentulism <- ASDR_Edentulism[order(ASDR_Edentulism$sex_name,ASDR_Edentulism$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_Edentulism_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASDR_Edentulism, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#-----------ASIR_PD_Global_sex---------------

ASDR_PD <- subset(ASDR,
                  location_name == "Global" &
                    cause_name == "Periodontal diseases")



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_PD <- ASDR_PD[,c("sex_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_PD <- ASDR_PD[order(ASDR_PD$sex_name,ASDR_PD$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_PD_Global_sex.csv"
# 写入 Excel 文件
write_csv(ASDR_PD, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件




#--------------------------------------SDI--------------------------------------
#-----------ASIR_SDI---------------

ASDR_SDI <- ASDR %>% 
  filter(cause_name == "Oral disorders",
         sex_name=="Both",
         location_name %in% c("High SDI", "High-middle SDI", "Middle SDI",
                              "Low-middle SDI", "Low SDI"))



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_SDI <- ASDR_SDI[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_SDI <- ASDR_SDI[order(ASDR_SDI$location_name,ASDR_SDI$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_SDI.csv"
# 写入 Excel 文件
write_csv(ASDR_SDI, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件





#--------------------------------------Region--------------------------------------
#-----------ASIR_Region---------------

ASDR_Region <- ASDR %>% 
  filter(cause_name == "Oral disorders",
         sex_name=="Both",
         location_name %in% c("East Asia", "Southeast Asia", "Oceania", "Central Asia", "Central Europe", "Eastern Europe",
                              "High-income Asia Pacific", "Australasia", "Western Europe", "Southern Latin America", "High-income North America", 
                              "Caribbean","Andean Latin America","Central Latin America","Tropical Latin America",
                              "North Africa and Middle East", "South Asia", "Central Sub-Saharan Africa", "Eastern Sub-Saharan Africa", 
                              "Southern Sub-Saharan Africa","Western Sub-Saharan Africa"))



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_Region <- ASDR_Region[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_Region <- ASDR_Region[order(ASDR_Region$location_name,ASDR_Region$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_Region.csv"
# 写入 Excel 文件
write_csv(ASDR_Region, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件



#-------------------------------------- location--------------------------------------
#-----------ASIR_location---------------
ASDR_location <- ASDR %>% 
  filter(cause_name == "Oral disorders",
         sex_name == "Both",
         !location_name %in% c(
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



# 从ASIR数据框中选择性别、年份、数值和标准误差这几列
ASDR_location <- ASDR_location[,c("location_name","year","val","SE")]  # 使用列选择方式提取需要的列，并用这些列更新ASIR数据框





# 将数据按性别和年份升序排列，以便进行Joinpoint回归分析
ASDR_location <- ASDR_location[order(ASDR_location$location_name,ASDR_location$year),]  # 使用order()函数对数据进行排序，先按性别再按年份升序排列



library(readr)
# 将处理后的数据写入CSV文件
# 定义保存路径
output_path <- "/Users/fqq/Documents/GBD口腔老年人/Joinpoint/ASDR_location.csv"
# 写入 Excel 文件
write_csv(ASDR_location, output_path)  # 使用write.csv()函数将处理后的数据保存到CSV文件中，文件名为'Incidence_joinpoint.csv'
# 尝试再次创建文件








####joinpoint上打开Incidence_joinpoint.csv进行回归分析####
# 注：此处你需要在Joinpoint软件中打开Incidence_joinpoint.csv进行实际的回归分析




#--------Incidence_Total-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-Total-sex-incidence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Incidence_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'



#--------Prevalence_Total-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-Total-sex-prevalence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 








#--------ASDR_Total-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-Total-sex-ASDR.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 











#--------Incidence_CPT-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-CPT-sex-incidence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Incidence_CPT_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_CPT_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'




#--------Prevalence_CPT-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-CPT-sex-prevalence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_CPT_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 









#--------ASDR_CPT-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-CPT-sex-ASDR.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_CPT_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 











#--------Incidence_Edentulism-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-Edentulism-sex-incidence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Incidence_Edentulism_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_CPT_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'





#--------Prevalence_Edentulism-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-Edentulism-sex-prevalence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_Edentulism_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 


#--------ASDR_Edentulism-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-Edentulism-sex-ASDR.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_Edentulism_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 












#--------Incidence_PD-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-PD-sex-incidence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Incidence_PD_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_CPT_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'




#--------Prevalence_PD-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-PD-sex-prevalence.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_PD_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 









#--------ASDR_PD-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/Global-PD-sex-ASDR.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_PD_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 









#--------Incidence_SDI-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/incidence-sdi.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Incidence_sdi_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'



#--------Prevalence_sdi-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/prevalence-sdi.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_sdi_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 

#--------ASDR_sdi-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR-SDI.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_sdi_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 








#--------Incidence_region-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/incidence-region.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Incidence_region_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'



#--------Prevalence_region-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/prevalence-region.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_region_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 








#--------ASDR_region-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR-region.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_region_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 






#----------------location数据处理-----------------
#--------Incidence_region-----------
####第二步 调整发病率及可信区间的表示形式
# 读取AAPC（年平均百分比变化）数据


library(readxl)
AAPC <- read.table('/Volumes/NO NAME/Joinpoint/ASDR-location.Export.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower,"-",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_location_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'



#-------APC暂不需要------------------
# 读取APC（年百分比变化）数据
APC <- read.table('E:/05Joinpoint 回归/export.Export.APC.txt',header = T)  # 使用read.table()函数读取APC数据，header = T表示数据中包含列名




# 修改APC数据框中第6到第8列的列名
names(APC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'



# 将APC数据框中的数值列保留两个小数位
APC$val <- sprintf("%0.2f",APC$val)  # 使用sprintf()函数格式化数值，保留两位小数
APC$lower <- sprintf("%0.2f",APC$lower)  # 同上，对lower列进行格式化
APC$upper <- sprintf("%0.2f",APC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列APC，将val、lower和upper拼接成格式化的字符串
APC$APC <- paste0(APC$val," (",APC$lower," - ",APC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间
# 将调整后的APC数据框写入CSV文件
# 将调整后的AAPC数据框写入CSV文件
output_path <- "E:/05Joinpoint 回归/Incidence_APC.csv"
# 写入 Excel 文件
write_csv(APC, output_path) 
# 将格式化后的AAPC数据写入CSV文件，文件名为'Incidence_AAPC.csv'# 将格式化后的APC数据写入CSV文件，文件名为'Incidence_APC.csv'



#--------Prevalence_region-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/prevalence-region.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/Prevalence_region_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 








#--------ASDR_region-----------
AAPC <- read.table('/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR-region.AAPC.txt',header = T)  # 使用read.table()函数读取AAPC数据，header = T表示数据中包含列名




# 修改AAPC数据框中第6到第8列的列名
names(AAPC)[6:8] <- c('val','lower','upper')  # 使用names()函数修改列名，第6到8列分别改为'val'、'lower'、'upper'




# 将AAPC数据框中的数值列保留两个小数位
AAPC$val <- sprintf("%0.2f",AAPC$val)  # 使用sprintf()函数格式化数值，保留两位小数




AAPC$lower <- sprintf("%0.2f",AAPC$lower)  # 同上，对lower列进行格式化



AAPC$upper <- sprintf("%0.2f",AAPC$upper)  # 同上，对upper列进行格式化



# 创建一个新的列AAPC，将val、lower和upper拼接成格式化的字符串
AAPC$AAPC <- paste0(AAPC$val," (",AAPC$lower," to ",AAPC$upper,")")  # 使用paste0()函数将三个数值拼接成字符串，表示可信区间





#  在这行代码中，逗号（,）是 paste0() 函数中的参数分隔符，用来分开多个要拼接的字符串或变量。具体作用如下： paste0() 是一个用于将多个字符串或变量拼接在一起的函数。逗号用于将要拼接的各个部分（即 AAPC$val、" ("、AAPC$lower," - ",AAPC$upper,")"）分隔开，使它们作为独立的参数传入 paste0() 函数中。; 这些独立的部分会依次被拼接成一个完整的字符串。; 在这个例子中，逗号用于拼接的部分表示：; ; AAPC$val: 数值; " (": 左括号，作为字符串; AAPC$lower: 可信区间的下限; " - ": 一个表示区间范围的短划线和空格，作为字符串; AAPC$upper: 可信区间的上限; ")": 右括号，作为字符串; 最终输出的格式类似于 12.34 (10.12 - 14.56)。
# 将调整后的AAPC数据框写入CSV文件
output_path <- "/Users/fqq/documents/GBD口腔老年人/Joinpoint/ASDR_region_AAPC.csv"
# 写入 Excel 文件
write_csv(AAPC, output_path) 