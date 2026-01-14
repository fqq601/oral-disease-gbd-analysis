# 分疾病类型的地图----------------------------------------------------------------------------
# 设置工作目录（替换为你的实际路径）
setwd("/Users/fqq/Documents/GBD口腔老年人/全球地区国家三指标数据")

#预备地图csv获取
final_data<-read.csv("Final_combined_data.csv")



# Caries of permanent teeth-----------------
#Incidence
new_data <- final_data %>%
  filter(year %in% c(2021) & sex_name == "Both"& cause_name == "Caries of permanent teeth"&age_name == "Age-standardized")

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






load('/Users/fqq/Documents/小论文/肝胆外/定观老师/Figure1/GBD_maps.RData')


#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Incidence2021_ASR  <- new_data %>%
  filter(
         measure_name == "Incidence") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(Incidence2021_ASR$val, na.rm = TRUE)

# 最小值
min(Incidence2021_ASR$val, na.rm = TRUE)


breaks <- c(-2,-1,0,1,2)

###3.2.2 根据节点形成区间break_labels####
breaks_labels <- imap_chr(breaks, function(., idx){
  return(paste0(breaks[idx], " to ", breaks[idx+1]))
})
breaks_labels <- breaks_labels[1:length(breaks)-1]  
breaks_labels[length(breaks_labels)] <- paste0('>=', str_split(breaks_labels[length(breaks_labels)],' ',simplify = T)[1])
breaks_labels


### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
Incidence2021_ASR_map <- left_join(Incidence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




Incidence2021_ASR_map_plot <- ggplot(Incidence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(5000,10000,15000,20000,25000,30000)), # 调整颜色节点位置
    limits = c(5000, 30000), # 根据数据范围调整
    breaks = c(10000, 20000, 30000), # 简化的刻度
    labels = c("10000", "20000", "30000"), # 简化的标签
    name = "ASIR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
Incidence2021_ASR_map_plot






#Prevalence

#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Prevalence2021_ASR  <- new_data %>%
  filter(
    measure_name == "Prevalence") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(Prevalence2021_ASR$val, na.rm = TRUE)

# 最小值
min(Prevalence2021_ASR$val, na.rm = TRUE)



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
Prevalence2021_ASR_map <- left_join(Prevalence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




Prevalence2021_ASR_map_plot <- ggplot(Prevalence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(5000,25000,45000,65000)), # 调整颜色节点位置
    limits = c(5000, 65000), # 根据数据范围调整
    breaks = c(5000,25000,45000,65000), # 简化的刻度
    labels = c("5000", "25000", "45000","65000"), # 简化的标签
    name = "ASPR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
Prevalence2021_ASR_map_plot






#DALYs

#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
DALYs2021_ASR  <- new_data %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(DALYs2021_ASR$val, na.rm = TRUE)

# 最小值
min(DALYs2021_ASR$val, na.rm = TRUE)



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
DALYs2021_ASR_map <- left_join(DALYs2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




DALYs2021_ASR_map_plot <- ggplot(DALYs2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(0,20,40,60)), # 调整颜色节点位置
    limits = c(0, 60), # 根据数据范围调整
    breaks = c(0,20,40,60), # 简化的刻度
    labels = c("0", "20", "40","60"), # 简化的标签
    name = "ASDR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
DALYs2021_ASR_map_plot







# 安装并加载patchwork包
# install.packages("patchwork")
library(patchwork)


# 使用较小的边距
combined_plot <- 
  (Incidence2021_ASR_map_plot + theme(plot.margin = margin(0, 0, 8, 0))) / 
  (Prevalence2021_ASR_map_plot + theme(plot.margin = margin(0, 0, 8, 0))) / 
  DALYs2021_ASR_map_plot +
  plot_layout(ncol = 1)

combined_plot

# 保存为适当尺寸
ggsave("combined_plots.pdf", combined_plot, width = 10, height = 12, dpi = 300)









# Edentulism-----------------
#Incidence
new_data <- final_data %>%
  filter(year %in% c(2021) & sex_name == "Both"& cause_name == "Edentulism"&age_name == "Age-standardized")

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






load('/Users/fqq/Documents/小论文/肝胆外/定观老师/Figure1/GBD_maps.RData')


#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Incidence2021_ASR  <- new_data %>%
  filter(
    measure_name == "Incidence") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(Incidence2021_ASR$val, na.rm = TRUE)

# 最小值
min(Incidence2021_ASR$val, na.rm = TRUE)


breaks <- c(0,500,1000,1500,2000,2500)

###3.2.2 根据节点形成区间break_labels####
breaks_labels <- imap_chr(breaks, function(., idx){
  return(paste0(breaks[idx], " to ", breaks[idx+1]))
})
breaks_labels <- breaks_labels[1:length(breaks)-1]  
breaks_labels[length(breaks_labels)] <- paste0('>=', str_split(breaks_labels[length(breaks_labels)],' ',simplify = T)[1])
breaks_labels



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
Incidence2021_ASR_map <- left_join(Incidence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




Incidence2021_ASR_map_plot <- ggplot(Incidence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(0,500,1000,1500,2000,2500)), # 调整颜色节点位置
    limits = c(0, 2500), # 根据数据范围调整
    breaks = c(0,500,1500,2500), # 简化的刻度
    labels = c("0","500", "1500", "2500"), # 简化的标签
    name = "ASIR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
Incidence2021_ASR_map_plot






#Prevalence

#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Prevalence2021_ASR  <- new_data %>%
  filter(
    measure_name == "Prevalence") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(Prevalence2021_ASR$val, na.rm = TRUE)

# 最小值
min(Prevalence2021_ASR$val, na.rm = TRUE)



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
Prevalence2021_ASR_map <- left_join(Prevalence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




Prevalence2021_ASR_map_plot <- ggplot(Prevalence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(10000,30000,50000,70000)), # 调整颜色节点位置
    limits = c(10000, 70000), # 根据数据范围调整
    breaks = c(10000,30000,50000,70000), # 简化的刻度
    labels = c("10000", "30000", "50000","70000"), # 简化的标签
    name = "ASPR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
Prevalence2021_ASR_map_plot






#DALYs

#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
DALYs2021_ASR  <- new_data %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(DALYs2021_ASR$val, na.rm = TRUE)

# 最小值
min(DALYs2021_ASR$val, na.rm = TRUE)



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
DALYs2021_ASR_map <- left_join(DALYs2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




DALYs2021_ASR_map_plot <- ggplot(DALYs2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(200,800,1400,2000)), # 调整颜色节点位置
    limits = c(200, 2000), # 根据数据范围调整
    breaks = c(200,800,1400,2000), # 简化的刻度
    labels = c("200", "800", "1400","2000"), # 简化的标签
    name = "ASDR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
DALYs2021_ASR_map_plot







# 安装并加载patchwork包
# install.packages("patchwork")
library(patchwork)


# 使用较小的边距
combined_plot <- 
  (Incidence2021_ASR_map_plot + theme(plot.margin = margin(0, 0, 20, 0))) / 
  (Prevalence2021_ASR_map_plot + theme(plot.margin = margin(0, 0, 20, 0))) / 
  DALYs2021_ASR_map_plot +
  plot_layout(ncol = 1)

combined_plot

# 保存为适当尺寸
ggsave("combined_plots.pdf", combined_plot, width = 10, height = 12, dpi = 300)








# Periodontal diseases-----------------
#Incidence
new_data <- final_data %>%
  filter(year %in% c(2021) & sex_name == "Both"& cause_name == "Periodontal diseases"&age_name == "Age-standardized")

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






load('/Users/fqq/Documents/小论文/肝胆外/定观老师/Figure1/GBD_maps.RData')


#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Incidence2021_ASR  <- new_data %>%
  filter(
    measure_name == "Incidence") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(Incidence2021_ASR$val, na.rm = TRUE)

# 最小值
min(Incidence2021_ASR$val, na.rm = TRUE)


breaks <- c(0,500,1000,1500,2000,2500)

###3.2.2 根据节点形成区间break_labels####
breaks_labels <- imap_chr(breaks, function(., idx){
  return(paste0(breaks[idx], " to ", breaks[idx+1]))
})
breaks_labels <- breaks_labels[1:length(breaks)-1]  
breaks_labels[length(breaks_labels)] <- paste0('>=', str_split(breaks_labels[length(breaks_labels)],' ',simplify = T)[1])
breaks_labels



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
Incidence2021_ASR_map <- left_join(Incidence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




Incidence2021_ASR_map_plot <- ggplot(Incidence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(600,1200,1800,2400,3000)), # 调整颜色节点位置
    limits = c(600, 3000), # 根据数据范围调整
    breaks = c(600,1400,2200,3000), # 简化的刻度
    labels = c("600","1400","2200" ,"3000"), # 简化的标签
    name = "ASIR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
Incidence2021_ASR_map_plot







#Prevalence

#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Prevalence2021_ASR  <- new_data %>%
  filter(
    measure_name == "Prevalence") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(Prevalence2021_ASR$val, na.rm = TRUE)

# 最小值
min(Prevalence2021_ASR$val, na.rm = TRUE)



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
Prevalence2021_ASR_map <- left_join(Prevalence2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




Prevalence2021_ASR_map_plot <- ggplot(Prevalence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(4000,14000,24000,34000,44000)), # 调整颜色节点位置
    limits = c(4000, 44000), # 根据数据范围调整
    breaks = c(4000,24000,44000), # 简化的刻度
    labels = c("4000", "24000", "44000"), # 简化的标签
    name = "ASPR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
Prevalence2021_ASR_map_plot






#DALYs

#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
DALYs2021_ASR  <- new_data %>%
  filter(
    measure_name == "DALYs (Disability-Adjusted Life Years)") %>%
  select("location_id", "location_name","val") #注意：location选id和name，id用于大地图，name用于小地图


##3.2 确定色阶 (A.确定节点breaks；B.根据节点形成区间break_labels; C.给不同的区间加上不同的颜色 pal)####
###3.2.1 确定节点breaks####
# 最大值
max(DALYs2021_ASR$val, na.rm = TRUE)

# 最小值
min(DALYs2021_ASR$val, na.rm = TRUE)



### 3.2.3 给不同的区间加上自定义颜色 pal（方法1：手动色板）####
# 自定义 7 段颜色（低→高，色盲友好）

# 7 个颜色节点：深蓝 → 白 → 深红
pal_nodes <- c("#053061", "#2166ac",  # 深蓝系
               "#f7f7f7",                         # 浅色（白）
               "#d6604d", "#b2182b")   # 深红系



##3.3 整理画图数据 (该数据是用于可视化的基础，在本情景下，主要是生成两列变量：1.每个国家的地理边界经纬度信息；2.每个国家的色阶labels)####
DALYs2021_ASR_map <- left_join(DALYs2021_ASR, world_GBD, by = c('location_id' = 'Location.ID')) %>%
  mutate(val2 = cut(val, 
                    breaks = breaks,
                    labels = breaks_labels, 
                    include.lowest = T,right = F)) #cut函数：目的将连续型变量转化为分类型数据；使用时包含的数据有：变量、breaks、labels、逻辑变量（是否包含左右端点）




DALYs2021_ASR_map_plot <- ggplot(DALYs2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(20,120,240)), # 调整颜色节点位置
    limits = c(20, 240), # 根据数据范围调整
    breaks = c(20,120,240), # 简化的刻度
    labels = c("20", "120", "240"), # 简化的标签
    name = "ASDR in 2021\n(per 100 000)",
    guide = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin = 100,
      barwidth = unit(0.25, "cm"),
      barheight = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width = unit(0.25, "cm"),
    legend.title = element_text(size = 9, face = "bold"),
    legend.text = element_text(size = 8),
    text = element_text(size = 6)
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)

# 显示图形
DALYs2021_ASR_map_plot







# 安装并加载patchwork包
# install.packages("patchwork")
library(patchwork)


# 使用较小的边距
combined_plot <- 
  (Incidence2021_ASR_map_plot + theme(plot.margin = margin(0, 0, 20, 0))) / 
  (Prevalence2021_ASR_map_plot + theme(plot.margin = margin(0, 0, 20, 0))) / 
  DALYs2021_ASR_map_plot +
  plot_layout(ncol = 1)

combined_plot

# 保存为适当尺寸
ggsave("combined_plots.pdf", combined_plot, width = 10, height = 12, dpi = 300)
