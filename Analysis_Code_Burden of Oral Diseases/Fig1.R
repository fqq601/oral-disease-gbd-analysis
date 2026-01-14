# 设置工作目录（替换为你的实际路径）
setwd("/Users/fqq/Documents/小论文/肝胆外/定观老师/Figure1")


#1.下载并加载R包
library(ggplot2)
library(ggmap)
library(terra)
library(maps)
library(dplyr)
library(cowplot)
library(patchwork)
library(purrr)
library(stringr)
library(sf)
library(tmaptools)
library(data.table)



#2.导入数据(2大数据：GBD数据库 & 世界地图数据)

Countries_1990to2021 <- fread('预备地图.csv', header=T) 
load('/Users/fqq/Documents/小论文/肝胆外/定观老师/Figure1/GBD_maps.RData')


#3.大地图数据清洗_Incidence_ASR_2019

##3.1 数据筛选（目的：最终选出153个location的val)####
Incidence2021_ASR  <- Countries_1990to2021 %>%
  filter(year == "2021",
         age_name == "AAPC",
         measure_name == "DALYs (Disability-Adjusted Life Years)") %>%
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
    colours = pal_nodes,
    values  = scales::rescale(0:5),                # 5 节点
    limits  = range(Incidence2021_ASR_map$val, na.rm = TRUE),
    breaks  = c(-2, -1, 0, 1, 2),                # ← 色阶刻度数值
    labels  = c("-2", "-1", "0", "1", "2"),      # ← 对应文字
    name    = "ASIR in 2021\n(per 100 000)"
  )+
    guide   = guide_colourbar(
      frame.colour = "black",
      ticks.colour = "black",
      nbin         = 100,
      barwidth     = unit(0.25, "cm"),
      barheight    = unit(2, "cm")
    )
  ) +
  theme_void() +
  theme(
    legend.position   = c(0.06, 0.1),
    legend.key.height = unit(2, "cm"),
    legend.key.width  = unit(0.25, "cm"),
    # 字体放大
    legend.title      = element_text(size = 9, face = "bold"),
    legend.text       = element_text(size = 8),
    text              = element_text(size = 6)  # 其他文字保持原大小
  ) +
  coord_sf(xlim = c(-180, 208), expand = FALSE)



Incidence2021_ASR_map_plot





# 修正后的代码ASDR-AAPC-R-9.21
Incidence2021_ASR_map_plot <- ggplot(Incidence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes,
    values = scales::rescale(c(-1.2, -0.8, -0.4,0, 0.4, 0.8,1.2)), # 与pal_nodes颜色节点对应
    limits = c(-1.2,0.8), # 设置颜色映射的范围
    breaks = c(-1.0,  -0.5, 0, 0.5, 1.0), # 色阶上显示的刻度数值
    labels = c("-1.0", "-0.5", "0.0", "0.5", "1.0"), # 色阶上显示的标签文字
    name = "AAPC of ASDR",
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








# 修正后的代码ASIR-AAPC-R-9.21
Incidence2021_ASR_map_plot <- ggplot(Incidence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(-2.2, -1.2, 0, 0.2, 0.4)), # 调整颜色节点位置
    limits = c(-2.2, 0.4), # 扩展一点范围以包含所有数据
    breaks = c(-2.0, -1.0, 0, 0.2, 0.4), # 色阶上显示的刻度数值
    labels = c("-2.0", "-1.0", "0.0", "0.2", "0.4"), # 色阶上显示的标签文字
    name = "ASIR of AAPC",
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





# 修正后的代码ASPR-AAPC-R-9.21
Incidence2021_ASR_map_plot <- ggplot(Incidence2021_ASR_map, aes(fill = val)) +
  geom_sf(aes(geometry = geometry)) +
  scale_fill_gradientn(
    colours = pal_nodes, # 使用您原有的5色配色
    values = scales::rescale(c(-1.1, -0.8, -0.05, 0.05, 0.3)), # 调整颜色节点位置
    limits = c(-1.1, 0.3), # 根据数据范围调整
    breaks = c(-1.0, -0.5, 0, 0.2), # 简化的刻度
    labels = c("-1.0", "-0.5", "0.0", "0.2"), # 简化的标签
    name = "ASPR of AAPC",
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
