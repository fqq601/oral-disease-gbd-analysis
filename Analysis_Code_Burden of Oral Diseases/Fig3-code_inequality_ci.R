setwd("G:\\oral_disease_70")
library(dplyr)
library(tidyr)
library(ggplot2)
library(splines)

##data preparation
data <- read.csv('Final_combined_data.csv')
ASR <- data |>
  dplyr::filter(age_name=="Age-standardized"|age_name=="All ages")|>
  dplyr::select(
    measure = measure_name,
    location = location_name,
    sex = sex_name,
    age = age_name,
    cause = cause_name,
    metric = metric_name,
    year,
    val,
    upper,
    lower
  )
ASR_DALY <- ASR|>
  dplyr::filter(measure=="DALYs (Disability-Adjusted Life Years)")|>
  dplyr::filter(cause=="Oral disorders"|cause=="Caries of permanent teeth"|
           cause=="Periodontal diseases"|cause=="Edentulism")
ASR_pre <- ASR|>
  dplyr::filter(measure=="Prevalence")|>
  dplyr::filter(cause=="Oral disorders"|cause=="Caries of permanent teeth"|
           cause=="Periodontal diseases"|cause=="Edentulism")
ASR_inc <- ASR|>
  dplyr::filter(measure=="Incidence")|>
  dplyr::filter(cause=="Oral disorders"|cause=="Caries of permanent teeth"|
           cause=="Periodontal diseases"|cause=="Edentulism")

##inequality
#data preparation
country <- read.csv('country.csv', header = F)
filter_data <- function(data) {
  data |>
    dplyr::filter(sex == 'Both') |>
    dplyr::filter(year == 1990 | year == 2021) |>
    dplyr::filter(!location %in% c(
      'Global', 'East Asia', 'Southeast Asia', 'South Asia', 'Oceania',
      'High-income Asia Pacific', 'Australasia', 'Western Europe',
      'Southern Latin America', 'High-income North America',
      'Andean Latin America', 'Central Latin America', 'Tropical Latin America',
      'North Africa and Middle East', 'Central Sub-Saharan Africa',
      'Eastern Sub-Saharan Africa', 'Southern Sub-Saharan Africa',
      'Western Sub-Saharan Africa', 'Caribbean', 'Central Europe',
      'Eastern Europe', 'Central Asia', 'High SDI', 'High-middle SDI',
      'Middle SDI', 'Low-middle SDI', 'Low SDI'
    ))
}

# 应用函数
ASR_DALY_ineq <- filter_data(ASR_DALY)
ASR_inc_ineq <- filter_data(ASR_inc)
ASR_pre_ineq <- filter_data(ASR_pre)

ASR_DALY_ineq_all <- ASR_DALY_ineq |>
  dplyr::filter(cause=='Oral disorders')
ASR_inc_ineq_all <- ASR_inc_ineq |>
  dplyr::filter(cause=='Oral disorders')
ASR_pre_ineq_all <- ASR_pre_ineq |>
  dplyr::filter(cause=='Oral disorders')

#匹配国家
# 定义重命名函数
rename_locations <- function(data) {
  data %>%
    dplyr::mutate(location = case_when(
      location == "Cote d'Ivoire" ~ "Coted'Ivoire",
      location == "Federated States of Micronesia" ~ "Micronesia (Federated States of)",
      location == "The Gambia" ~ "Gambia",
      location == "Macedonia" ~ "North Macedonia",
      location == "Czech Republic" ~ "Czechia",
      location == "Swaziland" ~ "Eswatini",
      location == "South Korea" ~ "Republic of Korea",
      location == "North Korea" ~ "Democratic People's Republic of Korea",
      location == "Virgin Islands, U.S." ~ "United States Virgin Islands",
      location == "United States" ~ "United States of America",
      location == "Venezuela" ~ "Venezuela (Bolivarian Republic of)",
      location == "Bolivia" ~ "Bolivia (Plurinational State of)",
      location == "Syria" ~ "Syrian Arab Republic",
      location == "Vietnam" ~ "Viet Nam",
      location == "Laos" ~ "Lao People's Democratic Republic",
      location == "Brunei" ~ "Brunei Darussalam",
      location == "Cape Verde" ~ "Cabo Verde",
      location == "Moldova" ~ "Republic of Moldova",
      location == "The Bahamas" ~ "Bahamas",
      location == "Iran" ~ "Iran (Islamic Republic of)",
      location == "Tanzania" ~ "United Republic of Tanzania",
      TRUE ~ location
    ))
}

# 应用函数
ASR_DALY_ineq_all <- rename_locations(ASR_DALY_ineq_all)
ASR_inc_ineq_all <- rename_locations(ASR_inc_ineq_all)
ASR_pre_ineq_all <- rename_locations(ASR_pre_ineq_all)


ages <- c('Age-standardized','All ages')

# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)


## ---------------------------------分别计算concentration index------------------------
# DALY
# 匹配 sdi数据, 生成 data
data2 <- dplyr::left_join(ASR_DALY_ineq_all,sdi,by=c("location","year"))
# 获得人口数据
Population <- read.csv('Population.csv')
Population <- Population[,-1]
pop <- Population %>%
  dplyr::filter(age %in% ages) %>%
  dplyr::select("location","sex","year","val") %>%
  dplyr::group_by(location,sex,year) %>%
  dplyr::mutate(val=sum(val)) %>%
  dplyr::rename(pop=val) %>% unique()
# 合并人口数据, 生成 mydata
mydata <- dplyr::left_join(data2,pop,
                           by=c("location","sex","year"))
mydata <- mydata %>%
  dplyr::filter(metric=="Rate") %>% 
  dplyr::mutate(number=as.numeric(pop)*as.numeric(val))

# 第一步：整理数据 ----------------------------------------------------------------
# 这一步主要是提取204个国家水平的all ages rate或者age-standardized rate数据，并与人口和sdi值进行匹配
# 计算累积人口
a <- mydata %>%
  dplyr::filter(metric=="Rate") %>% dplyr::mutate(pop=as.numeric(pop)) %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::summarise(pop_global=sum(pop))  %>% unique()

mydata2 <- dplyr::left_join(mydata,a,
                            by=c("measure","sex","age","cause","metric","year"))

# 计算累积daly值（或者其他measure指标）
a <- mydata %>%
  dplyr::filter(metric=="Rate") %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::summarise(total_daly=sum(number))

mydata2 <- mydata2  %>% dplyr::filter(metric=="Rate") %>%
  dplyr::left_join(a, by=c("measure","sex","age","cause","metric","year"))

# 计算
rank <- mydata2 %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::arrange(sdi) %>%
  dplyr::mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  dplyr::mutate(half=pop/2) %>% # 计算该国家人口的一半
  dplyr::mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  dplyr::mutate(weighted_order=midpoint/pop_global) %>% # 人口中点与总人口相比即为改国的相对位置
  dplyr::mutate(cummu_daly=cumsum(number)) %>% # 计算累积 daly
  dplyr::mutate(frac_daly=cummu_daly/total_daly) %>% # 计算累积 daly 所占总体的比例
  dplyr::mutate(frac_population=cummu/pop_global) # 计算累积人口所占总体人口的比例

rank_DALY <-rank %>% dplyr::arrange(measure,sex,age,cause,metric,year,sdi) %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::mutate(rank = row_number(sdi))


write.csv(rank_DALY,'rank_DALY.csv')

# incidence
# 匹配 sdi数据, 生成 data
data2 <- dplyr::left_join(ASR_inc_ineq_all,sdi,by=c("location","year"))
# 获得人口数据
Population <- read.csv('Population.csv')
Population <- Population[,-1]
pop <- Population %>%
  dplyr::filter(age %in% ages) %>%
  dplyr::select("location","sex","year","val") %>%
  dplyr::group_by(location,sex,year) %>%
  dplyr::mutate(val=sum(val)) %>%
  dplyr::rename(pop=val) %>% unique()
# 合并人口数据, 生成 mydata
mydata <- dplyr::left_join(data2,pop,
                           by=c("location","sex","year"))
mydata <- mydata %>%
  dplyr::filter(metric=="Rate") %>% 
  dplyr::mutate(number=as.numeric(pop)*as.numeric(val))

# 第一步：整理数据 ----------------------------------------------------------------
# 这一步主要是提取204个国家水平的all ages rate或者age-standardized rate数据，并与人口和sdi值进行匹配
# 计算累积人口
a <- mydata %>%
  dplyr::filter(metric=="Rate") %>% dplyr::mutate(pop=as.numeric(pop)) %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::summarise(pop_global=sum(pop))  %>% unique()

mydata2 <- dplyr::left_join(mydata,a,
                            by=c("measure","sex","age","cause","metric","year"))

# 计算累积incidence值（或者其他measure指标）
a <- mydata %>%
  dplyr::filter(metric=="Rate") %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::summarise(total_daly=sum(number))

mydata2 <- mydata2  %>% dplyr::filter(metric=="Rate") %>%
  dplyr::left_join(a, by=c("measure","sex","age","cause","metric","year"))

# 计算
rank <- mydata2 %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::arrange(sdi) %>%
  dplyr::mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  dplyr::mutate(half=pop/2) %>% # 计算该国家人口的一半
  dplyr::mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  dplyr::mutate(weighted_order=midpoint/pop_global) %>% # 人口中点与总人口相比即为改国的相对位置
  dplyr::mutate(cummu_daly=cumsum(number)) %>% # 计算累积 daly
  dplyr::mutate(frac_daly=cummu_daly/total_daly) %>% # 计算累积 daly 所占总体的比例
  dplyr::mutate(frac_population=cummu/pop_global) # 计算累积人口所占总体人口的比例

rank_inc <-rank %>% dplyr::arrange(measure,sex,age,cause,metric,year,sdi) %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::mutate(rank = row_number(sdi))


write.csv(rank_inc,'rank_inc.csv')

# prevalence
# 匹配 sdi数据, 生成 data
data2 <- dplyr::left_join(ASR_pre_ineq_all,sdi,by=c("location","year"))
# 获得人口数据
Population <- read.csv('Population.csv')
Population <- Population[,-1]
pop <- Population %>%
  dplyr::filter(age %in% ages) %>%
  dplyr::select("location","sex","year","val") %>%
  dplyr::group_by(location,sex,year) %>%
  dplyr::mutate(val=sum(val)) %>%
  dplyr::rename(pop=val) %>% unique()
# 合并人口数据, 生成 mydata
mydata <- dplyr::left_join(data2,pop,
                           by=c("location","sex","year"))
mydata <- mydata %>%
  dplyr::filter(metric=="Rate") %>% 
  dplyr::mutate(number=as.numeric(pop)*as.numeric(val))

# 第一步：整理数据 ----------------------------------------------------------------
# 这一步主要是提取204个国家水平的all ages rate或者age-standardized rate数据，并与人口和sdi值进行匹配
# 计算累积人口
a <- mydata %>%
  dplyr::filter(metric=="Rate") %>% dplyr::mutate(pop=as.numeric(pop)) %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::summarise(pop_global=sum(pop))  %>% unique()

mydata2 <- dplyr::left_join(mydata,a,
                            by=c("measure","sex","age","cause","metric","year"))

# 计算累积incidence值（或者其他measure指标）
a <- mydata %>%
  dplyr::filter(metric=="Rate") %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::summarise(total_daly=sum(number))

mydata2 <- mydata2  %>% dplyr::filter(metric=="Rate") %>%
  dplyr::left_join(a, by=c("measure","sex","age","cause","metric","year"))

# 计算
rank <- mydata2 %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::arrange(sdi) %>%
  dplyr::mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  dplyr::mutate(half=pop/2) %>% # 计算该国家人口的一半
  dplyr::mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  dplyr::mutate(weighted_order=midpoint/pop_global) %>% # 人口中点与总人口相比即为改国的相对位置
  dplyr::mutate(cummu_daly=cumsum(number)) %>% # 计算累积 daly
  dplyr::mutate(frac_daly=cummu_daly/total_daly) %>% # 计算累积 daly 所占总体的比例
  dplyr::mutate(frac_population=cummu/pop_global) # 计算累积人口所占总体人口的比例

rank_pre <-rank %>% dplyr::arrange(measure,sex,age,cause,metric,year,sdi) %>%
  dplyr::group_by(measure,sex,age,cause,metric,year) %>%
  dplyr::mutate(rank = row_number(sdi))

write.csv(rank_pre,'rank_pre.csv')

##打开stata
#stata代码：conindex val [pweight=pop], rankvar(rank) truezero
#筛选年份：keep if year==1990  /    keep if year==2021

#------------concentration index绘图--------------------

color <- c("#6699FF","#990000")
rank_DALY$year <- as.factor(rank_DALY$year)
p_ci_DALY <- rank_DALY %>%
  ggplot(aes(x=frac_population,y=frac_daly,fill=year,color=year,group=year))+
  # 增加 X=0,y=0 两条线段
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype=1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype=1,size=1,color="gray")+
  # 对角线
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               color="#CD853F",linetype=1,size=0.7,alpha=1)+
  # 散点
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,shape=21)+
  scale_fill_manual(values = color)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  # 立方样条函数拟合洛伦兹曲线 (设置节点，边界条件)
  geom_smooth(method = "gam", # 这里也可以直接用 geom_line 把点连起来
              formula = y ~ ns(x,
                               knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),# 设置节点为
                               # df = 3, 上面设置了这里就不需要设置
                               Boundary.knots = c(0,1)),
              linetype=1,size=0.1,alpha=0.6,se=T)+
  scale_color_manual(values = color)+
  # 增加两个年份的集中指数
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label="1990: 0.07",x=0.75,y=0.3,size=4,color="#6699FF")+
  annotate("text",label="2021: 0.08",x=0.75,y=0.25,size=4,color="#990000")+
  # 增加某些国家的标签，1990 年
  geom_text(aes(label=ifelse(location=="China"&year=="1990"|location=="India"&year=="1990",
                             as.character(location),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  # 增加某些国家的标签，2019 年
  geom_text(aes(label=ifelse(location=="China"&year=="2021"|location=="India"&year=="2021",
                             as.character(location),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # 增加某些国家标签，人口大国
  geom_text(aes(label=ifelse(location%in%a&year=="1990",
                             as.character(location),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  geom_text(aes(label=ifelse(location%in%a&year=="2021",
                             as.character(location),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # xy 标签
  xlab("Cumulative fraction of population ranked by SDI")+
  ylab("Cumulative fraction of ASDR")+
  theme_bw()
p_ci_DALY



color <- c("#6699FF","#990000")
rank_inc$year <- as.factor(rank_inc$year)
p_ci_inc <- rank_inc %>%
  ggplot(aes(x=frac_population,y=frac_daly,fill=year,color=year,group=year))+
  # 增加 X=0,y=0 两条线段
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype=1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype=1,size=1,color="gray")+
  # 对角线
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               color="#CD853F",linetype=1,size=0.7,alpha=1)+
  # 散点
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,shape=21)+
  scale_fill_manual(values = color)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  # 立方样条函数拟合洛伦兹曲线 (设置节点，边界条件)
  geom_smooth(method = "gam", # 这里也可以直接用 geom_line 把点连起来
              formula = y ~ ns(x,
                               knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),# 设置节点为
                               # df = 3, 上面设置了这里就不需要设置
                               Boundary.knots = c(0,1)),
              linetype=1,size=0.1,alpha=0.6,se=T)+
  scale_color_manual(values = color)+
  # 增加两个年份的集中指数
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label="1990: 0.01",x=0.75,y=0.3,size=4,color="#6699FF")+
  annotate("text",label="2021: -0.01",x=0.75,y=0.25,size=4,color="#990000")+
  # 增加某些国家的标签，1990 年
  geom_text(aes(label=ifelse(location=="China"&year=="1990"|location=="India"&year=="1990",
                             as.character(location),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  # 增加某些国家的标签，2019 年
  geom_text(aes(label=ifelse(location=="China"&year=="2021"|location=="India"&year=="2021",
                             as.character(location),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # 增加某些国家标签，人口大国
  geom_text(aes(label=ifelse(location%in%a&year=="1990",
                             as.character(location),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  geom_text(aes(label=ifelse(location%in%a&year=="2021",
                             as.character(location),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # xy 标签
  xlab("Cumulative fraction of population ranked by SDI")+
  ylab("Cumulative fraction of ASIR")+
  theme_bw()
p_ci_inc



color <- c("#6699FF","#990000")
rank_pre$year <- as.factor(rank_pre$year)
p_ci_pre <- rank_pre %>%
  ggplot(aes(x=frac_population,y=frac_daly,fill=year,color=year,group=year))+
  # 增加 X=0,y=0 两条线段
  geom_segment(x=0,xend=1,
               y=0,yend=0,
               linetype=1,size=1,color="gray")+
  geom_segment(x=1,xend=1,
               y=0,yend=1,
               linetype=1,size=1,color="gray")+
  # 对角线
  geom_segment(x=0,xend=1,
               y=0,yend=1,
               color="#CD853F",linetype=1,size=0.7,alpha=1)+
  # 散点
  geom_point(aes(fill=year,size=pop/1e6),alpha=0.75,shape=21)+
  scale_fill_manual(values = color)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  # 立方样条函数拟合洛伦兹曲线 (设置节点，边界条件)
  geom_smooth(method = "gam", # 这里也可以直接用 geom_line 把点连起来
              formula = y ~ ns(x,
                               knots = c(0.0000000001,0.25,0.5,0.75,0.9999999),# 设置节点为
                               # df = 3, 上面设置了这里就不需要设置
                               Boundary.knots = c(0,1)),
              linetype=1,size=0.1,alpha=0.6,se=T)+
  scale_color_manual(values = color)+
  # 增加两个年份的集中指数
  annotate("text",label="Concentration Index",x=0.75,y=0.35,size=5)+
  annotate("text",label="1990: 0.00",x=0.75,y=0.3,size=4,color="#6699FF")+
  annotate("text",label="2021: 0.00",x=0.75,y=0.25,size=4,color="#990000")+
  # 增加某些国家的标签，1990 年
  geom_text(aes(label=ifelse(location=="China"&year=="1990"|location=="India"&year=="1990",
                             as.character(location),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  # 增加某些国家的标签，2019 年
  geom_text(aes(label=ifelse(location=="China"&year=="2021"|location=="India"&year=="2021",
                             as.character(location),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # 增加某些国家标签，人口大国
  geom_text(aes(label=ifelse(location%in%a&year=="1990",
                             as.character(location),"")),
            hjust=-0.6,vjust=0.8,
            size=3)+
  geom_text(aes(label=ifelse(location%in%a&year=="2021",
                             as.character(location),"")),
            hjust=1.8,vjust=-0.0,
            size=3)+
  # xy 标签
  xlab("Cumulative fraction of population ranked by SDI")+
  ylab("Cumulative fraction of ASPR")+
  theme_bw()
p_ci_pre

library(patchwork)

p_ci_inc1 <- p_ci_inc + labs(tag = 'd')
p_ci_pre1 <- p_ci_pre + labs(tag = 'e')
p_ci_DALY1 <- p_ci_DALY + labs(tag = 'f')

p_ci <- p_ci_inc1+p_ci_pre1+p_ci_DALY1

p_inequality <- p_sii/p_ci

ggsave("p_inequality.png",width = 16,height = 9,dpi = 450)
ggsave("p_inequality.pdf",width = 16,height = 9,dpi = 900)
