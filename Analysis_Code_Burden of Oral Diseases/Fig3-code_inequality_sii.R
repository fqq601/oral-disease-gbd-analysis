setwd("G:\\oral_disease_70")
library(dplyr)
library(tidyr)
library(data.table)
library(car)# 异方差诊断
library(MASS)# 稳健回归
library(ggplot2)
library(ggbrace)# 画括号
library(mgcv)# 提供洛伦兹曲线拟合 (样条函数等)
library(splines)# 拟合样条函数
library(broom)


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
ASR_DALY_ineq <- ASR_DALY|>
  dplyr::filter(sex=='Both')|>
  dplyr::filter(year==1990 | year==2021)|>
  dplyr::filter(location != 'Global'&
           location != 'East Asia'&
           location != 'Southeast Asia'&
           location != 'South Asia'&
           location != 'Oceania'&
           location != 'High-income Asia Pacific'&
           location != 'Australasia'&
           location != 'Western Europe'&
           location != 'Southern Latin America'&
           location != 'High-income North America'&
           location != 'Andean Latin America'&
           location != 'Central Latin America'&
           location != 'Tropical Latin America'&
           location != 'North Africa and Middle East'&
           location != 'Central Sub-Saharan Africa'&
           location != 'Eastern Sub-Saharan Africa'&
           location != 'Southern Sub-Saharan Africa'&
           location != 'Western Sub-Saharan Africa'&
           location != 'Caribbean'&
           location != 'Central Europe'&
           location != 'Eastern Europe'&
           location != 'Central Asia'&
           location != 'High SDI'&
           location != 'High-middle SDI'&
           location != 'Middle SDI'&
           location != 'Low-middle SDI'&
           location != 'Low SDI')
ASR_inc_ineq <- ASR_inc|>
  dplyr::filter(sex=='Both')|>
  dplyr::filter(year==1990 | year==2021)|>
  dplyr::filter(location != 'Global'&
           location != 'East Asia'&
           location != 'Southeast Asia'&
           location != 'South Asia'&
           location != 'Oceania'&
           location != 'High-income Asia Pacific'&
           location != 'Australasia'&
           location != 'Western Europe'&
           location != 'Southern Latin America'&
           location != 'High-income North America'&
           location != 'Andean Latin America'&
           location != 'Central Latin America'&
           location != 'Tropical Latin America'&
           location != 'North Africa and Middle East'&
           location != 'Central Sub-Saharan Africa'&
           location != 'Eastern Sub-Saharan Africa'&
           location != 'Southern Sub-Saharan Africa'&
           location != 'Western Sub-Saharan Africa'&
           location != 'Caribbean'&
           location != 'Central Europe'&
           location != 'Eastern Europe'&
           location != 'Central Asia'&
           location != 'High SDI'&
           location != 'High-middle SDI'&
           location != 'Middle SDI'&
           location != 'Low-middle SDI'&
           location != 'Low SDI')
ASR_pre_ineq <- ASR_pre|>
  dplyr::filter(sex=='Both')|>
  dplyr::filter(year==1990 | year==2021)|>
  dplyr::filter(location != 'Global'&
           location != 'East Asia'&
           location != 'Southeast Asia'&
           location != 'South Asia'&
           location != 'Oceania'&
           location != 'High-income Asia Pacific'&
           location != 'Australasia'&
           location != 'Western Europe'&
           location != 'Southern Latin America'&
           location != 'High-income North America'&
           location != 'Andean Latin America'&
           location != 'Central Latin America'&
           location != 'Tropical Latin America'&
           location != 'North Africa and Middle East'&
           location != 'Central Sub-Saharan Africa'&
           location != 'Eastern Sub-Saharan Africa'&
           location != 'Southern Sub-Saharan Africa'&
           location != 'Western Sub-Saharan Africa'&
           location != 'Caribbean'&
           location != 'Central Europe'&
           location != 'Eastern Europe'&
           location != 'Central Asia'&
           location != 'High SDI'&
           location != 'High-middle SDI'&
           location != 'Middle SDI'&
           location != 'Low-middle SDI'&
           location != 'Low SDI')

ASR_DALY_ineq_all <- ASR_DALY_ineq |>
  dplyr::filter(cause=='Oral disorders')
ASR_inc_ineq_all <- ASR_inc_ineq |>
  dplyr::filter(cause=='Oral disorders')
ASR_pre_ineq_all <- ASR_pre_ineq |>
  dplyr::filter(cause=='Oral disorders')

#匹配国家
ASR_DALY_ineq_all <- ASR_DALY_ineq_all %>%
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
    TRUE ~ location  # 其他保持不变
  ))

ASR_inc_ineq_all <- ASR_inc_ineq_all %>%
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
    TRUE ~ location  # 其他保持不变
  ))

ASR_pre_ineq_all <- ASR_pre_ineq_all %>%
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
    TRUE ~ location  # 其他保持不变
  ))


ages <- c('Age-standardized','All ages')

# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)
# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)
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

#计算总人口
a <- mydata %>%
  filter(metric=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]
# 计算加权次序
rank <- mydata %>%
  mutate(pop_global=ifelse(year==1990,pop1990,pop2021)) %>%
  group_by(year,metric) %>%
  arrange(sdi) %>%
  mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  mutate(half=pop/2) %>% # 计算该国家人口的一半
  mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  mutate(weighted_order=midpoint/pop_global) # 人口中点与总人口相比即为改国的相对位置
# 把年份设置为 factor
rank$year <- factor(rank$year)


temp1 <- rank %>%
  filter(metric=="Rate") %>%
  filter(year==1990)
temp2 <- rank %>%
  filter(metric=="Rate") %>%
  filter(year==2021)
# 建模计算斜度指数
fit1 <- lm(data = temp1,val~weighted_order)
fit2 <- lm(data = temp2,val~weighted_order)
# 查看是否存在异方差（存在异方差）
ncvTest(fit1)
ncvTest(fit2)
# 使用稳健（robust）回归：重复迭代加权
r.huber1 <- rlm(data = temp1,val~weighted_order)
r.huber2 <- rlm(data = temp2,val~weighted_order)
# 获得系数与截距
coef(r.huber1)
coef(r.huber2)
# 计算稳健回归的 95% 可信区间
confint.default(r.huber1)
confint.default(r.huber2)


color <- c("#6699FF","#990000")
colnames(rank)
p_sii_DALY <- rank %>%
  filter(metric=="Rate") %>%
  ggplot(aes(x=weighted_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_smooth(method = "rlm",size=0.6,alpha=0.1)+
  scale_fill_manual(values = color)+
  scale_color_manual(values = color)+
  # 增加水平虚线
  geom_segment(x=0.02,xend=0.99,
               y=795.1072,yend=795.1072, # 截距的位置
               color="#6699FF",linetype=2,size=0.4,alpha=0.4)+
  geom_segment(x=0.02,xend=0.99,
               y=778.6195,yend=778.6195, # 截距的位置
               color="#990000",linetype=2,size=0.4,alpha=0.4)+
  # 增加某些国家的标签: 比如中国与印度
  geom_text(aes(label=ifelse(location=="China"|location=="India",as.character(location),""),
                color=year),
            hjust=0,vjust=1.7,# 避免点和文字重合
            size=3)+
  # 增加斜度指数标签
  annotate("text",label="535.10",x=1.15,y=795.1072+535.0966/2,size=3.5)+ # 位置
  annotate("text",label="419.92",x=1.15,y=778.6195+419.9207/2,size=3.5)+
  scale_x_continuous(limits = c(0,1.22),labels = c("0","0.25","0.50","0.75","1.00",""))+
  xlab("Relative rank by SDI")+
  ylab("ASDR (per 100,000)")+
  theme_bw()
p_sii_DALY


#-------------------incidence-----------------------
# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)
# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)
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

#计算总人口
a <- mydata %>%
  filter(metric=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]
# 计算加权次序
rank <- mydata %>%
  mutate(pop_global=ifelse(year==1990,pop1990,pop2021)) %>%
  group_by(year,metric) %>%
  arrange(sdi) %>%
  mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  mutate(half=pop/2) %>% # 计算该国家人口的一半
  mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  mutate(weighted_order=midpoint/pop_global) # 人口中点与总人口相比即为改国的相对位置
# 把年份设置为 factor
rank$year <- factor(rank$year)


temp1 <- rank %>%
  filter(metric=="Rate") %>%
  filter(year==1990)
temp2 <- rank %>%
  filter(metric=="Rate") %>%
  filter(year==2021)
# 建模计算斜度指数
fit1 <- lm(data = temp1,val~weighted_order)
fit2 <- lm(data = temp2,val~weighted_order)
# 查看是否存在异方差（存在异方差）
ncvTest(fit1)
ncvTest(fit2)
# 使用稳健（robust）回归：重复迭代加权
r.huber1 <- rlm(data = temp1,val~weighted_order)
r.huber2 <- rlm(data = temp2,val~weighted_order)
# 获得系数与截距
coef(r.huber1)
coef(r.huber2)
# 计算稳健回归的 95% 可信区间
confint.default(r.huber1)
confint.default(r.huber2)


color <- c("#6699FF","#990000")
colnames(rank)
p_sii_inc <- rank %>%
  filter(metric=="Rate") %>%
  ggplot(aes(x=weighted_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_smooth(method = "rlm",size=0.6,alpha=0.1)+
  scale_fill_manual(values = color)+
  scale_color_manual(values = color)+
  # 增加水平虚线
  geom_segment(x=0.02,xend=0.99,
               y=17981.745,yend=17981.745, # 截距的位置
               color="#6699FF",linetype=2,size=0.4,alpha=0.4)+
  geom_segment(x=0.02,xend=0.99,
               y=17480.156,yend=17480.156, # 截距的位置
               color="#990000",linetype=2,size=0.4,alpha=0.4)+
  # 增加某些国家的标签: 比如中国与印度
  geom_text(aes(label=ifelse(location=="China"|location=="India",as.character(location),""),
                color=year),
            hjust=0,vjust=1.7,# 避免点和文字重合
            size=3)+
  # 增加斜度指数标签
  annotate("text",label="5129.66",x=1.15,y=17981.745+5129.659/2,size=3.5)+ # 位置
  annotate("text",label="5193.00",x=1.15,y=17480.156+5192.977/2,size=3.5)+
  scale_x_continuous(limits = c(0,1.22),labels = c("0","0.25","0.50","0.75","1.00",""))+
  xlab("Relative rank by SDI")+
  ylab("ASIR (per 100,000)")+
  theme_bw()
p_sii_inc


#-------------------prevalence-----------------------
# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)
# 获得sdi数据
sdi <- read.csv('sdi.csv')
names(sdi)[3] <- 'sdi'
sdi$year <- as.integer(sdi$year)
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

#计算总人口
a <- mydata %>%
  filter(metric=="Number") %>%
  group_by(year) %>%
  summarise(sum=sum(pop))
pop1990 <- a$sum[1]
pop2021 <- a$sum[2]
# 计算加权次序
rank <- mydata %>%
  mutate(pop_global=ifelse(year==1990,pop1990,pop2021)) %>%
  group_by(year,metric) %>%
  arrange(sdi) %>%
  mutate(cummu=cumsum(pop)) %>% # 计算累积人口
  mutate(half=pop/2) %>% # 计算该国家人口的一半
  mutate(midpoint=cummu-half) %>% # 累积人口减去该国家人口一半即为人口中点
  mutate(weighted_order=midpoint/pop_global) # 人口中点与总人口相比即为改国的相对位置
# 把年份设置为 factor
rank$year <- factor(rank$year)


temp1 <- rank %>%
  filter(metric=="Rate") %>%
  filter(year==1990)
temp2 <- rank %>%
  filter(metric=="Rate") %>%
  filter(year==2021)
# 建模计算斜度指数
fit1 <- lm(data = temp1,val~weighted_order)
fit2 <- lm(data = temp2,val~weighted_order)
# 查看是否存在异方差（存在异方差）
ncvTest(fit1)
ncvTest(fit2)
# 使用稳健（robust）回归：重复迭代加权
r.huber1 <- rlm(data = temp1,val~weighted_order)
r.huber2 <- rlm(data = temp2,val~weighted_order)
# 获得系数与截距
coef(r.huber1)
coef(r.huber2)
# 计算稳健回归的 95% 可信区间
confint.default(r.huber1)
confint.default(r.huber2)


color <- c("#6699FF","#990000")
colnames(rank)
p_sii_pre <- rank %>%
  filter(metric=="Rate") %>%
  ggplot(aes(x=weighted_order,y=val,fill=year,group=year,color=year))+
  geom_point(aes(color=year,size=pop/1e6),alpha=0.8,shape=21)+
  scale_size_area("Population\n(million)",breaks=c(200,400,600,800,1000,1200))+
  geom_smooth(method = "rlm",size=0.6,alpha=0.1)+
  scale_fill_manual(values = color)+
  scale_color_manual(values = color)+
  # 增加水平虚线
  geom_segment(x=0.02,xend=0.99,
               y=66020.340,yend=66020.340, # 截距的位置
               color="#6699FF",linetype=2,size=0.4,alpha=0.4)+
  geom_segment(x=0.02,xend=0.99,
               y=63135.172,yend=63135.172, # 截距的位置
               color="#990000",linetype=2,size=0.4,alpha=0.4)+
  # 增加某些国家的标签: 比如中国与印度
  geom_text(aes(label=ifelse(location=="China"|location=="India",as.character(location),""),
                color=year),
            hjust=0,vjust=1.7,# 避免点和文字重合
            size=3)+
  # 增加斜度指数标签
  annotate("text",label="5128.81",x=1.15,y=66020.340+5128.814/2,size=3.5)+ # 位置
  annotate("text",label="4723.24",x=1.15,y=63135.172+4723.239/2,size=3.5)+
  scale_x_continuous(limits = c(0,1.22),labels = c("0","0.25","0.50","0.75","1.00",""))+
  xlab("Relative rank by SDI")+
  ylab("ASPR (per 100,000)")+
  theme_bw()
p_sii_pre

library(patchwork)

p_sii_inc1 <- p_sii_inc + labs(tag = 'a')
p_sii_pre1 <- p_sii_pre + labs(tag = 'b')
p_sii_DALY1 <- p_sii_DALY + labs(tag = 'c')

p_sii <-  p_sii_inc1 + p_sii_pre1 +p_sii_DALY1