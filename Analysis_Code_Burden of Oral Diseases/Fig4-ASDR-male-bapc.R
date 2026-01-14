####Incidence####

#---------------male-----------------
#先处理发生人数数据#
###筛选需要的数据
IBD_in_male<- subset(IBD_china,
                     
                     
                     (IBD_china$age_name %in% age1) &
                       
                       
                       IBD_china$sex_name=="Male"&
                       
                       
                       IBD_china$location_name=='Global'&
                       
                       
                       IBD_china$metric_name== 'Number' &
                       
                       IBD_china$cause_name== 'Oral disorders' &
                       
                       IBD_china$measure_name=='DALYs (Disability-Adjusted Life Years)') #这些指标可以改




unique(IBD_in_male$age_name)



####年龄改为因子，并按升序排列
IBD_in_male$age_name <- factor(IBD_in_male$age_name, 
                               
                               levels = c("70-74", "75-79", "80-84", "85-89", 
                                          "90-94", "95+"))


# 提取数据Measure_name,age_name,year,val
IBD_in_male <- IBD_in_male[,c("measure_name", "age_name","year","val")]




#长转宽
IBD_in_male_n <- reshape2::dcast(data=IBD_in_male, #  reshape2::dcast()：将数据从长格式转换为宽格式，适用于以某个变量为行，另一个变量为列的情况。
                                 
                                 year ~ age_name,  #  指定 year 作为行，age_name 作为列。
                                 
                                 value.var="val")  # value.var = "val"：指定 val 列中的数据作为新表中每个单元格的值。

# 最终结果 IBD_in_both_n 是一个宽格式的数据框，其中行是不同的年份，列是不同的年龄组，每个单元格对应特定年份和年龄组的发病人数。



#让year做行名
rownames(IBD_in_male_n) <- IBD_in_male_n$year


#删除第一列
IBD_in_male_n <- IBD_in_male_n[,-1]


#IBD_in_both_n <- apply(IBD_in_both_n,c(1,2),as.integer) %>% as.data.frame


####含义####
IBD_in_male_n <- apply(IBD_in_male_n,
                       
                       c(1,2),round) %>% 
  as.data.frame  

#  解释：apply(IBD_in_both_n, c(1, 2), round)：apply() 是一个 R 函数，适用于矩阵或数据框。
#  c(1, 2)：指定对数据的每一行和每一列应用函数。1 表示按行操作，2 表示按列操作，c(1, 2) 表示对矩阵的每个元素应用函数。
#  round：这是要应用的函数，它对每个数值进行四舍五入。因此，这段代码会对 IBD_in_both_n 中的每个元素进行四舍五入;
#  %>%：管道操作符，将 apply() 的输出作为 as.data.frame 的输入。as.data.frame：将四舍五入后的结果转换为数据框。
#  总结：这段代码的作用是对 IBD_in_both_n 中的每个数值元素进行四舍五入，并将处理后的结果转换为数据框格式。






####人口数据#####

#筛选性别
GBD_male_population<- subset(GBD_population,
                             
                             GBD_population$sex_name =="Male")





#筛选预测数据
GBD_male_prediction <- GBD_population_prediction %>%    
  dplyr::select(prediction_var_name) %>%           
  dplyr::filter(location_name %in% 'Global' &        
                  year_id %in% 2022:2041   &         
                  sex %in% "Male")                  


unique(GBD_male_prediction$age_group_name)



GBD_5year <- GBD_male_prediction %>% 
  
  
  filter(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","1 to 4")) %>%    #  这一步筛选出年龄组为 "Early Neonatal"（早期新生儿）、"Late Neonatal"（晚期新生儿）、"Post Neonatal"（新生儿后期）和 "1 to 4"（1到4岁）的数据。这些年龄组数据将被合并为一个新的年龄组, 即"<5" 这个新的年龄组。
  
  group_by(location_name,sex,year_id) %>%     #  对筛选后的数据按地区 (location_name)、性别 (sex)、年份 (year_id) 进行分组。这个分组操作是为了在后续的 summarise() 中对每个分组计算总和。例如，中国的男性人口数据在 2022 年的小于5岁的这几类年龄组会被分为一个组，方便进一步操作。
  
  
  
  summarise(val=sum(val)) %>%                 #  在每个分组中，summarise() 函数将 val 列中的数值进行求和，计算不同年龄组的总人口数。比如，对于某一年，"Early Neonatal"、"Late Neonatal"、"Post Neonatal" 和 "1 to 4" 这些年龄组的人口会相加，得到总的 <5 人口数。例如：对于某一年，合并后的 <5 人口数等于各年龄段（新生儿阶段和 1-4 岁）的总和。
  #  含义：val = sum(val)：这里 summarise() 对每个分组的数据进行操作，将变量 val 的数值进行汇总（求和）。具体来说：val 是你要操作的变量，它可能表示人口数量。sum(val) 计算每个分组中 val 列的总和（即将相同地区、性别、年份内，不同年龄组的人口数加起来）。使用场景：计算总和：如你的代码中，将特定分组内的数值相加。计算均值：你也可以使用 mean() 来计算某一变量的平均值。计算计数：可以使用 n() 来计算每个分组中有多少行。
  
  
  
  mutate(age_group_name="<5")                 #   为新生成的数据添加一个新的变量或修改现有变量。在这一步，将新分组后的这些数据的年龄组名称都设置为 "<5"，表示该组是小于5岁的人口。

#  总结：这段代码的作用是从 GBD_population_prediction 数据中筛选出小于5岁的人口（包含 "Early Neonatal", "Late Neonatal", "Post Neonatal", "1 to 4" 这些年龄段），并将这些年龄段的人口数据合并为一个新的年龄组 <5，然后按地区、性别和年份对这些数据进行分组，计算每个分组内的总人口数。







GBD_male_prediction <- GBD_male_prediction %>% 
  
  filter(!(age_group_name %in% c("Early Neonatal","Late Neonatal", "Post Neonatal","All Ages","1 to 4"))) %>%     #  这一步使用 filter() 函数来筛选数据，保留不在 c("Early Neonatal", "Late Neonatal", "Post Neonatal", "All Ages", "1 to 4") 列表中的年龄组。!（取反运算符）：表示排除这些年龄组的数据。这意味着，GBD_population_prediction 数据集中属于这些年龄组的行会被删除。这些被排除的年龄组已经在前面的步骤中汇总到 <5 这个年龄组，所以不再需要保留它们。
  
  rbind(GBD_5year)                 #  rbind(GBD_5year)：rbind() 函数用于将两个数据框按行合并。GBD_5year 是之前汇总得到的新的 <5 岁年龄组的数据集，表示小于5岁的人口总数。通过 rbind()，将这个 <5 岁的数据合并到已经过滤的 GBD_population_prediction 数据集中。总结：这段代码的作用是先删除 GBD_population_prediction 数据集中和 <5 岁相关的原始年龄组数据（包括早期新生儿、晚期新生儿、1到4岁等），然后将之前通过汇总生成的 <5 岁人口数据（即 GBD_5year）合并到数据集中。最终，GBD_population_prediction 会包含新的 <5 岁人口数据，同时保留其他不受影响的年龄组的数据。



names(GBD_male_prediction)[names(GBD_male_prediction) == 'age_group_name'] <- 'age_name'  



#  names() 函数用于获取或设置数据框列的名称（列名）。将 GBD_population_prediction 数据框中的 age_group_name 列的名称改为 age_name，以便统一列名或方便后续的数据操作。




GBD_male_prediction$age_name<-gsub(" to ","-",GBD_male_prediction$age_name)


GBD_male_prediction$age_name<-gsub(" plus","+",GBD_male_prediction$age_name)



unique(GBD_male_prediction$age_name)



colnames(GBD_male_prediction)<-var_name


#  "location_name" "sex_name"      "year"  "age_name"      "val"   



GBD_male_prediction<-GBD_male_prediction%>% 
  filter(age_name %in% ages_2 )

unique(GBD_male_prediction$age_name)



#####合并人口学数据1990-2036#####
GBD_male <- rbind(GBD_male_population, GBD_male_prediction)  



#  解释：rbind()：rbind() 函数用于将两个或多个数据框按行进行合并。它会将第二个数据框的行添加到第一个数据框的底部。
#  在这个例子中，GBD_Male_population 和 GBD_population_prediction 是两个已经存在的数据框，它们包含的是关于男性人口的历史和预测数据。
#  GBD_Male_population：这个数据框应该包含从过去到某一时间点的男性人口数据，通常是从实际统计数据中得来，代表历史人口数据。
#  GBD_population_prediction：这是之前预测出来的未来男性人口数据，经过一系列筛选和处理后，代表了从未来特定年份（比如 2022-2036 年）的预测人口数据。
#  GBD：GBD 是新的数据框，合并了 GBD_Male_population 和 GBD_population_prediction，因此包含了历史数据和预测数据。这将用于进一步的分析和模型预测。
#  总结：这段代码将历史的男性人口数据（GBD_Male_population）和预测的男性人口数据（GBD_population_prediction）按行合并，创建了一个包含两者的综合数据集 GBD，它涵盖了过去和未来的男性人口数据。


GBD_male$age_name<-factor(GBD_male$age_name, levels = c("70-74", "75-79", "80-84", "85-89", 
                                                        "90-94", "95+"))
unique(GBD_male$age_name)








# 整理人口学数据变成BAPC能够识别的数据形式

GBD_China_male <- subset(GBD_male,
                         
                         location_name=="Global" & 
                           
                           sex_name=="Male")


GBD_China_male$age_name<-factor(GBD_China_male$age_name, 
                                
                                levels = c("70-74", 
                                           "75-79", "80-84", "85-89", "90-94", "95+"))


#长改宽
GBD_China_male_n <- dcast(data=GBD_China_male, 
                          
                          year~age_name, 
                          
                          value.var=c("val")) %>% 
  as.data.frame()



#改行名
rownames(GBD_China_male_n) <- GBD_China_male_n$year


#删除第一列
GBD_China_male_n <- GBD_China_male_n[,-1]



#转为数值、数据框
GBD_China_male_n <- apply(GBD_China_male_n, 
                          
                          c(1,2), 
                          
                          as.numeric) %>% as.data.frame()

#四舍五入
GBD_China_male_n <- apply(GBD_China_male_n, 
                          
                          c(1,2), 
                          
                          round) %>% as.data.frame()




# 补充没有发病人数数据的年份
IBD_pro_male <- matrix(data=NA,                             #  使用 matrix() 函数创建一个矩阵，参数 data = NA 表示矩阵中的所有初始值为 NA，即缺失值。这个矩阵相当于一个空白表格，等待填充数据。
                       
                       
                       nrow=2041-2021,                      #  指定矩阵的行数为 2036 - 2021，即 15 行。这意味着矩阵的每一行代表一个年份，从 2022 年到 2036 年。不是16行.
                       
                       ncol=ncol(GBD_China_male_n)) %>%     #  ncol() 函数用于获取数据框 GBD_China_Male_n 的列数(20)。GBD_China_Male_n 的列数(20)应该对应不同的年龄组，这一步是确保新创建的矩阵 IBD_pro 的列数与 GBD_China_Male_n 相同。例如，如果 GBD_China_Male_n 有 20 个年龄组，那么 IBD_pro 也会有 20 列，每列将来会用来存储相应年龄组的预测数据。
  
  
  as.data.frame()                      #  as.data.frame()：使用管道操作符 %>% 将 matrix() 创建的矩阵转换为标准的 R 数据框格式，以便更容易进行后续的数据操作。矩阵和数据框的区别在于数据框支持更复杂的数据结构，且更适合数据分析。




#年份做行名
rownames(IBD_pro_male) <- seq(2022, 2041, 1)



#年龄组做列名
colnames(IBD_pro_male) <- names(IBD_in_male_n)


#合并1990-2041年的发病数据框
IBD_pro_n_male <- rbind(IBD_in_male_n, IBD_pro_male)



IBD_pro_n_male <- apply(IBD_pro_n_male, 
                        
                        c(1,2), 
                        
                        as.numeric) %>% 
  
  as.data.frame()



IBD_pro_n_male <- apply(IBD_pro_n_male, 
                        
                        c(1,2), 
                        
                        round) %>% 
  
  as.data.frame()

require(INLA)
#  require(INLA) 是用于加载 R 中的 INLA 包的函数。



# 模型预测


IBD_input_male <- APCList(IBD_pro_n_male, 
                          
                          GBD_China_male_n, 
                          
                          gf=5)##gf为年份间隔

#  详细解释：APCList(IBD_pro_n, GBD_China_Male_n, gf = 5)：APCList 函数用于创建一个适合 BAPC 分析的对象。该对象包含人口数据和发病数据，并可用于后续的预测分析。


#  IBD_pro_n：这是包含 IBD 发病数据的一个数据框，按照不同年份和年龄组排列。这个数据将作为模型的输入之一，用于分析 IBD 在不同年龄和年份中的发病率。


#  GBD_China_Male_n：这是中国男性人口的一个数据框，按年份和年龄分层。它提供了相应的年龄组人口数据，通常在进行发病率预测或调整时使用。


#  gf = 5：gf 参数代表年份的间隔。在这里，gf = 5 表示以 5 年为一个时间间隔。这个参数决定了如何在模型中对年份进行分组或预测。


#  IBD_input：这是定义的变量名，用于存储 APCList 函数的输出，即包含发病率和人口信息的列表对象。该对象将用于后续分析，比如 BAPC 模型的预测。


#  总结：IBD_pro_n 是 IBD 的发病人数数据，GBD_China_Male_n 是中国男性人口数据。


#  APCList 函数将这两个数据集与 5 年的时间间隔参数（gf = 5）结合起来，创建一个对象，用于后续的 APC 模型预测。


IBD_bapc_result_male <- BAPC(IBD_input_male, 
                             
                             predict=list(npredict=20, retro=T), 
                             
                             secondDiff=FALSE, 
                             
                             stdweight=wstand, 
                             
                             verbose=F)

#  详细解释：IBD_bapc_result <- BAPC(...)：这行代码调用 BAPC 函数，并将结果存储在 IBD_bapc_result 变量中。
#  BAPC 是一个用于处理年龄-时期-队列数据的模型，常用于预测某个群体中不同年龄组的发病率或死亡率。
#  IBD_input：这是通过 APCList 函数生成的输入数据对象，包含了 IBD 发病数据和中国男性人口数据，已经按年龄和年份进行分组。该对象为 BAPC 模型提供基础数据，用于进行分析和预测。


#  predict = list(npredict = 15, retro = T)：npredict = 15：表示模型预测未来 15年 的数据。这意味着模型不仅会计算历史数据，还会生成未来15年的预测结果。
#  retro = T：表示模型将进行回溯预测，即除了预测未来的数据外，模型还会对已知的历史数据进行验证性预测（回顾性分析）。



#  secondDiff = FALSE：这是控制 APC 模型的平滑参数。secondDiff = FALSE 表示不使用二阶差分来对数据趋势进行额外的平滑。二阶差分是平滑数据波动的一种方法，用于减少预测中的剧烈变化。如果设为 TRUE，模型会通过平滑处理减少波动，但可能会忽略一些真实的趋势。


#  stdweight = wstand：这里传递的是 wstand，即标准构成比。wstand 通常是基于标准化年龄结构的权重（例如 WHO 标准人口），用于调整发病率或死亡率的年龄分布。使用 stdweight 可以得到标准化的预测结果，方便与其他地区或时间段进行比较。


#  verbose = F：设为 FALSE 表示在模型运行过程中不显示详细的输出信息。如果设为 TRUE，则会在运行过程中输出详细的进程信息。
#  总结：这段代码使用 BAPC 模型对 IBD（炎症性肠病）的发病率进行预测，基于历史数据预测未来15年的发病趋势，并且使用标准构成比对发病率进行年龄标准化。结果存储在 IBD_bapc_result 中，后续可以用该结果进行可视化或进一步分析。

p3<-plotBAPC(IBD_bapc_result_male, 
             
             scale=10^5, 
             
             type = 'ageStdRate', 
             
             showdata = TRUE)



#提取数据
both_de =data.frame(IBD_bapc_result_male@agestd.rate)
#  详细解释：：@agestd.rate 是 IBD_bapc_result 对象中的一个槽位，它存储了年龄标准化发病率的结果。
#  这些发病率数据已经通过 BAPC 模型计算，并根据标准化权重（如 wstand）进行了调整，去除了不同年龄组之间的差异影响，使得发病率具有可比性。
#  agestd.rate 是按年份和年龄分布的预测发病率，标准化后可以方便不同年龄组、地区或时间段之间的比较。
#  data.frame()：data.frame() 函数将 IBD_bapc_result@agestd.rate 转换为一个标准的 R 数据框格式。数据框是一种常用的数据结构，便于后续数据处理、可视化和分析。
#  Male_de：这是最终生成的数据框，包含了 IBD_bapc_result 的年龄标准化发病率。该数据框中的每一列代表不同的年龄组，每一行则可能对应某一年份的标准化发病率。
#  总结：这段代码将 BAPC 模型预测的年龄标准化发病率提取出来，并存储在数据框 Male_de 中。这个数据框可以用于进一步的分析、可视化，或者与其他群体或地区的数据进行对比。


# 将分解结果保存为CSV文件
write.csv(both_de, "ASPR-male.csv")

