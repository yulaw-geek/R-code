#############----------------------------------------------------------------------------读取数据文件的操作
#header参数：true则excel第一行用于列名称
#stringsAsFactors=FALSE就是不变成属性数据,按字符串读入。
#sep参数：
#csv 文件是用逗号分隔的，故而 sep = ","tsv 文件是用制表符分隔的，故而 sep = "\t"常用的分隔符还有空格 
#sep = " "分隔符是任意的，可根据具体情况指定的。在输入的时候，
#原内容是用什么符号分隔的，sep就要保持一致，否则可能无法正确读取
a <- c('a','b',0,1,2,3,4,5,6,7,8,9)
b <- combn(a,3)#-----组合遍历
a <- read.csv("温度.csv",header = TRUE,sep = ",",encoding = 'UTF-8',row.names = "XXX")#选择row.names可以设置行名称
a <- read_csv("ouhe.csv")#生成tibble格式
b <- write.csv(data,file = "温度.csv")#写一个csv文件
#############---------------------------------------------------------------------------------快捷键
#############----------------------------------------------------------------------------------计算
scale(c(1,2,4,2,5,2))#标准化处理
count22 <- xtabs(freq~.,data = exer6_2)#生成列联表，data是对应的数据集，freq是第三变量（非行列变量）
rep(3,4)#把3重复4遍
#############-------------------------------------------------------------------------------创建数据集
row.data <- data.frame(a$ID)#第一列给出
commentdata<-data.frame(id=seq(1,length(text),1),term=text)
final_result = data.frame(id=idname ,posneg=filepart$weight[wordindex],result)#依据变量名创造数据集
member_info=data.frame(read.csv('member_info.csv' ,header = 1))#读取文件并创造数据集
#############-------------------------------------------------------------------------------数据类型转换
data$field<-as.numeric(data$field)   #数值转换
data$field<-as.factor(data$field)    #因子转化
commentdata$term<-as.character(commentdata$term)#转化为字符串
for (i in 1:25) {
  data[,i] <- as.numeric(as.vector(data[,i]))
}   #循环技术进行数据类型转化
#############--------------------------------------------------------------------------------常规数据查看
data(data)     #加载数据集data
head(data)     #显示数据集data的前若干条数据
tail(a)     #显示数据集data的后若干条数据
str(data)      #探寻数据集内部结构
summary(a)   #获取数据集data的概括信息
dim(a)      #查看数据集data的纪录数和维度数
table(a$Temperature)    #数值分布
prop.table(a$Temperature) #比例分布
nrow(filepart) #查看行数
ncol(filepart) #查看列数
unique(a$ID) #查看非重复变量
names(a) #使用names函数查看数据表的列名称
#############--------------------------------------------------------------------------------数据集修改名字、行列顺序变换
names(score) [5]="chinese"#更改变量名字，但是会改变原数据集(重命名)
rename(score,c(pl="chinese"))  #使用reshape包改变变量名字（重命名）
#改名
rename(flights,tail_num = tailnum)
colnames(score)[5]="Chinese"  #改变列名
trading = trading[, c(16:17, 1:15)]#选择所示列的顺序
trading = trading[, c(2,3,5,1)]#选择所示列的顺序
refcols <- c("Trader", "System")#先创建一个开头索引1-2
trading <- trading[, c(refcols, setdiff(names(trading), refcols))]#选择所示的列的顺序
dplyr::select(a,ID,Temperature)#选择a中的ID,Temperature两个变量
#############-----------------------------------------------------------------------------------------根据条件选择/赋值
iris[1:9,]#选择对应的行或者列
iris[,2:5]
iris[1:9,2:5]
iris$Sepal.Length[ID==i]#查看数据集ID为i对应的Sepal.Length
baogao <- c[3,'Temperature']#提取第i行对应的变量值（单变量值）
c <- subset(a,ID==2)#提取函数:提取a数据集ID取值为i的对应数据行（多行）
library(tidyverse)
data %>% filter(ActualValue < Total) %>%split(f = .$Date)
dim(dataframe)[0]#-----计算数据框的行和列数
table(unlist(z))#----列联表
iris[iris$Sepal.Length > 7,]#通过中括号条件选择来选取,可以用逻辑连接符来限制多重条件
iris[iris$Sepal.Length > 7 & iris$Sepal.Width > 3 & iris$Petal.Length >6,]
data<- a[which(a[,1]==1),]    #选取data数据表里第11列维度为1的数据
y[which(y<1.5)] <- 1#根据所构造的条件进行赋值
data$is_do <- ifelse(data$is_do > 0.7 ,2,ifelse(data$is_do > 0.3 ,1,0))
x<-quantile(a$Temperature, 0.03,na.rm=TRUE)  #data数据表中维度AGE从小到大排序前3%处数值
q2_AGE<-quantile(data$AGE, 0.97,na.rm=TRUE)  #data数据表中维度AGE从小到大排序前97%处数值
data$AGE <- ifelse(data$AGE < q1_AGE,q1_AGE,data$AGE)
data$AGE <- ifelse(data$AGE > q2_AGE,q2_AGE,data$AGE)
data <- data[names(data) %in% c("CN_EFF_DATE", "CN_EXP_DATE")]   #选取data数据表中字段CN_EFF_DATE、CN_EXP_DATE
data <- data[!(names(data) %in% c("CN_EFF_DATE", "CN_EXP_DATE"))]   #删除data数据表中字段CN_EFF_DATE、CN_EXP_DATE
aa <- data[names(a) %in% c("Temperature")]# %in%是match函数的缩写
train_sub=sample(nrow(train_example),3/4*nrow(train_example))#按比例划分数据     
train_data=train_example[train_sub,]
test_data=train_example[-train_sub,]
#############缺失值
a$Temperature[complete.cases(a$Temperature)]#找出a数据集中Temperature变量不是NA的值,查看NA值则加!
tstterm$adv_score[!complete.cases(tstterm$adv_score)]<--999#找出NA并赋值
wordfile$idx[!is.na(wordfile$weight)]#找出weight中NA值对应的ID
a1 <- dplyr::filter(a, !is.na(a$Temperature))#去除NA值对应的行
na.rm=TRUE#去除NA的参数操作
which(!complete.cases(a$Temperature))#找出NA对应的行数
aaa<- a[-which(!complete.cases(a$Temperature)),]#减去NA值对应的行
table(!complete.cases(aaa$Temperature))
sum(!is.na(x))#计算非缺失值的数量
#############----------------------------------------------------------------------------------------删除操作-----
df=data[-which(data$VOLUME==30),]#减去data数据集中VOLUME值为30对应的行
#############-----------------------------------------------------------------------------------------抽样--------
data<-data[sample(nrow(data),10000),]    #随机从数据集data中选取10000条纪录
number=sample(nrow(data),1/4*nrow(data))  #计算数据集data样本量1/4的数值
train_example=data[number,]               #对数据集data随机抽样number个数
#############-----------------------------------------------------------------------------------矩阵归一化（0到1）
b1=(p[,1]-min(p[,1]))/(max(p[,1])-min(p[,1]))  
#############--------------------------------------------------------------------------------根据变量拆分/批量处理
##split拆分，f参数表示按照拆分的列,拆分后得到的是列表
iris_splited<-split(iris,f=iris$Species)
#使用reshape2包可以分组使用函数，第一个参数是数据集，第二个数据集是参数划分特征，第三个需要计算的特征列，第四个参数使用函数
library(reshape2)
dcast(iris,Species~.,value.var = 'Petal.Length',fun = mean)
#melt重构数据，第一个参数数据对象，第二个分类变量
iris_long<-melt(iris,id = "Species")
head(iris_long)
#dcast分类处理
dcast(iris_long,Species~variable,value.var='value',fun=mean)
#还是基于tips数据集，定义计算小费和总餐费的比率函数
fee_fun<-function(x){
  sum(x$tip)/sum(x$total_bill)
}
##plyr分类处理ddply，dd表示输入时dataframe输出是dataframe，以此类推有dlply,ld,ll等等，还是很好记的,但处理函数要涉及所有的数值列
library(plyr)
ddply(tips,"sex",fee_fun)
ddply(tips,sex~smoker,fee_fun)
ddply(tips,sex~smoker+time,fee_fun)
#使用aggdata函数对数据集中相同变量(cyl,gear)组合进行函数(求均值)运算
aggdata<-aggregate(mtcars,by=list(cyl,gear),FUN=mean,na.rm=TRUE)
#############------------------------------------------------------------------------------------数据链接、匹配------------
##自定义量df
df1<-data.frame(id=c(1,2,3),gender=c(23,24,26))
df2<-data.frame(id=c(2,4,1),name=c("d","w","t"))
##整合连接,会自动连接,inner模式是merge的默认匹配模式
merge(df1,df2,by="id")
merge(df1,df2,all= TRUE,sort=TRUE)#outer模式匹配
merge(df1,df2,all.x=TRUE,sort=TRUE)#left模式匹配,保留df1，以此为基础对df2中的内容进行匹配
merge(df1,df2,all.y=TRUE,sort=TRUE)#right模式匹配,保留df2，以此为基础对df1中的内容进行匹配
##join连接,type可以选择连接方式,现在好像找不到join函数了，应该用dplyr包中的left_join函数
join(df1,df2,by='id',type='right')
join(df1,df2,by='id',type='left')
join(df1,df2,by='id')
tstterm_nnid<-join(tstterm,idnotnull,type="inner")
#字段合并
data_new<-data.frame(data[,c(1:16)],age_new) 
#字符链接
subid<-paste(id,"-",unlist(lapply(temp,groupid)),seq="")
paste("a","b",sep = "")#链接字符，无空隙
q <- data.frame(id=c(3,4,6,3))#行拼接，列拼接为cbind()
e <- data.frame(id=NA)
w <- rbind(e,q)
#############----------------------------------------------------------------------------------for循环
#利用for创建数据集累加
row.data <- data.frame()#生成空的数据集
for (i in 1:50) {
  for.data <- i
  row.data <- rbind(row.data,for.data)
} 
#根据关键词清除相关广告帖
dataclean<-function(text,c){
  noise<-c()
  for (i in 1:length(c)) {
    noise1 <- text%>%subset(grepl(c[i],title))
    noise <- rbind(noise,noise1)
  }
  jianzhu1 <- text[-noise$xulie,]
  write.csv(jianzhu1,file = "jianzhu1.csv")
}
#使用for循环赋值
age_new<-c()
for (i in 1:length(data$AGE)){
  if(data$AGE[i]>=0&data$AGE[i]<18) 
  {age_new[i]<-'少年'}
  if(data$AGE[i]>=18&data$AGE[i]<30)
  {age_new[i]<-'青年'}
  if(data$AGE[i]>=30&data$AGE[i]<60) 
  {age_new[i]<-'壮年'}
  if(data$AGE[i]>=60)
  {age_new[i]<-'老年'}
}
#-------删除离群点，把离群点设置为NA---
x <- matrix(1:100,nrow = 20,ncol = 5)
x[20,1] <- 200
x[20,2] <- 300
x[20,3] <- 500
for (i in 1:10) {
  b <- boxplot.stats(x[,i],coef = 1.5,do.conf = TRUE,do.out = TRUE)
  x[,i][which(x[,i]==b$out)] <- NA
}
#--Rcpp包内的函数对于文本分词是必要的，虽然分词时并没有加载这个包
#--R包如果不能利用在Rstudio中删除，可以在library文件夹中自行删除，然后安装最新版
#-----计算文本相似度-----
#首先利用jiebaR包实现文本分词，然后利用tm包构建文档词条矩阵（Vcorpus函数向量储存位置）
#,之后计算余弦相似度
#-----余弦相似度公式
top_similarity[j] <- sum(x*y)/sqrt(sum(x^2)*sum(y^2))


#---通过原序列和连续序列对比，找出间断点和连续的区间
a <- c(1:100)
b <- c(50:150)
c <- setdiff(a, b)
#----寻找连续值——---
length(c(a[-1],0)-a)
result = vector("list", 100)
b = c(WVL_Cellulose[-1],0)
c = b - WVL_Cellulose
k = 1
result[[1]] = WVL_Cellulose[1]
for(i in 1:length(c))
{
  
  if(c[i] == 1)
  {
    result[[k]] = c(result[[k]], WVL_Cellulose[i+1])
  }
  if(c[i] != 1)
  {
    k = k+1
    result[[k]] = WVL_Cellulose[i+1]
  }
  
}
result = result[1:(k-1)]
#-----管道符号幂次运算#
1:5%>%-3
1:5 %>% .+2
1:5%>% .^2
(1:5)/2
(1:5)+3
#----批量处理文件名（暂时没有解决）
rm(list=ls())#删除变量
setwd("E:/环界")
nm=dir()#查看文件名字
nm2=strsplit(nm,split = "_")##分隔文件
nm3=as.data.frame(matrix(c(strsplit(nm,split = "_"),recursive =T),ncol = 3,byrow = T))
nm3$V2=paste0(nm3$V1,"_",as.numeric(nm3$V2)+8,"_",nm3$V3)#生成新名字
#paste()与paste0()不仅可以连接(链接)多个字符串，还可以将对象自动转换为字符串再相连，另外还能处理向量
#paste0函数，默认是sep=""(两个函数的唯一区别)
#读取文件与输出文件(修改名字)
for (i in 1:length(nm)) {
  a=read.csv(nm[i])
  colnames(a)=c("随便写")
  write.csv(a,paste0("E:/环界\\",nm3[i,5]),row.names=FALSE)
}
#---判断数是否在序列中
4.5 %in% seq(3,5,0.1)
a <- c(3.7,4.5,4.56)
for (i in 1:length(a)) {
  if (a[i] %in% seq(3.5,5.5,0.01))
  {
    print("TRUE")
    }
}
(0.2+0.2) == 0.4
all.equal((0.2+0.2),0.4)
#筛选（选择）行
library(nycflights13)
filter(flights,month == 1,day ==1)
#或运算（&代表与，！代表非）
aaa <- filter(flights,month==1|month ==12)
filter(flights,month %in% c(1,12))
filter(flights,!(arr_delay > 120 | dep_delay > 120))
filter(flights,arr_delay <=120, dep_delay <=120)
#使用arrange排列行,类似于excel的排列，可以按照多个列进行排序
library(nycflights13)
head(flights)
arrange(flights,dep_time,day)
arrange(flights,desc(arr_delay))
df <- tibble(x=c(2,1,3,4),y=c(5,6,7,8),z=c(1,2,3,4))
ff <- tibble(x=c(1,2,NA))
arrange(df,y)
#----选择（筛选）列
select(flights,year,month,day)
select(flights,year:dep_time)
select(flights,-(year:day))
select(flights,starts_with("arr"))
select(flights,ends_with("time"))
select(flights,contains('time'))
#添加变量,使用Mutate(新变量)
flights_sml <- select(flights,year:day,ends_with("delay"),distance,air_time)
mutate(flights_sml,gain = arr_delay -dep_delay,speed = distance/air_time*60)
transmute(flights_sml,gain = arr_delay -dep_delay,speed = distance/air_time*60)#只保留新变量
#分组计算（分组摘要）
summarize(flights,delay = mean(dep_delay,na.rm = TRUE))
by_day <- group_by(flights,year,month,day)#分组
summarize(by_day,delay=mean(dep_delay,na.rm = TRUE))
#管道函数多种操作哦
delays <- flights %>% group_by(dest) %>% summarize(count = n(),
                                                   dist =mean(distance,na.rm =TRUE),
                                                   delay =mean(arr_delay,na.rm =TRUE)
                                                   %>% filter(count>20,dest!="HNL"))

#计数（统计频数）
diamonds%>%count(cut)#上下等价
diamonds%>%group_by(cut)%>%summarize(n())
daily <- group_by(flights,year,month,day)
per_day <- summarize(daily,flights = n())#每次统计摘要都会用掉一个分组变量
per_month <- summarize(per_day,flights = sum(flights))
per_year <- summarize(per_month,flights = sum(flights))
#取消分组
daily %>% ungroup()%>%summarize(flights=n())
#分组筛选（分组新变量）
#找出每个分组中最差的成员
flights_sml %>% group_by(year,month,day) %>% filter(rank(desc(arr_delay))<10)
flights%>%group_by(dest)%>%filter(n()>365)

###########--------------tibble简单数框操作(tibble不能改变输入类型、变量名称，不能创建行名称)
as_tibble(iris)
tibble(x=1:5,Y=1,Z=x^2+Y)
#tibble取子集
df <- tibble(x=runif(5),y=rnorm(5))
df$x 
df[["x"]]
df[1]
df[[1]]
as.data.frame(df)#tibble转化成data.frame
#tibble和data.frame操作对比
df <- data.frame(abc=1,xyz="a")
df$X
df[,"xyz"]
df[,c("abc","xyz")]
df <- tibble(abc=1,xyz="a")
df$X
df[,"xyz"]
df[,c("abc","xyz")]
ouhe <- read_csv("C:/Users/liu/Desktop/study/ouhe.csv",skip=2,comment = "#")#跳过前2行，丢弃以“#”开头的行
read_csv("a,b,c
         1,2,4 
         4,5,6")
#当数据没有列名称时，使用col_name =FALSE来通知read_csv不要将第一行作为列标题，
#而依次标注为X1到Xn
read_csv("1,2,3\n4,5,6\n3,4,5")
read_csv("1,2,3\n4,5,6",col_names = FALSE)
read_csv("1,2,3\n4,5,6",col_names = c("X","Y","Z"))
#na设定使用哪个值表示文件的缺失值
read_csv("a,b,c\n1,2,.",na=".")
#预留问题：1、用什么函数读取由“|"分隔的文件2、read_tsv() read_fwf()
#解析向量
str(parse_logical(c("TRUE","FALSE")))
str(parse_integer(c("1","2")))
str(parse_date(c("2001-01-02")))
parse_integer(c("1","23",".","455"),na=".")
x <- parse_integer(c("2313","33"))
class(x)
parse_double("1.34")
parse_double("1,33",locale = locale(decimal_mark =","))
fruit <- c("apple","banana")
parse_factor(c("apple","banana","gggg"),levels = fruit)
#解析时间
parse_datetime("2000-10-01T2010")
parse_datetime("20101010")
parse_date("2010-10-20")
library(hms)#用来表示时间数据
parse_time("01:10 am")
parse_time("01:10 pm")
parse_date("01/02/15","%m/%d/%y")
parse_date("01/02/15","%d/%m/%y")
parse_date("01/02/15","%y/%m/%d")
challenge <- read_csv(readr_example("challenge.csv"))#readr_example可以找出包含在R包中的文件路径
problems(challenge)
write_csv(challenge,"challenge.csv")
read_csv("challenge.csv")

#####验证主键8y
library(tidyverse)
   planes %>% count(tailnum)%>%filter(n>1)
weather%>% count(year,month,day,hour,origin)%>%filter(n>1)
library(nycflights13)
flights%>%count(year,month,day,flight)%>%filter(n>1)
flights%>%count(year,month,day,tailnum)%>%filter(n>1)
#添加代理键
a <- flights%>%mutate(zhujian=row_number(flights$month))
View(a)
flights2 <- flights%>%select(year:day,hour,origin,dest,tailnum,carrier)                                                                                                                                                                                                                    
flights2%>% select(-origin,-dest)%>%left_join(airlines,by="carrier")
x <- tribble(
  ~key,~val_x,
  1,"x1",
  2,"x2",
  3,"x3"
)
flights2%>%left_join(weather)
flights2%>%left_join(planes,by="tailnum")

#----正则表达式模式匹配
library(stringr)
striing1 <- c("12","rr","tt")
writeLines(striing1)
x <- c("apple","banana","pear")
str_view(x,"an")
str_view(x,".a.")
dot <- "\\."
writeLines(dot)
str_view(c("abc","a.c","bef"),"a\\.c")


####因子处理
library(forcats)                                                                                        
gss_cat
##使用lubridate处理日期和时间
library(lubridate)
today()
now()
aa <- c("2014-02-12","2000-10-20","2010-12-31")
bb <- c()
for (i in length(aa)) {
  bb[i] <- as.Date(aa[i])
}
a <- as.Date("2000-10-20")
is.Date(aa[1])
c <- read.csv("fc.csv")
as.Date(x[1],)
#临时创建一个环境
env <- environment()

####条件执行
if(condition){
  
}else{
  
}
a <- c(1,3,4,5)
class(a)
typeof(a)
####列表操作
x <- list(1,2,3)
str(x)
y <- list("a",2,1L,TRUE)
str(y)
######################使用modelr实现基础模型
library(tidyverse)
library(modelr)
ggplot(sim1,aes(x,y))+geom_point()
models <- tibble(
  a1=runif(250,-20,40),
  a2=runif(250,-5,5)
)
ggplot(sim1,aes(x,y))+geom_abline(aes(intercept=a1,slope=a2),data=models,alpha=1/4)+geom_point()
8

z <- tibble(
  自变量=runif(20,-5,5),
  因变量=runif(20,-10,10)
)
ggplot(z,aes(自变量,因变量))+geom_point()

####自定义函数迭代方式:1、通过设置while条件 2、通过内部重复引用函数



