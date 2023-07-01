library("stringr")
library("readxl")
library("xlsx")
library("jiebaRD")
library("jiebaR")
library("plyr")
library("dplyr")
#评论数据清洗函数
#---------------------------------------------数据清洗.函数封装---------------------------
# 1-1 函数功能：清理文本数据
# 1-2 参数说明：
# 1-3 text：文本向量
dataclean<-function(text){
  text <- gsub(pattern = " ", replacement ="", text) 
  #有时需要使用\\\t     
  text <- gsub("\n|\\\t", "", text)
  text<-gsub(pattern="([0-9]{4}年)?([0-9]*月)?[0-9]{1,}日","",text)
  text<-gsub(pattern="([0-9]{4}年)","",text)
  text<-gsub(pattern="([0-9]{1,}月)","",text)
  text<-gsub(pattern="[0-9]{1,}","",text)
  #清除英文字符\特殊字符：
  text <- gsub("︵", "", text)
  text <- gsub("～", "", text)
  text <- gsub("&#x", "", text)
  text <- gsub("﹏", "", text)
  text <- gsub("[a-zA-Z]", "", text)
  text <- gsub("[\\*|\\%]", "", text)
  #去除空格
  text <- gsub(pattern = " ", replacement ="", text)
  #剔除清洗后，文本内容为NA的评论数据：
  text <- text[!is.na(text)]
  #设定将清洗后字符长度小于2的文本评论去除：
  text <- text[!nchar(text) < 2]
  return(text)
}
# 2-1 函数功能：分片段并打上标识
# 2-2 参数说明：
# 2-3 text：文本向量
splitsentence <- function(text) {
  #[1]将评论文本转为数据框，为每条评论打上编号：
  commentdata<-data.frame(id=seq(1,length(text),1),term=text)
  commentdata$term<-as.character(commentdata$term)
  #[2]以标点符号作为分隔符把句子分成片段
  subcon<-strsplit(text,",|\\.|\\?|;|~|，|。|！|\\？|；|～|…|﹏﹏|。。。。。。|\\.\\.\\.\\.\\.\\.")
  
  #[3]计算每条评论片段数
  temp<-unlist(lapply(subcon,length))
  
  #[4]生成每条评论标号，标号数量和片段数相同
  id<-rep(commentdata$id,temp)
  
  #[5]把片段结果对象变成向量
  term<-unlist(subcon)
  
  #[6]打上分句 id
  groupid<-function(x){
    subid<-seq(1:x)
    return(subid)
  }
  
  #[7]生成片段标识
  subid<-paste(id,"-",unlist(lapply(temp,groupid)),seq="")
  subcondata<-data.frame(id=id,term=term,subid=subid)
  subcondata$term<-as.character(subcondata$term)
  subcondata$subid<-as.character(subcondata$subid)
  return(subcondata)
}
# 3-1 函数功能：分词
# 3-2 参数说明：
# useridc：用户自定义词典文件名
# stopword：停用词词典文件名
# subdf：数据框，需要分词的数据，每一行为一条文本片段
segword_trn<-function(userdic,stopword,subdf){
  #[1]载入分词空间
  wk = worker(user=userdic,stop_word=stopword,'tag',bylines=TRUE,lines=5000000)
  #本考虑将情感词作为用户词典加入分词引擎进行分词，但效果不好
  #new_user_word(wk,posneg_tot$term)
  
  #[2]分词函数
  tt<-wk[subdf$term]
  
  #[3]给每个分词标号
  temp_fc<-unlist(lapply(tt,length))
  id_fc<-rep(subdf[,"subid"],temp_fc)
  term_fc<-unlist(tt)
  segterm_fc<-data.frame(id=id_fc,term=term_fc,cx=names(unlist(tt)))
  segterm_fc$id<-as.character(segterm_fc$id) #转为字符串格式;
  segterm_fc$term<-as.character(segterm_fc$term) #转为字符串格式;
  segterm_fc$cx<-as.character(segterm_fc$cx) #转为字符串格式;
  segterm_fc$id_tot<-as.numeric(unlist(lapply(strsplit(segterm_fc$id,'-'),function(x) x[1])))
  return(segterm_fc)
}
#-----------------------------[4]情感倾向强弱分析:--------------------------------# #-------------------[1]基础情感词典构建、程度副词、否定词、用户自定义词典：
userdic<-'userdic.txt' #用户字典
stopword<-'lunwen_stop.txt'#停用词词
postivedic<-"lunwen_pos_word.txt" #正向情感词
negtivedic<-"lunwen_neg_word.txt" #负向情感词
advworddic<-"lunwen_程度词.xlsx" #程度副词字典
denyworddic<-"否定词.csv" #否定词字典
#-------------------[2]导入情感词并附上权重:
#postive=readLines(postivedic,encoding='UTF-8')
#nagtive=readLines(negtivedic,encoding='UTF-8')
#pos<-data.frame(term=postive,weight=rep(1,length(postive)))
#neg<-data.frame(term=nagtive,weight=rep(-1,length(nagtive)))


#posneg_tot<-rbind(pos,neg) #正负面情感词典合并; #-------------------[3]导入程度副词、否定词:
#----导出情感词和权重
#write.csv(posneg_tot,file = "情感词权重修改.csv")
pone <- read.csv("情感词权重修改.csv",header = TRUE,sep = ",")
posneg_tot <- data.frame(pone)
#-------------------------
advword<-read.xlsx(advworddic,sheetName = "Sheet1",
                   encoding = "UTF-8")
denyword<-read.csv(denyworddic,header=TRUE,stringsAsFactors=FALSE)
#-------------------[4]文本导入与清洗、分词:
content <- read.csv("泰山.csv",header = TRUE,sep = ",")
commenttext<-content$comment #选取 comment 列,赋予 commenttext 对象;
commenttext <- unique(commenttext) #对评论进行去重操作; #数据清理
commenttext<-dataclean(commenttext)
#转数据集备用：
comments <- as.data.frame(commenttext)
comments$id <- seq(1,3186,1)
#分句并转换成数据框并且表上 subid
subcondata<-splitsentence(commenttext)
#分词
segworddata<-segword_trn(userdic,stopword,subcondata)
#-------------------[5]关联情感词、程度副词和否定词:原书代码有误，这里选定匹配类型为“first”，即匹配第一个;
tstterm<-join(segworddata,posneg_tot,by = 'term',match = 'first')
tstterm<-join(tstterm,advword,by = 'term',match = 'first')
names(tstterm)[length(names(tstterm))]<-"adv_score" 
tstterm<-join(tstterm,denyword,by='term',match = "first")
names(tstterm)[length(names(tstterm))]<-"deny_score"
tstterm$adv_score[!complete.cases(tstterm$adv_score)]<--999
tstterm$deny_score[!complete.cases(tstterm$deny_score)]<--999
tstterm$id_tot<-as.numeric(gsub(" ","",tstterm$id_tot))
#-------------------[6]语句情感得分计算：
#------注意：
#           仅计算有情感词的片段的情感值，因为副词及否定词用于修饰情感词[根据研究内容定立]

# 4-1函数功能：对片段进行情感性打分
# 4-2参数说明：
#             idname：片段标号
#             fliename：带有否定词、副词和正负情感词的文本
#####################################################################
word_segment <- function(idname,filename){
  
  #[1]打行号
  #抽取片段
  filepart = subset(filename,id==idname)
  #[2]对片段中每个分词打上id:也就是分词结果中，每个词的位置
  wordfile = data.frame(
    filepart
    ,idx=1:nrow(filepart) )
  
  #[3]找出正负情感词在片段中的位置
  wordindex = wordfile$idx[!is.na(wordfile$weight)]
  
  #[4]上下限表
  citeration = data.frame(
    wordindex
    ,left  = wordindex-3
    ,right = wordindex+3
    ,leftidx = c(wordindex[1]-4,head(wordindex,-1))
    ,rightidx = c(tail(wordindex,-1),wordindex[length(wordindex)]+4)
    ,left_up=c(tail(wordindex-3,-1),wordindex[length(wordindex-3)]+3)
  )
  
  #[5]窗口期判定函数:wordindex为情感词所在位置
  computevalue <- function(i,citeration,wordindex,filepart){
    left = ifelse(citeration$left[wordindex==i]<0,0,citeration$left[wordindex==i])#0
    right= citeration$right[wordindex==i]#6
    leftidx= ifelse(citeration$leftidx[wordindex==i]<0,0,citeration$leftidx[wordindex==i])#0
    rightidx= citeration$rightidx[wordindex==i]#7
    left_up=citeration$left_up[wordindex==i]#6
    wdidx=citeration$wordindex[wordindex==i]#3
    
    result = cbind(
      ifelse(right<rightidx
             ,max((filepart$adv_score[max(left,leftidx+1):max(wdidx,left_up-1)]),na.rm=T)
             ,max(filepart$adv_score[max(left,leftidx+1):wdidx],na.rm=T)
      )
      ,ifelse(right<rightidx
              ,max(filepart$deny_score[max(left,leftidx+1):max(wdidx,left_up-1)],na.rm=T)
              ,max(filepart$deny_score[max(left,leftidx+1):wdidx],na.rm=T))
    )
    return(result)
  }
  #[6]计算值:
  result = data.frame(t(sapply(wordindex,computevalue,citeration,wordindex,filepart)))
  names(result) = c('adv','deny')
  
  final_result = data.frame(
    id=idname 
    ,posneg=filepart$weight[wordindex]
    ,result
  )
  
  return(final_result)
}
# 5-1函数功能：综合计算每条评论总得分
# 5-2参数说明：
#             texttb:评论文本（打上情感词、否定词和副词标签后的）

#情感词综合打分

valuefun<-function(texttb){
  #[1]抽取正负情感词所在的片段:片段id(即仅提取含有情感词的片段)
  idnotnull<-data.frame(id=unique(texttb$id[complete.cases(texttb$weight)]))
  idnotnull$id<-as.character(idnotnull$id)
  
  #[2]查找有情感值的片段所对应的整个语句:
  tstterm_nnid<-join(texttb,idnotnull,type="inner")
  
  #[3]获取含有情感值片段所在id:
  #此时评论是包含情感值的评论;
  #注意该片段里面必须含有情感词否则不算;
  word_index<-unique(tstterm_nnid$id)
  
  system.time(score_combine<-lapply(word_index,word_segment,tstterm_nnid))
  score_combine_tb<-do.call("rbind", score_combine) 
  score_combine_tb$id<-as.character(score_combine_tb$id)
  score_combine_tb$adv[score_combine_tb$adv==-999]<-1
  score_combine_tb$deny[score_combine_tb$deny==-999]<-1
  #[4]片段得分计算:
  score_combine_tb$value<-score_combine_tb$posneg*score_combine_tb$adv*score_combine_tb$deny
  
  #[5]片段得分汇总聚合:
  subconvalue<-aggregate(score_combine_tb$value,by=list(score_combine_tb$id),sum)
  subconvalue$idtot<-as.numeric(unlist(lapply(strsplit(subconvalue$Group.1,'-'),function(x) x[1])))
  
  #[6]语句得分汇总聚合
  commentvalue<-aggregate(subconvalue$x,by=list(subconvalue$idtot),sum)
  names(commentvalue)[1]<-'id'
  commentvalue$x<-round(commentvalue$x,2)
  return(commentvalue)
}
system.time(valuetb<-valuefun(tstterm))
head(valuetb)#最终结果展示；

#-------------------[6]使用内连接,索引回原评论：

all_result <- inner_join(comments,valuetb,by = "id")
names(all_result)<-c("content","uid","score")
pos_comments <- all_result[all_result$score>0,]
neu_comments <- all_result[all_result$score==0,]
neg_comments <- all_result[all_result$score<0,]

write.csv(all_result,"1111.csv")
summary(pos_comments$score)
summary(neu_comments$score)
summary(neg_comments$score)
#将正负面评论数据结果导出备用：
posword <- tstterm[tstterm$id_tot %in% pos_comments$uid,]
negword <- tstterm[tstterm$id_tot %in% neg_comments$uid,]

pos.frep <- table(posword$term)
pos.frep <- sort(pos.frep,decreasing = TRUE)
pos.frep <- as.data.frame(pos.frep)
pp1 <- pos.frep[nchar(as.character(pos.frep$Var1))>=2,]
head(pp1,50)

neg.frep <- table(negword$term)
neg.frep <- sort(neg.frep,decreasing = TRUE)
neg.frep <- as.data.frame(neg.frep)
nn1 <- neg.frep[nchar(as.character(neg.frep$Var1))>=2,]
head(nn1,50)

library("gsubfn")
library("proto")
library("RSQLite")
library("sqldf")

#正负面评论结果导出：
head(posword)
posword$content_type <- "pos"
#计算每个语句的词数:
p_word_n <- as.data.frame(sqldf('select count(id) from posword group by id_tot'))
posword$index_word <- unlist(sapply(p_word_n$`count(id)`,seq))
posword$index_word <- posword$index_word - 1
posword <- posword[,c(8,4,3,1,9)]
names(posword) <- c("content_type","index_content","nature","word","index_word")

head(negword)
negword$content_type <- "neg"
#计算每个语句的词数:
n_word_n <- as.data.frame(sqldf('select count(id) from negword group by id_tot'))
negword$index_word <- unlist(sapply(n_word_n$`count(id)`,seq))
negword$index_word <- negword$index_word - 1
negword <- negword[,c(8,4,3,1,9)]
names(negword) <- c("content_type","index_content","nature","word","index_word")





#数据导出：
write.csv(posword,"E:\\论文输出\\沂蒙山posdata.csv",row.names = F)
write.csv(negword,"E:\\论文输出\\沂蒙山negdata.csv",row.names = F)

#情感分类结果及得分导出
valuetb1 <- valuetb[valuetb$x != 0,]
valuetb1$a_type[valuetb1$x >0] <- "pos" 
valuetb1$a_type[valuetb1$x <0] <- "neg" 

valuetb1$content_type <- ""
valuetb1$content_type[valuetb1$id <= 1000] <- "pos"
valuetb1$content_type[valuetb1$id > 1000] <- "neg"


valuetb2 <- valuetb1[,c(1,3,4)]
names(valuetb2) <- c("index_content","a_type","content_type")
write.table(valuetb2,"E:\\论文输出\\沂蒙山outputfile.csv",row.names=FALSE,col.names=TRUE,sep=",")



