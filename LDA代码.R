#R语言代码LDA主题建模：
#R包载入：
library("tm")
library("NLP")
library("dplyr")
library("topicmodels")









head(posword)
head(negword)
adv <- read.csv("adv.csv",header = TRUE,sep = ",")
#---此处开始重复
po1 <- as.data.frame(posword[,4])
ne1 <- as.data.frame(negword[,4])
colnames(po1)[1] <- "word"
colnames(ne1)[1] <- "word"
po1 <- po1 %>% anti_join(adv, by = "word")
ne1 <- ne1 %>% anti_join(adv, by = "word")
#可导出备用:
write.table (po1, file ="三孔posLDA.txt", sep ="\n", row.names =FALSE, col.names =FALSE, quote =FALSE,fileEncoding = "UTF-8")
write.table (ne1, file ="三孔negLDA.txt", sep ="\n", row.names =FALSE, col.names =FALSE, quote =FALSE,fileEncoding = "UTF-8")

posLDA <- scan("三孔posLDA.txt",sep = '\n',what = character(),encoding = "UTF-8")
negLDA <- scan("三孔negLDA.txt",sep = '\n',what = character(),encoding = "UTF-8")
pos.corpus <- Corpus(VectorSource(posLDA),readerControl = list(language = "UTF-8"))
neg.corpus <- Corpus(VectorSource(negLDA),readerControl = list(language = "UTF-8"))
#文档--词条矩阵
posjz <- DocumentTermMatrix(pos.corpus,
                            control = list(   #control里面可以放各种调整参数
                              wordLengths=c(1, Inf), # 限制词长，将词长设置为大于等于2
                              bounds = list(global = c(5,Inf)), #bounds参数设置词频，最小词频为5
                              removeNumbers = TRUE,encoding="UTF-8"))  #移除数字
negjz <- DocumentTermMatrix(neg.corpus,
                            control = list(   #control里面可以放各种调整参数
                              wordLengths=c(1, Inf), # 限制词长，将词长设置为大于等于2
                              bounds = list(global = c(5,Inf)), #bounds参数设置词频，最小词频为5
                              removeNumbers = TRUE,encoding="UTF-8"))
#寻找最优主题数：
lda.k <- function(jz){
  #初始化平均余弦相似度：
  mean_similarity <- c()
  mean_similarity[1] = 1
  #循环生成主题并计算主题间的相似度：
  for(i in 2:10){
    control <- list(burnin = 500,iter = 1000,keep = 100,seed=1234)
    Gibbs <- LDA(jz,k = i,method = "Gibbs",control = control)
    term <- terms(Gibbs,50)  #提取主题词
    word <- as.vector(term)   #列出所有词
    freq <- table(word)
    unique_word <- names(freq)
    mat <- matrix(rep(0,i*length(unique_word)),nrow = i,ncol = length(unique_word))
    colnames(mat) <- unique_word
    #生成词频向量：
    for(k in 1:i){
      for(t in 1:50){
        mat[k,grep(term[t,k],unique_word)] <- mat[k,grep(term[t,k],unique_word)]+1
        
      }
    }
    p <- combn(c(1:i),2)
    l <- ncol(p)
    top_similarity <- c()
    for(j in 1:l){
      #计算余弦相似度：
      x <- mat[p[,j][1],]
      y <- mat[p[,j][2],]
      top_similarity[j] <- sum(x*y)/sqrt(sum(x^2)*sum(y^2))
    }
    mean_similarity[i] <- sum(top_similarity)/l
    message("top_num",i)
  }
  return(mean_similarity)
}
#计算余弦相似度：

#rowSums是直接对矩阵的行进行求和，然后找到>0的那些行
posjz<-posjz[ rowSums(as.matrix(posjz)) > 0 , ]
negjz<-negjz[ rowSums(as.matrix(negjz)) > 0 , ]

unique_lines = unique(posjz$i)
posjz1 = posjz[unique_lines,]

unique_lines1 = unique(negjz$i)
negjz1 = negjz[unique_lines1,]


pos_k <-lda.k(posjz1)
neg_k <-lda.k(negjz1)
dev.new()
par(mfrow = c(2,1))
plot(pos_k,type = "l",main = "正面评论LDA主题数寻优",xlab = "主题数",ylab = "平均余弦距离")
plot(neg_k,type = "l",main = "负面评论LDA主题数寻优",xlab = "主题数",ylab = "平均余弦距离")


#LDA主题分析;
control <- list(burnin = 500,iter = 1000,keep = 100,seed=1234)
pos.gibbs <- LDA(posjz1,k = 4,method = "Gibbs",control = control )
pos.termsl <- terms(pos.gibbs,10)
pos.termsl 
neg.gibbs <- LDA(negjz1,k = 4,method = "Gibbs",control = control )
pos.termsl <- terms(pos.gibbs,10)
neg.termsl <- terms(neg.gibbs,10)
write.csv(pos.termsl, file ="E:\\论文输出\\三孔term1.csv",quote =FALSE)
write.csv(neg.termsl, file ="E:\\论文输出\\三孔term2.csv",quote =FALSE)

write.csv(table(posLDA),file = "词频统计.csv")

library(tidyverse)
library(tidytext)
chapters_lda_td <- tidy(pos.gibbs)
top_terms <- chapters_lda_td %>%group_by(topic) %>%top_n(10, beta) %>%
ungroup() %>%arrange(topic, -beta)
library(dplyr)
top_terms %>%
  group_by(term) %>%
  summarise(beta = sum(beta))
view(top_terms)
myplott <- top_terms %>%
  mutate(term = reorder(term, beta)) %>%
  ggplot( aes(term,beta, fill = factor(topic))) +
  geom_bar(alpha = 2, stat = "identity", show.legend = FALSE) +
  facet_wrap(~ topic, scales = "free") +
  coord_flip()+labs(x="主题词",y="概率")
print(myplott)
jpeg(file="myplott.jpeg")
dev.off()