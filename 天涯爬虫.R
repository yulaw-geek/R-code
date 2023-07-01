pacman::p_load(XML,rvest,jiebaR,dplyr,stringr)
statistic <- data.frame()
for (i in seq(370690,470690,by=100)){
  #发现url规律，利用字符串函数进行url拼接并规定编码：
  web <- read_html(str_c("http://bbs.tianya.cn/list.jsp?item=141&order=1&nextid=", i), encoding = "UTF-8")
#----获取要爬取的新闻信息
news <- web%>%html_nodes(xpath ="//tr/td[1]/a")
#----获取新闻标题
title1 <- news%>%html_text()
library(stringr)
title2<- str_replace(title1, "\r\n\t\t\t\t\t\t\t","")
title<- str_replace(title2, "\r\n\t\t\t\t\t\t","")
#-----点击数 回复数 时间
number_points <- web%>%html_nodes(xpath = "//tr/td[3]")%>%html_text()
number_answers <- web%>%html_nodes(xpath = "//tr/td[4]")%>%html_text()
time1 <- web%>%html_nodes(xpath = "//tr/td[5]")%>%html_attrs()
time2 <- unlist(time1)
time<- str_replace(time2, "title","")

#-----数据整合-----
statistici <- data.frame(title,number_points,number_answers,time)
statistic <- rbind(statistic,statistici)
}
write.csv(statistic,file = "天涯进出口贸易3.csv")
