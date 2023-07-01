pacman::p_load(XML,rvest,jiebaR,dplyr,stringr)
statistic <- data.frame()
for (i in seq(50,10000,by=50)){
  web <- read_html("https://tieba.baidu.com/f?kw=%E6%96%BD%E5%B7%A5%E5%91%98&ie=utf-8&pn=", encoding = "UTF-8")
  web <- read_html(str_c("https://tieba.baidu.com/f?kw=%E6%96%BD%E5%B7%A5%E5%91%98&ie=utf-8&pn=", i), encoding = "UTF-8")
  title <- web%>%html_nodes("a.j_th_tit")%>%html_text()
  time1 <- web%>%html_nodes(".j_reply_data")%>%html_text()
  time2<- str_replace(time1, "\r\n            ","")
  time <- str_replace(time2,"        ","")
  answer <- web%>%html_nodes(".j_threadlist_li_left .center_text")%>%html_text()
  statistici <- data.frame(title,answer,time)
  statistic <- rbind(statistic,statistici)
}
write.csv(statistic,file = "data.csv")
