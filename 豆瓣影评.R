mood_data <- as.matrix(symptom_data$data[, 1:12]) # 子集变量
mood_labels <- symptom_data$colnames[1:12] # 子集变量标签
colnames(mood_data) <- mood_labels
time_data <- symptom_data$data_time

require(quantmod)
att <- getSymbols("T", from = "1985-01-01", to = "2015-12-31", auto.assign = FALSE)

plot(density(attr), main = "Distribution of AT&T Returns")

rug(jitter(attr))


library(fpp2)
gass<- read.csv("gass.csv",header = TRUE,sep = ",")
fit <- auto.arima(gass[,"汽油零售价格"], xreg=gass[,"柴油零售价格"])
fit <- auto.arima(gass[,"汽油零售价格"])
checkresiduals(fit)
