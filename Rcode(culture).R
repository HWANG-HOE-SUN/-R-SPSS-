install.packages("ggplot2")
library("ggplot2")
library(disk.frame)
library(zoo)

setwd('E:/취업준비/문화빅/Data')
data <- read.csv('card_grouped_ta_1.csv')

as.Date(('201603'),format='%Y%m')

d <- c("200701", "200702")
lastday <- as.yearmon(d, "%Y%m")
lastday

# 날짜는 임의로 초일로 설정(일은 별 의미 없음)
data$date = as.Date(as.yearmon(as.character(data$ta_ym), "%Y%m")) 
head(data)
plot(data)

myvector <- c(data$log_vlm,data$usec,data$atdtr_encod,data$cln_age_r)
myts <- ts(myvector, start=c(2020, 7), end=c(2021, 5), frequency=11)
myts

plot(myts)
myvector

mat = cbind(data$log_vlm,data$usec,data$atdtr_encod,data$cln_age_r)
mat

y=ts(mat,start=c(2020,7),freq=11)
y
plot(y,plot.type='single',lty=1:4)
