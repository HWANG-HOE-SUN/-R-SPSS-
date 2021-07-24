install.packages('readxl')
install.packages('disk.frame')
install.packages("ggplot2")
install.packages('zoo')
install.packages("TSstudio")
library(ggplot2)
library(readxl)
library(disk.frame)
library(zoo) # for making ts
library(TSstudio) # plotting ts
library(TTR)
library(forecast)
setwd('E:/C드라이브/새다운로드')
getwd()

data=read_excel('card_grouped_ta_1(re).xlsx')
data

data$date = as.Date(as.yearmon(as.character(data$ta_ym), "%Y%m")) 
head(data)
plot(data)

names <- colnames(data)[c(4,5,7,8,9)] # ts 칼럼 이름들들
names
mat = cbind(data$log_vlm,data$usec,data$atdtr_encod,data$cln_age_r,data$card_counts)
scaled_mat = scale(mat)

y<-ts(scaled_mat,start=c(2020,7),freq=11)
colnames(y) <- names # 변수명 대체
colnames(y)

ts_plot(y)

### 시계열 분석

#분해

y_uni_1 <- ts(scaled_mat[,1],start=c(2020,7),freq=11)
ts_plot(y_uni_1,title="atdtr_encod") # 
y_uni_2 <- ts(scaled_mat[,2],start=c(2020,7),freq=11)
ts_plot(y_uni_2,title="cln_age_r")
y_uni_3 <- ts(scaled_mat[,3],start=c(2020,7),freq=11)
ts_plot(y_uni_3,title="log_vlm")

names

plot(stl(y_uni_1,"periodic"))

### 인구 data
pop = read.csv('E:/C드라이브/새다운로드/2020-2021후반_연령별인구.csv')
pop

colnames(pop)
pop[,2]

View(pop)
pop[,2] # 1월 총 인구수
pop[,38] # 2월 총 인구수
pop[,74] # 3월 총 인구수

# 아래 코드의 설명용 예시
num_list <- lapply(pop[,2],function(x) as.numeric(gsub(",","",x)))
num_list <- unlist(num_list)
typeof(num_list)
num_list # 1월 지역별 총 인구수

### 반복문으로 각 월별 총 인구수 뽑아 list 만들기
for(i in 1:12){
  pop[,2]
  assign(paste0("test_",i),
         lapply(pop[,2+36*(i-1)],
                function(x) as.numeric(gsub(",","",x))))
}
test_list <- mget(ls(pattern="test_"))

View(test_list)
test_list


