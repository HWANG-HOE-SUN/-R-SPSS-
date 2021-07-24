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


### 인구 data
pop = read.csv('E:/C드라이브/새다운로드/2020-2021후반_연령별인구.csv')
pop

colnames(pop)
pop[,2]

View(pop)
pop[2:18,2] # 20년 1월 총 인구수
pop[2:18,38] # 20년 2월 총 인구수
pop[2:18,74] # 20년 3월 총 인구수

pop[2:18,2+36*6] # 20년 7월 총 인구수
pop[2:18,2+36*11] # 20년 12월 총 인구수

pop[2:18,2+36*12] # 21년 1월  2+36*12 = 434
pop[2:18,434+39] # 21년 2월 
pop[2:18,434+39*2] # 21년 3월
pop[2:18,434+39*3] # 21년 4월
pop[2:18,434+39*4] # 21년 5월

# 아래 코드의 설명용 예시
num_list <- lapply(pop[,2],function(x) as.numeric(gsub(",","",x)))
num_list <- unlist(num_list)
typeof(num_list)
num_list # 1월 지역별 총 인구수

### 반복문으로 각 월별 총 인구수 뽑아 list 만들기
# 20년 7월 ~ 21년 1월
for(i in 7:13){
  if(i<13){ # 20년 7월 ~ 20년 12월
    assign(paste0("test_",i),
           lapply(pop[2:18,2+36*(i-1)],
                                    function(x) as.numeric(gsub(",","",x))))
  } #21년 1월의 경우 이름을 다르게 저장
  else{
    assign(paste0("test_",i-12),
           lapply(pop[2:18,2+36*(i-1)],
                  function(x) as.numeric(gsub(",","",x))))
  }
}
test_list <- mget(ls(pattern="test_")) # 결과 리스트

# 21년 2월 ~ 21년 5월
for(i in 2:5){
  assign(paste0("test2_",i),
         lapply(pop[2:18,434+39*(i-1)],
                function(x) as.numeric(gsub(",","",x))))
}
test_list2 <- mget(ls(pattern="test2_"))

test_list
test_list2
View(test_list2)

sunseo = c(1,10,11,12,c(7:9)) # index 순서 (test_list의)
### 반복문으로 지역별 각 월 mat생성
for(i in 7:13){ # 20년7월~ 21년 1월
  assign(paste0("pop_mon_",sunseo[i-6]), scale(unlist(test_list[i-6])))
}
for(i in 1:4){ # 21년2월~ 21년 5월
  assign(paste0("pop_mon_",i+1),scale(unlist(test_list2[i])))
}



# 각 값이 지역별 변수이므로 행<->열 전치해준다. 
# 20년 7월 ~ 21년 5월. matrix형태로 자료구조 변환
row1 <- t(matrix(pop_mon_7))
row2 <- t(matrix(pop_mon_8))
row3 <- t(matrix(pop_mon_9))
row4 <- t(matrix(pop_mon_10))
row5 <- t(matrix(pop_mon_11))
row6 <- t(matrix(pop_mon_12))
row7 <- t(matrix(pop_mon_1)) 
row8 <- t(matrix(pop_mon_2))
row9 <- t(matrix(pop_mon_3))
row10 <- t(matrix(pop_mon_4))
row11 <- t(matrix(pop_mon_5))


# 같은열=같은지역, 같은행 = 같은 시점
total_mat <- matrix(c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11),
       byrow=T,nrow=11,ncol=17)
total_mat

# 지역별 ts 생성 ( 인구 변화 추세 )
seoul_ts <- ts(total_mat[,1],start=c(2020,7),freq=11) #서울지역 ts데이터
busan_ts <- ts(total_mat[,2],start=c(2020,7),freq=11) #부산지역 ts데이터
daegu_ts <- ts(total_mat[,3],start=c(2020,7),freq=11) #대구지역 ts데이터
incheon_ts <- ts(total_mat[,4],start=c(2020,7),freq=11) #인천지역 ts데이터
gwangju_ts <- ts(total_mat[,5],start=c(2020,7),freq=11) #광주지역 ts데이터
daejeon_ts <- ts(total_mat[,6],start=c(2020,7),freq=11) #대전지역 ts데이터
ulsan_ts <- ts(total_mat[,7],start=c(2020,7),freq=11) #울신지역 ts데이터
sejong_ts <- ts(total_mat[,8],start=c(2020,7),freq=11) #세종지역 ts데이터
gyeongi_ts <- ts(total_mat[,9],start=c(2020,7),freq=11) #경기도지역 ts데이터
gangwon_ts <- ts(total_mat[,10],start=c(2020,7),freq=11) #강원도지역 ts데이터
chungcheongnorth_ts <- ts(total_mat[,11],start=c(2020,7),freq=11) #충청북도지역 ts데이터
chungcheongsouth_ts <- ts(total_mat[,12],start=c(2020,7),freq=11) #충청남도지역 ts데이터
jeonranorth_ts <- ts(total_mat[,13],start=c(2020,7),freq=11) #전라북도지역 ts데이터
jeonrasouth_ts <- ts(total_mat[,14],start=c(2020,7),freq=11) #전라남도지역 ts데이터
gyeongsangnorth_ts <- ts(total_mat[,15],start=c(2020,7),freq=11) #경상북도지역 ts데이터
gyeongsangsouth_ts <- ts(total_mat[,16],start=c(2020,7),freq=11) #경상남도지역 ts데이터
jeju_ts <- ts(total_mat[,17],start=c(2020,7),freq=11) #제주도지역 ts데이터


#지역별 ts plot
par(mfrow=c(3,6))
plot.ts(seoul_ts,main="Seoul")
plot.ts(busan_ts,main="Busan")
plot.ts(daegu_ts,main="Daegu")
plot.ts(incheon_ts,main="Incheon")
plot.ts(gwangju_ts,main="Gwangju")
plot.ts(daejeon_ts,main="Daejeon")
plot.ts(ulsan_ts,main="Ulsan")
plot.ts(sejong_ts,main="Sejong")
plot.ts(gyeongi_ts,main="Gyeongi")
plot.ts(gangwon_ts,main="Gangwon")
plot.ts(chungcheongnorth_ts,main="Chungnorth")
plot.ts(chungcheongsouth_ts,main="Chungsouth")
plot.ts(jeonranorth_ts,main="Jeonranorth")
plot.ts(jeonrasouth_ts,main="Jeonrasouth")
plot.ts(gyeongsangnorth_ts,main="GyeongsangNorth")
plot.ts(gyeongsangsouth_ts,main="GyeongsangSouth")
plot.ts(jeju_ts,main="Jeju")
