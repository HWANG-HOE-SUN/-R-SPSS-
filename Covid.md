# Rproject

# 데분응 개인프로젝트 - 서울특별시 소독업소 정보
# 목적 - 의사결정 지원(업소 소독능력)
# 기본 세팅
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lattice")
install.packages("FSelector")
library(FSelector)
library(lattice)
library(dplyr)
library(ggplot2)
library(caret)
getwd()
data <- read.csv("서울특별시 소독업소 정보원본.csv",header=T)
data_df <- tbl_df(data)

#데이터 기본요소 파악
View(data_df)
dim(data_df) #행,렬 갯수 파악
str(data) #데이터종류, 타입파악
head(data_df)
names(data_df)

#불필요데이터 사전제거
Data_df <- subset(data_df, select=-c(인허가번호,영업상태명,폐업일자,휴업시작일자,휴업종료일자,재개업일자,소재지면적,소재지우편번호,진공청소기수,전화번호))
#인허가번호대신 번호를 데이터Key로 사용할거고, 영업상태명 대신
#더 구체적인 상세영업상태명이 있으며 모든 업체가 영업중으로 동일한값을 가짐
# 모두가 Null이거나 대부분 Null값인 폐업일자~소재지우편번호도 제거
# 분석목적을 고려하여 필요없는 것들은 다 제거했다(전화번호 등)
View(Data_df)

plot.new()
#시각화 및 의미 분석

# 1. 소독능력 관련변수 분석
#빈도분석(boxplot,hist)
#소독능력 후보군(추후 점점 줄일것임)
par(mfrow=c(1,3)) 
boxplot(Data_df$방독면수,main='방독면수',cex=1.5,cex.main=2) # 보조품 
boxplot(Data_df$보호용의복수,main='보호용의복수',cex=1.5,cex.main=2) #보조품
boxplot(Data_df$보호안경수,main='보호안경수',cex=1.5,cex.main=2) #보조품
which.max(Data_df$수동식분무기수) #방독~보호안경 모두 746 지칭.
Bigpoint <- Data_df[746,] #이상값 따로 보관
Data_df <- Data_df[-746,] #이상값 다시금 제거

plot.new()
par(mfrow=c(2,4))
boxplot(Data_df$방독면수,main='방독면수',cex=1.5,cex.main=2) # 보조품 
boxplot(Data_df$보호용의복수,main='보호용의복수',cex=1.5,cex.main=2) #보조품
boxplot(Data_df$보호안경수,main='보호안경수',cex=1.5,cex.main=2) #보조품
boxplot(Data_df$동력분무기수,main='동력분무기수',cex=1.5,cex.main=2) #핵심1
boxplot(Data_df$수동식분무기수,main='수동식분무기수',cex=1.5,cex.main=2) #핵심2
boxplot(Data_df$초미립자살포기수,main='초미립자살포기수',cex=1.5,cex.main=2) #핵심3
boxplot(Data_df$휴대용소독기수,main='휴대용소독기수',cex=1.5,cex.main=2) #핵심4

fivenum(Data_df$동력분무기수)
fivenum(Data_df$방독면수)
fivenum(Data_df$보호안경수)
fivenum(Data_df$보호용의복수)
fivenum(Data_df$수동식분무기수)
fivenum(Data_df$초미립자살포기수)
fivenum(Data_df$휴대용소독기수)

#2. 주소(위치) 관련변수 분석.
newData <- transform(Data_df,시구정보=substr(Data_df$소재지전체주소,1,10))

unique(newData$시구정보) # ""로 되있는 Null값 처리해야함. 
#상계동->노원구, 상봉동->중랑구 중구 광,봉,오,신,충,필,중,무 -> 중구 로 바꿔야함.
newData$시구정보 <- as.character(newData$시구정보) #위 작업을 위해 타입을 변경한다.

#주소 Null값 처리(1Step)
w <- which(newData$시구정보=="") #DF에서 주소 Null값인걸 인식 못하니 수동으로 파악해준다.
w #소재지전체주소가 빈건, 도로명주소를 사용해서 저장한다.
도로명시구정보 <- substr(Data_df$도로명전체주소,1,10)
(도로명시구정보)
도로명시구정보[w]=="" #도로명시구정보를 넣으면 빈값을 다 대체할 수 있다.
newData$시구정보[w] <- 도로명시구정보[w]

unique(newData$시구정보) #중구 퇴, 상계동 ~~등 이상한 주소값 교정한다.
#잘린 주소값 처리(2Step)
grep('중구',newData$시구정보) #중구 남,퇴,청,다,동...등 // 상계동 37번지등.
(hi <- newData[grep('중구',newData$시구정보),]$시구정보)
num <- length(grep('중구',newData$시구정보))
i<-1
while(i<=num){
  newData$시구정보[grep('중구',newData$시구정보)] <- "서울특별시 중구"
  i=i+1
}
grep('까치산로',newData$시구정보)#까치산로,가마산로 구로변경
grep('가마산로',newData$시구정보)
grep('상봉동',newData$시구정보)
grep('상계동',newData$시구정보)
newData[828,]$시구정보 <- "서울특별시 강서구" 
newData[316,]$시구정보 <- "서울특별시 구로구"
newData[217,]$시구정보 <- "서울특별시 중랑구"
newData[c(448,678),]$시구정보 <- "서울특별시 노원구"
newData$시구정보 <- trimws(x = newData$시구정보, which = "right") #후행공백제거
unique(newData$시구정보)# 주소값 데이터클렌징(전처리) 완료
View(newData$시구정보)

#0분산 제거
nearZeroVar(newData, saveMetrics=TRUE)
#nzv=T인건 방독면수,보호안경수,보호용의복수, 초미립자살포기수,휴대용소독기수, 상세영업상태명이다.
#소독과 관계된 소독기,분무기수는 sum하여 소독력으로 남겨두고
#nzv=F인 동력분무기수,수동식분무기수는 남기되 나머지 nzv=T인 후보군들은 지우겠다.

newData$소독력 <- newData$동력분무기수+newData$수동식분무기수+newData$초미립자살포기수+newData$휴대용소독기수

newData_df <- subset(newData, select=-c(초미립자살포기수,휴대용소독기수,방독면수,보호용의복수,보호안경수,상세영업상태명))
nearZeroVar(newData_df,saveMetrics = T)

gg1 <- ggplot(newData_df, aes(x=위치정보.X.,y=위치정보.Y.))
gg1+geom_point(aes(size=소독력,color=시구정보))


Final_df <- newData_df
dim(Final_df)
#이상값 고려해주기(소독력이 매우 큰 점이므로 현실문제에서 무시할 수 없음)
#이상값 변수 튜닝
newBig <- subset(Bigpoint,select=-c(초미립자살포기수,휴대용소독기수,방독면수,보호용의복수,보호안경수,상세영업상태명))
newBig[1,12] <- trimws(x = substr(Bigpoint$소재지전체주소,1,10), which = "right")
newBig[1,13] <- Bigpoint$동력분무기수+Bigpoint$수동식분무기수+Bigpoint$초미립자살포기수+Bigpoint$휴대용소독기수 #소독력

Final_df[1039,] <- newBig

#결론 도출

#이상값 고려 ggplot
gg2 <- ggplot(Final_df, aes(x=위치정보.X.,y=위치정보.Y.))
gg3 <- gg2+geom_point(aes(size=소독력,color=시구정보))
gg4 <- gg3+labs(title="서울특별시 소독력 분포지도")+theme(plot.title=element_text(size=15))
gg4 #구별 분포 정보

(force_mean <- aggregate(소독력~시구정보,Final_df,mean)) #이상값이 있는 성동구가 압도적이다.
(force_count <- Final_df %>% group_by(시구정보) %>% summarise(업소수=n()))
Final_force <- merge(force_count,force_mean)
Final_force$최종소독력 <- force_mean$소독력*force_count$업소수
Final_force$시구정보 <- substr(Final_force$시구정보,7,10)
ggplot(Final_force,aes(시구정보, 최종소독력,fill=시구정보))+geom_bar(stat='identity',width=0.6)+labs(title="서울시 지역별 최종소독력")+theme(axis.text.x=element_text(size=6))+geom_text(aes(label=최종소독력), vjust=-0.2)
aggregate(소독력~시구정보,Final_df,mean)
mean(Final_force$최종소독력)
Final_force[Final_force$시구정보=="성동구",]$최종소독력
