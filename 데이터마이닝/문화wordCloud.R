install.packages("disk.frame") # for Data처리  
install.packages("fmsb") # for RaderChart
install.packages('ggmap') # for Google Map
install.packages('rJava') # for KONLP
install.packages('readxl') # excel import
library(rJava) 
library(ggmap)
library(readxl)
library(ggplot2) # for 시각화
library(dplyr)
library(disk.frame)
library(fmsb) 
library(MASS) # for parallel coordinate  

# writeLines('PATH="${RTOOLS40_HOME}\\usr\\bin;${PATH}"', con = "~/.Renviron")
# Sys.which("make")

## konlp 관련 패키지
install.packages("multilinguer")
library(multilinguer)
multilinguer::install_jdk() 
install.packages(c('stringr', 'hash', 'tau', 'Sejong', 'RSQLite', 'devtools'), type = "binary")
install.packages("remotes")
remotes::install_github('haven-jeon/KoNLP', upgrade = "never", INSTALL_opts=c("--no-multiarch"))
install.packages("wordcloud")
library(KoNLP)
library(wordcloud)

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_201')

dir = 'E://대학생활//4-1학기//데이터과학//adult.xlsx'
dataset <- read_excel(dir)
#View(dataset)
head(dataset)
str(dataset)
summary(dataset)

# Age 시각화
par(mfrow=c(1,2))
a_age <- dataset$Age
hist(a_age)  # histogram
plot(density(a_age)) # density plot

a_cap_gain <- dataset$'Capital-Gain'
par(mfrow=c(1,2))
boxplot(main='Age',a_age)
boxplot(main='Capital_gain',a_cap_gain)

a_edunum <- dataset$EducationNum
a_HPW <- dataset$`Hour-per-Week` 

a_race <- dataset$Race
a_sex <- dataset$Gender
par(mfrow=c(1,2))
pie(table(a_race),main='race')
pie(table(a_sex),main='sex')

x1 <- a_race[dataset$Target=="<=50K"]
x1_freq <- table(a_race[dataset$Target=="<=50K"])
x1_prop <- round(prop.table(x1_freq),3) #상대도수(비율)

x2 <- a_race[dataset$Target==">50K"]
x2_freq <- table(a_race[dataset$Target==">50K"])
x2_prop <- round(prop.table(x2_freq),3) #상대도수(비율)

y1 <- a_sex[dataset$Target=="<=50K"]
y1_freq <- table(a_race[dataset$Target=="<=50K"])
y1_prop <- round(prop.table(y1_freq),3)

y2 <- a_sex[dataset$Target==">50K"]
y2_freq <- table(a_race[dataset$Target==">50K"])
y2_prop <- round(prop.table(y2_freq),3)

par(mfrow=c(2,2))
pie(table(x1),main='<=50k, race',labels=paste(x1_prop,'%'))
pie(table(x2),main='>50k, race',labels=paste(x2_prop,'%'))
pie(table(y1),main='<=50k, sex',labels=paste(y1_prop,'%'))
pie(table(y2),main='>50k, sex',labels=paste(y2_prop,'%'))

# age, h-p-w, c-g, e-n

numeric_dt <- cbind(a_age,a_cap_gain,a_edunum,a_HPW)

pairs(numeric_dt) # scatter plot matrix

par(mfrow=c(1,1)) # 다시 1x1로 plot설정 초기화
set.seed(1234)
dt_sample <- numeric_dt[sample(1:nrow(numeric_dt), 3),]
dt_sample <- data.frame(scale(dt_sample)) # 정규화

dt_sample
radarchart(dt_sample) # radar chart

dt_sample2 <- data.frame(numeric_dt[1:100,])
parcoord(dt_sample2,col = colnames(dt_sample2))
dt_sample2$target <- dataset$Target[1:100] 

install.packages('GGally')
library(GGally) #GGally 패키지 불러오기
ggparcoord(dt_sample2, columns = 1:4, groupColumn = 5, scale='std')

dataset$ED_Num_New <- ifelse(dataset$EducationNum<5, 1 , ifelse(dataset$EducationNum<9,2,3))
# Education-Num이 5보다 작으면 ED-Num-New는 1, 9보다 작으면 2, 이외는 3이다.

# target + race 모자이크
target.race.tbl <- table(dataset$Target,a_race)
mosaicplot(target.race.tbl, color=TRUE)

# target + edu 모자이크
target.edu.tbl <- table(dataset$Target,dataset$Education)
mosaicplot(target.edu.tbl, color=TRUE)
target.edunum.tbl <- table(dataset$Target,dataset$EducationNum)
mosaicplot(target.edunum.tbl, color=TRUE)

target.newedu.tbl <- table(dataset$Target,dataset$ED_Num_New)
mosaicplot(target.newedu.tbl, color=TRUE)


# Google Map API Plot
register_google(key='AIzaSyDJkQvPrytRqSK8dlmS4qDiJz0FrFPY4LA')
locs <- c('서초중학교','상문고등학교','신중초등학교','서초고등학교','양재고등학교','홍익대학교')
addr <- c('서울특별시 서초구 서초3동 반포대로13길 48',
'서울특별시 서초구 방배3동 명달로 45',
'서울특별시 서초구 서초동 남부순환로317길 15', 
'서울특별시 서초구 서초동 반포대로27길 29', 
'서울특별시 서초구 서초2동 남부순환로346길 9'
,'서울특별시 마포구 상수동 와우산로 94') 

gcode <- geocode(enc2utf8(addr))
gcode

Gmap_df <- data.frame(name=locs, lon=gcode$lon,lat=gcode$lat)
Gmap_df

MapCenter <- c(mean(Gmap_df$lon),mean(Gmap_df$lat))
MapCenter
map <- get_googlemap(center=MapCenter,maptype='roadmap',
                     zoom=12, size=c(800,800),marker=gcode)
ggmap(map)


# Word Cloud

install.packages("tm")
library(RColorBrewer) #글자색 표현
library(tm) # 불용어 제거
pal <- brewer.pal(8,"Dark2")

useSejongDic()

myText <- readLines(file.choose())
head(myText)

termNoun <- sapply(myText, extractNoun, USE.NAMES = F)
head(termNoun,10)
termNoun

# 행 단위 명사 통합
noun2 <- unlist(termNoun)
termCount <- table(noun2)
head(termCount)

termCount

# 단어들 내림차순 정렬
head(sort(termCount,decreasing = T),10)
myPal <- brewer.pal(8,'Dark2')
wordcloud(names(termCount),freq=termCount,scale=c(6,0,3),
          )

# 명사 정리
# 불필요 단어 삭제
noun2 <- gsub("들이","",noun2)
noun2 <- Filter(function(x){nchar(x)>=2},noun2) # 글자수 2 이상만 추출
termCount <- table(noun2)
head(sort(termCount,decreasing = T),10)
myPal <- brewer.pal(8,'Dark2')
wordcloud(names(termCount),freq=termCount,scale=c(12,0,9),
         min.freq=3,random.order=F,rot_per=0.1,col=myPal)
# 출력 결과 저장

