# 기타 패키지들
install.packages("dplyr")
install.packages("ggplot2")
install.packages("lattice")
install.packages("FSelector")
library(FSelector)
library(lattice)
library(dplyr)
library(ggplot2)
library(caret)
# 1. 데이터 불러오기
library(MASS) # Boston데이터를 포함함
attach(Boston) # 이제 자유롭게 Dataset 사용 가능

summary(Boston)
