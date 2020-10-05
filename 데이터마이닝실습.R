#데.마 실습_01 기초

#연습문제1

(13^7)/9    # 13의 7승을 9로 나눈값

(3+4)^4/9   # 3,4더한값 4제곱을 9로 나눈 몫
(3+4)^4%/%9 # 정수 몫
(3+4)^4%%9  # 과 나머지

11<=9 # 11<=9진위판정 = False

sqrt(((10/4)+3)^3) 

cos(45)
cos(270)
sin(45)
sin(135)

#연습문제 2

xx <- (13^3)/9

xx <- (3+4)^3%/%11
yy <- (3+4)^3%%11

isTRUE(xx>yy)

isTRUE(xx>100 & xx<=300)  #isTRUE없어도 되긴함
xx>100 & xx<=300

isTRUE(yy<=20 & yy>40)

#연습문제 3

xSeries <- seq(1,34,3) #seq(min, max, step)

sum(xSeries*3)

dim(xSeries) <- c(3,4) #dim(배열) <- 차원지정 행렬로 변환기능
xSeries

xBx = c(T,    T,F,TRUE,F,FALSE)
yBy = c(FALSE,T,F,F,   F,TRUE)
xBx & yBy  #두번째만 and연산시 T!

xCx <- c('A','CBD','WXYZ','F')
yCy <- c('B','CB','WXYSE','f')
xCx > yCy   # 사전 순서로 대소정해짐 'b' > 'abcde'...

#연습문제 4

yVec <- seq(2,35,3)
mean(yVec)
var(yVec)

yVec[c(3,7,9,10)]

yVec[-c(3,7,9)]

yVec[yVec>14]<-88 # 등호(=)를 써도 됨.
yVec # yVec원소중 14보다 큰 것들의 값을 88로 변경!

# ※어래이 만드는법.
xarr <- array(3:12,dim=c(2,5))

#연습문제 5

zVec <- seq(1,34,3)
dim(zVec) <- c(3,4)
zVec

zMat <- matrix(seq(1,34,3),nrow=4,byrow=T)
zMat

zMat[2,-2]

zMat[,3][zMat[,3]>20] #진위판단만 할거면 왼쪽 제외

zz<-c(55,57,59)
zMat <- rbind(zMat,zz)
zMat
