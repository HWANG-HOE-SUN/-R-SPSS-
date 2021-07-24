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
setwd('E:/C����̺�/���ٿ�ε�')
getwd()

data=read_excel('card_grouped_ta_1(re).xlsx')
data

data$date = as.Date(as.yearmon(as.character(data$ta_ym), "%Y%m")) 
head(data)
plot(data)

names <- colnames(data)[c(4,5,7,8,9)] # ts Į�� �̸����
names
mat = cbind(data$log_vlm,data$usec,data$atdtr_encod,data$cln_age_r,data$card_counts)
scaled_mat = scale(mat)

y<-ts(scaled_mat,start=c(2020,7),freq=11)
colnames(y) <- names # ������ ��ü
colnames(y)

ts_plot(y)

### �ð迭 �м�

#����

y_uni_1 <- ts(scaled_mat[,1],start=c(2020,7),freq=11)
ts_plot(y_uni_1,title="atdtr_encod") # 


### �α� data
pop = read.csv('E:/C����̺�/���ٿ�ε�/2020-2021�Ĺ�_���ɺ��α�.csv')
pop

colnames(pop)
pop[,2]

View(pop)
pop[2:18,2] # 20�� 1�� �� �α���
pop[2:18,38] # 20�� 2�� �� �α���
pop[2:18,74] # 20�� 3�� �� �α���

pop[2:18,2+36*6] # 20�� 7�� �� �α���
pop[2:18,2+36*11] # 20�� 12�� �� �α���

pop[2:18,2+36*12] # 21�� 1��  2+36*12 = 434
pop[2:18,434+39] # 21�� 2�� 
pop[2:18,434+39*2] # 21�� 3��
pop[2:18,434+39*3] # 21�� 4��
pop[2:18,434+39*4] # 21�� 5��

# �Ʒ� �ڵ��� ������ ����
num_list <- lapply(pop[,2],function(x) as.numeric(gsub(",","",x)))
num_list <- unlist(num_list)
typeof(num_list)
num_list # 1�� ������ �� �α���

### �ݺ������� �� ���� �� �α��� �̾� list �����
# 20�� 7�� ~ 21�� 1��
for(i in 7:13){
  if(i<13){ # 20�� 7�� ~ 20�� 12��
    assign(paste0("test_",i),
           lapply(pop[2:18,2+36*(i-1)],
                                    function(x) as.numeric(gsub(",","",x))))
  } #21�� 1���� ��� �̸��� �ٸ��� ����
  else{
    assign(paste0("test_",i-12),
           lapply(pop[2:18,2+36*(i-1)],
                  function(x) as.numeric(gsub(",","",x))))
  }
}
test_list <- mget(ls(pattern="test_")) # ��� ����Ʈ

# 21�� 2�� ~ 21�� 5��
for(i in 2:5){
  assign(paste0("test2_",i),
         lapply(pop[2:18,434+39*(i-1)],
                function(x) as.numeric(gsub(",","",x))))
}
test_list2 <- mget(ls(pattern="test2_"))

test_list
test_list2
View(test_list2)

sunseo = c(1,10,11,12,c(7:9)) # index ���� (test_list��)
### �ݺ������� ������ �� �� mat����
for(i in 7:13){ # 20��7��~ 21�� 1��
  assign(paste0("pop_mon_",sunseo[i-6]), scale(unlist(test_list[i-6])))
}
for(i in 1:4){ # 21��2��~ 21�� 5��
  assign(paste0("pop_mon_",i+1),scale(unlist(test_list2[i])))
}



# �� ���� ������ �����̹Ƿ� ��<->�� ��ġ���ش�. 
# 20�� 7�� ~ 21�� 5��. matrix���·� �ڷᱸ�� ��ȯ
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


# ������=��������, ������ = ���� ����
total_mat <- matrix(c(row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11),
       byrow=T,nrow=11,ncol=17)
total_mat

# ������ ts ���� ( �α� ��ȭ �߼� )
seoul_ts <- ts(total_mat[,1],start=c(2020,7),freq=11) #�������� ts������
busan_ts <- ts(total_mat[,2],start=c(2020,7),freq=11) #�λ����� ts������
daegu_ts <- ts(total_mat[,3],start=c(2020,7),freq=11) #�뱸���� ts������
incheon_ts <- ts(total_mat[,4],start=c(2020,7),freq=11) #��õ���� ts������
gwangju_ts <- ts(total_mat[,5],start=c(2020,7),freq=11) #�������� ts������
daejeon_ts <- ts(total_mat[,6],start=c(2020,7),freq=11) #�������� ts������
ulsan_ts <- ts(total_mat[,7],start=c(2020,7),freq=11) #������� ts������
sejong_ts <- ts(total_mat[,8],start=c(2020,7),freq=11) #�������� ts������
gyeongi_ts <- ts(total_mat[,9],start=c(2020,7),freq=11) #��⵵���� ts������
gangwon_ts <- ts(total_mat[,10],start=c(2020,7),freq=11) #���������� ts������
chungcheongnorth_ts <- ts(total_mat[,11],start=c(2020,7),freq=11) #��û�ϵ����� ts������
chungcheongsouth_ts <- ts(total_mat[,12],start=c(2020,7),freq=11) #��û�������� ts������
jeonranorth_ts <- ts(total_mat[,13],start=c(2020,7),freq=11) #����ϵ����� ts������
jeonrasouth_ts <- ts(total_mat[,14],start=c(2020,7),freq=11) #���󳲵����� ts������
gyeongsangnorth_ts <- ts(total_mat[,15],start=c(2020,7),freq=11) #���ϵ����� ts������
gyeongsangsouth_ts <- ts(total_mat[,16],start=c(2020,7),freq=11) #��󳲵����� ts������
jeju_ts <- ts(total_mat[,17],start=c(2020,7),freq=11) #���ֵ����� ts������


#������ ts plot
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