install.packages('dtw')
install.packages('dtwclust')
library("dtw")

idx<-seq(0,6.24,len=100);
query<-cos(idx)
reference<-sin(idx)
dtw(query,reference,keep=TRUE)->alignment
dtwPlotThreeWay(alignment)
hq <- (0:8)/8
hq <- round(hq*100)
hw <- (alignment$index1 %in% hq)
hi <- (1:length(alignment$index1))[hw];
dtwPlotThreeWay(alignment,match.indice s=hi)

a1 <- c(7,9,6,9,12,6,4,6,8)
a2 <- c(5,6,4,3,9,5,6,8,9)

xrange <- range(1:9)
yrange <- range(c(a1,a2))


plot(xrange, yrange, type="n", xlab="time",
     ylab="value", xaxp  = c(0,10,10), yaxp  = c(3,12,9)) 
lines(a1, col='blue', type='l')
lines(a2, col='magenta', type='l')

dtw(a1,a2)$index1
dtw(a1,a2)$index2

plot(dtw(a1,a2), xlab="a1 - blue", ylab="a2 - magenta", xaxp  = c(0,10,10), yaxp = c(0,10,10))

plot(dtw(a1,a2, keep=TRUE), xlab="a1 - blue", ylab="a2 - magenta", xaxp  = c(0,10,10), yaxp = c(0,10,10), type="threeway")

plot(dtw(a1,a2, keep=TRUE), xaxp  = c(0,10,10), yaxp = c(0,10,10), type="twoway", col=c('blue', 'magenta'))

ita <- dtw(a1,a2,keep=TRUE,step=typeIIIc)
dtwPlotDensity(ita, main="Slope-limited asymmetric step (Itakura)")
dtw(a1,a2,keep=TRUE,window=itakuraWindow)->ita;
dtwPlotDensity(ita,
               main="Symmetric step with Itakura parallelogram window")
