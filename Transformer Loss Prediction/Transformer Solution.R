Data<-read.csv("C:/Users/admin/Downloads/nkpl_June-Sep.csv")
#plot(Data$PFLT,Data$KWLT,type="l")
#require(GGally); require(ggplot2)
#g<-ggpairs(Data[,2:21],lower=list(continuous="smooth")) #Not Working
################# SCATTER PLOT
Data$Loss<-Data$KWHT-Data$KWLT
library(gclus)
dta <- cbind(Data[,2:11],Data$Loss) # get data 
dta.r <- abs(cor(dta)) # get correlations
dta.col <- dmat.color(dta.r) # get colors
# reorder variables so those with highest correlation
# are closest to the diagonal
dta.o <- order.single(dta.r) 
cpairs(dta, dta.o, panel.colors=dta.col, gap=.5,main="Scatter Plot With Correlation" )


Train<-Data[1:7910,2:21]
Test<-Data[7911:8010,2:21]
rm(Data)
Train1<-cbind(Train[Train$VHT>15000,],Train[Train$VHT>15000,]$kWHT-Train[Train$VHT>15000,]$kWLT)
names(Train1)[21]<-"Loss"
Train2<-cbind(Train[Train$VHT<=15000,],Train[Train$VHT<=15000,]$kWHT-Train[Train$VHT<=15000,]$kWLT)
names(Train2)[21]<-"Loss"
fit1<-lm(Loss~IHT+ILT, data=Train1)
summary(fit1)
fit21<-lm(kWLT~ILT, data=Train2)
summary(fit21)
fit22<-lm(kWHT~IHT, data=Train2)
summary(fit22)
pred<-predict(fit2,newdata=Test)
Test<-cbind(Test,pred)
f<-function(x,y){
  z<-34.1508865+17.92*x-0.7299*y
  return(z)
  }
pred1<-f(Test$IHT,Test$ILT)
anova(fit2)
