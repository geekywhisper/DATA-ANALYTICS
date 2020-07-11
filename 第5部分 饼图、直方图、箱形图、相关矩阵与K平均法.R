#第3題************************************************************************#
#載入區**********************************************************************#
library(psych)
library(moments)
library(MASS)
library(dplyr)
library(kknn)
library(boot)
library(forecast)
library(tseries)
library(rpart)
library(rpart.plot)
library(klaR)
library(corrplot)
library(plotrix)
library(readr)
library(corrplot)
library(kknn)
library(scatterplot3d)

library(readxl)
Housing <- read_excel("C:/Users/赖逸儒/Desktop/Housing.xlsx")
View(Housing)
mydata=Housing

#基本分析與簡單的可視化**************************************************#
class(mydata)
names(mydata)
head(mydata,n=10)
dim(mydata)
#可知導入數據是一個645行，7列的數據框。
mydata=na.omit(mydata)
dim(mydata)
#由此可知原數據無缺失值。
str(mydata)
#可知所有數據原始類型均為number。
attach(mydata)

##描述性統計分析##
summary(mydata)

#相關矩陣  
correlation.matrix=cor(mydata)
correlation.matrix
par(mfrow=c(1,1))
corrplot(correlation.matrix)

##餅圖##
par(mfrow=c(2,3))
x = table(Bedroom)
piepercent=round(100*x/sum(x), 1)
piepercent =paste(piepercent, "%", sep = "")
label = c("1","2","3","4","5","6","7")
pie(x,labels=piepercent, main="公寓擁有的睡房個數",col= rainbow(length(x)))
legend("topright",label, cex=0.8, fill=rainbow(length(x)))


x=table(Living.Room)
piepercent=round(100*x/sum(x), 1)
piepercent =paste(piepercent, "%", sep = "")
label = c("0","1","2","3","4")
pie(x,labels=piepercent, main="公寓擁有的客廳個數",col= rainbow(length(x)))
legend("topright",label, cex=0.8, fill=rainbow(length(x)))

x=table(Bathroom)
piepercent=round(100*x/sum(x), 1)
piepercent =paste(piepercent, "%", sep = "")
label = c("0","1","2","3","4","5")
pie(x,labels=piepercent, main="公寓擁有的浴室個數",col= rainbow(length(x)))
legend("topright",label, cex=0.8, fill=rainbow(length(x)))

##箱形圖##
par(mfrow=c(1,1))
boxplot(Bedroom~cPrice,main="低中高三檔租房的臥室個數分佈情況",
        varwidth=TRUE,col="cyan",
        horizontal = FALSE)
boxplot(Living.Room~cPrice,main="低中高三檔租房的客廳個數分佈情況",
        varwidth=TRUE,col="pink",
        horizontal = FALSE)
boxplot(Bathroom~cPrice,main="低中高三檔租房的浴室個數分佈情況",
        varwidth=TRUE,col="orange",
        horizontal = FALSE)

##直方圖## 
par(mfrow=c(1,2))
hist(Price,freq=FALSE,main="Histogram of Price", ylim = c(0,2*10^(-5)),
     xlab="Price",ylab = "Frequency",col="lightpink",
     border="pink")
lines(density(Price),col="orange")
hist(Sqft,freq=FALSE,main="Histogram of sqft", ylim = c(0,6*10^(-4)),
     xlab="sqft",ylab = "Frequency",col="lightblue",
     border="blue")
lines(density(Sqft),col="orange")

#建模區*************************************************************************#
##（一）線性回歸模型##
model1=lm(Price~.,data=mydata)
summary(model1)
model1.new=stepAIC(model1, direction = "both")
summary(model1.new)
#Price ~ Sqft + Floor + Bedroom + Living.Room + Bathroom

##（二）分类
mydata=Housing
attach(mydata)
mydata$Floor = factor(mydata$Floor)
mydata$TotalFloor = factor(mydata$TotalFloor)
mydata$Bedroom = factor(mydata$Bedroom)
mydata$Living.Room = factor(mydata$Living.Room)
mydata$Bathroom = factor(mydata$Bathroom)
lp = log(Price)
mlp = mean(lp)
sdp = sd(lp)
lowp = sum(lp<=mlp-sdp)/645
midp = sum(lp>mlp-sdp&lp<=mlp+sdp)/645
hip = sum(lp>mlp+sdp)/645
hist(log(Price),freq=FALSE,main="Histogram of Price",
     xlab="Price",ylab = "Frequency",col="lightgreen",
     border="green")
lines(density(log(Price)),col="orange")
#經過log轉換後的Price近似正態分布，
#將Price離散化成3類0,1,2（低中高），取組內平均值為預測值（用於計算相對價格誤差）
prelow = exp((mlp-sdp) / 2)  #由於做了Log處理，所以還需要把預測結果轉化為原數據。
premid = exp(mlp)
prehi = exp((max(lp)+mlp+sdp)/2)

mydata$Price[lp<mlp-sdp] = 0
mydata$Price[lp>mlp-sdp&lp<=mlp+sdp] = 1
mydata$Price[lp>mlp+sdp] = 2
mydata$Price = factor(mydata$Price)
table(mydata$Price)

cPrice = Price
cPrice[lp<mlp-sdp] = 0
cPrice[lp>mlp-sdp&lp<=mlp+sdp] = 1
cPrice[lp>mlp+sdp] = 2


# lda  不適用/預測準確率不佳
model2 = lda(Price~.,CV=FALSE,data=mydata)
my.prediction2=predict(model2)$class
table(cPrice,my.prediction2)
#类别预测准确率
mean(cPrice==my.prediction2)

prePrice = Price
prePrice[my.prediction2==0] = prelow
prePrice[my.prediction2==1] = premid
prePrice[my.prediction2==2] = prehi
#相对价格误差
sum(abs(prePrice-Price))/sum(Price)

#lda cv=TRUE
model3 = lda(Price~.,CV=TRUE,data=mydata)
table(cPrice,model3$class)
#类别预测准确率
mean(cPrice==model3$class)

#qda 不適用/預測準確率不佳
model4 = qda(Price~.,CV=FALSE,data=mydata)
my.prediction4=predict(model4)$class
table(cPrice,my.prediction4)
#类别预测准确率
mean(cPrice==my.prediction4)

model4 = qda(Price~.,CV=TRUE,data=mydata)
my.prediction4=predict(model4)$class
table(cPrice,my.prediction4)
#类别预测准确率
mean(cPrice==my.prediction4)

#KNN
model5=train.kknn(Price ~., data = mydata, distance =2, kmax = 20)
summary(model5)
plot(model5)
my.prediction5=predict(model5,mydata[,-7])
table(cPrice, my.prediction5) 
mean(cPrice==my.prediction5)


#回归分类树
library(rpart)
par(mfrow=c(1,1))
model6 <- rpart(formula=Price~ .,  data=mydata,minsplit=5,xval=10, cp=0.01) 
printcp(model6)
plotcp(model6)

library(rpart.plot)
rpart.plot(model6, type=2, extra=1)
my.prediction6=predict(model6,type="class")
rpart.plot(model6, extra=2,type=4,roundint=FALSE,cex=1)

table(cPrice,my.prediction6)
predict6=table(cPrice,my.prediction6)
sum(diag(predict6))/sum(predict6)
mean(cPrice==my.prediction6)

# K平均法
#作為非監督式學習法，在有標籤的情況下其學習效果通常不及監督式學習方法
#此處僅做應用展示。
#之所以設定k≠3，是考慮到會把多個聚類都歸為某檔租金的情形。
bss = 0
bk = 0
for (k in 2:15){
  model7=kmeans(mydata, k)
  css = model7$betweenss/model7$totss
  if (bss < css){
    bss = css
    bk = k
    bm7 = model7
  }
  cat("currentssrate:",css,"k:",k,"\n")
}
# bm7
cat("best k:", bk)

rm(list=ls())
