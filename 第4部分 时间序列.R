
#第1題************************************************************************#
library(forecast)
library(tseries)

#*************************************(a)*************************************#
set.seed(888)
ts.sim=20+arima.sim(list(ar=0.7), sd = sqrt(16), n = 5000)
ts.sim

#*************************************(b)*************************************#
#见word文檔

#*************************************(c)*************************************#
par(mfrow=c(1,1))
plot.ts(ts.sim,main="a1 is 0.7")
#時間序列的長度為5000
#圍繞20，取值上下波動
#觀察圖形可推斷序列是平穩的

#*************************************(d)*************************************#
par(mfrow=c(1,2))
acf(ts.sim, main="AC")
pacf(ts.sim, main="PAC")
#AC圖形呈指數下降
#PAC圖形在呈現一條之後迅速下跌近乎消失

#*************************************(e)*************************************#
my.arima=arima(ts.sim, order=c(1,0,0))
my.arima
#ar1=0.6985,代表對序列建模后的係數為0.6985，標準誤為0.0101。
#計算t值后可知a1=0.6985與真實的a1=0.7在統計上無任何差別。
#截距20.1555代表平均數，標準誤為0.1868。
#計算t值后可知intercept=20.1555與真實的平均數20在統計上無任何差別。
#方差為15.87，真實的方差為16。
#aic = 28018.66，AIC代表擬合優度，AIC越小，代表擬合優度越好。


#第2題************************************************************************#
library(haven)
Okun <- read_dta("C:/Users/赖逸儒/Desktop/Okun.dta")
View(Okun)
library(forecast)
library(tseries)

#*************************************(a)*************************************#
par(mfrow=c(1,1))
ts.pcrgdp=ts(Okun$pcrgdp,start=1959)
ts.cunem=ts(Okun$cunem,start=1959)
plot.ts(ts.pcrgdp,main="實際GDP(紅)和失業率(藍)的變化",ylab="%",
        xlab="Year",col="red")  
  lines(ts.cunem,col = "blue")
#觀察圖形猜測這兩個序列都是平穩的

#*************************************(b)*************************************#
adf.test(ts.pcrgdp,alternative = "stationary")
#由於做了一階差分，所以cunem在t=1959的位置為NA，所以需要做一下處理：
adf.test(ts.cunem[-1],alternative = "stationary")
#由P值可知，均平穩。

#*************************************(c)*************************************#
fit=lm(pcrgdp~cunem,data=Okun)
summary(fit)
#可知截距項的估計值不為3，斜率項的估計值不為負2。

#*************************************(d)*************************************#
#方法一：詳見word文檔
#方法二：
confint(fit,level=0.9)

#*************************************(d)*************************************#
#方法一：詳見word文檔
#方法二：
confint(fit,level=0.9)

rm(list=ls())

#第2題************************************************************************#
#*************************************(b)*************************************#
library(readxl)
CPI <- read_excel("C:/Users/赖逸儒/Desktop/CPI.xls")
View(CPI)

library(forecast)
library(tseries)

mydata=CPI
ts.cpi<-ts(mydata$cpi[-133], start = c(2009,1), frequency=12)
summary(ts.cpi)
ts.plot(ts.cpi, main="CPI当月同比",col="Red", ylab="%")
adf.test(ts.cpi)
#不平穩
par(mfrow=c(1,2))
Acf(ts.cpi, main='')
Pacf(ts.cpi, main='')
par(mfrow=c(1,1))
model=auto.arima(ts.cpi)
model
my.forceast=forecast(model,h=1, level=c(90,95))
my.forceast
plot(my.forceast,  main="my forecast")
mydata$cpi=ts(mydata$cpi,start= c(2009,1), frequency=12)
lines(mydata$cpi)

rm(list=ls())