

#第1題************************************************************************#
library(readr)
admission <- read.csv(file.choose())  #選取admission.csv
View(admission)

#*************************************(a)*************************************#
pairs(admission[1:4],main="Admission Data",pch=5,cex=0.3,
      col=factor(admission$admit),
      lower.panel = NULL,font=2)
#部分不符合直覺，整體情況相對符合直覺。

#【admit與gre】在已錄取的學生當中，gre均高於300，而未錄取的學生當中，存在gre分數低於300的學生。。
#【admit與gpa】在已錄取的學生當中，不存在gpa異常低的學生。
#【admit與rank】在已錄取的學生當中，來自各個檔次的大學的學生都有。
#【gre與gpa】gre與gpa之間存在較明顯的正相關關係。
#【gre與rank】各個檔次學校都存在gre好與不好的學生。通過b題的箱形圖可看出兩者更明顯的關係。
#【gpa與rank】可看出各個檔次的大學都存在高gpa和低gpa的學生，且gpa極低的學生都相對較少。

#*************************************(b)*************************************#
par(mfrow=c(1,1))
boxplot(admission$gre~admission$rank,xlab="Rank of University",
        ylab="GRE",horizontal=FALSE,col="cyan",varwidth=TRUE)
#第一檔次的大學的學生GRE分數相對較高，
#聰第1檔次到第4檔次的大學的學生的GRE中位數逐漸降低。
#第二、三檔次的大學的學生GRE分數存在極低的異常值。
#由varwidth=TRUE帶來的圖形性質可知屬於第二、第三檔次的學校的學生較多。

#*************************************(c)*************************************#
table(admission$admit,admission$rank)  
#由表格可知，錄取的學生中，來自第一、二、三、四檔次的大學的學生人數分別為
#33,54,28,12人。

#*************************************(d)*************************************#
model1=lm(admit~gpa,data=admission) 
summary(model1)
#斜率估計值0.218代表當其他條件不變的情況下，gpa每增加1個單位,
#平均而言被錄取的概率會增加0.218。

#*************************************(e)*************************************#
plot(admission$gpa,admission$admit,main="Scatter Plot",
     col="red",pch=8,cex=.5,xlab="GPA",ylab="Admit or NOT")
abline(model1,col="green",lwd=2)
#可以發現樣本回歸線不能很好地擬合實際情況

#*************************************(f)*************************************#
model2=glm(formula=admit~.,data=admission,family=binomial)
summary(model2)
#gre和gpa的係數為正，rank的係數為負，代表gre和gpa的提升對錄取概率有正面作用，而
#學校檔次的降低（數值升高）會對錄取概率有負面作用
#但它們的係數不可像普通lm一樣直接被解讀

#*************************************(g)*************************************#
predict1=predict(model2,newdata=data.frame(gre=680,gpa=3.6,rank=1),
                type="response")
predict1
#被錄取的概率為0.586

#*************************************(h)*************************************#
predict2=predict(model2,data=admission,type="response")
my.prediction<-rep ("0",400)       
my.prediction[predict2>=0.5]<-"1" 
my.prediction[predict2<0.5]<-"0" 

#*************************************(i)*************************************#
table(my.prediction)
#根據（h）的結果，49人會被錄取，351人不會被錄取。

#*************************************(j)*************************************#
mean(admission$admit==my.prediction)
#結果的準確率僅為70.5%，原因可能是样本结构的不平衡性、
#解釋變量之間存在多重線性關係等Logistic模型的缺陷所致。


#第2題************************************************************************#
library(haven)
CEOSAL2 <- read_dta(file.choose())  #選取CEOSAL2.dta
View(CEOSAL2)

#*************************************(a)*************************************#
attach(CEOSAL2)
library(psych)
describe(sales)
var(sales)
describe(mktval)
var(mktval)
describe(salary)
var(salary)

#*************************************(b)*************************************#
hist(salary,xlab="Salary",ylab="Frequency",
     xlim=range(0:6000),ylim=range(0:80),breaks=8,font=2,border="blue")
#觀察圖形可知，薪資近似服從右偏分佈，而右偏分佈具備平均數大於中位數的特點，
#這與(a)題所得數值相呼應。

#*************************************(c)*************************************#
plot(age,salary,font=1,pch=5,col="green",
     main="Salary and Age")
#由圖形可知，薪資和年齡的關係不明顯，各個年齡段都有,薪資大多集中在2000以下，
#50-65歲的CEO數量相對較多。

#*************************************(d)*************************************#
table(grad,college)
#由表格可知，有5位CEO不擁有大學學歷，78位CEO只擁有大學學歷，
#94位CEO既擁有大學學歷也擁有碩士學歷。

#*************************************(e)*************************************#
t.test(salary,conf.level=0.99)$conf.int

#*************************************(f)*************************************#
t.test(salary-750,conf.level=0.99)$conf.int
#由於99%的置信區間不是一個包含0的置信區間，所以拒絕原假設，
#即我們有理由認為CEO的薪資不為750

#*************************************(g)*************************************#
model3=lm(lsalary~lsales+lmktval)
summary(model3)
#在其他變量不變的情況下，平均而言，
#sales每增加1%，salary會增加0.1621%，market value每增加1%，salary會增加0.1067%
#模型擬合優度為0.29，即y的變動大約有29%能被模型中的自變量解釋。

#*************************************(h)*************************************#
confint(model3,level=0.9)
#β0的置信區間為[4.2002,5.0416],表明我們對真實模型的截距會落于該區間有90%的信心。
#β1的置信區間為[0.0965,0.2277],表明我們對真實的lsales的係數會落于該區間有90%的信心。
#β2的置信區間為[0.0238,0.1896],表明我們對真實的lmktval的係數會落于該區間有90%的信心。

#*************************************(i)*************************************#
model4=lm(lsalary~lsales+lmktval+ceoten+ceotensq+college+grad+age+profits)
summary(model4)
#在5%的顯著性水平下，只有lsales,lmktval,ceoten,centensq是顯著的。
#college和grad不取對數的原因：兩者為虛擬變量，當取值為0時，log(x)為負無窮。
#age不取對數的原因：age作為年齡，若取百分比的變動，不符合實際意義。
#profits不取對數的原因：profits的取值存在負數，不可取對數。
#從R平方和調整後的R平方的角度，該模型確實比(g)更佳，
#但這不代表模型裏的每壹個新增變量都是必要保留的。

#*************************************(j)*************************************#
library(MASS)
model5=stepAIC(model4,direction = "both")
summary(model5)
#model5保留的變量為lsales,lmktval,ceoten,ceotensq
#因為AIC準則是在提高最大似然與不希望模型過於複雜之間做權衡，
#所以AIC會把顯著的變量保留下來，因為它們帶來的益處高過參數的新增帶來的懲罰。
#值得注意的是，某些變量在最初的模型中不顯著，但不是把不顯著的變量全部去掉，
#因為去掉之後原本顯著的可能變為不顯著，解決辦法是用AIC進行增減雙向檢驗。

#*************************************(k)*************************************#
#判定係數为0.343，調整的判定係數為0.3277
#不高。
#CEO薪資除了受銷售額、公司價值和任期的影響，還會受到其他很多因素的影響

#*************************************(l)*************************************#
model6=lm(salary~lsales+lmktval+ceoten+ceotensq+college+grad+age+profits)
summary(model6)

#*************************************(m)*************************************#
#因變量不同，(i)題的因變量是log(salary),(l)題的因變量是salary
#(i)題因變量的設置使得模型的意義在於解釋自變量對薪資變動幅度的影響，而
#(l)題因變量的設置使得模型的意義在於解釋自變量對薪資變動量的影響。

#*************************************(n)*************************************#
library(MASS)
model7=stepAIC(model6,direction = "both")
summary(model7)
#model7保留的變量為lsales,lmktval,ceoten,ceotensq

predict3=predict(model7,newdata=CEOSAL2[1,],interval="prediction",
                 level=0.90)
predict3
#該置信區間代表我們有90%的信心第一筆首席執行官的年薪
#會落于區間[384.2638,2093.638]

#*************************************(o)*************************************#
#不滿意。
#擬合值與真實值差距較大，且置信區間太大了。
#置信區間如此之大的原因：
#1、樣本容量太小；
#2、模型擬合優度太低；
#3、解釋變量的分散度不夠


#第3題************************************************************************#
library(readr)
Bank <- read_csv(file.choose())  #選取Bank.csv
View(Bank)

#*************************************(a)*************************************#
library(dplyr)
mydata=select(Bank,PersonalLoan, Age, Experience, 
              Income, Family,CCAvg,Education, `CD Account`)

#*************************************(b)*************************************#
summary(mydata)
mydata$PersonalLoan=as.factor(mydata$PersonalLoan)
#工作經驗最小值為負數。

#*************************************(c)*************************************#
mydata=mydata[!(mydata$Experience<0),]
summary(mydata$Experience)

#*************************************(d)*************************************#
dim(mydata)  #由此可知去除工作經驗為負的行後還剩4948筆數據
set.seed(888)
train=sample(4948,4500)

training.validation.set=mydata[train,]
test.set=mydata[-train,]

#*************************************(e)*************************************#
library(kknn)
model7=train.kknn(PersonalLoan~.,
                  data=training.validation.set,kmax=30)
#代表需要預測的y為是否最終向銀行借款（PersonalLoan）
#用於預測的變量為其餘7個變量。
#為了避免模型運行太久，設定最大鄰居數kmax=30,
#即訓練模型時嘗試附近有1-30個鄰居的這30種情況。
#默認的distance=2，即歐吉里氏距離。
model7
plot(model7)

#*************************************(f)*************************************#
options(digits=3)
predict4=predict(model7,test.set[,-1])
predict4
actual=test.set$PersonalLoan
table(actual,predict4)
mean(actual==predict4)
#準確率為0.973

#*************************************(g)*************************************#
#滿意。
#因為PersonalLoan已事先被因子化，所以預測結果中不會出現介於0與1之間的小數。
#另一种处理方式是不将PersonalLoan事先因子化，但在做出predict4之后执行
#predict4[predict4>=0.5]=1
#predict4[predict4<0.5]=0
#在同样是set.seed(888)的情况下，准确率也是0.973。


#第4題************************************************************************#
library(MASS)
mydata=Pima.tr
attach(mydata)

#*************************************(a)*************************************#
summary(mydata)

#*************************************(b)*************************************#
par(mfrow=c(1,1))
hist(bmi,main="Histogram of BMI",ylim=c(0,80))
hist(bp,breaks=7,main="Histogram of BP",xlim=c(20,120),ylim=c(0,70))
hist(age,main="Histogram of Age",ylim=c(0,80),xlim=c(15,75))
boxplot(bmi,bp,age,main="Box Plot of BMI, BP & Age") #該圖包括3個指標的箱形圖
#觀察直方圖和箱形圖可知，BMI和BP近似服從正態分布，但年齡不服從。

#*************************************(c)*************************************#
library(MASS)
mydata.lda<-lda(type~.,data=mydata,CV=TRUE)
mydata.lda

#*************************************(d)*************************************#
mydata.lda$class  #是否為糖尿病的預測結果
mydata$type       #是否為糖尿病的實際情況

#*************************************(e)*************************************#
table(mydata$type,mydata.lda$class)
mean(mydata.lda$class==mydata$type)
#由混淆矩陣可知，準確率為0.755

#*************************************(f)*************************************#
TruePositiveRate=
  sum(mydata$type=="Yes"&mydata.lda$class=="Yes")/sum(mydata$type=="Yes")
TruePositiveRate
#真陽率為0.544
FalsePositiveRate=
  sum(mydata$type=="No"&mydata.lda$class=="Yes")/sum(mydata$type=="No")
FalsePositiveRate
#假陽率為0.136
#分類結果不算太好，真陽率不高，還伴隨了相對較高的假陽率。

#*************************************(g)*************************************#
library(DAAG)
confusion(mydata$type, mydata.lda$class)
#模型的整體準確率為0.755
#該矩陣展示了四個比率，
#從左到右、從上到下分別為：真陰率、假陽率、假陰率、真陽率。

#*************************************(h)*************************************#
mydata.qda<-qda(type~.,data=mydata,CV=TRUE)
mydata.qda

table(mydata$type,mydata.qda$class)
mean(mydata.qda$class==mydata$type)
#模型的準確率為0.735

TruePositiveRate=
  sum(mydata$type=="Yes"&mydata.qda$class=="Yes")/sum(mydata$type=="Yes")
TruePositiveRate
#真陽率為0.529
FalsePositiveRate=
  sum(mydata$type=="No"&mydata.qda$class=="Yes")/sum(mydata$type=="No")
FalsePositiveRate
#假陽率為0.159

#*************************************(i)*************************************#
confusion(mydata$type, mydata.qda$class)
#由準確率、混淆矩陣的信息可知，本課題使用LDA效果更佳。

#*************************************(j)*************************************#
#參見第5題Rscript的方法四，若運行QDA，系統會提示樣本量太小。
#理論上，QDA更能形容社會狀況，但它要估的參數太多，若樣本量不大，
#會導致不穩定、不精確，此時更適合用LDA。
#實務中，不知道每個類別的方差矩陣是否一樣，則建兩個模，看哪個準確率更高。
#總結：（1）當樣本數不夠多時，通常用LDA更好，反之則反。
#（2）當每個y類別的自變量方差矩陣都相同時，LDA更好，反之則反。


#第5題************************************************************************#
#方法一：紙筆計算，詳見Word文檔
#方法二：用代碼驗證紙筆計算結果
age = c(55,46,35,29,45,35,28,24,48,52)
salary = c(105000,65000,60000,50000,170000,70000,36000,110000,
           45000,145000)
edu = c(17,14,14,16,12,9,13,12,12,16)
min_age = min(age)
max_age = max(age)
min_sa = min(salary)
max_sa = max(salary)
min_edu = min(edu)
max_edu = max(edu)
exp = c('No','Yes','Yes','Yes','No','Yes','Yes','No','No','No')

mydata = data.frame(age,salary,edu,exp)
standardization<-function(x){
  return((x-min(x))/(max(x)-min(x)))}

for (i in 1:3){
  mydata[,i] = standardization(mydata[,i])
}
mydata#此為標準化后的數據表

age11 = (30-min_age) / (max_age-min_age)
sa11 = (50000-min_sa)/ (max_sa-min_sa)
edu11 = (12-min_edu) / (max_edu-min_edu)
p11 = c(age11,sa11,edu11)
d = c()
for (i in 1:10){
  d[i] = ((p11[1]-mydata[i,1]) ^ 2 + (p11[2]-mydata[i,2]) ^ 2
          + (p11[3]-mydata[i,3]) ^ 2)^ 0.5
}

sd = sort(d)[1:3]  #sort(d)將d向量從小到大排序后的新向量，sd是這個新向量的第1-3個元素
y = c()   #新建一個空向量，用於記錄最小距離對應的樣本編號
j = 1     #j用於記錄要給y的哪一個元素賦值
for (x in sd){  #x在第一次循環中是第一小距離，之後為第二小距離、第三小距離
  for (i in 1:10){  #該循環用於找尋第一、第二、第三小距離對應的樣本編號
    if (x == d[i]){
      y[j] = i
      j = j + 1
      break
    }
  }
}
table(mydata[y,4]) 
#距離最近的三筆數據均為Yes，與紙筆計算結果相符，
#推斷此人會違約

#方法三：使用kknn
library(kknn) 
age = standardization(age)
salary = standardization(salary)
edu = standardization(edu)
age[11] = p11[1]
salary[11] = p11[2]
edu[11] = p11[3]
exp[11] = "NA"
mydata =  data.frame(age,salary,edu,exp)

model8 <- kknn(exp ~., mydata[1:10,], mydata[11,], k=3)
summary(model8)
predict5 <- fitted(model8)
predict5   #預測結果為Yes,即違約

#方法四：使用LDA和QDA
#LDA
model9=lda(exp~.,data=mydata,CV=FALSE)
predict6=predict(model9,data.frame(age=p11[1],salary=p11[2],edu=p11[3]))
predict6$class
#QDA#QDA不可用，系統會提示樣本量太小。
model10=qda(exp~.,data=mydata,CV=FALSE)


rm(list=ls())