
#第1題************************************************************************#
#*************************************(a)*************************************#
#方法一：紙筆計算，詳見word文檔
#方法二：用代碼驗證紙筆計算結果
#吉尼係數
sex = c("男性","男性","男性","男性","男性","女性","女性","女性","女性","女性")
wage = c("低","高","低","高","低","低","高","高","低","低")
edu = c("小学","大学","大学","中学","中学","小学","中学","大学","小学","中学")
pla = c("大","大","小","大","小","小","小","大","小","大")
y = c(0,0,1,1,0,1,0,0,1,1) #0表示單身，1表示已婚
gini = function(v){
  y1 = sum(v)/length(v)
  y0 = 1 - y1
  g = 1-y0^2 - y1^2
  return(g)
}
mydata = data.frame(sex,wage,edu,pla,y)
oyg = gini(y) #orginal y gini
sy1 = mydata$y[mydata$sex=="男性"]  #s代表sex,y代表y列，1代表男性。
sy0 = mydata$y[mydata$sex=="女性"]
syg = oyg - 0.5 * gini(sy1) - 0.5 * gini(sy0) #計算得到信息增益為0.0200

wy1 = mydata$y[mydata$wage=="低"]   #w代表wage,y代表y列，1代表低薪。
wy0 = mydata$y[mydata$wage=="高"]
wyg = oyg - 0.6 * gini(wy1) - 0.4 * gini(wy0)

ey0 = mydata$y[mydata$edu=="小学"]  #e代表education,y代表y列
ey1= mydata$y[mydata$edu=="中学"]   #0、1、2分別代表小學中學大學
ey2 = mydata$y[mydata$edu=="大学"]
eyg = oyg - 0.3 * gini(ey0) - 0.4 * gini(ey1) - 0.3 * gini(ey2)

py0 = mydata$y[mydata$pla=="小"]    #p代表place,y代表y列，
py1 = mydata$y[mydata$pla=="大"]    #0代表小鄉鎮，1代表大都市
pyg = oyg - 0.5 * gini(py0) - 0.5 * gini(py1)

options(digits=4)
cat(syg,wyg,eyg,pyg) #與紙筆計算結果一致

#熵
entr = function(v){
  y1 = sum(v)/length(v)
  y0 = 1 - y1
  g = -y0*log2(y0) - y1*log2(y1)
  return(g)
}
oyg = entr(y)
sy1 = mydata$y[mydata$sex=="男性"]
sy0 = mydata$y[mydata$sex=="男性"]
syg = oyg - 0.5 * entr(sy1) - 0.5 * entr(sy0)

wy0 = mydata$y[mydata$wage=="低"]
wy1 = mydata$y[mydata$wage=="高"]
wyg = oyg - 0.6 * entr(wy0) - 0.4 * entr(wy1)

ey0 = mydata$y[mydata$edu=="小学"]
ey1 = mydata$y[mydata$edu=="中学"]
ey2 = mydata$y[mydata$edu=="大学"]
eyg = oyg - 0.3 * entr(ey0) - 0.4 * entr(ey1) - 0.3 * entr(ey2)

py0 = mydata$y[mydata$pla=="小"]
py1 = mydata$y[mydata$pla=="大"]
pyg = oyg - 0.5 * entr(py0) - 0.5 * entr(py1)

options(digits=4)
cat(syg,wyg,eyg,pyg) #與紙筆計算結果一致


#第2題************************************************************************#
library(party)
data("readingSkills")
View(readingSkills)

#*************************************(a)*************************************#
mydata=readingSkills
summary(mydata)

#*************************************(b)*************************************#
#方法一
plot(mydata$shoeSize,mydata$score,
     main="Relationship between shoeSize and Score",
     xlab="shoeSize",ylab="Score",pch=8,col="red")

#方法二
pairs(~mydata$score+mydata$shoeSize,col=mydata$shoeSize)
#鞋碼與成績成正比關係，說明兩者存在關聯，但不能說兩者之間存在因果關係。

#*************************************(c)*************************************#
library(rpart)
mytree <- rpart(nativeSpeaker~ .,
                data=mydata, minsplit=20, cp=0.05,xval=10) 
printcp(mytree, digits=4)
par(mfrow=c(1,1))
plotcp(mytree)

#*************************************(d)*************************************#
options(digits=4)
base = 0.005
for (i in 1:86){
  ccp = base * i
  tree = rpart(nativeSpeaker~ ., data=mydata,minsplit=20,xval=10,cp = ccp)
  cat("cp=",ccp)
  printcp(tree)
}


#*************************************(e)*************************************#
library(rpart.plot)
for (i in 1:4){
  rpart.plot(mytree, type=4, extra=i,cex.main=300,font=2)
}
print(mytree)

#第3題************************************************************************#
#方法一：Excel計算，詳見Word文檔
#方法二：使用K鄰近法
age = c(55,36,28,24,40,21,46,31,43)
salary = c(160000,56000,75000,50000,140000,50000,65000,110000,
           45000)
edu = c(17,14,12,15,12,9,16,12,12)
min_age = min(age)
max_age = max(age)
min_sa = min(salary)
max_sa = max(salary)
min_edu = min(edu)
max_edu = max(edu)
exp = c("No","Yes","Yes","Yes","No","Yes","No","No","No")

stdv<-function(x){
  return((x-min(x))/(max(x)-min(x)))}

age10 = (25-min_age) / (max_age-min_age)
sa10= (36000-min_sa)/ (max_sa-min_sa)
edu10 = (13-min_edu) / (max_edu-min_edu)
p10 = c(age10,sa10,edu10)


library(kknn)
age = stdv(age)
salary = stdv(salary)
edu = stdv(edu)
age[10] = p10[1]
salary[10] = p10[2]
edu[10] = p10[3]
exp[10] = NA
mydata =  data.frame(age,salary,edu,exp)
d = c()
for (i in 1:9){
  d[i] = ((p10[1]-mydata[i,1]) ^ 2 + (p10[2]-mydata[i,2]) ^ 2
          + (p10[3]-mydata[i,3]) ^ 2)^ 0.5
}

options(digits=3)
mydata#此為標準化后的數據表

model <- kknn(exp ~., mydata[1:9,], mydata[10,], k=3)
summary(model)
predict <- fitted(model)
predict   #預測結果為Yes,即違約


rm(list=ls())
