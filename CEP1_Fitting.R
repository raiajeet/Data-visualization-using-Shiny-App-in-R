#Data: teras-cep-1_20171201-095502
rm(list=ls()) 
data=read.csv("C:\\Users\\AJIT\\Documents\\teras-cep-1_20171201-0955021.csv",header=TRUE, sep=",") #Loading data
data<- subset(data, select = -c(X))
head(data)
nrow(data)
str(data)

data$DATE.TIME <- format(as.Date(data$DATE.TIME),"%d%b%Y")
head(data)
data=data[order(as.Date(data$DATE.TIME, format="%d%b%Y")),]
data1=data[1:10175,]
tail(data1)

head(data1)
nrow(data1)  
str(data1)

data1$DATE.TIME=as.Date(data1$DATE.TIME,"%d%b%Y")
data2=aggregate(.~DATE.TIME, data=data1, mean)
nrow(data2)
head(data2)
x=data2[,"DATE.TIME"]
y=data2[,"CONDENSATE.PMP.INLET.TEMP"]



library(ggplot2)
library(ggpmisc)
formula=y~ poly(x,4)
model<- lm(y~poly(x,4), data2)
summary(lm(formula))

ggplot(data = data2, aes(x = DATE.TIME, y = CONDENSATE.PMP.INLET.TEMP)) +
  geom_point(col="blue") +scale_size_continuous(range = c(10,20))+
  geom_smooth(method="lm", formula=formula,col="red") +
  stat_poly_eq(parse=T,aes(label = ..eq.label..), formula=formula)

###############################################################################################
nrow(data2)
head(data2)
data=subset(data2,select=-c(DATE.TIME))
data$t=1:nrow(data)
head(data)
train_ind <- sample(seq_len(nrow(data)), size =84)
train <- data[train_ind, ]
test <- data[-train_ind, ]

#Regression Tree
library(tree)
model1=tree(t~.,train)
result=predict(model1,test)
rmse <- sqrt(mean((test$t-result)^2))   # rmse :30.80
result1=floor(result)
print("Days Left :")
nrow(data)-result1

#Random Forest
library(randomForest)
model2=randomForest(t~.,train)
result=predict(model2,test)
rmse <- sqrt(mean((test$t-result)^2))   #rmse : 20.90
rmse
result1=floor(result)
print("Days Left :")
nrow(data)-result1

############################Feature Selection#####################################################
COND.PMP.A.MOT.STR.PH.A.TEMP
COND.PMP.A.DE.JRNL.BRG.TEMP
library(randomForest)
rf <-randomForest(t~.,data=data,
                  importance=TRUE,ntree=500)
importance(rf)
#COND.PMP.A.MOT.STR.PH.A.TEMP
#COND.PMP.A.DE.JRNL.BRG.TEMP
#above two variables are most significant
varImpPlot(rf)
plot(data$COND.PMP.A.MOT.STR.PH.A.TEMP,data$t,col="blue",cex=0.9,type="p")
plot(data$COND.PMP.A.DE.JRNL.BRG.TEMP,data$t,col="blue",cex=0.9,type="p")
model2=randomForest(t~COND.PMP.A.MOT.STR.PH.A.TEMP
                    ,train)
result=predict(model2,test)
rmse <- sqrt(mean((test$t-result)^2))   #rmse : 25.81
rmse
result1=floor(result)
print("Days Left :")
nrow(data)-result1


#Conclusion
#Performing Random Forest with all variables gives rmse: 20.90

