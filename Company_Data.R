
comData <- read.csv("D:/Study Material/DataScience/Decision Tree/Company_Data.csv")
comData <-comData[,c(1:6,8,9)]
View(comData )
comData$Sales<- ifelse(comData$Sales>=8, "Yes", "No")
comData$Sales <- as.factor(comData$Sales)
View(comData$Sales)
str(comData)

#install.packages("lattice")
library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(comData$Sales,p=.75,list=F)
training <- comData[inTraininglocal,]
View(training)
testing <- comData[-inTraininglocal,]
table(testing$Sales)
#model building
model <- C5.0(training$Sales~.,data = training,trails = 40)
# Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing)
table(pred)
a <- table(testing$Sales,pred)
a
sum(diag(a)/sum(a))

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(comData$Sales,p=.85,list=F)
  training1<-comData[inTraininglocal,]
  testing<-comData[-inTraininglocal,]
  
  fittree<-C5.0(training1$Sales~.,data=training1)
  pred<-predict.C5.0(fittree,testing)
  a<-table(testing$Sales,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

acc
summary(acc)

