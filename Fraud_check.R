
Fraud_check <- read.csv("D:/Study Material/DataScience/Decision Tree/Fraud_check.csv")
Fraud_check <-Fraud_check[,c(3:5)]
View(Fraud_check)

Fraud_check$Taxable.Income<- ifelse(Fraud_check$Taxable.Income <= 30000, "Risky", "Good")
Fraud_check$Taxable.Income <- as.factor(Fraud_check$Taxable.Income)
View(Fraud_check$Taxable.Income)
str(Fraud_check)

#install.packages("lattice")
library(caret)
library(C50)
# Data partion for model building and testing
inTraininglocal <- createDataPartition(Fraud_check$Taxable.Income,p=.75,list=F)
training <- Fraud_check[inTraininglocal,]
View(training)
testing <- Fraud_check[-inTraininglocal,]
table(testing$Taxable.Income)
#model building
model <- C5.0(training$Taxable.Income~.,data = training,trails = 40)
# Generating the model summary
summary(model)
plot(model)
pred <- predict.C5.0(model,testing)
table(pred)
a <- table(testing$Taxable.Income,pred)
a
sum(diag(a)/sum(a))

###Bagging####
acc<-c()
for(i in 1:100)
{
  print(i)
  inTraininglocal<-createDataPartition(Fraud_check$Taxable.Income,p=.85,list=F)
  training1<-Fraud_check[inTraininglocal,]
  testing<-Fraud_check[-inTraininglocal,]
  
  fittree<-C5.0(training1$Taxable.Income~.,data=training1)
  pred<-predict.C5.0(fittree,testing)
  a<-table(testing$Taxable.Income,pred)
  
  acc<-c(acc,sum(diag(a))/sum(a))
  
}

acc
summary(acc)

