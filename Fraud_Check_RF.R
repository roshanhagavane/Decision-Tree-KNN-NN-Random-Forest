# Using Random Forest
library(randomForest)

Fraud_check <- read.csv("D:/Study Material/DataScience/Decision Tree/Fraud_check.csv")
Fraud_check <-Fraud_check[,c(3:5)]
View(Fraud_check)

Fraud_check$Taxable.Income<- ifelse(Fraud_check$Taxable.Income <= 30000, "Risky", "Good")
Fraud_check$Taxable.Income <- as.factor(Fraud_check$Taxable.Income)
View(Fraud_check$Taxable.Income)
str(Fraud_check)

Fraud_check_Risky<-Fraud_check[Fraud_check$Taxable.Income=="Risky",] # 50
Fraud_check_Good <- Fraud_check[Fraud_check$Taxable.Income=="Good",] # 50
View(Fraud_check_Risky)
#Fraud_check_virginica <- Fraud_check[Fraud_check$Taxable.Income=="virginica",] # 50
Fraud_check_train <- rbind(Fraud_check_Risky[1:25,],Fraud_check_Good[1:25,])
Fraud_check_test <- rbind(Fraud_check_Risky[26:50,],Fraud_check_Good[26:50,])
str(Fraud_check_train)
# Building a random forest model on training data 
fit.forest <- randomForest(Taxable.Income~.,data=Fraud_check_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(Fraud_check_train$Taxable.Income==predict(fit.forest,Fraud_check_train)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=Fraud_check_test)
mean(pred_test==Fraud_check_test$Taxable.Income) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(Fraud_check_test$Taxable.Income, pred_test)

