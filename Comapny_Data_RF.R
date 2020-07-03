# Using Random Forest
library(randomForest)

comData <- read.csv("D:/Study Material/DataScience/Decision Tree/Company_Data.csv")
comData <-comData[,c(1:6,8,9)]
# Splitting data into training and testing. As the Sales are in order 
# splitting the data based on Sales 
comData$Sales<- ifelse(comData$Sales>=8, "Yes", "No")
comData$Sales <- as.factor(comData$Sales)
View(comData$Sales)
str(comData)

comData_Yes<-comData[comData$Sales=="Yes",] # 50
comData_No <- comData[comData$Sales=="No",] # 50
View(comData_Yes)
#comData_virginica <- comData[comData$Sales=="virginica",] # 50
comData_train <- rbind(comData_Yes[1:25,],comData_No[1:25,])
comData_test <- rbind(comData_Yes[26:50,],comData_No[26:50,])
str(comData_train)
# Building a random forest model on training data 
fit.forest <- randomForest(Sales~.,data=comData_train, na.action=na.roughfix,importance=TRUE)
fit.forest$ntree
# Training accuracy 
mean(comData_train$Sales==predict(fit.forest,comData_train)) # 100% accuracy 

# Predicting test data 
pred_test <- predict(fit.forest,newdata=comData_test)
mean(pred_test==comData_test$Sales) 
library(gmodels)
# Cross table 
rf_perf<-CrossTable(comData_test$Sales, pred_test)

