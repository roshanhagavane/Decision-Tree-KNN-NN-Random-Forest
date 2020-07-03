setwd("D:\\ML\\R\\Nueral Networks")
library(readr)
startups <- read.csv("D:/Study Material/DataScience/NN/Assignment/50_Startups.csv")
startups <-startups[,-c(4)]
View(startups)
str(startups)
attach(startups)
#normal_startups<-scale(startups)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
startups_norm<-as.data.frame(lapply(startups,FUN=normalize))

View(startups_norm)
summary(startups_norm$Profit)
#summary(normal_startups)
#summary(startups$Profit)
startups_train<-startups_norm[1:25,]
#View(startups_train)
startups_test<-startups_norm[26:50,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

# Building model
startups_model <- neuralnet(Profit~.,data = startups_train)
str(startups_model)
plot(startups_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(startups_model,startups_test[1:4])
predicted_Profit <- model_results$net.result
predicted_Profit
#model_results$neurons
cor(predicted_Profit,startups_test$Profit)
plot(predicted_Profit,startups_test$Profit)
# New model
model_5<-neuralnet(Profit~.,data= startups_norm)
plot(model_5)
model_5_res<-compute(model_5,startups_test[1:4])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,startups_test$Profit)
plot(pred_strn_5,startups_test$Profit)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased


