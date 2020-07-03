setwd("D:\\ML\\R\\Nueral Networks")
library(readr)
forestfires <- read.csv("D:/Study Material/DataScience/NN/Assignment/forestfires.csv")
forestfires <-forestfires[,-c(1,2,31)]
View(forestfires)
str(forestfires)
attach(forestfires)
#normal_forestfires<-scale(forestfires)
## or 
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}
forestfires_norm<-as.data.frame(lapply(forestfires,FUN=normalize))

View(forestfires_norm)
summary(forestfires_norm$area)
#summary(normal_forestfires)
#summary(forestfires$area)
forestfires_train<-forestfires_norm[1:258,]
#View(forestfires_train)
forestfires_test<-forestfires_norm[259:517,]

# Using multilayered feed forward nueral network
# package nueralnet
install.packages("neuralnet")
install.packages("nnet")
library(neuralnet)
library(nnet)

# Building model
forestfires_model <- neuralnet(area~.,data = forestfires_train)
str(forestfires_model)
plot(forestfires_model)

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
model_results <- compute(forestfires_model,forestfires_test[1:28])
predicted_area <- model_results$net.result
predicted_area
#model_results$neurons
cor(predicted_area,forestfires_test$area)
plot(predicted_area,forestfires_test$area)
# New model
model_5<-neuralnet(area~.,data= forestfires_norm)
plot(model_5)
model_5_res<-compute(model_5,forestfires_test[1:28])
pred_strn_5<-model_5_res$net.result
cor(pred_strn_5,forestfires_test$area)
plot(pred_strn_5,forestfires_test$area)
# SSE has reduced and training steps had been increased as the number of nuerons 
# under hidden layer are increased


