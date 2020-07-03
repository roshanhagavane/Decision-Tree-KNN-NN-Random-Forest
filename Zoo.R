# Read the dataset
zoo <- read.csv("D:/Study Material/DataScience/KNN/Zoo.csv")
View(zoo)
#First colum in dataset is id which is not required so we will be taking out

#table of diagonis B <- 357 and M <- 212
table(zoo$type)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
#glass$Type <- factor(glass$Type, levels = c("B","M"), labels = c("Benign","Malignant"))

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(zoo$type))*100,1)
summary(zoo[c("hair","eggs","milk")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
#norm(c(1,2,3,4,5))
#norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
zoo_n <- as.data.frame(lapply(zoo[2:18], norm))
View(zoo_n)

#create training and test datasets
zoo_train <- zoo_n[1:71,]
zoo_test <- zoo_n[72:101,]

#Get labels for training and test datasets

zoo_train_labels <- zoo[1:71,1]
zoo_test_labels <- zoo[72:101,1]

# Build a KNN model on taining dataset
install.packages("class")
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_zoo_pred <- knn(train=zoo_train,test=zoo_test,cl=zoo_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_zoo_pred==zoo_train_labels))
  test_zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_zoo_pred==zoo_test_labels))
}


# Testing Accuracy 

# Plotting 2 different graphs on same window
par(mfrow=c(1,2)) # c(1,2) => indicates 1 row and 2 columns
plot(seq(3,200,2),train_acc,type="l",main="Train_accuracy",col="blue")
plot(seq(3,200,2),test_acc,type="l",main="Test_accuracy",col="red")

acc_neigh_df <- data.frame(list(train_acc=train_acc,test_acc=test_acc,neigh=seq(3,200,2)))
# Plotting 2 different graphs on same co-ordinate axis
install.packages("ggplot2")
library(ggplot2)
ggplot(acc_neigh_df,aes(x=neigh))+
  geom_line(aes(y=train_acc,colour="train_acc"),lwd=1.5)+
  geom_line(aes(y=test_acc,colour="test_acc"),lwd=1.5)+
  scale_fill_manual(" ",breaks=c("train_acc","test_acc"),values = c("train_acc"="green","test_acc"="red"))

#Below is the final method 
zoo_pred <- knn(train = zoo_train, test = zoo_test, cl = zoo_train_labels, k=21)
mean(zoo_pred==zoo_test_labels)

