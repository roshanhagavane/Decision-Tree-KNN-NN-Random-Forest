# Read the dataset
glass <- read.csv("D:/Study Material/DataScience/KNN/glass.csv")
View(glass)
#First colum in dataset is id which is not required so we will be taking out

#table of diagonis B <- 357 and M <- 212
table(glass$Type)

# Replace B with Benign and M with Malignant. Diagnosis is factor with 2 levels that is B and M. We also replacing these two entery with Benign and Malignat
#glass$Type <- factor(glass$Type, levels = c("B","M"), labels = c("Benign","Malignant"))

# table or proportation of enteries in the datasets. What % of entry is Bengin and % of entry is Malignant
round(prop.table(table(glass$type))*100,1)
summary(glass[c("RI","Na","Mg")])
#Create a function to normalize the data
norm <- function(x){ 
  return((x-min(x))/(max(x)-min(x)))
}
#test normalization
#norm(c(1,2,3,4,5))
#norm(c(10,20,30,40,50))
#Apply the normalization function to wbcd dataset
glass_n <- as.data.frame(lapply(glass[1:10], norm))
View(glass_n)

#create training and test datasets
glass_train <- glass_n[1:169,]
glass_test <- glass_n[170:214,]

#Get labels for training and test datasets

glass_train_labels <- glass[1:169,1]
glass_test_labels <- glass[170:214,1]

# Build a KNN model on taining dataset
install.packages("class")
library("class")
# Building the KNN model on training dataset and also need labels which we are including c1. Once we build the preduction model
# we have to test on test dataset
test_acc <- NULL
train_acc <- NULL
for (i in seq(3,200,2))
{
  train_glass_pred <- knn(train=glass_train,test=glass_test,cl=glass_train_labels,k=i)
  train_acc <- c(train_acc,mean(train_glass_pred==glass_train_labels))
  test_glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=i)
  test_acc <- c(test_acc,mean(test_glass_pred==glass_test_labels))
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
glass_pred <- knn(train = glass_train, test = glass_test, cl = glass_train_labels, k=21)
mean(glass_pred==glass_test_labels)

