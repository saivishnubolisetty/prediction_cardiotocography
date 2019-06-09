#installing Packages required for classification
install.packages("caret")
install.packages("sqldf")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("imputeTS")
install.packages("class")

#Loading The packages 
library(ggplot2)
library(lattice)
library(caret)
library(rpart)
library(rpart.plot)
library(imputeTS)
library(dplyr)
library(e1071)
library(class)


#Reading Dataset(Filtered)
setwd("D:\\MLProject")
cd <- read.csv("CTGG2.csv",sep=',',header=TRUE)
str(cd)
head(cd)

#Creating a subset by considering chi-Squared values
cdd<-subset(cd,select=c("LB","AC","FM","UC","DL","DS","DP","ASTV","MSTV","ALTV","MLTV","Width","Min","Max","Nmax","Nzeros","Mode","Mean","Median","Variance","Tendency","NSP","CLASS"))
head(cdd)

#If any Na's Imputing values
for(i in 1:ncol(cdd)){
  cdd[is.na(cdd[,i]), i] <- round(mean(cdd[,i], na.rm = TRUE))
}

#Dividing Dataset into Training and Testing
head(cdd)
set.seed(3033)
intrain= sort(sample(nrow(cdd),nrow(cdd)*.7))
training <- cdd[intrain,]
testing <-cdd[-intrain,]
train_target <- cdd[intrain,13]
test_target <- cdd[-intrain,15]
dim(training)
dim(testing)
anyNA(cdd)
summary(cdd)

#Creating Model by taking NSP Values for Classification

model1<- knn(train=training, test=testing, cl=train_target, k=16)
table(test_target, model1)
plot(model1)
cm = as.matrix(table(test_target, model1))
sum(diag(cm))/length(test_target)
