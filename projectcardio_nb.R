#installing Packages required for classification
install.packages("caret")
install.packages("sqldf")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("imputeTS")

#Loading The packages 
library(ggplot2)
library(lattice)
library(caret)
library(e1071)


#Reading Dataset(Filtered)
setwd("D:\\MLProject")
cd <- read.csv("CTGG2.csv",sep=',',header=TRUE)
str(cd)
head(cd)

#Creating a subset by considering chi-Squared values
cdd<-subset(cd,select=c("LB","AC","FM","UC","DL","DS","DP","ASTV","MSTV","ALTV","MLTV","Width","Min","Max","Nmax","Nzeros","Mode","Mean","Median","Variance","Tendency","NSP","CLASS"))
head(cdd)

#graphs
boxplot(cdd$NSP)
plot(density(cdd$NSP))
hist(cdd$NSP)

#checking for NA Values in Datset
anyNA(cdd)

#If any Na's Imputing values
for(i in 1:ncol(cdd)){
  cdd[is.na(cdd[,i]), i] <- round(mean(cdd[,i], na.rm = TRUE))
}

head(cdd)
#Dividing Dataset into Training and Testing
set.seed(3033)
intrain= sort(sample(nrow(cdd),nrow(cdd)*.7))
training <- cdd[intrain,]
testing <-cdd[-intrain,]
dim(training)
dim(testing)

summary(cdd)

#Creating Model by taking NSP Values for Classification
head(training)
head(testing)
NB<- naiveBayes(as.factor(NSP)~.,data=training)
print(NB[1:2])

#Predicting values for testing dataset
anyNA(NB)
subtest<-subset(testing,select=c("LB","AC","FM","UC","DL","DS","DP","ASTV","MSTV","ALTV","MLTV","Width","Min","Max","Nmax","Nzeros","Mode","Mean","Median","Variance","Tendency","CLASS"))
qpredNB1 <- predict(NB,subtest,type=c("class"))
print(colnames(testing))
print(qpredNB1)
table(testing$NSP,qpredNB1)

#Calculating Accuracy and others
printALL=function(model){
  trainPred=predict(model, newdata = testing, type = "class")
  trainTable=table(training$NSP[1:length(trainPred)], trainPred)
  testPred=predict(NB, newdata=subtest, type="class")
  testTable=table(testing$NSP, testPred)
  trainAcc=(trainTable[1,1]+trainTable[2,2]+trainTable[3,3])/sum(trainTable)
  testAcc=(testTable[1,1]+testTable[2,2]+testTable[3,3])/sum(testTable)
  message("Contingency Table for Training Data")
  print(trainTable)
  message("Contingency Table for Test Data")
  print(testTable)
  message("Accuracy")
  print(round(cbind(trainAccuracy=trainAcc, testAccuracy=testAcc),3))
}
printALL(NB)

