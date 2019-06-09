#installing Packages required for classification
install.packages("caret")
install.packages("sqldf")
install.packages("rpart")
install.packages("rpart.plot")
install.packages("imputeTS")
install.packages("e1071")
install.packages("depmixS4")
install.packages("mhsmm")

#Loading The packages 
library(ggplot2)
library(lattice)
library(caret)
library(rpart)
library(rpart.plot)
library(imputeTS)
library(dplyr)
library(e1071)
library(nnet)
library(MASS)
library(Rsolnp)
library(depmixS4)
library(mvtnorm)
library(mhsmm)


#Reading Dataset(Filtered)
setwd("D:\\MLProject")
cd <- read.csv("CTGG2.csv",sep=',',header=TRUE)
str(cd)
head(cd)

#Creating a subset by considering chi-Squared values
cdd<-subset(cd,select=c("LB","AC","UC","DL","DP","ASTV","MSTV","ALTV","MLTV","Width","Min","Max","Nmax","Nzeros","Mode","Mean","Median","Variance","NSP"))
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
head(training)
head(testing)
length(testing$NSP)
hmm2 <- depmix(NSP~1,data=training,nstates=2,
               family=multinomial("identity"))
fm <- fit(hmm2, emcontrol=em.control(classification="soft", maxit = 60))
hmm2 <- setpars(hmm2,getpars(fm))
hmm2 <- fit(hmm2)
predStates <- posterior(hmm2)
cm = as.matrix(table(train_target[1:length(predStates$state)], predStates$state))
sum(diag(cm))/length(train_target)

#Predicting values for testing dataset
anyNA(NB)
subtest<-subset(testing,select=c("LB","AC","UC","DL","DP","ASTV","MSTV","ALTV","MLTV","Width","Min","Max","Nmax","Nzeros","Mode","Mean","Median","Variance"))
hmm3 <- depmix(NSP~1,data=testing,nstates=2,
               family=multinomial("identity"))
sm <- fit(hmm3, emcontrol=em.control(classification="soft", maxit = 60))
hmm3 <- setpars(hmm3,getpars(fm))
hmm3 <- fit(hmm3)
predState <- posterior(hmm3)

#predState$state
#table(test_target[1:length(predStates$state)], predStates$state)
#plot(predStates$state)
cm1 = as.matrix(table(test_target[1:length(predState$state)], predState$state))
sum(diag(cm))/length(test_target)

