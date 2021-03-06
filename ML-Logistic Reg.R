setwd("D:/MBA_3RD SEM/ML")

## Logistic Regression


flierresponse=read.csv(file = "FlierResponse.csv", header = T, sep= ",")
names(flierresponse)
str(flierresponse)
flierresponse$Response <- as.factor(flierresponse$Response)
summary(flierresponse)

#flierresponseglm <- glm(Response~Age, data = flierresponse, family = "binomial")

framingham=read.csv("framingham.csv")
str(framingham)
framingham$education=factor(framingham$education)
str(framingham)

install.packages("caTools")
library(caTools)
#library(DAAG)
#library(rms)
install.packages("car")
library(car)

#Randomly split the data into training and testing series
set.seed(1000)
split=sample.split(framingham$TenYearCHD, SplitRatio = 0.70)

#Split up the data using subset
train =subset(framingham, split==TRUE)
test =subset(framingham, split==FALSE)

#Logistic Regression Model
framinghamLog =glm(TenYearCHD~ ., data = train, family=binomial)
summary(framingham)
car::vif(framinghamLog)
#rms::vif(framinghamLog)
#DAAG::vif(framinghamLog)

#accuracy on training set
predictTrain = predict(framinghamLog,type = "response",newdata = train)
predictTrain
#confusion  matrix with threshold of 0.5
table(train$TenYearCHD,predictTrain>0.5)

#accuracy on train set 

#accuracy(all correct/all)=TP+TN/TP+TN+FP+FN
(2710+30)/(2170+30+357+9)
#Precision (true positives/predicted positives)=TP/TP+FP
(2170)/(2170-357)
#Sensitivity aka Recall (true positives/all actual positives)=TP/TP+FN
(2170)/(2170+9)
#Specificity (true negatives/all actual negatives)=TN/TN+FP
(30)/(30+357)

#predictions on the test set
predictTest = predict(framinghamLog,type = "response",newdata= test)
predictTest

#confusionmatrix with threshold of 0.9
table(test$TenYearCHD,predictTest>0.5)

#accuracy on test set
(916+13)/(916+13+157+6)

# Home work -  Accuracy levels
# Confusion matrix with threshold of 0.9
table(test$TenYearCHD, predictTest > 0.9)
# Confusion matrix with threshold of 0.7
table(test$TenYearCHD, predictTest > 0.7)
# Confusion matrix with threshold of 0.5
table(test$TenYearCHD, predictTest > 0.5)
# Confusion matrix with threshold of 0.3
table(test$TenYearCHD, predictTest > 0.3)
# Confusion matrix with threshold of 0.1
table(test$TenYearCHD, predictTest > 0.1)

#test set AUC
install.packages("ROCR")
library(ROCR)
library(ggplot2)

ROCRpred = prediction(predictTest, test$TenYearCHD)
as.numeric(performance(ROCRpred, "auc")@y.values)
ROCRperf <- performance(ROCRpred, "tpr", "fpr")
par(mfrow=c(1,1))
plot(ROCRperf, colorize = TRUE, print.cutoffs.at=seq(0,1,by=0.1), text.adj=c(-0.2,1.7))

