install.packages("ROCR")
install.packages("gplots")
library(gplots)
library(purrr)
library(tidyr)
library(ggplot2)
library(ROCR)

setwd("~/Desktop")
dat<-read.csv("framingham.csv")
dat1 <- dat[complete.cases(dat), ]
summary(dat1)

dat1 %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
  facet_wrap(~ key, scales = "free") +
  geom_histogram()

bound <- floor((nrow(dat1)/4)*3)         #define % of training and test set
df <- dat1[sample(nrow(dat1)), ]           #sample rows 
df.train <- df[1:bound, ]              #get training set
df.test <- df[(bound+1):nrow(df), ]   #get test set

framinghamLog = glm(TenYearCHD ~ ., data =df.train, family=binomial)
summary(framinghamLog)

Oddratio1 = exp(coef(framinghamLog))
Oddratio1

#predictTrain = predict(framinghamLog, type="response", newdata=df.train) 
#acctable1<-table(df.train$TenYearCHD, predictTrain > 0.5)
#acctable1

#Accuracy2=sum(diag(acctable1))/(sum(acctable1))
#Accuracy2


predictTest = predict(framinghamLog, type="response", newdata=df.test) 
summary(predictTest)
acctable<-table(df.test$TenYearCHD, predictTest > 0.5)
acctable
Accuracy1=sum(diag(acctable))/(sum(acctable))
Accuracy1

ROCRpred = prediction(predictTest, df.test$TenYearCHD)
prf <- performance(ROCRpred, measure = "tpr", x.measure = "fpr")
plot(prf)
as.numeric(performance(ROCRpred, "auc")@y.values)

library(MASS)
backward=step(framinghamLog)
summary(backward)
names(dat1)
FraminghamLog1 = glm(TenYearCHD ~ .-education-BPMeds-currentSmoker-BMI-diaBP-diabetes-prevalentStroke-heartRate, data = df.train, family=binomial)
summary(FraminghamLog1)


predictTest1 = predict(FraminghamLog1, type="response", newdata=df.test)
summary(predictTest1)
acc1=table(df.test$TenYearCHD, predictTest1 > 0.5)
Accuracy=sum(diag(acc1))/(sum(acc1))
Accuracy

ROCRpred1 = prediction(predictTest1, df.test$TenYearCHD)
prf1 <- performance(ROCRpred1, measure = "tpr", x.measure = "fpr")
plot(prf1)
as.numeric(performance(ROCRpred1, "auc")@y.values)

Oddratio = exp(FraminghamLog1$coefficients)
Oddratio




