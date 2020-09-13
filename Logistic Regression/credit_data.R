
library(readr)


credit_data = read.csv(file.choose())

credit_data = credit_data[,-1]

#############         EDA       ##########
str(credit_data)
summary(credit_data1)
boxplot(credit_data)


#treating outliers

x <- credit_data$reports
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$reports=x
boxplot(credit_data$reports)
#
x <- credit_data$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$age=x
boxplot(credit_data$age)
#

x <- credit_data$income
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$income=x
boxplot(credit_data$income)


x <- credit_data$share
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$share=x
boxplot(credit_data$share)
#
x <- credit_data$expenditure
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$expenditure=x
#
x <- credit_data$dependents
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$dependents=x
boxplot(credit_data$dependents)
#
x <- credit_data$months
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$months=x
boxplot(credit_data$months)
#

x <- credit_data$active
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
credit_data$active=x
boxplot(credit_data$active)


library(plyr)
credit_data$card=revalue(credit_data$card,c("yes"="1","no"="0"))
credit_data$card=as.factor(credit_data$card)

credit_data$owner=revalue(credit_data$owner,c("yes"="1","no"="0"))
credit_data$owner=as.factor(credit_data$owner)
credit_data$selfemp= revalue(credit_data$selfemp,c("yes"="1","no"="0"))
credit_data$selfemp=as.factor(credit_data$selfemp)


summary(credit_data)

#########standardisation###########


credit_data1=credit_data
credit_data1$reports=scale(credit_data1$reports)
credit_data1$age=scale(credit_data1$age)
credit_data1$income=scale(credit_data1$income)
credit_data1$share=scale(credit_data1$share)
credit_data1$expenditure=scale(credit_data1$expenditure)
credit_data1$dependents=scale(credit_data1$dependents)
credit_data1$months=scale(credit_data1$months)
credit_data1$majorcards= scale(credit_data1$majorcards)
credit_data1$active=scale(credit_data1$active)

#model building

model.credit= glm(card~.,data = credit_data1,family =binomial)
summary(model.credit)


#to checck influencing variables
influence.measures(model.credit1)#540,607,650 seem to be in fluential variBLES
influenceIndexPlot(model.credit1)
influencePlot(model.credit1)

 

library("MASS")
stepAIC(model.credit)
model.credit1= glm(card~reports+expenditure+dependents+active,family = "binomial",data = credit_data1[-607,-540,-650])
summary(model.credit1)



##confusion matrix table
prob=predict(model.credit1,type=c("response"),credit_data1)
prob
confusion =table(prob>0.6,credit_data1$card)
confusion

#roc curve

install.packages("ROCR")
library(ROCR)
rocrpred= prediction(prob,credit_data1$card)
rocrpref=performance(rocrpred,'tpr','fpr')
plot(rocrpref,colorize=T)#CURVE IS COVERING MOST OF THE ARE SO ACCURATE


#to checck influencing variables
influence.measures(model.credit1)#540,607,650 seem to be in fluential variBLES
influenceIndexPlot(model.credit1)
influencePlot(model.credit1)



