library(readr)



###############EDA##############
str(bank.full)
summary(bank.full)
boxplot(bank.full)




#treating outliers

boxplot(bank.full$age)
x <- bank.full$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bank.full$age=x
boxplot(bank.full$age)
#


boxplot(bank.full$balance)
x <- bank.full$balance
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bank.full$balance=x
boxplot(bank.full$balance)
#

x <- bank.full$duration
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bank.full$duration=x
boxplot(bank.full$duration)


x <- bank.full$pdays
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bank.full$pdays=x
boxplot(bank.full$pdays)
#
x <- bank.full$campaign
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bank.full$campaign=x
boxplot(bank.full$campaign)
#
x <- bank.full$previous
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
bank.full$previous=x
boxplot(bank.full$previous)

library(plyr)
bank.full$default=revalue(bank.full$default,c("yes"="1","no"="0"))
bank.full$housing=revalue(bank.full$housing,c("yes"="1","no"="0"))
bank.full$loan=revalue(bank.full$loan,c("yes"="1","no"="0"))
bank.full$y=revalue(bank.full$y,c("yes"="1","no"="0"))

str(bank.full)
summary(bank.full)


##########standardisation#############


bank.full$age=scale(bank.full$age)
bank.full$balance=scale(bank.full$balance)
bank.full$day=scale(bank.full$day)
bank.full$month=scale(bank.full$month)
bank.full$duration=scale(bank.full$duration)
bank.full$campaign=scale(bank.full$campaign)
bank.full$pdays=scale(bank.full$pdays)
bank.full$previous=scale(bank.full$previous)





#model building

model.bank= glm(y~.,data = bank.full,family =binomial)
summary(model.bank)


library("MASS")
stepAIC(model.bank)
model.bank1= glm(y~(factor(job)+factor(marital)+factor(education)+factor(housing)+factor(loan)+factor(contact)+day+factor(month)+duration+campaign+factor(poutcome)),family = "binomial",data = bank.full[-41822,-42043,])
summary(model.bank1)



#to checck influencing variables
library(mvinfluence)
influence.measures(model.bank)#-41822,-42043 seem to be in fluential variBLES
influenceIndexPlot(model.bank)
influencePlot(model.bank)


##confusion matrix table
prob=predict(model.bank1,type=c("response"),bank.full)
prob
confusion =table(prob>0.8,bank.full$y)
confusion
confusion=as.data.frame(confusion)

#roc curve

install.packages("ROCR")
library(ROCR)
rocrpred= prediction(prob,bank.full$y)
rocrpref=performance(rocrpred,'tpr','fpr')
plot(rocrpref,colorize=T)   #CURVE IS COVERING MOST OF THE ARE SO ACCURATE



