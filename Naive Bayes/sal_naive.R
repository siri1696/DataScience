salary_data= read.csv(file.choose())
str(salary_data)
summary(salary_data)
#no na values

#finding outliers
boxplot(salary_data$age)
boxplot(salary_data$capitalgain)
boxplot(salary_data$capitalloss)
boxplot(salary_data$hoursperweek)#all of these vhave out;iers

#remvoing outliers
boxplot(salary_data$age)
x <-salary_data$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data$age=x
boxplot(salary_data$age)

boxplot(salary_data$capitalgain)
x <-salary_data$capitalgain
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data$capitalgain=x
boxplot(salary_data$capitalgain)

boxplot(salary_data$capitalloss)
x <-salary_data$capitalloss
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data$capitalloss=x
boxplot(salary_data$capitalloss)

boxplot(salary_data$hoursperweek)
x <-salary_data$hoursperweek
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data$hoursperweek=x
boxplot(salary_data$hoursperweek)

#standardisation
salary_data$age=scale(salary_data$age)
salary_data$capitalgain=scale(salary_data$capitalgain)
salary_data$capitalloss=scale(salary_data$capitalloss)
salary_data$hoursperweek=scale(salary_data$hoursperweek)

salary_data1=salary_data[-4]


#data splitting
library(caTools)
fraud_train= sample.split(salary_data1,SplitRatio = 0.80)
train1=subset(salary_data1,fraud_train==TRUE)
test1=subset(salary_data1,fraud_train==FALSE)

sal_train_labels <- train1[,13]
sal_test_labels <- test1[,13]


library(e1071)

sal_class= naiveBayes(train1,train1$Salary)
sal_class

#evaluating performence
sal_test_pred= predict(sal_class,test1)

table(sal_test_pred)
prop.table(table(sms_test_pred))#sal<=50k=75.3%,sal>50k=25%
           
           
library(gmodels)
CrossTable(sal_test_pred,test1$Salary,
                      prop.chisq = FALSE,prop.t = FALSE,prop.r = FALSE,
                      dnn = c('predicted','actual'))


mean(sal_test_pred==sal_test_labels) #98% accurate model

#2 are missclassified as >50k and 37 as<=50k