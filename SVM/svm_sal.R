salary_data_test= read.csv(file.choose())
salary_data_test=read.csv(file.choose())


str(salary_data_test)
str(salary_data_test)


summary(salary_data_test)
summary(salary_data_test)
is.na(salary_data_test)

#finding outliers
boxplot(salary_data_test$age)
boxplot(salary_data_test$capitalgain)
boxplot(salary_data_test$capitalloss)
boxplot(salary_data_test$hoursperweek)#all of these values have outliers

boxplot(salary_data_test$age)
boxplot(salary_data_test$capitalgain)
boxplot(salary_data_test$capitalloss)
boxplot(salary_data_test$hoursperweek)#all of these values have outliers


#remvoing outliers



boxplot(salary_data_test$age)
x <-salary_data_test$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$age=x
boxplot(salary_data_test$age)

boxplot(salary_data_test$capitalgain)
x <-salary_data_test$capitalgain
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$capitalgain=x
boxplot(salary_data_test$capitalgain)

boxplot(salary_data_test$capitalloss)
x <-salary_data_test$capitalloss
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$capitalloss=x
boxplot(salary_data_test$capitalloss)

boxplot(salary_data_test$hoursperweek)
x <-salary_data_test$hoursperweek
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$hoursperweek=x
boxplot(salary_data_test$hoursperweek)

#########

boxplot(salary_data_test$age)
x <-salary_data_test$age
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$age=x
boxplot(salary_data_test$age)

boxplot(salary_data_test$capitalgain)
x <-salary_data_test$capitalgain
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$capitalgain=x
boxplot(salary_data_test$capitalgain)

boxplot(salary_data_test$capitalloss)
x <-salary_data_test$capitalloss
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$capitalloss=x
boxplot(salary_data_test$capitalloss)

boxplot(salary_data_test$hoursperweek)
x <-salary_data_test$hoursperweek
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
salary_data_test$hoursperweek=x
boxplot(salary_data_test$hoursperweek)






#standardisation
salary_data_test$age=scale(salary_data_test$age)
salary_data_test$capitalgain=scale(salary_data_test$capitalgain)

salary_data_test$hoursperweek=scale(salary_data_test$hoursperweek)


salary_data_test$age=scale(salary_data_test$age)
salary_data_test$capitalgain=scale(salary_data_test$capitalgain)

salary_data_test$hoursperweek=scale(salary_data_test$hoursperweek)



library(klaR)

library(e1071)
library(kernlab)
library(caret)
model1<-ksvm(Salary ~.,data = salary_data_train,kernel = "vanilladot")
model1

help(kvsm)
??kvsm

# Different types of kernels 
# "rbfdot", "polydot", "tanhdot", "vanilladot", "laplacedot", 
# "besseldot", "anovadot", "splinedot", "matrix"

# kernel = rfdot 
model_rfdot<-ksvm(Salary ~.,data = salary_data_train,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=salary_data_test)
mean(pred_rfdot==salary_data_test$Salary ) # 84.075

# kernel = vanilladot
model_vanilla<-ksvm(Salary  ~.,data = salary_data_train,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=salary_data_test)
mean(pred_vanilla==salary_data_test$Salary ) #83.075


# kernal = besseldot
model_besseldot<-ksvm(Salary  ~.,data = salary_data_train,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=salary_data_test)
mean(pred_bessel==salary_data_test$Salary )#68 % accuracy

# kernel = polydot

model_poly<-ksvm(Salary  ~.,data = salary_data_train,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = salary_data_test)
mean(pred_poly==salary_data_test$Salary ) # 83.925

#RBFDOTmodel seems to have highest accuracy with 84%


