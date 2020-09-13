#importing data
forest_data=read.csv(file.choose())
str(forest_data)
summary(forest_data)
boxplot(forest_data)

#removing the outliers
boxplot(forest_data$FFMC)
x <-forest_data$FFMC
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$FFMC=x
boxplot(forest_data$FFMC)


boxplot(forest_data$DMC)
x <-forest_data$DMC
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$DMC=x
boxplot(forest_data$DMC)

boxplot(forest_data$ISI)
x <-forest_data$ISI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$ISI=x
boxplot(forest_data$ISI)

boxplot(forest_data$temp)
x <-forest_data$temp
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$temp=x
boxplot(forest_data$temp)


boxplot(forest_data$RH)
x <-forest_data$RH
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$RH=x
boxplot(forest_data$RH)

boxplot(forest_data$wind)
x <-forest_data$wind
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$wind=x
boxplot(forest_data$wind)


boxplot(forest_data$area)
x <-forest_data$area
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data$area=x
boxplot(forest_data$area)

forest_data1=forest_data[-1]
forest_data1=forest_data1[-1]

#scaling the data wuth standardisation
forest_data1$FFMC=scale(forest_data1$FFMC)
forest_data1$DMC=scale(forest_data1$DMC)
forest_data1$DC=scale(forest_data1$DC)
forest_data1$ISI=scale(forest_data1$ISI)
forest_data1$temp=scale(forest_data1$temp)
forest_data1$RH=scale(forest_data1$RH)
forest_data1$wind=scale(forest_data1$wind)
forest_data1$rain=scale(forest_data1$rain)
forest_data1$area=scale(forest_data1$area)

#splitting the data to test and train 
library(caTools)
forest_train= sample.split(forest_data1,SplitRatio = 0.80)
train1=subset(forest_data1,forest_train==TRUE)
test1=subset(forest_data1,forest_train==FALSE)


#model building with difeerent kernals 
# it improve the accuracy and check with the best model
library(klaR)

library(e1071)
library(kernlab)
library(caret)
model1<-ksvm(size_category ~.,data = train1,kernel = "vanilladot")
model1
# kernel = rfdot 
model_rfdot<-ksvm(size_category~.,data = train1,kernel = "rbfdot")
pred_rfdot<-predict(model_rfdot,newdata=test1)
mean(pred_rfdot==test1$size_category) # 85.075

# kernel = vanilladot
model_vanilla<-ksvm(size_category ~.,data = train1,kernel = "vanilladot")
pred_vanilla<-predict(model_vanilla,newdata=test1)
mean(pred_vanilla==test1$size_category) # 96.22


# kernal = besseldot
model_besseldot<-ksvm(size_category  ~.,data = train1,kernel = "besseldot")
pred_bessel<-predict(model_besseldot,newdata=test1)
mean(pred_bessel==test1$size_category)#61%

# kernel = polydot

model_poly<-ksvm(size_category ~.,data = train1,kernel = "polydot")
pred_poly<-predict(model_poly,newdata = test1)
mean(pred_poly==test1$size_category) # 96.22


#so the kernals vanilladot and pot with  the models model_vanilla and model_poly respectively are preferable models with 96% acuuracy







