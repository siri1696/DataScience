forest_data=read.csv(file.choose())
str(forest_data)
forest_data1=forest_data[-1]
forest_data1=forest_data1[-1]
str(forest_data1)
summary(forest_data1)

#outlier detection and removal

boxplot(forest_data1$FFMC)
x <-forest_data1$FFMC
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$FFMC=x
boxplot(forest_data1$FFMC)


boxplot(forest_data1$DMC)
x <-forest_data1$DMC
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$DMC=x
boxplot(forest_data1$DMC)

boxplot(forest_data1$ISI)
x <-forest_data1$ISI
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$ISI=x
boxplot(forest_data1$ISI)

boxplot(forest_data1$temp)
x <-forest_data1$temp
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$temp=x
boxplot(forest_data1$temp)


boxplot(forest_data1$RH)
x <-forest_data1$RH
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$RH=x
boxplot(forest_data1$RH)

boxplot(forest_data1$wind)
x <-forest_data1$wind
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$wind=x
boxplot(forest_data1$wind)


boxplot(forest_data1$area)
x <-forest_data1$area
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
forest_data1$area=x
boxplot(forest_data1$area)


#creating dummy variables for the output variables as they are categorical data 
library(mlr)
forest_data1$size_category=createDummyFeatures(forest_data1$size_category)
summary(forest_data1)


#normalisation
normalize<-function(x){
  return ( (x-min(x))/(max(x)-min(x)))
}

forest_norm=as.data.frame(lapply(forest_data1,normalize))
summary(forest_norm)


#splitting of data
library(caTools)
forest_train= sample.split(forest_norm,SplitRatio = 0.80)
train1=subset(forest_norm,forest_train==TRUE)
test1=subset(forest_norm,forest_train==FALSE)



#feauture selection for model

library(Boruta)
# Decide if a variable is important or not using Boruta
boruta_output <- Boruta(size_category.large+size_category.large ~ ., data=na.omit(forest_norm), doTrace=2)  # perform Boruta search

boruta_signif <- names(boruta_output$finalDecision[boruta_output$finalDecision %in% c("Confirmed", "Tentative")])  # collect Confirmed and Tentative variables
print(boruta_signif)  # significant variables
#ffmc,isi,temp,area,monthdec

plot(boruta_output, cex.axis=.7, las=2, xlab="", main="Variable Importance")

# Using multilayered feed forward nueral network
# package nueralnet
# install.packages("neuralnet")
# install.packages("nnet")
library(neuralnet)  # regression
library(nnet) # classification 

# Building model
attach(train1)
forest_model <- neuralnet(size_category.large+size_category.small~FFMC+ISI+temp+area+monthdec,data =train1)
str(forest_model)
plot(forest_model)#0.28=ERROR,steps=4652

# SSE sum of squared errors . least SSE best model
# Evaluating model performance
# compute function to generate ouput for the model prepared
set.seed(12323)
model_results <- compute(forest_model,test1[1:28])
str(model_results)
predicted_results<- model_results$net.result
predicted_results=as.data.frame(predicted_results)
colnames(predicted_results)=c("large","small")
head(predicted_results)
result=colnames(predicted_results)[apply(predicted_results,1, which.max)]
result=as.data.frame(result)
forest_test_data=colnames(test1[,29:30])[apply(test1[,29:30],1,which.max)]
forest_test_data=as.data.frame(forest_test_data)
table(result$result,forest_test_data$forest_test_data)
               #   size_category.large   size_category.small
#  large                  29                   0
#  small                   1                  77
#this is the result  

#with 3 hidden layers

forest_model1 <- neuralnet(size_category.large+size_category.small~FFMC+ISI+temp+area+monthdec,data =train1,hidden =c(3))
str(forest_model)
plot(forest_model)


model_results1 <- compute(forest_model1,test1[1:28])
str(model_results1)
predicted_results1<- model_results1$net.result
predicted_results1=as.data.frame(predicted_results1)
colnames(predicted_results1)=c("large","small")
head(predicted_results1)
result1=colnames(predicted_results1)[apply(predicted_results1,1, which.max)]
result1=as.data.frame(result1)
forest_test_data1=colnames(test1[,29:30])[apply(test1[,29:30],1,which.max)]
forest_test_data1=as.data.frame(forest_test_data1)
table(result1$result1,forest_test_data1$forest_test_data1)

#even with hidden layers there is no much of difference any model works best


