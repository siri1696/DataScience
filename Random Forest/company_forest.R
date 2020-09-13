company_data=read.csv(file.choose())
str(company_data)


#changing into categorical data
summary(company_data)#s,mean and median are quite similar seems to have no outliers
company_data$Sales=ifelse(company_data$Sales<=9,"highsales","lowsales")
company_data$Sales=as.factor(company_data$Sales)

##boxplot

boxplot(company_data$CompPrice)
boxplot(company_data$Income)
boxplot(company_data$Advertising)
boxplot(company_data$Population)
boxplot(company_data$Price)


#test for normality
shapiro.test(company_data$CompPrice)#normal distribution
shapiro.test(company_data$Income)#normal distribution
shapiro.test(company_data$Advertising)#normal distribution
shapiro.test(company_data$Population)
shapiro.test(company_data$Price)
shapiro.test(company_data$Age)

chisq.test(company_data$Sales,company_data$Urban)
scatterplot3d::scatterplot3d(company_data$Sales,company_data$CompPrice,company_data$Income)

#standardisation
company_data$CompPrice=scale(company_data$CompPrice)
company_data$Income=scale(company_data$Income)
company_data$Advertising=scale(company_data$Advertising)
company_data$Population=scale(company_data$Population)
company_data$Price=scale(company_data$Price)
company_data$Age=scale(company_data$Age)
company_data$Education=scale(company_data$Education)

#splitting the data
library(caTools)
company_train= sample.split(company_data,SplitRatio = 0.70)
train1=subset(company_data,company_train==TRUE)
test1=subset(company_data,company_train==FALSE)


# Building a random forest model on training data 

# Using Random Forest
install.packages("randomForest")
library(randomForest)

attach(train1)
fit.forest <- randomForest(Sales~.,data=train1, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(test1$Sales==predict(fit.forest,train1)) # 100% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,train1)
library(caret)


# Confusion Matrix
library(heuristica)
caret::confusionMatrix(train1$Sales,pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=test1)
mean(pred_test==test1$Sales) # Accuracy = 82%accurate% 


# Confusion Matrix 

confusionMatrix(test1$Sales, pred_test) #8 high sales are misclassified as low sales and 17lowsales are misclassified as high sales values 


plot(fit.forest)
legend("topright",colnames(fit.forest$err.rate),col=1:3,cex=0.8,fill=1:3)

acc_comp <- mean(company_data$Sales==predict(fit.forest))
acc_comp
varImpPlot(fit.forest)
#shelve location and the price company charges for car seats causes high sales


