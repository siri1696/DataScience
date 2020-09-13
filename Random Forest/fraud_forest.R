#importing the data set
fraud_data= read.csv(file.choose())
str(fraud_data)
summary(fraud_data)


#boxplot
boxplot(fraud_data$City.Population)#no outlierss
boxplot(fraud_data$Work.Experience)#no outliers

#checking for na values
is.na(fraud_data)

shapiro.test(fraud_data$City.Population)#normal distribution
shapiro.test(fraud_data$Work.Experience)#normal distribution


#converting to categorical data
fraud_data$Taxable.Income=ifelse(fraud_data$Taxable.Income<= 30000,"risky","good")
fraud_data$Taxable.Income= as.factor(fraud_data$Taxable.Income)


#standardisation of the data
fraud_data$City.Population=scale(fraud_data$City.Population)
fraud_data$Work.Experience=scale(fraud_data$Work.Experience)


#splitting the data
library(caTools)
fraud_train= sample.split(fraud_data,SplitRatio = 0.80)
train1=subset(fraud_data,fraud_train==TRUE)
test1=subset(fraud_data,fraud_train==FALSE)


# Building a random forest model on training data 

# Using Random Forest
install.packages("randomForest")
library(randomForest)


fit.forest <- randomForest(Taxable.Income~.,data=test1, na.action=na.roughfix,importance=TRUE)
# Training accuracy 
mean(test1$Taxable.Income==predict(fit.forest,test1)) # 95% accuracy 

# Prediction of train data
pred_train <- predict(fit.forest,test1)
library(caret)


# Confusion Matrix
library(heuristica)
caret::confusionMatrix(test1$Taxable.Income,pred_train)


# Predicting test data 
pred_test <- predict(fit.forest,newdata=iris_test)
mean(pred_test==iris_test$Species) # Accuracy = 94.6 % 


# Confusion Matrix 

confusionMatrix(iris_test$Species, pred_test) #8 values are missclassified

# Visualization 
plot(fit.forest)
legend("topright", colnames(fit.forest$err.rate),col=1:4,cex=0.8,fill=1:4)


