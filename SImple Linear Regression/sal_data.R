# Load salary_data.csv dataset
library(readr)
sal_data<- read_csv(file.choose())
View(sal_data)

# ##############################Exploratory data analysis
#histogram

hist(sal_data$YearsExp) # not normally distributed
hist(sal_data$Salary) #not normally distribution

#qqplot

qqnorm(sal_data$YearsExp)
qqline(sal_data$YearsExp)  #most of the data does not falls on the line so both data comes from different distribution

qqnorm(sal_data$Salary)
qqline(sal_data$Salary)  #most of the data not on the line so different distributions

#boxplot

boxplot(sal_data$YearsExp)
boxplot(sal_data$Salary) #no outliers in boxplot


summary(sal_data)


#######standardization###

sal_data1 = scale(sal_data)
sal_data1 = as.data.frame(sal_data1)
summary(sal_data1)

#Scatter plot

plot(sal_data1$YearsEx, sal_data1$`Delivery Time`)  # plot(X,Y)


summary(sal_data1)




attach(sal_data1)


#Correlation Coefficient (r)
cor(YearsExp, Salary)     #r=0.97        # cor(X,Y)

# Simple Linear Regression model
reg <- lm(Salary ~ YearsExp) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals) #1.3

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(sal_data1))  #RMSE 0.2

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
pred_1=predict(reg,interval="confidence")
pred_1=as.data.frame(pred_1)

# ggplot for adding regresion line for data
library(ggplot2)



ggplot(data = sal_data1, aes(x = YearsExp, y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_data1, aes(x=YearsExp, y=pred))



########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(YearsExp); y = Salary

plot(log(YearsExp), Salary)
cor(log(YearsExp), Salary)

reg_log <- lm(Salary ~ log(YearsExp))   # lm(Y ~ X)

summary(reg_log) #multipule r square=0.84 pr(>|t|)=57e-09,37e-05
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(sal_data1))  #RMSE =0.11

confint(reg_log,level=0.95)
pred_2=predict(reg_log,interval="confidence")
pred_2= as.data.frame(pred_2)

######################

# Exponential Model

# x = YearsExp and y = log(Salary)

plot(YearsExp, log(Salary))

cor(YearsExp, log(Salary))

reg_exp <- lm(log(Salary) ~ YearsExp)  #lm(log(Y) ~ X)

summary(reg_exp) #multipule r square =0.79

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logat <- predict(reg_exp)
at <- exp(logat)

error = sal_data1$Salary - at
error

sqrt(sum(error^2)/nrow(sal_data1))  #RMSE =1.41

confint(reg_exp,level=0.95)
pred_3=predict(reg_exp,interval="confidence")
pred_3=as.data.frame(pred_3)


##############################
# Polynomial model with 2 degree (quadratic model)

plot(YearsExp, Salary)
plot(YearsExp*YearsExp, Salary)

cor(YearsExp*YearsExp, Salary) r=0.36

plot(YearsExp*YearsExp, log(Salary))

cor(YearsExp, log(Salary))
cor(YearsExp*YearsExp, log(Salary))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Salary) ~ YearsExp + I(YearsExp*YearsExp))

summary(reg2degree) #multipule r square =0.85

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = sal_data1$Salary - expy

sqrt(sum(err^2)/nrow(sal_data1))  #RMSE=1.3

confint(reg2degree,level=0.95)
pred_4=predict(reg2degree,interval="confidence")
pred_4=as.data.frame(pred_4)


# visualization

ggplot(data = sal_data1, aes(x = YearsExp + I(YearsExp^2), y = log(Salary))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_data1, aes(x=YearsExp+I(YearsExp^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Salary)~YearsExp + I(YearsExp*YearsExp) + I(YearsExp*YearsExp*YearsExp))

summary(reg3degree) #multipule square value =0.85
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

confint(reg3degree,level=0.95)
pred_5=predict(reg3degree,interval="confidence")
pred_5=as.data.frame(pred_4)

##########logarithmic modelling is best##################


# visualization
ggplot(data = sal_data1, aes(x = YearsExp + I(YearsExp^2) + I(YearsExp^3), y = Salary)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = sal_data1, aes(x=YearsExp+I(YearsExp^2)+I(YearsExp^3), y=expy3))

################################
