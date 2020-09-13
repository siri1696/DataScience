

#reading calories_consumed.csv file

library(readr)
cal_con <- read.csv(file.choose())
View (cal_con)

# Exploratory data analysis

#histogram
hist(cal_con$Calories.Consumed)
hist(cal_con$Weight.gained..grams.)

#boplot
boxplot(cal_con$Weight.gained..grams.)
boxplot(cal_con$Calories.Consumed)

#qqline
qqnorm(cal_con$Weight.gained..grams.)
qqline(cal_con$Weight.gained..grams.)

qqnorm(cal_con$Calories.Consumed)
qqline(cal_con$Calories.Consumed)

summary(cal_con)

#Scatter plot
plot(cal_con$Weight.gained..grams., cal_con$Calories.Consumed) # plot(X,Y)

attach(cal_con)


#Correlation Coefficient (r)
cor(Calories.Consumed,Weight.gained..grams.)# cor(X,Y)

# Simple Linear Regression model
reg <- lm( Weight.gained..grams.~ Calories.Consumed) # lm(Y ~ X)

summary(reg)

pred_2 <- predict(reg,interval="prediction")
pred_2 <- as.data.frame(pred_2)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(cal_con))  #RMSE

predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

ggplot(data = cal_con, aes(x = Calories.Consumed, y = Weight.gained..grams.)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_con, aes(x=Calories.Consumed, y=Weight.gained..grams.))

?ggplot2

# Logrithamic Model

# x = log(calories_consumed); y = weight gained

plot(log(Calories.Consumed), Weight.gained..grams.)
cor(log(Calories.Consumed), Weight.gained..grams.)
reg_log <- lm(Weight.gained..grams. ~ log(Calories.Consumed))   # lm(Y ~ X)

summary(reg_log)
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(cal_con))  #RMSE

confint(reg_log,level=0.95)
predict(reg_log,interval="confidence")

######################

# Exponential Model

# x = calories_consumed and y = log(weight_gained)

plot(Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))

reg_exp <- lm(log(Weight.gained..grams.) ~ Calories.Consumed)  #lm(log(Y) ~ X)

summary(reg_exp)

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2))

logwg <- predict(reg_exp)
wg <- exp(logwg)

error = cal_con$Weight.gained..grams. - wg
error

sqrt(sum(error^2)/nrow(cal_con))  #RMSE

confint(reg_exp,level=0.95)
pred_3 <- predict(reg_exp,interval="confidence")
pred_3 <- as.data.frame(pred_3)  #sofar best model with 93%accuracy




##############Polynomial model with 2 degree (quadratic model)




plot(Calories.Consumed, Weight.gained..grams.)
plot(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

cor(Calories.Consumed*Calories.Consumed, Weight.gained..grams.)

plot(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

cor(Calories.Consumed, log(Weight.gained..grams.))
cor(Calories.Consumed*Calories.Consumed, log(Weight.gained..grams.))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Weight.gained..grams.) ~ Calories.Consumed + I(Calories.Consumed*Calories.Consumed))

summary(reg2degree)

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = cal_con$Weight.gained..grams. - expy

sqrt(sum(err^2)/nrow(cal_con))  #RMSE

confint(reg2degree,level=0.95)
pred_4 <- predict(reg2degree,interval="confidence")
pred_4 <- as.data.frame(pred_4)
# visualization
ggplot(data = cal_con, aes(x = Calories.Consumed + I(Calories.Consumed^2), y = log(Weight.gained..grams.))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = cal_con, aes(x=Calories.Consumed+I(Calories.Consumed^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Weight.gained..grams.)~Calories.Consumed + I(Calories.Consumed*Calories.Consumed) + I(Calories.Consumed*Calories.Consumed*Calories.Consumed))

summary(reg3degree)
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)
pred_5 <- predict(reg3degree,interval = "confidence")
pred_5 <-as.data.frame(pred_5)

#polynomial model is the best with multipule r square value = 0.97
