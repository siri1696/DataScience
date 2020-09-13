# Load emp_data.csv dataset
library(readr)
emp_data<- read_csv(file.choose())


# ##############################Exploratory data analysis
#histogram

hist(emp_data$Sal_hike) # not normally distributed
hist(emp_data$Cor) #not normally distribution

#qqplot

qqnorm(emp_data$Sal_hike)
qqline(emp_data$Sal_hike)  #most of the data  falls on the line so both data comes from same distribution

qqnorm(emp_data$Cor)
qqline(emp_data$Cor)  #most of the data  on the line so same distributions

#boxplot

boxplot(emp_data$Cor)
boxplot(emp_data$Sal_hike) #no outliers in boxplot


summary(emp_data)

###########scatter plot

plot(emp_data$Sal_hike, emp_data$Cor)  # plot(X,Y)


attach(emp_data)


#Correlation Coefficient (r)
cor(Sal_hike, Cor)     #r= -0.911 if |r|>0.85 then correlation is strong this has strong negative correlation       # cor(X,Y)

#################  Simple Linear Regression model  #############

reg <- lm(Cor ~ Sal_hike) # lm(Y ~ X)

summary(reg)

##probability value should be less than 0.05 here it is 1.96e-05
#multipuled r square value is 0.83 which is greter than 0.8(in general)
#adjusted r  square value is 0.81
#the probability value for F statistic is 0.0002386 which is less than 0.05

pred <- predict(reg)

reg$residuals
sum(reg$residuals) # -4.4

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(emp_data))  #RMSE 3.39

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
# obtained two equations let us calculate lower range and upper range 

pred_1=predict(reg,interval="confidence")
pred_1=as.data.frame(pred_1)

# ggplot for adding regresion line for data
library(ggplot2)



ggplot(data = emp_data, aes(x = Sal_hike, y = Cor)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Sal_hike, y=Cor))

#for better Rsquare value lets perform transformations on this model 
#adjusted R square value is 0.81

##################Logrithamic Model############

# x = log(sal_hike); y = cor

plot(log(Sal_hike), Cor)
cor(log(Sal_hike), Cor) #r = -0.9

reg_log <- lm(Cor ~ log(Sal_hike))   # lm(Y ~ X)

summary(reg_log) 

##multipule r square=0.84 
#pr(>|t|)=57e-09,37e-05
#adjusted r squre value = 0.82
#p value is 0.00015

predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(emp_data))  #RMSE = 3.78

confint(reg_log,level=0.95)
pred_2=predict(reg_log,interval="confidence")
pred_2= as.data.frame(pred_2)

#adjusted r squre value = 0.82

####################### Exponential Model

# x = Sal_hike and y = log(cor)              

plot(Sal_hike, log(Cor))

cor(Sal_hike, log(Cor)) r= 0.9 

reg_exp <- lm(log(Cor) ~ Sal_hike)  #lm(log(Y) ~ X)

summary(reg_exp) 
#####multipule r square =0.87
###adjusted r square value  is 0.85
#p- value 7.377e-05


confint(reg_exp,level=0.95)
pred_3=predict(reg_exp,interval="confidence")
pred_3=as.data.frame(pred_3)

# adjusted R square vakue is 0.87


##############################
# Polynomial model with 2 degree (quadratic model)

plot(Sal_hike, Cor)
plot(Sal_hike*Sal_hike,Cor)



plot(Sal_hike*Sal_hike, log(Cor))


cor(Sal_hike*Sal_hike, log(Cor))  #r=-0.92

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Cor) ~ Sal_hike + I(Sal_hike*Sal_hike))

summary(reg2degree) 

#multipule r square =0.98
#adjusted r square  is = 0.97
#p value is  5.634e-07

confint(reg2degree,level=0.95)
pred_4=predict(reg2degree,interval="confidence")
pred_4=as.data.frame(pred_4)


# visualization

ggplot(data = emp_data, aes(x = Sal_hike + I(Sal_hike^2), y = log(Cor))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = emp_data, aes(x=Sal_hike+I(Sal_hike^2), y=logpol))


###############################  Polynomial model with 3 degree

reg3degree<-lm(log(Sal_hike)~Cor + I(Sal_hike*Sal_hike) + I(Sal_hike*Sal_hike*Sal_hike))
summary(reg3degree) 
#multipule square value & 
#adjusted r square value is 1

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
