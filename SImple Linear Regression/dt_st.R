

# Load delivery_time.csv dataset


library(readr)
dt_st <- read_csv(file.choose())
View(dt_st)

#####################Exploratory data analysis############################

#histogram

hist(dt_st$`Delivery Time`) #normally distributed
hist(dt_st$`Sorting Time`) #multi-modal distribution

#qqplot

qqnorm(dt_st$`Delivery Time`)
qqline(dt_st$`Delivery Time`) #most of the data falls on the line so both data comes from same distribution

qqnorm(dt_st$`Sorting Time`)
qqline(dt_st$`Sorting Time`)  #most of the data not on the line so different distributions

#boxplot

boxplot(dt_st$`Delivery Time`)
boxplot(dt_st$`Sorting Time`) #no outliers in boxplot


summary(dt_st)

#Scatter plot

plot(dt_st$`Sorting Time`, dt_st$`Delivery Time`)  # plot(X,Y)


attach(dt_st)
########################model building############################

#Correlation Coefficient (r)
cor(`Sorting Time`, `Delivery Time`)  #r = 0.82             # cor(X,Y)

# Simple Linear Regression model


reg <- lm(`Delivery Time` ~ `Sorting Time`) # lm(Y ~ X)

summary(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(dt_st))  #RMSE

sqrt(mean(reg$residuals^2))

confint(reg,level=0.95)
predict<-predict(reg,interval="confidence")
predict<-as.data.frame(predict)

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = dt_st, aes(x = `Sorting Time`, y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`, y=`Delivery Time`))

?ggplot2

########################
# A simple ggplot code for directly showing the line

# ggplot(wc_at,aes(Waist,AT))+stat_summary(fun.data=mean_cl_normal) + geom_smooth(method='lm')

####################

# Logrithamic Model

# x = log(Sorting_Time); y = DELIVERY_Time

plot(log(`Sorting Time`), `Delivery Time`)
cor(log(`Sorting Time`), `Delivery Time`)

reg_log <- lm(`Delivery Time`~ log(`Sorting Time`))   # lm(Y ~ X)

summary(reg_log) #multipuled r square is 69%
predict(reg_log)

reg_log$residuals
sqrt(sum(reg_log$residuals^2)/nrow(dt_st))  #RMSE= 2.733

confint(reg_log,level=0.95)
predict_1= predict(reg_log,interval="confidence") 

predict_1= as.data.frame(predict_1) 


######################

# Exponential Model

# x = sorting_time and y = log(Delivery_time)

plot(`Sorting Time`, log(`Delivery Time`))

cor(`Sorting Time`, log(`Delivery Time`))

reg_exp <- lm(log(`Delivery Time`) ~ `Sorting Time`)  #lm(log(Y) ~ X)

summary(reg_exp) #multipule r square value is =0.71

reg_exp$residuals

sqrt(mean(reg_exp$residuals^2)) 

logat <- predict(reg_exp)
at <- exp(logat)


error = dt_st$AT - at
error

sqrt(sum(error^2)/nrow(dt_st))  #RMSE

confint(reg_exp,level=0.95)
predict_2=predict(reg_exp,interval="confidence")
predict_2= as.data.frame(predict_2)

##############################
# Polynomial model with 2 degree (quadratic model)

plot(`Sorting Time`, `Delivery Time`)
plot(`Sorting Time`*`Sorting Time`, `Delivery Time`)

cor(`Sorting Time`* `Sorting Time`, `Delivery Time`) #r= 0.79

plot(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

cor(`Sorting Time`, log(`Delivery Time`))
cor(`Sorting Time`*`Sorting Time`, log(`Delivery Time`))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(`Delivery Time`) ~ `Sorting Time` + I(`Sorting Time`*`Sorting Time`))

summary(reg2degree) #multippule r square value= 0.76

logpol <- predict(reg2degree)
expy <- exp(logpol)

err = dt_st$`Delivery Time` - expy

sqrt(sum(err^2)/nrow(dt_st))  #RMSE =2.7

confint(reg2degree,level=0.95)
predict_3=predict(reg2degree,interval="confidence")
predict_3=as.data.frame(predict_3)

# visualization
ggplot(data = dt_st, aes(x = `Sorting Time` + I(`Sorting Time`^2), y = log(`Delivery Time`))) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`+I(`Sorting Time`^2), y=logpol))


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(`Delivery Time`)~`Sorting Time` + I(`Sorting Time`*`Sorting Time`) + I(`Sorting Time`*`Sorting Time`*`Sorting Time`))

summary(reg3degree) #multipule-r squre=0.78
logpol3 <- predict(reg3degree)
expy3 <- exp(logpol3)

confint(reg2degree,level=0.95)
predict_4=predict(reg2degree,interval="confidence")
predict_4=as.data.frame(predict_4)


# visualization
ggplot(data = dt_st, aes(x = `Sorting Time` + I(`Sorting Time`^2) + I(`Sorting Time`^3), y = `Delivery Time`)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = dt_st, aes(x=`Sorting Time`+I(`Sorting Time`^2)+I(`Sorting Time`^3), y=expy3))

################################
