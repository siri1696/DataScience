library(readr)
comp_data = read.csv(file.choose())
comp_data1 =select(comp_data,c(-1))
##############exploratory data analysis###################

str(comp_data1)
 
summary(comp_data)


#converting the categorical data to discrete 
install.packages("plyr")
require(plyr)

comp_data1$cd = revalue(comp_data1$cd,c("yes"="1","no"="2"))
comp_data1$multi=revalue(comp_data1$multi,c("yes"="1","no"="2"))
comp_data1$premium=revalue(comp_data1$premium,c("yes"="1","no"="2"))

#changing the data type to int 
comp_data1$cd = as.integer(comp_data1$cd)
comp_data1$multi = as.integer(comp_data1$multi)
comp_data1$premium = as.integer(comp_data1$premium)

#################standardising data

comp_data1=scale(comp_data1)
comp_data1= as.data.frame(comp_data1)


############
install.packages("moments")
library(moments)

#skewness

skewness(comp_data1$price)#0.7
skewness(comp_data1$speed)#0.6
skewness(comp_data1$hd)#1.3
skewness(comp_data1$ram)#1.3
skewness(comp_data1$screen)#1.6
skewness(comp_data1$ads)#-0.5
skewness(comp_data1$trend)#0.2 #only ads and trend,cd are approximately symmetric
skewness(comp_data1$cd)#0.14
skewness(comp_data1$multi)#2.8
skewness(comp_data1$premium)#-2.7


#kurtosis
kurtosis(comp_data1$price)#3.7
kurtosis(comp_data1$speed)#2.7
kurtosis(comp_data1$hd)#5.4
kurtosis(comp_data1$ram)#4.4
kurtosis(comp_data1$screen)#4.8
kurtosis(comp_data1$ads)#2.4
kurtosis(comp_data1$trend)#2.3

###########data visualization
#histogram
hist(comp_data1$price)
hist(comp_data1$speed)
hist(comp_data1$hd)
hist(comp_data1$ram)
hist(comp_data1$screen)
hist(comp_data$ads)
hist(comp_data1$trend)


#boxplot
boxplot(comp_data1$price)#outliers
boxplot(comp_data1$speed)
boxplot(comp_data$hd)#outliers
boxplot(comp_data1$ram)
boxplot(comp_data1$screen)#outliers
boxplot(comp_data1$ads)
boxplot(comp_data1$trend)


summary(comp_data1)


library(dplyr)

attach(comp_data1)

#scatterplotfor y with input variables

pairs(comp_data1)

#correlation matrix

cor(comp_data1)


##############model building

attach(comp_data1)
model.comp=lm(price~.,data=comp_data1)
summary(model.comp) 
#Rsquare value=77% and adjusted r sqaure valueis =77%


library("MASS")
stepAIC(model.comp)
# we can model with have all the variables


#identification of influetial variables

install.packages("mvinfluence")
library(mvinfluence)
influence.measures(model.comp)# showing 1701 and 1441st variable to be influential
influenceIndexPlot(model.comp)
influencePlot(model.comp)

#regression after deleting 81st observation

attach(comp_data1)
model.comp2 = lm(price~.,data = comp_data1[-1701,-1441])
summary(model.comp2)

confint(model.comp2,level=0.95)
model.final=predict(model.comp2,interval="confidence")
model.final= as.data.frame(model.final)


# R square value and adjusted r square value are 77%

##########transformation


# Exponential Model

######## Exponential Model

# x =  and y = log(price)

plot(speed+hd+ram+screen+cd+multi+premium+ads+trend,log(price))

cor(speed+hd+ram+screen+cd+multi+premium+ads+trend, log(price))

reg_exp <- lm(log(price) ~ speed+hd+ram+screen+cd+multi+premium+ads+trend)  #lm(log(Y) ~ X)

summary(reg_exp)

######multipule r square value is 47% adjusted r squared 47%

##############################
# Polynomial model with 2 degree (quadratic model)

plot(speed+hd+ram+screen+cd+multi+premium+ads+trend,price)
plot(speed+hd+ram+screen+cd+multi+premium+ads+trend*speed+hd+ram+screen+cd+multi+premium+ads+trend)
cor(speed+hd+ram+screen+cd+multi+premium+ads+trend*speed+hd+ram+screen+cd+multi+premium+ads+trend,price)

plot(speed+hd+ram+screen+cd+multi+premium+ads+trend*speed+hd+ram+screen+cd+multi+premium+ads+trend, log(price))

cor(speed+hd+ram+screen+cd+multi+premium+ads+trend, log(price)
cor(speed+hd+ram+screen+cd+multi+premium+ads+trend*speed+hd+ram+screen+cd+multi+premium+ads+trend, log(price))
    
    # lm(Y ~ X + I(X*X) +...+ I(X*X*X...))
    
reg2degree <- lm(log(price) ~ speed+hd+ram+screen+cd+multi+premium+ads+trend + I(speed+hd+ram+screen+cd+multi+premium+ads+trend*speed+hd+ram+screen+cd+multi+premium+ads+trend))
summary(reg2degree)
    
    # both R SQUARE  and adjusted r square are 48% and 48%
    
    
##############################
    #  Polynomial model with 3 degree
reg3degree<-lm(log(price)~speed+hd+ram+screen+cd+multi+premium+ads+trend  + I(speed+hd+ram+screen+cd+multi+premium+ads+trend *Age+KM+HP+Gears+QT+WT ) + I(Age+KM+HP+Gears+QT+WT *speed+hd+ram+screen+cd+multi+premium+ads+trend*speed+hd+ram+screen+cd+multi+premium+ads+trend ))
    
summary(reg3degree)
    
    
    ############model.final is the best model with 77% suqare value and adjusted r square value 77%
    
    
    
    
    
    
    





