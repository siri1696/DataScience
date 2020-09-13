library(readr)
tc = read.csv(file.choose())

install.packages("dplyr")
library(dplyr)

#importing required variables for analysis
tc1 = select(tc,c(3,4,7,9,13,14,16,17,,18))

############EDA################

#performing standardisation 
tc2= scale(tc1) 
tc2= as.data.frame(tc2)
 

summary(tc2) # outliers in the data are there


############3rd and 4th moments of business decisons 
install.packages("moments")
library(moments)

#skewness
attach(tc2)

skewness(tc2)#most of the values are not between 0.5 and -0.5 so not symmentrical 

kurtosis(tc2)

###########data visualization
#histogram
hist(tc2$Price)#skewed to right 
hist(tc2$Age)#skewed to left
hist(tc2$KM)
hist(tc2$HP)
hist(tc2$cc)
hist(tc2$Doors)
hist(tc2$Gears)
hist(tc2$QT)


#boxplot
boxplot(tc2$Price)#outliers in data
boxplot(tc2$Age)
boxplot(tc2$KM)
boxplot(tc2$HP)
boxplot(tc2$cc)
boxplot(tc2$Doors)
boxplot(tc2$Gears)
boxplot(tc2$QT)

#scatterplot
pairs(tc2)

#correlation matrix 
cor(tc2)

#############model building

attach(tc2)
model.tc=lm(Price~.,data=tc2)
summary(model.tc) #cc ,doors show high significance


install.packages("corpcor")
library(corpcor)
cor2pcor(cor(start_up1))



##choosing stepaic function to choose the best model

library("MASS")
stepAIC(model.tc)
#here  del cc and doors



#identification of influetial variables
influence.measures(model.tc)# showing 81st variable to be influential
influenceIndexPlot(model.tc)
influencePlot(model.tc)

#regression after deleting 81st observation

attach(tc2)
model.tc1 = lm(Price~Age+KM+HP+Gears+QT+WT,data = tc2[-81])
summary(model.tc1)

confint(model.tc1,level=0.95)
model.final=predict(model.tc1,interval="confidence")
model.final= as.data.frame(model.final)


#multipule R suqre value =86% and adjusted r square value is =86%



#performing ############transformations#### for better accuracy

# Logrithamic Model

# x = log(age+km+hp+gears+qt+wt); y = price

plot(log(Age+KM+HP+Gears+QT+WT), Price)
cor(log(Age+KM+HP+Gears+QT+WT),Price)

reg_log <- lm(Price ~ log(Age+KM+HP+Gears+QT+WT))   # lm(Y ~ X)

summary(reg_log)

# VERY LESS multipule r squared value and adjusted r square value'


# Exponential Model

# x = Age+KM+HP+Gears+QT+WT and y = log(price)

plot(Age+KM+HP+Gears+QT+WT, log(Price))

cor(Age+KM+HP+Gears+QT+WT, log(Price))

reg_exp <- lm(log(Price) ~ Age+KM+HP+Gears+QT+WT)  #lm(log(Y) ~ X)

summary(reg_exp)

#multipuled r square value is 63% and adjusted r square value is 63%


##############################
# Polynomial model with 2 degree (quadratic model)

plot(Age+KM+HP+Gears+QT+WT,Price)
plot(Age+KM+HP+Gears+QT+WT*Age+KM+HP+Gears+QT+WT, Price)

cor(Age+KM+HP+Gears+QT+WT*Age+KM+HP+Gears+QT+WT, Price)

plot(Age+KM+HP+Gears+QT+WT*Age+KM+HP+Gears+QT+WT, log(Price))

cor(Age+KM+HP+Gears+QT+WT, log(Price)
cor(Age+KM+HP+Gears+QT+WT*Age+KM+HP+Gears+QT+WT, log(Price))

# lm(Y ~ X + I(X*X) +...+ I(X*X*X...))

reg2degree <- lm(log(Price) ~ Age+KM+HP+Gears+QT+WT + I(Age+KM+HP+Gears+QT+WT*Age+KM+HP+Gears+QT+WT))

summary(reg2degree)

# both R SQUARE  and adjusted r square are 65% and 64%


##############################
#  Polynomial model with 3 degree

reg3degree<-lm(log(Price)~Age+KM+HP+Gears+QT+WT  + I(Age+KM+HP+Gears+QT+WT *Age+KM+HP+Gears+QT+WT ) + I(Age+KM+HP+Gears+QT+WT *Age+KM+HP+Gears+QT+WT *Age+KM+HP+Gears+QT+WT ))

summary(reg3degree)


############model.final is the best model with 86% suqare value and adjusted r square value






 