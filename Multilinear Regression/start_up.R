library(readr)
start_up = read.csv(file.choose())
 
##############exploratory data analysis###################

str(start_up)# the variables except for state are numerical and state is categorical

####measures of central tendency 

#mean
mean(start_up$R.D.Spend)#73721
mean(start_up$Administration)#121344
mean(start_up$Marketing.Spend)#211025
mean(start_up$Profit)#112012

summary(start_up)
#here we get mean and meadian of all the variables the range an 1st and 3rd quaartile range 
#as state is a catergorical data we will get the count of the states

########measures of dispersion 
#varience
var(start_up$R.D.Spend) #2107017150
var(start_up$Administration)#784997271
var(start_up$Marketing.Spend)#14954920097
var(start_up$Profit)#1624588173

#standard deviation

sd(start_up$R.D.Spend)#45902.26
sd(start_up$Administration)#28017.8
sd(start_up$Marketing.Spend)#122290.3
sd(start_up$Profit)#40306.18

#range

range((start_up$R.D.Spend))#0.0 165349.2
range(start_up$Administration)#51283.14 182645.56
range(start_up$Marketing.Spend)#0.0 471784.1
range(start_up$Profit)#14681.4 192261.8

############3rd and 4th moments of business decisons 
 install.packages("moments")
 library(moments)
 
#skewness
 
 skewness(start_up$R.D.Spend)#0.1590 ,as these values are between 0.5to -0.5 these are approximately symeetric
 skewness(start_up$Administration)#-0.4742301
 skewness(start_up$Marketing.Spend)#-0.04506632
 skewness(start_up$Profit)#0.02258638
 
#kurtosis
 kurtosis(start_up$R.D.Spend)#2.194932
 kurtosis(start_up$Administration)#3.085538
 kurtosis(start_up$Marketing.Spend)#2.275967
 kurtosis(start_up$Profit)#2.824704
 
###########data visualization
#histogram
 hist(start_up$R.D.Spend)
 hist(start_up$Administration)
 hist(start_up$Marketing.Spend)
 hist(start_up$Profit)

#barplot
 barplot(start_up$R.D.Spend)
 barplot(start_up$Administration)
 barplot(start_up$Marketing.Spend)
 barplot(start_up$Profit)
 
#boxplot
 boxplot(start_up$R.D.Spend)
 boxplot(start_up$Administration)
 boxplot(start_up$Marketing.Spend)
 boxplot(start_up$Profit)

summary(start_up)

install.packages("dplyr")
library(dplyr)

#scatterplotfor y with input variables
start_up1 = select(start_up,c(1,2,3,5))
pairs(start_up1)

#correlation matrix 
cor(start_up1)



##########model building
model.startup = lm(Profit~.,data=start_up1)
summary(model.startup)
#with all the three variable Rsquare is 95% and adjusted r squared is 94%
#r.d spend and marketing spend are high significant values

attach(start_up1)

#profit with R and D
model.startupr<- lm(Profit~ R.D.Spend)
summary(model.startupr)#  with only r and d spend the Rsquare value is 94% and adjusted r square is 94%

#profit with marketing 
model.startupm = lm(Profit~Marketing.Spend)
summary(model.startupm) #these also gives very low mulipule r squared values and adjusted r square values of 55% respectively

#lets consider model of profit with both R&D and marketing spend
model.startuprm = lm(Profit~ R.D.Spend+Marketing.Spend)
summary(model.startuprm) # R SQUARED =95% and adjusted r square= 94% but marketing spend is highly significant because it is 0.06>0.05

#diagnostic plots
install.packages("car")
library(car)

install.packages("corpcor")
library(corpcor)
cor2pcor(cor(start_up1))


#identification of influetial variables
 influence.measures(model.startup)# showing 50,49 th variable to be influential
 influenceIndexPlot(model.startup)
 influencePlot(model.startup)

#regression after deleting 50th and 49th observation
 attach(start_up1)
 model.startup1 = lm(Profit~ .,data = start_up1[-50,-49])
 summary(model.startup1) #still administration shows high significance
 
 
 attach(start_up1)
 model.startup1 = lm(Profit~ R.D.Spend+Marketing.Spend,data = start_up1[-50,-49])
summary(model.startup1) #now marketing is significant when added with R&D 
#multipule r square= 96% and adjusted r square value is 95% #best model

#varience inflation factor 
vif(model.startup) #all are less than 10 so no collinoearity
avPlots(model.startup1,id.n=2,id.coex=0.8,col="red")


