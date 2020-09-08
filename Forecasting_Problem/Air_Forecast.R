library(readxl)
airlines_data= read_xlsx(file.choose(), 1)
plot(airlines_data$Passengers,type ="o")
#primarily wheni observe the graph seems to have trend and multiplicative seasonality but lets find out thre same through the models while building 

#creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 96), month.abb,"==") + 0 ) # Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
airdata<-cbind(airlines_data,X)
View(airdata)
colnames(airdata)

#input"t"

airdata["t"]<- 1:96
View(airdata)

airdata["log_passengers"]<-log(airdata["Passengers"])
airdata["t_square"]<-airdata["t"]*airdata["t"]
attach(airdata)

train<-airdata[1:84,]

test<-airdata[85:96,] #because we need one full cycle of a year in test data
########################### LINEAR MODEL #############################

linear_model<-lm(Passengers~t,data=train)
summary(linear_model)
#adjusted r square value is  78.9% and multipule r squared value is 79% 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Passengers-linear_pred$fit)^2,na.rm = T))
rmse_linear # 53 



######################### Exponential #################################

expo_model<-lm(log_passengers~t,data=train)
summary(expo_model)
#R-square value is 82% and adjusted r squared value is 82%
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Passengers-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo # 46 

######################### Quadratic ####################################

Quad_model<-lm(Passengers~t+t_square,data=train)
summary(Quad_model)
#multipule r suqred value and adjusted r squared value are 79.6 and 79.2 respectively
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Passengers-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 48.05


# in the above models exponential model gives the least rmse value with 
#46 so model seems to be having exponential trend  

######################### Additive Seasonality #########################

sea_add_model<-lm(Passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Passengers-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add # 132
#no that we knw there is seasonality component for our data 
#lets try it with additive seasonality with exponential


######################## Additive Seasonality with exponential #################

Add_sea_expo_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_expo_model)
#multipule r squared value is 97% and adjusted r squared avlue is 97%

Add_sea_expo_pred<-data.frame(predict(Add_sea_expo_model,interval='predict',newdata=test))
rmse_Add_sea_expo<-sqrt(mean((test$Passengers-Add_sea_expo_pred$fit)^2,na.rm=T))
rmse_Add_sea_expo # 325.69

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Passengers~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
#adjusted r squared value and multipuled r square value is 95
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Passengers-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad # 26

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_passengers~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Passengers-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea # 140

######################## Multiplicative Seasonality exponential trend ##########################

multi_add_sea_model<-lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_expo_sea<-sqrt(mean((test$Passengers-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_expo_sea # 10

#the data has exponential trend with multiplicative seasonality 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_Add_sea_expo","rmse_multi_sea","rmse_multi_expo_sea"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_expo,rmse_multi_sea,rmse_multi_expo_sea))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

##in the model table we can see that multiplicative seasonality with exponential trend 
##as the least value as 10###

#multiplicative seasonality with exponential trend has least RMSE value 
#so that will be our new model

new_model <- lm(log_passengers~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=airdata)


resid <- residuals(new_model)
resid[1:10]
windows()
acf(resid,lag.max = 10)
# By principal of parcimony we will consider lag - 1  as we have so 
# many significant lags 
# Building Autoregressive model on residuals consider lag-1 

k <- arima(resid, order=c(1,0,0))
str(k)

View(data.frame(res=resid,newresid=k$residuals))
windows()
acf(k$residuals,lag.max = 15)
pred_res<- as.data.frame(predict(arima(k$residuals,order=c(1,0,0)),n.ahead = 12))
str(pred_res)
pred_res$pred
acf(k$residuals)
write.csv(airdata,file="airdata.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
test_data<-read.csv(file.choose())
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)

