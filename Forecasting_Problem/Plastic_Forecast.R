library(readxl)
plastic_data= read.csv(file.choose())
plot(plastic_data$Sales,type ="o")
#primarily wheni observe the graph seems to have trend and additive seasonality but lets find out thre same through the models while building 

#creating 12 dummy variables 
X<- data.frame(outer(rep(month.abb,length = 60), month.abb,"==") + 0 ) # Creating dummies for 12 months
View(X)
colnames(X)<-month.abb # Assigning month names 
View(X)
airdata<-cbind(plastic_data,X)
View(airdata)
colnames(airdata)

#input"t"

airdata["t"]<- 1:60
View(airdata)

airdata["log_sales"]<-log(airdata["Sales"])
airdata["t_square"]<-airdata["t"]*airdata["t"]
attach(airdata)

train<-airdata[1:48,]

test<-airdata[49:60,] #because we need one full cycle of a year in test data
########################### LINEAR MODEL #############################

linear_model<-lm(Sales~t,data=train)
summary(linear_model)
#adjusted r square value is  31%% and multipule r squared value is 33% 
linear_pred<-data.frame(predict(linear_model,interval='predict',newdata =test))
View(linear_pred)
rmse_linear<-sqrt(mean((test$Sales-linear_pred$fit)^2,na.rm = T))
rmse_linear # 260



######################### Exponential #################################

expo_model<-lm(log_sales~t,data=train)
summary(expo_model)
#R-square value is 30% and adjusted r squared value is 31%
expo_pred<-data.frame(predict(expo_model,interval='predict',newdata=test))
rmse_expo<-sqrt(mean((test$Sales-exp(expo_pred$fit))^2,na.rm = T))
rmse_expo #268

######################### Quadratic ####################################

Quad_model<-lm(Sales~t+t_square,data=train)
summary(Quad_model)
#multipule r suqred value and adjusted r squared value are 33 and30 respectively
Quad_pred<-data.frame(predict(Quad_model,interval='predict',newdata=test))
rmse_Quad<-sqrt(mean((test$Sales-Quad_pred$fit)^2,na.rm=T))
rmse_Quad # 297



######################### Additive Seasonality #########################

sea_add_model<-lm(Sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(sea_add_model)
sea_add_pred<-data.frame(predict(sea_add_model,newdata=test,interval='predict'))
rmse_sea_add<-sqrt(mean((test$Sales-sea_add_pred$fit)^2,na.rm = T))
rmse_sea_add #235
#no that we knw there is seasonality component for our data 
#lets try it with additive seasonality with exponential


######################## Additive Seasonality with exponential #################

Add_sea_expo_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_expo_model)
#multipule r squared value is 98% and adjusted r squared avlue is 97%

Add_sea_expo_pred<-data.frame(predict(Add_sea_expo_model,interval='predict',newdata=test))
rmse_Add_sea_expo<-sqrt(mean((test$Sales-Add_sea_expo_pred$fit)^2,na.rm=T))
rmse_Add_sea_expo # 13325.69

######################## Additive Seasonality with Quadratic #################

Add_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data=train)
summary(Add_sea_Quad_model)
#adjusted r squared value and multipuled r square value is 98 and 97
Add_sea_Quad_pred<-data.frame(predict(Add_sea_Quad_model,interval='predict',newdata=test))
rmse_Add_sea_Quad<-sqrt(mean((test$Sales-Add_sea_Quad_pred$fit)^2,na.rm=T))
rmse_Add_sea_Quad #218

######################## Multiplicative Seasonality #########################

multi_sea_model<-lm(log_sales~Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_sea_model)
multi_sea_pred<-data.frame(predict(multi_sea_model,newdata=test,interval='predict'))
rmse_multi_sea<-sqrt(mean((test$Sales-exp(multi_sea_pred$fit))^2,na.rm = T))
rmse_multi_sea #239

######################## Multiplicative Seasonality exponential trend ##########################

multi_add_sea_model<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model) 
multi_add_sea_pred<-data.frame(predict(multi_add_sea_model,newdata=test,interval='predict'))
rmse_multi_expo_sea<-sqrt(mean((test$Sales-exp(multi_add_sea_pred$fit))^2,na.rm = T))
rmse_multi_expo_sea #160

######################## Multiplicative Seasonality Linear trend ##########################

multi_add_sea_model_lin<-lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov,data = train)
summary(multi_add_sea_model_lin) 
#98%multipule r square value and 97%adjusted r square value 
multi_add_sea_pred_lin<-data.frame(predict(multi_add_sea_model_lin,newdata=test,interval='predict'))
rmse_multi_add_sea_lin<-sqrt(mean((test$Sales-exp(multi_add_sea_pred_lin$fit))^2,na.rm = T))
rmse_multi_add_sea_lin #160


#######################multiplicative model with quadratic trend###############

mul_sea_Quad_model<-lm(Sales~t+t_square+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=train)
summary(mul_sea_Quad_model) 
#98%multipule r square value and 97%adjusted r square value 
mul_sea_Quad__model_pred<-data.frame(predict(mul_sea_Quad_model,newdata=test,interval='predict'))
rmse_mul_sea_Quad_model<-sqrt(mean((test$Sales-exp(mul_sea_Quad__model_pred$fit))^2,na.rm = T))
rmse_mul_sea_Quad_model #160


#the data has exponential trend with multiplicative seasonality 

# Preparing table on model and it's RMSE values 

table_rmse<-data.frame(c("rmse_linear","rmse_expo","rmse_Quad","rmse_sea_add","rmse_Add_sea_Quad","rmse_Add_sea_expo","rmse_multi_sea","rmse_multi_expo_sea","mul_sea_Quad_model","mul_sea_Quad_model"),c(rmse_linear,rmse_expo,rmse_Quad,rmse_sea_add,rmse_Add_sea_Quad,rmse_Add_sea_expo,rmse_multi_sea,rmse_multi_expo_sea,rmse_multi_add_sea_lin,rmse_mul_sea_Quad_model))
View(table_rmse)
colnames(table_rmse)<-c("model","RMSE")
View(table_rmse)

##in the model table we can see that multiplicative seasonality with exponential trend 
##as the least value as 160###

#multiplicative seasonality with exponential trend has least RMSE value 
#so that will be our new model

new_model <- lm(log_sales~t+Jan+Feb+Mar+Apr+May+Jun+Jul+Aug+Sep+Oct+Nov+Dec,data=airdata)


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
write.csv(airdata,file="plastic.csv",col.names = F,row.names = F)

####################### Predicting new data #############################
library(readxl)
test_data<-read.csv(file.choose())
View(test_data)
pred_new<-data.frame(predict(new_model,newdata=test_data,interval = 'predict'))
View(pred_new)

