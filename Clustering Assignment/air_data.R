library(openxlsx)
air_data= read.xlsx("C:\\Users\\user-pc\\Desktop\\EastWestAirlines (1).xlsx",sheet = "data")

##########eda#####
str(air_data)
summary(air_data)
air_data$`Award?`=as.numeric(air_data$`Award?`)


######treating outliers####

boxplot(air_data$Balance)
x <- air_data$Balance
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$Balance=x
boxplot(air_data$Balance)


boxplot(air_data$Qual_miles)
x <- air_data$Qual_miles
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$Qual_miles=x
boxplot(air_data$Qual_miles)


boxplot(air_data$cc1_miles)
x <- air_data$cc1_miles
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$cc1_miles=x
boxplot(air_data$cc1_miles)


boxplot(air_data$Bonus_miles)
x <- air_data$Bonus_miles
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$Bonus_miles=x
boxplot(air_data$Bonus_miles)


boxplot(air_data$cc2_miles)
x <- air_data$cc2_miles
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$cc2_miles=x
boxplot(air_data$cc2_miles)


boxplot(air_data$cc3_miles)
x <- air_data$cc3_miles
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$cc3_miles=x
boxplot(air_data$cc3_miles)

boxplot(air_data$Bonus_trans)
x <- air_data$Bonus_trans
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$Bonus_trans=x
boxplot(air_data$Bonus_trans)

boxplot(air_data$Flight_miles_12mo)
x <- air_data$Flight_miles_12mo
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$Flight_miles_12mo=x
boxplot(air_data$Flight_miles_12mo)

boxplot(air_data$Flight_trans_12)
x <- air_data$Flight_trans_12
qnt <- quantile(x, probs=c(.25, .75), na.rm = T)
caps <- quantile(x, probs=c(.05, .95), na.rm = T)
H <- 1.5 * IQR(x, na.rm = T)
x[x < (qnt[1] - H)] <- caps[1]
x[x > (qnt[2] + H)] <- caps[2]
air_data$Flight_trans_12=x
boxplot(air_data$Flight_trans_12)

#####scaling data
air_data1=air_data[-1]
air_data1$Balance=scale(air_data1$Balance)
air_data1$Qual_miles=scale(air_data1$Qual_miles)
air_data1$cc1_miles=scale(air_data1$cc1_miles)
air_data1$cc2_miles=scale(air_data1$cc2_miles)
air_data1$cc3_miles=scale(air_data1$cc3_miles)
air_data1$Bonus_miles=scale(air_data1$Bonus_miles)
air_data1$Bonus_trans=scale(air_data1$Bonus_trans)
air_data1$Flight_miles_12mo= scale(air_data1$Flight_miles_12mo)
air_data1$Flight_trans_12= scale(air_data1$Flight_trans_12)
air_data1$Days_since_enroll= scale(air_data1$Days_since_enroll)

summary(air_data1)


######modelbuilding
#hierarchial clustering

d <- dist(air_data1, method = "euclidean") # distance matrix
?hclust
fit <- hclust(d, method="complete")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)



#dendogram interpretation becomes difficult so lets try k means or clustering



fit1 = kmeans(air_data1,7)
str(fit1)
final =data.frame(air_data[-1],fit1$cluster)
final
aggregate(air_data[-1],by=list(fit1$cluster),FUN=mean)


#elbow curve

twss =c()
for (i in 1:8)
  {
  twss[i]=sum(kmeans(air_data1,centers = i)$withinss)
  
}

plot(1:8,twss,type="b",xlab="number of clusters",ylab="Within groups")
title(sub ="k-means clustering scree plot")

