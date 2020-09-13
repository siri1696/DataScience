crime_data=read.csv(file.choose())
#####################eda###############3
str(crime_data)
summary(crime_data)


norm_data= scale(crime_data[,2:5])
summary(norm_data)


boxplot(crime_data$Murder)#no outliers
boxplot(crime_data$Assault)
boxplot(crime_data$UrbanPop)
boxplot(crime_data$Rape)


hist(crime_data$Murder)
hist(crime_data$Assault)
hist(crime_data$UrbanPop)
hist(crime_data$Rape)


#########model building###########

d <- dist(norm_data, method = "euclidean") # distance matrix
?hclust
?dist
fit <- hclust(d, method="average")
?hclust
plot(fit) # display dendrogram
plot(fit, hang=-1)

?cutree
rect.hclust(fit, k=2, border="red")

groups <- cutree(fit, k=2) # cut tree into 2 clusters

membership<-as.matrix(groups) # groups or cluster numbers
final <- data.frame(crime_data, membership)

View(final)

write.csv(final, file="final.csv",row.names = F)

aggregate(crime_data[,-1],by=list(final$membership),mean)


#for urban population 68the assult ,murder,rape rates are higher than for the second group where urban population mean 63 they have comparitiely reducrd
