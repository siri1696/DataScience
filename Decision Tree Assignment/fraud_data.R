library(tree)
fraud_data=read.csv(file.choose())
str(fraud_data)

fraud_data$Taxable.Income=ifelse(fraud_data$Taxable.Income<= 30000,"risky","good")
fraud_data$Taxable.Income= as.factor(fraud_data$Taxable.Income)

library(caTools)
fraud_train= sample.split(fraud_data,SplitRatio = 0.80)
train1=subset(fraud_data,fraud_train==TRUE)
test1=subset(fraud_data,fraud_train==FALSE)


fraud_tree= tree(Taxable.Income~.,data=train1)


plot(fraud_tree)
text(fraud_tree)
pred_tree1=as.data.frame(predict(fraud_tree,train1))
names(pred_tree1)[1]="ti"
pred_tree1["final"]=NULL

pred_tree1["final"]= ifelse(pred_tree1<=30000,"risky","good")
mean((pred_tree1$ti)==(train1$Taxable.Income))

library(gmodels)
CrossTable(train1$Taxable.Income,pred_tree1$ti)
#97% accuracy

     