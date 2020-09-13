company_data=read.csv(file.choose())
str(company_data)

summary(company_data)
company_data$Sales=ifelse(company_data$Sales<=10,"highsales","lowsales")
company_data$Sales=as.factor(company_data$Sales)

library(caTools)
library(tree)
company_tree=tree(Sales~.,data = company_data)
plot(company_tree)
text(company_tree)

pred_tree=as.data.frame(predict(company_tree,company_data))

pred_tree["final"]=NULL

pred_tree["final"]= ifelse(pred_tree<=10,"highsales","lowsales")
mean((pred_tree$`predict(company_tree, company_data)`)==(company_data$Sales))


library(gmodels)
CrossTable(company_data$Sales,pred_tree$`predict(company_tree, company_data)`)
