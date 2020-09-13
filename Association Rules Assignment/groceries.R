

book_data=read.csv(file.choose())

#########eda######


summary(book_data)
str(book_data)

install.packages("arules")
library(arules)



#####finding rules from apriori algorithm#######
rules=apriori(book_data)###11253 rules
summary(rules)
#reducing number of rules using diff values of supp confidence and lift ratios
rules=apriori(book_data,parameter = list(minlen=4,maxlen=8,supp=0.7,conf=0.8))#10032 rules
rules=apriori(book_data,parameter = list(minlen=3,maxlen=5,supp=0.7,conf=0.6))#4125 rules

arules::inspect(rules)

#finding intresting rules
rules=apriori(book_data,parameter = list(minlen=4,maxlen=8,supp=0.7,conf=0.8),
              appearance = list(rhs=c("ChildBks=[0,1]"),default="lhs"))

rules.sorted=(sort(rules,by = "lift"))

##########removing redundant rules
subset.matrix = is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)]=NA
redundant= colSums(subset.matrix,na.rm = T)>= 1
which(redundant)

rules.pruned=rules[!redundant]
rules.pruned=sort(rules.pruned,by="lift")
arules::inspect(rules.pruned)

##visulization og the rules
install.packages("arulesViz")
library(arulesViz)
plot(rules,method="scatterplot")
plot(rules,method = "grouped")
plot(rules,method = "graph",control = list(type="items"))
