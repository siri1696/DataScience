movies_data= read.csv(file.choose())

movies_data1=movies_data[6:15]

str(movies_data1)
summary(movies_data1)

install.packages("arules")
library(arules)


itemFrequencyPlot(groceries,topN=20)

#####finding rules from apriori algorithm#######
rules=apriori(movies_data1) ###5120 rules
summary(rules)
#reducing number of rules using diff values of supp confidence and lift ratios
rules=apriori(movies_data1,parameter = list(minlen=4,supp=0.7,conf=0.8))#4660 rules
rules=apriori(movies_data1,parameter = list(minlen=4,maxlen=8,supp=0.7,conf=0.6))
rules= apriori(movies_data1,parameter = list(supp= 0.4,conf=0.5))#5120 rules as the conf and support values decrease  we get more umber of rules
rules= apriori(movies_data1,parameter = list(supp=0.1,conf=0.2))

#for obtaing shorter rules
subsets <- which(colSums(is.subset(rules,rules)) > 1) 
rules <- rules[-subsets]#84rules

rules_chi2 <- apriori(movies_data1, parameter = list(supp = 0.7, conf = 0.8, arem = "chi2"))#10 rules
arules::inspect(rules_chi2)

#finding intresting rules
rules=apriori(movies_data1,parameter = list(minlen=4,maxlen=8,supp=0.7,conf=0.8),
              appearance = list(rhs=c("Sixth.Sense=[0,1]"),default="lhs"))#456 rules

rules.sorted=(sort(rules,by = "lift"))

##########removing redundant rules
subset.matrix = is.subset(rules,rules)
subset.matrix[lower.tri(subset.matrix,diag = T)]=NA
redundant= colSums(subset.matrix,na.rm = T)>= 1
which(redundant)

rules.pruned=rules[!redundant]
rules.pruned1=sort(rules.pruned,by="lift")
arules::inspect(rules.pruned1)

##visulization og the rules
install.packages("arulesViz")
library(arulesViz)
plot(rules_chi2)
plot(rules_chi2,method = "grouped")
plot(rules_chi2,method = "graph",control = list(type="items"))

