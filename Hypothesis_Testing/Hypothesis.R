

#########################cutlets.mtw###############################
culets_data=read.csv(file.choose())
summary(culets_data)

#business objective:to check whether diameter of cutlet a is equal to b or not 
#null hypo =cutlet diameter of A and B are equal
#alternative hypo:= diameter of A and B are not equal

#####normality test####

#null hypo: data is normally distributed
#alter hypo: data is not normally distributed

shapiro.test(culets_data$Unit.A)
#p value is 0.32 so p high null fly => It follows normal distribution
shapiro.test(culets_data$Unit.B)
#p value is 0.5225 so p high null fly => It follows normal distribution

########paired t-test #######
t.test(culets_data$Unit.A,culets_data$Unit.B,conf.level = 0.95, paired = TRUE, alternative = "two.sided")
# alternative = "two.sided" means we are checking for equal and unequal
# means
# null Hypothesis -> Equal means
# Alternate Hypothesis -> Unequal Hypothesis
# p-value = 0.4562>0.05 accept null hypothesis 
#both are of equal diameter


##############################LabTAT.mtw############################

tat_data=read.csv(file.choose())

#business objective to to determine whether there is any difference in the average Turn Around Time (TAT)
#of reports of the laboratories on their preferred list or not 

#4 population
#null hypo= diff in the average tot 
#alter hypo=  there is no diff in the average tot

###########normality test############

#null hypo= data is normally distributed
#alter hypo= data is not normally distributed

attach(tat_data)

shapiro.test(tat_data$Laboratory.1)
#p value is 0.55 so p high null fly => It follows normal distribution

shapiro.test(tat_data$Laboratory.2)
#p value is 0.86 so p high null fly => It follows normal distribution

shapiro.test(tat_data$Laboratory.3)
#p value is 0.42 so p high null fly => It follows normal distribution

shapiro.test(tat_data$Laboratory.4)
#p value is 0.66 so p high null fly => It follows normal distribution

####to check varience is equal or not #######
#null hypo= equal varience
#alter hypo= unequal varience

bartlett.test(list(Laboratory.1,Laboratory.2,Laboratory.3,Laboratory.4,conf.level=0.95))
#p value is 0.10 so p high null fly =>equal varience
Anova_results <- aov(Laboratory.1~Laboratory.4,data = tat_data,conf.level=0.95)
summary(Anova_results)
#p value is 0.315 so p high null fly =>there is diff in  average tot of the laboratories


######################################buyerratio_data###########################

buyer_data=read.csv(file.choose())

#null hypo=>all are same 
#alter hypo=>atleast 1 are different

#
#chi-square test
attach(buyer_data)
chisq.test(buyer_data)
##p value is 0.66 so p high null fly=>all are same in all regions 

#######################################customer+order#################################

customer_data=read.csv(file.choose())

#null hypothesis=%of defective of all countries are equal 
#alter hypothesis=at least one is defective
attach(customer_data)
str(customer_data)
customer_data$India=as.numeric(customer_data$India)
customer_data$Phillippines=as.numeric(customer_data$Phillippines)
customer_data$Malta=as.numeric(customer_data$Malta)
customer_data$Indonesia=as.numeric(customer_data$Indonesia)

chisq.test(customer_data)
#we choose null hypothesis 
#p value is 1 so p high null fly =>=%of defective of all countries are equal 

#####################fantaloons.dtw##############
fantaloons_data=read.csv(file.choose())
#null hyputhesis= there is no diff in prop of female vs male in weekdays and weekends 
#alter hypothesis= there is difference in the prop of females vs males in weekdays and weekends 
attach(fantaloons_data)
table(Weekdays,Weekend)
prop.test(x=c(66,47),n=c(233,167),conf.level = 0.95,correct = FALSE,alternative = "two.sided")
##p value is 0.96 so p high null fly=> there is no diff in prop of female vs male in weekdays and weekends 
