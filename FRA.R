#Reading the data and checking for missing values, a particular 
#variable having 5% more than to that of the complete data will be dropped
rm(list=ls())
de=read.csv("DATA1.csv",header = T)
de=de[,-22]
summary(de)
de$Networth.Next.Year=ifelse(de$Networth.Next.Year>0,0,1)
de$Networth.Next.Year=as.factor(de$Networth.Next.Year)
colnames(de)[2]="Default"
de=de[,-1]
de[, 2:50] <- sapply(de[, 2:50], as.integer)
de1=de[,2:50]

Miss <- function(x){sum(is.na(x))/length(x)*100}
apply(de1,2,Miss)

de2 <- cbind(de1, de$Default)
de2[, 1:49] <- sapply(de2[, 1:49], as.numeric)


de2=de2[,-c(3,4,13,15,16,17,20,22,29,31,39:45,49)]
apply(de2,2,Miss)
colnames(de2)[32]="Default"

table(de2$Default)
243/(243+3298)

#Boxplots for  to check the spread of the data and to spot outliers, 
#will not treating them as they seem to be valid data and plus the iqr is very narrow.

boxplot(de2,main="Boxplots for all numeric variable")

#Bivariate analysis with Default as a factor
library(reshape2)
st1<- melt(de2[,c((1:10),32)], id = c("Default"))
st2<- melt(de2[,c((11:20),32)], id = c("Default"))
st3<- melt(de2[,c((21:31),32)], id = c("Default"))

#  box plots
library(tidyverse)
zz <- ggplot(st1, aes(x=Default, y=value))
zz+geom_boxplot(aes(color = Default), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()
zz <- ggplot(st2, aes(x=Default, y=value))
zz+geom_boxplot(aes(color = Default), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()
zz <- ggplot(st3, aes(x=Default, y=value))
zz+geom_boxplot(aes(color = Default), alpha=0.7 ) +
  facet_wrap(~variable,scales = "free_x", nrow = 3)+
  coord_flip()



#creating new variables in terms of ratios
de2$Asset_Turnover=round(de1$Sales/de1$Total.assets,1)
de2$Return_asset_ratio=round(de1$Total.income/de1$Total.assets,1)
de2$Return_Equity_ratio=round(de1$Total.income/de1$Shareholders.funds,1)
summary(de2)

de2$Return_Equity_ratio[is.infinite(de2$Return_Equity_ratio)]<-0
de2$Asset_Turnover[is.infinite(de2$Asset_Turnover)]<-0
de2$Return_asset_ratio[is.infinite(de2$Return_asset_ratio)]<-0
summary(de2)


#Imputing missing values by using predictive mean matching
library(mice)
md.pattern(de2)
mymice=mice(de2,m=5,method="pmm")
mymiceComplete=complete(mymice,2)
summary(mymiceComplete)
de3=mymiceComplete
summary(de3)


#Eliminating multi collinearity by using corrplot
library(corrplot)
corrplot(cor(de3[,1:31]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-1]

corrplot(cor(de3[,1:30]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-1]

corrplot(cor(de3[,1:29]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-3]

corrplot(cor(de3[,1:28]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-c(3,4)]

corrplot(cor(de3[,1:26]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-1]

corrplot(cor(de3[,1:25]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-1]

corrplot(cor(de3[,1:24]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-1]

corrplot(cor(de3[,1:23]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-1]

corrplot(cor(de3[,1:22]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-4]

corrplot(cor(de3[,1:21]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-7]

corrplot(cor(de3[,1:20]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-6]

corrplot(cor(de3[,1:19]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-4]

corrplot(cor(de3[,1:18]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-5]

corrplot(cor(de3[,1:17]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-7]

corrplot(cor(de3[,1:16]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-4]

corrplot(cor(de3[,1:15]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-8]

corrplot(cor(de3[,1:14]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-14]

corrplot(cor(de3[,1:13]),method = "number",tl.cex=0.6,number.cex = 0.5,type="upper")

de3=de3[,-4]

corrplot(cor(de3[,1:12]),method = "number",tl.cex=0.8,number.cex = 0.9,type="upper")

de3=de3[,-12]

corrplot(cor(de3[,1:11]),method = "number",tl.cex=0.8,number.cex = 0.9,type="upper")



#Running a logistic regression model with the significant variables
set.seed(1234)
ll=glm(Default~.,data=de3,family =binomial(link="logit"))
summary(ll)
library(car)
vif(ll)



#######Running the model on the original data without the split.
ll=glm(Default~PAT.as...of.net.worth+Total.capital+Current.assets+
         +Debt.to.equity.ratio..times.+Current.ratio..times.+Cash.to.average.cost.of.sales.per.day+
         Asset_Turnover+Return_Equity_ratio,data=de3,family = binomial(link="logit")
)
summary(ll)


#Preparing the test data for validation

library(readxl)
ts=read_excel("test1.xlsx")
dim(ts)
ts1=ts[,c(2,15,19,34,37,38,40)]
summary(ts1)
colnames(ts1)[2]="PAT.as...of.net.worth"
colnames(ts1)[3]="Total.capital"
colnames(ts1)[4]="Current.assets"
colnames(ts1)[5]="Current.ratio..times."
colnames(ts1) [6]="Debt.to.equity.ratio..times."
colnames(ts1)[7]="Cash.to.average.cost.of.sales.per.day"
summary(ts1)
ts1$`Default - 1`=as.factor(ts1$`Default - 1`)
colnames(ts1)[1]="Default"
ts1$Asset_Turnover=round(ts$Sales/ts$`Total assets`,1)
ts1$Return_asset_ratio=round(ts$`Total income`/ts$`Total assets`,1)
ts1$Return_Equity_ratio=round(ts$`Total income`/ts$`Shareholders funds`,1)
summary(ts1)


#Cleaning the test data

library(mice)
md.pattern(ts1)
mymice=mice(ts1,m=5,method="pmm")
mymiceComplete=complete(mymice,2)
summary(mymiceComplete)
ts1=mymiceComplete
summary(ts1)
ts1=ts1[,-9]
summary(ts1)

#Predicting the probabilties for the test data 

library(caret)
pred=predict(ll,newdata = ts1,type="response")
ts1$pred=predict(ll,ts1,type="response")
ts1$pred=round(ts1$pred,2)
ts1$prob=ifelse(ts1$pred>0.2,1,0)
ts1$prob=as.factor(ts1$prob)
confusionMatrix(ts1$Default,ts1$prob,positive = "1")

#choosing the right threshold
library(ROCR)
predr=prediction(pred,ts1$Default)
as.numeric(performance(predr,"auc")@y.values)
pref=performance(predr,"tpr","fpr")
plot(pref)

#####choose the right threshold
plot(pref,colorize=T,print.cutoffs.at=seq(0,1,0.06),text.adj=c(-0.2,1.7),main="ROC for Threshold")


#sorting the data by predicted probability and dividing the data 
#into deciles of 10 and check how much of default is every decile able to capture
library(dplyr)
ts1%>%arrange(desc(pred))->ts2
ts2%>%mutate(quantile=ntile(Default,10))%>%group_by(Default,quantile)%>%summarize(N=n())%>%filter(Default==1)->dat1
ts2%>%mutate(quantile=ntile(Default,10))%>%group_by(quantile)%>%summarize(N=n())->dat
dat$perc=round(dat1$N/dat$N,1)
dat
