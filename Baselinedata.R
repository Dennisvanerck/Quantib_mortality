library(readxl)
library(dplyr)
library("car")
library(naniar)

setwd('C:/Users/derck/OneDrive - HvA/Documents/GitHub/Quantib_mortality')
df <- read_excel("data_complete.xlsx")
,
df <- replace_with_na(df,replace=list(weight = -1))

#Calculate extra variables 
df$BMI<-df$weight/((df$length/100)*(df$length/100))
df$BSA<-sqrt((df$length*df$weight)/3600)
df$NYHAtwogroups <-ifelse(df$NYHA>2,'3/4','1/2')


incl<-subset(df, quality==1&observationtime>-1)
excl<-subset(df, quality==0|observationtime==-99)

#exclusion incorrect CT scans 
excl$reason<-factor(excl$reason_quality)
table(excl$reason)

#Baseline data
#sex
tot_sex<-table(df$sex)
incl_sex<-table(incl$sex)
excl_sex<-table(excl$sex)
t<-data.frame(c(incl_sex[1],incl_sex[2]),c(excl_sex[1],excl_sex[2]))
chisq.test(t)

#age
hist(df$age)
qqPlot(df$age)
hist(incl$age)
qqPlot(incl$age)
hist(excl$age)
qqPlot(excl$age)

sum(is.na(df$age))
mean(df$age, na.rm=TRUE)
sd(df$age, na.rm=T)

sum(is.na(incl$age))
mean(incl$age, na.rm=TRUE)
sd(incl$age, na.rm=T)

sum(is.na(excl$age))
mean(excl$age, na.rm=TRUE)
sd(excl$age, na.rm=T)

t.test(incl$age, excl$age)

#length
hist(df$length)
qqPlot(df$length)
hist(incl$length)
qqPlot(incl$length)
hist(excl$length)
qqPlot(excl$length)

sum(is.na(df$length))
mean(df$length, na.rm=TRUE)
sd(df$length, na.rm=T)

sum(is.na(incl$length))
mean(incl$length, na.rm=TRUE)
sd(incl$length, na.rm=T)

sum(is.na(excl$length))
mean(excl$length, na.rm=TRUE)
sd(excl$length, na.rm=T)

t.test(incl$length, excl$length)

#weight
hist(df$weight)
qqPlot(df$weight)
hist(incl$weight)
qqPlot(incl$weight)
hist(excl$weight)
qqPlot(excl$weight)

sum(is.na(df$weight))
mean(df$weight, na.rm=TRUE)
sd(df$weight, na.rm=T)

sum(is.na(incl$weight))
mean(incl$weight, na.rm=TRUE)
sd(incl$weight, na.rm=T)

sum(is.na(excl$weight))
mean(excl$weight, na.rm=TRUE)
sd(excl$weight, na.rm=T)

t.test(incl$weight, excl$weight)

#BMI
hist(df$BMI)
qqPlot(df$BMI)
hist(incl$BMI)
qqPlot(incl$BMI)
hist(excl$BMI)
qqPlot(excl$BMI)

sum(is.na(df$BMI))
mean(df$BMI, na.rm=TRUE)
sd(df$BMI, na.rm=T)

sum(is.na(incl$BMI))
mean(incl$BMI, na.rm=TRUE)
sd(incl$BMI, na.rm=T)

sum(is.na(excl$BMI))
mean(excl$BMI, na.rm=TRUE)
sd(excl$BMI, na.rm=T)

t.test(incl$BMI, excl$BMI)

#BSA
hist(df$BSA)
qqPlot(df$BSA)
hist(incl$BSA)
qqPlot(incl$BSA)
hist(excl$BSA)
qqPlot(excl$BSA)

sum(is.na(df$BSA))
mean(df$BSA, na.rm=TRUE)
sd(df$BSA, na.rm=T)

sum(is.na(incl$BSA))
mean(incl$BSA, na.rm=TRUE)
sd(incl$BSA, na.rm=T)

sum(is.na(excl$BSA))
mean(excl$BSA, na.rm=TRUE)
sd(excl$BSA, na.rm=T)

t.test(incl$BSA, excl$BSA)

#NYHA
table(df$NYHAtwogroups)
incl_NYHA<-table(incl$NYHAtwogroups)
excl_NYHA<-table(excl$NYHAtwogroups)
t1<-c(453,746)
t2<-c(66,140)
t3<-data.frame(t1,t2)
chisq.test(t3)

#Euroscore
hist(df$EURO2)
qqPlot(df$EURO2)
hist(incl$EURO2)
qqPlot(incl$EURO2)
hist(excl$EURO2)
qqPlot(excl$EURO2)

sum(is.na(df$EURO2))
median(df$EURO2, na.rm=T)
summary(df$EURO2, na.rm=T)

sum(is.na(incl$EURO2))
median(incl$EURO2, na.rm=T)
summary(incl$EURO2, na.rm=T)

sum(is.na(excl$EURO2))
median(excl$EURO2, na.rm=T)
summary(excl$EURO2, na.rm=T)

wilcox.test(incl$EURO2, excl$EURO2)

#COPD
table(df$COPD)
table(incl$COPD)
table(excl$COPD)
t1<-c(943,256)
t2<-c(156,50)
t3<-data.frame(t1,t2)
chisq.test(t3)
sum(is.na(incl$COPD))


#accessroute
table(df$Accessroute)
table(incl$Accessroute)
table(excl$Accessroute)
t1<-c(251,905)
t2<-c(48,152)
t3<-data.frame(t1,t2)
chisq.test(t3)
sum(is.na(incl$Accessroute))

#LVF
df <- replace_with_na(df,replace=list(LVF = -1))
df$LVF<-ifelse(df$LVF>45,0,df$LVF)
df$LVF<-ifelse(df$LVF==1,0,df$LVF)
df$LVF<-ifelse(df$LVF<=45&df$LVF>5 ,1,df$LVF)
df$LVF<-ifelse(df$LVF>1 & df$LVF<5,1,df$LVF)
df$LVF<-factor(df$LVF, levels=c(0,1), labels=c('good','impaired'))

incl <- replace_with_na(incl,replace=list(LVF = -1))
incl$LVF<-ifelse(incl$LVF>45,0,incl$LVF)
incl$LVF<-ifelse(incl$LVF==1,0,incl$LVF)
incl$LVF<-ifelse(incl$LVF<=45&incl$LVF>5 ,1,incl$LVF)
incl$LVF<-ifelse(incl$LVF>1 & incl$LVF<5,1,incl$LVF)
incl$LVF<-factor(incl$LVF, levels=c(0,1), labels=c('good','impaired'))

excl <- replace_with_na(excl,replace=list(LVF = -1))
excl$LVF<-ifelse(excl$LVF>45,0,excl$LVF)
excl$LVF<-ifelse(excl$LVF==1,0,excl$LVF)
excl$LVF<-ifelse(excl$LVF<=45&excl$LVF>5 ,1,excl$LVF)
excl$LVF<-ifelse(excl$LVF>1 & excl$LVF<5,1,excl$LVF)
excl$LVF<-factor(excl$LVF, levels=c(0,1), labels=c('good','impaired'))

sum(is.na(incl$LVF))

table(df$LVF)
table(incl$LVF)
table(excl$LVF)
t1<-c(853,328)
t2<-c(149,53)
t3<-data.frame(t1,t2)
chisq.test(t3)

#musclemass
df$musclearea_thres<-(df$Psoas_muscle_thresholded+df$Abdominal_muscle_thresholded+df$Longspine_muscle_thresholded)
df$SMI_thres<-df$musclearea_thres/((df$length/100)*(df$length/100))

incl<-subset(df, quality==1&observatietijd>-1)
excl<-subset(df, quality==0|observatietijd==-99)

sum(is.na(incl$SMI_thres))


hist(df$SMI_thres)
qqPlot(df$SMI_thres)
hist(incl$SMI_thres)
qqPlot(incl$SMI_thres)
hist(excl$SMI_thres)
qqPlot(excl$SMI_thres)

sum(is.na(df$SMI_thres))
mean(df$SMI_thres, na.rm=T)
sd(df$SMI_thres, na.rm=T)

incl$test<-(is.na(incl$SMI_thres))
incl2<-subset(incl, incl$test==T)
mean(incl$SMI_thres, na.rm=T)
sd(incl$SMI_thres, na.rm=T)

sum(is.na(excl$SMI_thres))
mean(excl$SMI_thres, na.rm=T)
sd(excl$SMI_thres, na.rm=T)

wilcox.test(incl$SMI_thres, excl$SMI_thres)


#HU
df$musclearea<-(df$Psoas_muscle+df$Abdominal_muscle+df$Longspine_muscle)
df$HUmuscle<-(df$HU_Psoas_muscle*df$Psoas_muscle+df$HU_Abdominal_muscle*df$Abdominal_muscle+df$HU_Longspine_muscle*df$Longspine_muscle)/df$musclearea

incl<-subset(df, quality==1&observatietijd>-1)
excl<-subset(df, quality==0|observatietijd==-99)



hist(df$HUmuscle)
qqPlot(df$HUmuscle)
hist(incl$HUmuscle)
qqPlot(incl$HUmuscle)
hist(excl$HUmuscle)
qqPlot(excl$HUmuscle)

sum(is.na(df$HUmuscle))
mean(df$HUmuscle, na.rm=T)
sd(df$HUmuscle, na.rm=T)

sum(is.na(incl$HUmuscle))
mean(incl$HUmuscle, na.rm=T)
sd(incl$HUmuscle, na.rm=T)

sum(is.na(excl$HUmuscle))
mean(excl$HUmuscle, na.rm=T)
sd(excl$HUmuscle, na.rm=T)

wilcox.test(incl$HUmuscle, excl$HUmuscle)

df$mis<-is.na(df$HUmuscle)
test<-data.frame(df$Pseudo, df$mis)
test<-subset(test,test$df.mis=='TRUE')

#IMAT
df$IMAT<-df$musclearea-df$musclearea_thres
df$IMATper<-df$IMAT/df$musclearea*100

incl<-subset(df, quality==1&observatietijd>-1)
excl<-subset(df, quality==0|observatietijd==-99)

hist(df$IMATper)
qqPlot(df$IMATper)
hist(incl$IMATper)
qqPlot(incl$IMATper)
hist(excl$IMATper)
qqPlot(excl$IMATper)

sum(is.na(df$IMATper))
mean(df$IMATper, na.rm=T)
sd(df$IMATper, na.rm=T)

df$mis<-is.na(df$IMAT)
test2<-data.frame(df$Pseudo, df$mis)
test2<-subset(test2,test2$df.mis=='TRUE')

sum(is.na(incl$IMATper))
mean(incl$IMATper, na.rm=T)
sd(incl$IMATper, na.rm=T)

sum(is.na(excl$IMATper))
mean(excl$IMATper, na.rm=T)
sd(excl$IMATper, na.rm=T)

wilcox.test(incl$IMATper, excl$IMATper)

##male/female

incl_male<-subset(incl, incl$geslacht==1)
incl_female<-subset(incl, incl$geslacht==2)
excl_male<-subset(excl, excl$geslacht==1)
excl_female<-subset(excl, excl$geslacht==2)
df_male<-subset(df, df$geslacht==1)
df_female<-subset(df, df$geslacht==2)


#heigth
#male
hist(df_male$length)
qqPlot(df_male$length)
hist(incl_male$length)
qqPlot(incl_male$length)
hist(excl_male$length)
qqPlot(excl_male$length)

sum(is.na(df_male$length))
mean(df_male$length, na.rm=TRUE)
sd(df_male$length, na.rm=T)

sum(is.na(incl_male$length))
mean(incl_male$length, na.rm=TRUE)
sd(incl_male$length, na.rm=T)

sum(is.na(excl_male$length))
mean(excl_male$length, na.rm=TRUE)
sd(excl_male$length, na.rm=T)

t.test(incl_male$length, excl_male$length)

#female
hist(df_female$length)
qqPlot(df_female$length)
hist(incl_female$length)
qqPlot(incl_female$length)
hist(excl_female$length)
qqPlot(excl_female$length)

sum(is.na(df_female$length))
mean(df_female$length, na.rm=TRUE)
sd(df_female$length, na.rm=T)

sum(is.na(incl_female$length))
mean(incl_female$length, na.rm=TRUE)
sd(incl_female$length, na.rm=T)

sum(is.na(excl_female$length))
mean(excl_female$length, na.rm=TRUE)
sd(excl_female$length, na.rm=T)

t.test(incl_female$length, excl_female$length)

#weigth
#male
hist(df_male$gewicht)
qqPlot(df_male$gewicht)
hist(incl_male$gewicht)
qqPlot(incl_male$gewicht)
hist(excl_male$gewicht)
qqPlot(excl_male$gewicht)

sum(is.na(df_male$gewicht))
mean(df_male$gewicht, na.rm=TRUE)
sd(df_male$gewicht, na.rm=T)

sum(is.na(incl_male$gewicht))
mean(incl_male$gewicht, na.rm=TRUE)
sd(incl_male$gewicht, na.rm=T)

sum(is.na(excl_male$gewicht))
mean(excl_male$gewicht, na.rm=TRUE)
sd(excl_male$gewicht, na.rm=T)

t.test(incl_male$gewicht, excl_male$gewicht)

#female
hist(df_female$gewicht)
qqPlot(df_female$gewicht)
hist(incl_female$gewicht)
qqPlot(incl_female$gewicht)
hist(excl_female$gewicht)
qqPlot(excl_female$gewicht)

sum(is.na(df_female$gewicht))
mean(df_female$gewicht, na.rm=TRUE)
sd(df_female$gewicht, na.rm=T)

sum(is.na(incl_female$gewicht))
mean(incl_female$gewicht, na.rm=TRUE)
sd(incl_female$gewicht, na.rm=T)

sum(is.na(excl_female$gewicht))
mean(excl_female$gewicht, na.rm=TRUE)
sd(excl_female$gewicht, na.rm=T)

t.test(incl_female$gewicht, excl_female$gewicht)

#BMI
#male
hist(df_male$BMI)
qqPlot(df_male$BMI)
hist(incl_male$BMI)
qqPlot(incl_male$BMI)
hist(excl_male$BMI)
qqPlot(excl_male$BMI)

sum(is.na(df_male$BMI))
mean(df_male$BMI, na.rm=TRUE)
sd(df_male$BMI, na.rm=T)

sum(is.na(incl_male$BMI))
mean(incl_male$BMI, na.rm=TRUE)
sd(incl_male$BMI, na.rm=T)

sum(is.na(excl_male$BMI))
mean(excl_male$BMI, na.rm=TRUE)
sd(excl_male$BMI, na.rm=T)

t.test(incl_male$BMI, excl_male$BMI)

#female
hist(df_female$BMI)
qqPlot(df_female$BMI)
hist(incl_female$BMI)
qqPlot(incl_female$BMI)
hist(excl_female$BMI)
qqPlot(excl_female$BMI)

sum(is.na(df_female$BMI))
mean(df_female$BMI, na.rm=TRUE)
sd(df_female$BMI, na.rm=T)

sum(is.na(incl_female$BMI))
mean(incl_female$BMI, na.rm=TRUE)
sd(incl_female$BMI, na.rm=T)

sum(is.na(excl_female$BMI))
mean(excl_female$BMI, na.rm=TRUE)
sd(excl_female$BMI, na.rm=T)

t.test(incl_female$BMI, excl_female$BMI)

#BSA
#male
hist(df_male$BSA)
qqPlot(df_male$BSA)
hist(incl_male$BSA)
qqPlot(incl_male$BSA)
hist(excl_male$BSA)
qqPlot(excl_male$BSA)

sum(is.na(df_male$BSA))
mean(df_male$BSA, na.rm=TRUE)
sd(df_male$BSA, na.rm=T)

sum(is.na(incl_male$BSA))
mean(incl_male$BSA, na.rm=TRUE)
sd(incl_male$BSA, na.rm=T)

sum(is.na(excl_male$BSA))
mean(excl_male$BSA, na.rm=TRUE)
sd(excl_male$BSA, na.rm=T)

t.test(incl_male$BSA, excl_male$BSA)

#female
hist(df_female$BSA)
qqPlot(df_female$BSA)
hist(incl_female$BSA)
qqPlot(incl_female$BSA)
hist(excl_female$BSA)
qqPlot(excl_female$BSA)

sum(is.na(df_female$BSA))
mean(df_female$BSA, na.rm=TRUE)
sd(df_female$BSA, na.rm=T)

sum(is.na(incl_female$BSA))
mean(incl_female$BSA, na.rm=TRUE)
sd(incl_female$BSA, na.rm=T)

sum(is.na(excl_female$BSA))
mean(excl_female$BSA, na.rm=TRUE)
sd(excl_female$BSA, na.rm=T)

t.test(incl_female$BSA, excl_female$BSA)

#musclemass
#male
hist(df_male$SMI_thres)
qqPlot(df_male$SMI_thres)
hist(incl_male$SMI_thres)
qqPlot(incl_male$SMI_thres)
hist(excl_male$SMI_thres)
qqPlot(excl_male$SMI_thres)

sum(is.na(df_male$SMI_thres))
mean(df_male$SMI_thres, na.rm=TRUE)
sd(df_male$SMI_thres, na.rm=T)

sum(is.na(incl_male$SMI_thres))
mean(incl_male$SMI_thres, na.rm=TRUE)
sd(incl_male$SMI_thres, na.rm=T)

sum(is.na(excl_male$SMI_thres))
mean(excl_male$SMI_thres, na.rm=TRUE)
sd(excl_male$SMI_thres, na.rm=T)

t.test(incl_male$SMI_thres, excl_male$SMI_thres)

#female
hist(df_female$SMI_thres)
qqPlot(df_female$SMI_thres)
hist(incl_female$SMI_thres)
qqPlot(incl_female$SMI_thres)
hist(excl_female$SMI_thres)
qqPlot(excl_female$SMI_thres)

sum(is.na(df_female$SMI_thres))
mean(df_female$SMI_thres, na.rm=TRUE)
sd(df_female$SMI_thres, na.rm=T)

sum(is.na(incl_female$SMI_thres))
mean(incl_female$SMI_thres, na.rm=TRUE)
sd(incl_female$SMI_thres, na.rm=T)

sum(is.na(excl_female$SMI_thres))
mean(excl_female$SMI_thres, na.rm=TRUE)
sd(excl_female$SMI_thres, na.rm=T)

t.test(incl_female$SMI_thres, excl_female$SMI_thres)

#HU
#male
hist(df_male$HUmuscle)
qqPlot(df_male$HUmuscle)
hist(incl_male$HUmuscle)
qqPlot(incl_male$HUmuscle)
hist(excl_male$HUmuscle)
qqPlot(excl_male$HUmuscle)

sum(is.na(df_male$HUmuscle))
mean(df_male$HUmuscle, na.rm=TRUE)
sd(df_male$HUmuscle, na.rm=T)

sum(is.na(incl_male$HUmuscle))
mean(incl_male$HUmuscle, na.rm=TRUE)
sd(incl_male$HUmuscle, na.rm=T)

sum(is.na(excl_male$HUmuscle))
mean(excl_male$HUmuscle, na.rm=TRUE)
sd(excl_male$HUmuscle, na.rm=T)

t.test(incl_male$HUmuscle, excl_male$HUmuscle)

#female
hist(df_female$HUmuscle)
qqPlot(df_female$HUmuscle)
hist(incl_female$HUmuscle)
qqPlot(incl_female$HUmuscle)
hist(excl_female$HUmuscle)
qqPlot(excl_female$HUmuscle)

sum(is.na(df_female$HUmuscle))
mean(df_female$HUmuscle, na.rm=TRUE)
sd(df_female$HUmuscle, na.rm=T)

sum(is.na(incl_female$HUmuscle))
mean(incl_female$HUmuscle, na.rm=TRUE)
sd(incl_female$HUmuscle, na.rm=T)

sum(is.na(excl_female$HUmuscle))
mean(excl_female$HUmuscle, na.rm=TRUE)
sd(excl_female$HUmuscle, na.rm=T)

t.test(incl_female$HUmuscle, excl_female$HUmuscle)

#IMAT
#male
hist(df_male$IMATper)
qqPlot(df_male$IMATper)
hist(incl_male$IMATper)
qqPlot(incl_male$IMATper)
hist(excl_male$IMATper)
qqPlot(excl_male$IMATper)

sum(is.na(df_male$IMATper))
mean(df_male$IMATper, na.rm=TRUE)
sd(df_male$IMATper, na.rm=T)

sum(is.na(incl_male$IMATper))
mean(incl_male$IMATper, na.rm=TRUE)
sd(incl_male$IMATper, na.rm=T)

sum(is.na(excl_male$IMATper))
mean(excl_male$IMATper, na.rm=TRUE)
sd(excl_male$IMATper, na.rm=T)

t.test(incl_male$IMATper, excl_male$IMATper)

#female
hist(df_female$IMATper)
qqPlot(df_female$IMATper)
hist(incl_female$IMATper)
qqPlot(incl_female$IMATper)
hist(excl_female$IMATper)
qqPlot(excl_female$IMATper)

sum(is.na(df_female$IMATper))
mean(df_female$IMATper, na.rm=TRUE)
sd(df_female$IMATper, na.rm=T)

sum(is.na(incl_female$IMATper))
mean(incl_female$IMATper, na.rm=TRUE)
sd(incl_female$IMATper, na.rm=T)

sum(is.na(excl_female$IMATper))
mean(excl_female$IMATper, na.rm=TRUE)
sd(excl_female$IMATper, na.rm=T)

t.test(incl_female$IMATper, excl_female$IMATper)
