library(readxl)
library(dplyr)
library("car")
library(naniar)

#set work directory to location dataset
setwd('C:/Users/derck/OneDrive - HvA/Documents/GitHub/Quantib_mortality')
df <- read_excel("data_complete.xlsx")

df <- replace_with_na(df,replace=list(weight = -1))
df <- replace_with_na(df,replace=list(LVF = -1))

#Calculate variables 
df$BMI<-df$weight/((df$length/100)*(df$length/100))
df$BSA<-sqrt((df$length*df$weight)/3600)
df$NYHAtwogroups <-ifelse(df$NYHA>2,'3/4','1/2')

df$musclearea_thres<-(df$Psoas_muscle_thresholded+df$Abdominal_muscle_thresholded+df$Longspine_muscle_thresholded)
df$SMI_thres<-df$musclearea_thres/((df$length/100)*(df$length/100))

df$musclearea<-(df$Psoas_muscle+df$Abdominal_muscle+df$Longspine_muscle)
df$HUmuscle<-(df$HU_Psoas_muscle*df$Psoas_muscle+df$HU_Abdominal_muscle*df$Abdominal_muscle+df$HU_Longspine_muscle*df$Longspine_muscle)/df$musclearea

df$IMAT<-df$musclearea-df$musclearea_thres
df$IMATper<-df$IMAT/df$musclearea*100

#split in and exclusion
incl<-subset(df, quality==1&observationtime>-1)
excl<-subset(df, quality==0|observationtime==-99)

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
t<-data.frame(c(incl_NYHA[1],incl_NYHA[2]),c(excl_NYHA[1],excl_NYHA[2]))
chisq.test(t)

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
incl_COPD<-table(incl$COPD)
excl_COPD<-table(excl$COPD)

t<-data.frame(c(incl_COPD[1],incl_COPD[2]),c(excl_COPD[1],excl_COPD[2]))
chisq.test(t)

#accessroute
table(df$accessroute)
incl_acc<-table(incl$accessroute)
excl_acc<-table(excl$accessroute)

t<-data.frame(c(incl_acc[1], incl_acc[2]),c(excl_acc[1], excl_acc[2]))
chisq.test(t)

#LVF
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

table(df$LVF)
incl_LVF<-table(incl$LVF)
excl_LVF<-table(excl$LVF)

t<-data.frame(c(incl_LVF[1],incl_LVF[2]),c(excl_LVF[1], excl_LVF[2]))
chisq.test(t)

#Musclemass
hist(incl$SMI_thres)
qqPlot(incl$SMI_thres)

sum(is.na(incl$SMI_thres))
mean(incl$SMI_thres, na.rm=T)
sd(incl$SMI_thres, na.rm=T)

#HU
hist(incl$HUmuscle)
qqPlot(incl$HUmuscle)

sum(is.na(incl$HUmuscle))
mean(incl$HUmuscle, na.rm=T)
sd(incl$HUmuscle, na.rm=T)

#IMAT
hist(incl$IMATper)
qqPlot(incl$IMATper)

sum(is.na(incl$IMATper))
mean(incl$IMATper, na.rm=T)
sd(incl$IMATper, na.rm=T)

##male/female
incl_male<-subset(incl, incl$sex==1)
incl_female<-subset(incl, incl$sex==2)
excl_male<-subset(excl, excl$sex==1)
excl_female<-subset(excl, excl$sex==2)
df_male<-subset(df, df$sex==1)
df_female<-subset(df, df$sex==2)

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
hist(incl_male$SMI_thres)
qqPlot(incl_male$SMI_thres)

sum(is.na(incl_male$SMI_thres))
mean(incl_male$SMI_thres, na.rm=TRUE)
sd(incl_male$SMI_thres, na.rm=T)

#female
hist(incl_female$SMI_thres)
qqPlot(incl_female$SMI_thres)

sum(is.na(incl_female$SMI_thres))
mean(incl_female$SMI_thres, na.rm=TRUE)
sd(incl_female$SMI_thres, na.rm=T)

#HU
#male
hist(incl_male$HUmuscle)
qqPlot(incl_male$HUmuscle)

sum(is.na(incl_male$HUmuscle))
mean(incl_male$HUmuscle, na.rm=TRUE)
sd(incl_male$HUmuscle, na.rm=T)

#female
hist(incl_female$HUmuscle)
qqPlot(incl_female$HUmuscle)

sum(is.na(incl_female$HUmuscle))
mean(incl_female$HUmuscle, na.rm=TRUE)
sd(incl_female$HUmuscle, na.rm=T)

#IMAT
#male
hist(incl_male$IMATper)
qqPlot(incl_male$IMATper)

sum(is.na(incl_male$IMATper))
mean(incl_male$IMATper, na.rm=TRUE)
sd(incl_male$IMATper, na.rm=T)

#female
hist(incl_female$IMATper)
qqPlot(incl_female$IMATper)

sum(is.na(incl_female$IMATper))
mean(incl_female$IMATper, na.rm=TRUE)
sd(incl_female$IMATper, na.rm=T)

