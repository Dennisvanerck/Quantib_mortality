library(readxl)
library(survival)
library(survminer)
library(dplyr)
library(Hmisc)
library(splines)
library(naniar)
library(zoo)
library(mice)
setwd('C:/Users/derck/OneDrive - HvA/Documents/GitHub/Quantib_mortality')

df <- read_excel("data_complete.xlsx")

df<-subset(df, quality==1)
df<-subset(df, observationtime>-1)

df <- replace_with_na(df,replace=list(length = -9))
df <- replace_with_na(df,replace=list(weight = -1))
df <- replace_with_na(df,replace=list(NYHA = -1))

df$NYHAtwogroups <-ifelse(df$NYHA>2,1,2)

#LVF
df <- replace_with_na(df,replace=list(LVF = -1))
df$LVF<-ifelse(df$LVF>45,0,df$LVF)
df$LVF<-ifelse(df$LVF==1,0,df$LVF)
df$LVF<-ifelse(df$LVF<=45&df$LVF>5 ,1,df$LVF)
df$LVF<-ifelse(df$LVF>1 & df$LVF<5,1,df$LVF)

#eGFR
df$eGFR <-ifelse(df$sex==1, 175*(df$kreatine/88.4)^-1.154 * (df$age)^-0.203, (175*(df$kreatine/88.4)^-1.154 * (df$age)^-0.203)* 0.742)
df$CKD<- ifelse(df$eGFR<60,1,0)

#Imputation of confounders 
notimputing<-df[c(1:4,13:33)]
imputing<-select(df, c(1,5:12,34:37))

x<-mice(data=imputing, m=1, maxit=10, seed=2)
complete<- complete(x, action=1)

df<-inner_join(notimputing,complete, by=c("Pseudo"))
rm(x, complete, imputing, notimputing)

#Calculate variables 
df$musclearea<-(df$Psoas_muscle+df$Abdominal_muscle+df$Longspine_muscle)
df$SMI<-df$musclearea/((df$length/100)*(df$length/100))

df$musclearea_thres<-(df$Psoas_muscle_thresholded+df$Abdominal_muscle_thresholded+df$Longspine_muscle_thresholded)
df$SMI_thres<-df$musclearea_thres/((df$length/100)*(df$length/100))

df$HUmuscle<-(df$HU_Psoas_muscle*df$Psoas_muscle+df$HU_Abdominal_muscle*df$Abdominal_muscle+df$HU_Longspine_muscle*df$Longspine_muscle)/df$musclearea

df$BMI<-df$weight/((df$length/100)*(df$length/100))
df$BSA<-sqrt((df$length*df$weight)/3600)

df$IMAT<-df$musclearea-df$musclearea_thres
df$IMATper<-df$IMAT/df$musclearea*100

df$mortality_1year<-ifelse(df$mortality==1 & df$observationtime<=365, 1, 0)
df$mortality_1year<-as.factor(df$mortality_1year)

df$observationtime365<-df$observationtime
df$observationtime365[df$observationtime>365]<- 366

#opdelen in 3 groepen
male<-subset(df, df$sex==1)
female<-subset(df, df$sex==2)

cut2(male$SMI_thres, g=3,onlycuts=TRUE)
cut2(female$SMI_thres, g=3,onlycuts=TRUE)

male$muscleareaclass <- as.factor(cut2(male$SMI_thres,g=3))
levels(male$muscleareaclass) <- c('low','mid', 'high')

female$muscleareaclass <- as.factor(cut2(female$SMI_thres,g=3))
levels(female$muscleareaclass) <- c('low','mid', 'high')

cut2(male$HUmuscle, g=3,onlycuts=TRUE)
cut2(female$HUmuscle, g=3,onlycuts=TRUE)

male$HUclass <- as.factor(cut2(male$HUmuscle,g=3))
levels(male$HUclass) <- c('low','mid', 'high')

female$HUclass <- as.factor(cut2(female$HUmuscle,g=3))
levels(female$HUclass) <- c('low','mid', 'high')

cut2(male$IMATper, g=3,onlycuts=TRUE)
cut2(female$IMATper, g=3,onlycuts=TRUE)

male$IMATclass <- as.factor(cut2(-male$IMATper,g=3))
levels(male$IMATclass) <- c('low','mid', 'high')

female$IMATclass <- as.factor(cut2(-female$IMATper,g=3))
levels(female$IMATclass) <- c('low','mid', 'high')

df<- as.data.frame(rbind(female, male))

df$muscleareaclass2[df$muscleareaclass== 'low'] <- 1
df$muscleareaclass2[df$muscleareaclass== 'mid'|df$muscleareaclass== 'high'] <- 2
df$muscleareaclass2<-factor(df$muscleareaclass2, labels=c('low', 'high'))
df$muscleareaclass2<-relevel(df$muscleareaclass2, ref = 'high')

df$HUclass2[df$HUclass== 'low'] <- 1
df$HUclass2[df$HUclass== 'mid'|df$HUclass== 'high'] <- 2
df$HUclass2<-factor(df$HUclass2, labels=c('low', 'high'))
df$HUclass2<-relevel(df$HUclass2, ref = 'high')

#catagorical
df$IMATclass2[df$IMATclass== 'low'] <- 1
df$IMATclass2[df$IMATclass== 'mid'|df$IMATclass== 'high'] <- 2
df$IMATclass2<-factor(df$IMATclass2, labels=c('low', 'high'))
df$IMATclass2<-relevel(df$IMATclass2, ref = 'high')

df$combi_areaHU[df$muscleareaclass== 'low' & df$HUclass=='low'] <- 1
df$combi_areaHU[] <- 2
df$combi_areaHU[df$muscleareaclass== 'low' & df$HUclass=='low'] <- 1

df$combi_areaIMAT[df$muscleareaclass== 'low' & df$IMATclass=='low'] <- 1
df$combi_areaIMAT[] <- 2
df$combi_areaIMAT[df$muscleareaclass== 'low' & df$IMATclass=='low'] <- 1

df$combi_areaHU<-factor(df$combi_areaHU, labels=c('lowlow','rest'))
df$combi_areaHU<-relevel(df$combi_areaHU, ref = 'rest')

df$combi_areaIMAT<-factor(df$combi_areaIMAT, labels=c('lowlow','rest'))
df$combi_areaIMAT<-relevel(df$combi_areaIMAT, ref = 'rest')

#theme
theme <- theme(axis.line = element_line(colour = "black", size=0.4),
               panel.grid.major = element_line(colour = "grey90", size=0.4),
               panel.grid.minor = element_blank(),
               panel.border = element_blank(),
               panel.background = element_blank(),
               axis.ticks.x = element_blank(),
               axis.ticks.y = element_blank(),
               axis.text = element_text(color = "black",size=12, face = 'plain'),
               text=element_text(color = "black",size=12, face = 'plain')
) 

######
#SMI
#Kaplan Meier
fit <- survfit(Surv(observationtime, mortality) ~ muscleareaclass, data = df)
fit

p<-ggsurvplot(fit,xlab="Time (Years)",xscale="d_y", size=0.7,axes.offset=FALSE,
           break.time.by=(365.25),pval=F, test.for.trend=FALSE,
           conf.int=FALSE,label.curves=TRUE, ggtheme=theme_minimal()+theme,
           tables.theme=theme_cleantable(), xlim=c(0,1700),expand_limits=c(0,0),
           risk.table=T, risk.table.fontsize=4, risk.table.y.text = F, censor=F, palette = c("red",'black','blue'), ylim = c(0.4, 1.0))
print(p)
#export => Save as PDF => 5x6 inch

#log rank
surv_diff <- survdiff(Surv(observationtime, mortality) ~ muscleareaclass, data = df)
surv_diff

#Cox regression 
df$observationtime<-ifelse(df$observationtime==0, 1, df$observationtime)

#crude model
#1year
res.cox <- coxph(Surv(observationtime365, mortality_1year==1) ~ muscleareaclass2 , data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observationtime, mortality==1) ~ muscleareaclass2 , data = df)
summary(res.cox)

#adjusted model
#1year
res.cox <- coxph(Surv(observatietime365, mortality_1year==1) ~ muscleareaclass2+age+ sex+EURO2+NYHAtwogroups+COPD+LVF+accessroute+CKD , data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observationtime, mortality==1) ~ muscleareaclass2+age+ sex+EURO2+NYHAtwogroups+COPD+LVF+accessroute+CKD , data = df)
summary(res.cox)

###### 
#SMD
#Kaplan Meier
fit <- survfit(Surv(observationtime, mortality) ~ HUclass, data = df)
fit
summary(fit)$table

p<-ggsurvplot(fit,xlab="Time (Years)",xscale="d_y", size=0.7,axes.offset=FALSE,
              break.time.by=(365.25),pval=F, test.for.trend=FALSE,
              conf.int=FALSE,label.curves=TRUE, ggtheme=theme_minimal()+theme,
              tables.theme=theme_cleantable(), xlim=c(0,1700),expand_limits=c(0,0),
              risk.table=T, risk.table.fontsize=4,  risk.table.y.text = F, censor=F, palette = c("red",'black','blue'), ylim = c(0.4, 1.0))
print(p)
#export => Save as PDF => 5x6 inch

#log rank
surv_diff <- survdiff(Surv(observationtime, mortality) ~ HUclass, data = df)
surv_diff
res <- pairwise_survdiff(Surv(observationtime, mortality) ~ HUclass, data = df)
res
#Cox regression 
df$observationtime<-ifelse(df$observationtime==0, 1, df$observationtime)


#crude model
#1year
res.cox <- coxph(Surv(observatietijd365, overleden_1jaar==1) ~ HUclass2 , data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observationtime, mortality==1) ~ HUclass2 , data = df)
summary(res.cox)

#adjusted model
#1year
res.cox <- coxph(Surv(observationtime365, mortality_1year==1) ~ HUclass2+age+ sex+EURO2+NYHAtwogroups+COPD+LVF+accessroute+CKD , data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observationtime, mortality==1) ~ HUclass2+age+ sex+EURO2+NYHAtwogroups+COPD+LVF+accessroute+CKD , data = df)
summary(res.cox)

###### 
#IMAT
#Kaplan Meier
fit <- survfit(Surv(observationtime, mortality) ~ IMATclass, data = df)
fit
summary(fit)$table

p<-ggsurvplot(fit,xlab="Time (Years)",xscale="d_y", size=0.7,axes.offset=FALSE,
           break.time.by=(365.25),pval=F, test.for.trend=FALSE,
           conf.int=FALSE,label.curves=TRUE, ggtheme=theme_minimal()+theme,
           tables.theme=theme_cleantable(), xlim=c(0,1700),expand_limits=c(0,0),
           risk.table=T, risk.table.fontsize=4, censor=F, risk.table.y.text = F, palette = c("red",'black', "blue"), ylim = c(0.4, 1.0))
print(p)
#export => Save as PDF => 5x6 inch

#log rank
surv_diff <- survdiff(Surv(observationtime, mortality) ~ IMATclass, data = df)
surv_diff

#Cox regression 
#crude model
#1 year
res.cox <- coxph(Surv(observationtime365, mortality_1year==1) ~ IMATclass2 , data = df)
summary(res.cox)
#5jaar
res.cox <- coxph(Surv(observatietijd, Overleden==1) ~ IMATclass2 , data = df)
summary(res.cox)

#adjusted model  
#1 year
res.cox <- coxph(Surv(observationtime365, mortality_1year==1) ~ IMATclass2+age+ sex+EURO2+NYHAtwogroups+COPD+LVF+accessroute+CKD , data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observationtime, mortality==1) ~ IMATclass2+age+ sex+EURO2+NYHAtwogroups+COPD+LVF+accessroute+CKD , data = df)
summary(res.cox)

######
#combi


#KM
fit <- survfit(Surv(observatietijd, Overleden) ~ df$combi2, data = df)
fit
summary(fit)$table

p<-ggsurvplot(fit,xlab="Time (Years)",xscale="d_y", size=0.7,axes.offset=FALSE,
           break.time.by=(365.25),pval=F, test.for.trend=FALSE,
           conf.int=FALSE,label.curves=TRUE, ggtheme=theme_minimal()+theme,
           tables.theme=theme_cleantable(), xlim=c(0,1700),expand_limits=c(0,0),
           risk.table=T, risk.table.fontsize=5, censor=F, risk.table.y.text = F, palette = c("red",'blue'), ylim = c(0.4, 1.0))
print(p)
#export => Save as PDF => 6.5x6.5 inch

#log rank
surv_diff <- survdiff(Surv(observatietijd, Overleden) ~ combi, data = df)
surv_diff



#combi SMI-SMD
#Crude
#1year
res.cox <- coxph(Surv(observatietijd365, overleden_1jaar==1) ~ df$combi, data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observatietijd, Overleden==1) ~ combi , data = df)
summary(res.cox)

#Adjusted model        
#1year
res.cox <- coxph(Surv(observatietijd365, overleden_1jaar==1) ~ combi+Age+ geslacht+EURO2+NYHA+COPD+LVF+Accessroute+CKD , data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observatietijd, Overleden==1) ~ combi+Age+ geslacht+EURO2+NYHA+COPD+LVF+Accessroute+CKD , data = df)
summary(res.cox)

#combi2 SMI-IMAT
#Crude
#1year
res.cox <- coxph(Surv(observatietijd365, overleden_1jaar==1) ~ df$combi2, data = df)
summary(res.cox)
#long-term
res.cox <- coxph(Surv(observatietijd, Overleden==1) ~ combi2 , data = df)
summary(res.cox)

#Adjusted model        
#1jaar
res.cox <- coxph(Surv(observatietijd365, overleden_1jaar==1) ~ combi2+Age+ geslacht+EURO2+NYHA+COPD+LVF+Accessroute+CKD , data = df)
summary(res.cox)
#5jaar
res.cox <- coxph(Surv(observatietijd, Overleden==1) ~ combi2+Age+ geslacht+EURO2+NYHA+COPD+LVF+Accessroute+CKD , data = df)
summary(res.cox)
        




