#LOAD DATA & LIBRARY:=====
library(tidyverse)
library(lme4)
library(lmerTest)

data<- read.csv("TopsoilEmergenceData.csv")
str(data)#data.frame':	38247 obs. of  24 variables:

#Stats on WEEDS Spring 2012 (all treatments):=====
#Subset the year one emergence data (densitties per su [sampling unit]):
year1data<-data[data$TST== 0.5 & data$nat=="invasive",] #TST = time since transfer
year1data$su <-factor(year1data$su) #factor is the way to drop emtpy levels
length(levels(year1data$su))#673 = Correct!
str(year1data)#	7817 obs. of  24 variables:

#Run aggregate to get total number of su in year two:
i1 <-aggregate(year1data$count1m2,              
                      by = list(su=year1data$su, site=year1data$site, fence=year1data$fence,
                                Transdepth=year1data$Transdepth, rip=year1data$rip,
                                cluster=year1data$cluster, plot2=year1data$plot2), FUN = "sum")
length(levels(droplevels(i1$su)))#673= total number of su-s (sampling units) in year two
#number of plots went up in year two compared to year one
#as we increased number of observation units (su) due to high mortality after year one surveys,
colnames(i1)[8]<-"sum1m2"
i1$year<-"one"
head(i1)


#We filter out unrelated variables = shade, shade.semi, 
w1<-filter(i1, plot2 == "smoke"|plot2 == "herbicide"|plot2 == "control"|plot2 == "plastic"|plot2 == "smoke.plastic")
dim(w1)# 625   9

w1$rip<- factor(w1$rip, levels = c( "unripped","ripped"))#relevel to show ripped in stats table
w1$Transdepth<- factor(w1$Transdepth, levels = c("shallow","deep"))#relevel to show deep in stats table

#Fit model:
fit.w1<-glmer(sum1m2~Transdepth*rip+fence+plot2
               +(1|site) +(1|cluster), family = poisson(link="log"), data=w1)

summary(fit.w1)#all plot-scale treatments (plot2) were within the fence:
#STATS OUTPUT:Fixed effects:
#########################Estimate Std. Error     df   t value Pr(>|t|)    
(Intercept)               4.77547    0.18784  25.423  < 2e-16 ***
Transdepthdeep           -0.22172    0.07547  -2.938  0.00330 ** 
ripripped                -0.51285    0.07562  -6.782 1.19e-11 ***
fenceopen                -0.06263    0.05734  -1.092  0.27479    
plot2herbicide           -0.41827    0.02507 -16.683  < 2e-16 ***
plot2plastic             -0.16946    0.02347  -7.221 5.17e-13 ***
plot2smoke               -0.06004    0.02285  -2.627  0.00861 ** 
plot2smoke.plastic       -0.15200    0.02337  -6.505 7.79e-11 ***
Transdepthdeep:ripripped -0.06353    0.10714  -0.593  0.55325 #Any disturbance reduces weed density. Ripping>Herbicide>Transdepth>plastic

library(broom)#getting table broom-way into manuscript:
w1.output<-tidy(fit.w1, effects ="fixed")# WE need to round them:
w1.output$Season <- "Spring 2012"
w1.output

#Stats on WEEDS Spring 2013 (all treatments):=====
library(lme4)
library(lmerTest)
library(tidyverse)
data<- read.csv("TopsoilEmergenceData.csv")

#subset the year one emergence data:
year2data<-data[data$TST == 1.5 & data$nat=="invasive",] #TST = time since transfer
year2data$su <-factor(year2data$su) #factor is the way to drop emtpy levels
length(levels(droplevels((year2data$su))))#847 = Correct.

#Summarize by all plots(su), by all Site.Treatments:
i2 <-aggregate(year2data$count1m2,              
               by = list(su=year2data$su, site=year2data$site, fence=year2data$fence,
                         Transdepth=year2data$Transdepth, rip=year2data$rip,
                         cluster=year2data$cluster, plot2=year2data$plot2), FUN = "sum")
length(levels(droplevels(i2$su)))#847 = total number of su-s (sampling units) in year two
#number of plots went up in year two compared to year one
#as we increased number of observation units (su) due to high mortality after year one surveys,
colnames(i2)[8]<-"sum1m2"
i2$year<-"one"
head(i2)


#Keep the relevant plot-scale treatments.
w2<-filter(i2, plot2 == "smoke"|plot2 == "herbicide"|plot2 == "control"|plot2 == "plastic")
dim(w2)#726   9

w2$rip<- factor(w2$rip, levels = c( "unripped","ripped"))
w2$Transdepth<- factor(w2$Transdepth, levels = c("shallow","deep"))

#Fit model:
fit.w2<-glmer(sum1m2~Transdepth*rip+fence+plot2+(1|site/cluster),family = poisson(link="log"), data=w2)
summary(fit.w2)
#STATS OUTPUT:
#######################Estimate Std. Error     df t value Pr(>|t|)    
(Intercept)               5.19417    0.12676   40.98   <2e-16 ***
Transdepthdeep            0.19532    0.12952    1.51   0.1315    
ripripped                -0.23868    0.12958   -1.84   0.0655 .  
fenceopen                -0.12796    0.09736   -1.31   0.1887    
plot2herbicide           -0.03307    0.01728   -1.91   0.0556 .  
plot2plastic             -0.02512    0.01725   -1.46   0.1453    
plot2smoke               -0.22780    0.01795  -12.69   <2e-16 ***
Transdepthdeep:ripripped -0.22422    0.18326   -1.22   0.2212  

library(broom)#getting table broom-way into manuscript:
w2.output<-tidy(fit.w2, effects ="fixed")# WE need to round them:
w2.output$Season <- "Spring 2013"

#Write Table:
w1w2 <- rbind (w1.output, w2.output)
#write.table(w1w2,row.names = FALSE, file="StatsOutput_Weeds_All.csv",sep=",")
