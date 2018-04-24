#METADATA for EmData----------
#EmData = R-File for Emergence Data Analysis & Visualization.
#It contains data on emergence of plant species on restoration sites
#(Details in Pawel Waryszak 's PhD thesis)
#TST = Time since Topsoil transfer, 0.5 = spring 2012 (half a year after transfer) & 1.5 = Spring 2013
#newTST - relates to when the seedling was first recorded (important for survial chapter)
#count4m2 = total Count per 4 square meters vegetation survey plot.
#count1m2 = totCnt/4 to express it per 1 square m.
#cluster = block of 4m2 plots, here called "sub"
#install.packages("lme4", "tidyverse", "Rmisc", "lmerTest", "broom") #run this first if no packages present on your R.
library(tidyverse)
library(Rmisc)
library(lme4)
library(lmerTest)
library(broom)

#Load Topsoil Emergence DATA (Spring 2012 & Spring 2013 seasons) :======
data<- read.csv("TopEmergenceGood.csv")
str(data)#40924 obs. of  22 variables:
data$rip<-factor(data$rip, levels = c("unripped","ripped")) #reshuffling levels to show rip effect in stats tables.

#Subset: PERRENIALS SITE-TREATMENT ONLY (CONTROLS ONLY YEAR ONE)======
gnative<- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
gnative1<-gnative[gnative$plot2=="control" & gnative$TST== 0.5,]#only controls in spring2012=tst05
gnative2<-gnative1[gnative1$TST== 0.5 & gnative1$nat=="native" ,]#only natives
levels(droplevels(gnative2$nat))#native only (double check if we got natives only)
levels(droplevels(gnative1$plot2))#control only  (double check if we go controls only)
dim(gnative2)#6451  22 (looking at the dimension of our new data)
n1<-levels(droplevels(gnative1$su))
length(n1)#433 = number of levels in su (sampling unit)
n2<-levels(droplevels(gnative2$su))
length(n2)#433 all good! (double check on redudant levels)

#Alternatively:
g2<-data[data$TST== 0.5 & data$nat=="native" & data$plot2=="control" ,]#only natives
dim(g2)#6451   22 = gnative2

#Compute densities of natives' emergence per su, per longevity (spr12)======
#in the first spring after topsoil transfer (spr12):
a1<-aggregate(gnative2$count4m2,
              by = list(su=gnative2$su,site=gnative2$site,cluster=gnative2$cluster,
                        Transdepth=gnative2$Transdepth, rip=gnative2$rip,
                        fence=gnative2$fence,plot2=gnative2$plot2,longevity=gnative2$longevity), FUN = "sum")
dim(a1)# 834   9 data dimensions of all annuals & perennials in spring 2012
a1$year<-"one" # create a column "year" for latter merging and computations.
colnames(a1)[9]<-"Density" # create a column "year"Density" for latter merging and computations.

#Droping longevity to produce qa1 (checking for empty su-s)
qa1<-aggregate(gnative2$count4m2, by = list(su=gnative2$su,site=gnative2$site,cluster=gnative2$cluster,
                                                  Transdepth=gnative2$Transdepth, rip=gnative2$rip,
                                                  fence=gnative2$fence,plot2=gnative2$plot2), FUN = "sum")
dim(qa1)#all 433 rows by 8 columns = all good, 433 spring 2012su-s(sampling units)
colnames(qa1)[8]<-"Density"
qa1$Density<-0   #assigning zeros for clarity and further merging
qa1$longevity<-"perennial"
qa1$year<-"one"
head(qa1)#all native controls spring 2012


#SUBSET PERENNIALS ONLY FROM YEAR ONE:======
a1.perennial<-a1[a1$longevity=="perennial",]
#checking for empty plots and merging them in:
dim(a1.perennial)#426 rows by 10 cols  -> 426-433 = 7 missing zero su-s
mm<-merge(a1.perennial,qa1, all.x = TRUE,all.y = TRUE, by="su") #finding & merging in the missing su-s
mm
nn<-mm[is.na(mm$site.x),]#NA-s in x = su-s missing
nn
oo<-nn[,c(1,11:19)]#cutting the col of missing su-s
oo# (double check)
dim(oo) #= 7 rows  by 10 cols
#changing the corresponding colnames for rbind:
names(a1.perennial)#"su" ,"site","cluster","Transdepth","rip","fence","plot2","longevity","Density"        "year"
colnames(oo)<-c("su" ,"site","cluster","Transdepth","rip","fence","plot2","Density","longevity","year")
perennials.year.one<-rbind(a1.perennial,oo)#data.frame':	433 obs. of  10 variables:
str(perennials.year.one)#433 obs. of  10 variables:
levels(droplevels((perennials.year.one$longevity)))# "perennial" only

#STATS on YEAR ONE (SPR12) GERMINATION: PERENNIALS: SITE-TREATMENT ONLY:=====
library(lme4)
PER.glmer.spr12<-glmer(Density ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth
                       +(1|site/cluster),family = poisson(link="log"), data=perennials.year.one)
summary(PER.glmer.spr12)

glmer.per.output.random=tidy(PER.glmer.spr12, effects = "ran_pars")
glmer.per.output=tidy(PER.glmer.spr12, effects ="fixed")
glmer.per.output
#write.table(glmer.per.output,file="glmerOutputPerennials.spr12.csv",sep=",")
#STATS Output:Fixed effects:
############################Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  3.936863   0.195655  20.121  < 2e-16 ***
ripripped                   -0.953234   0.237262  -4.018 5.88e-05 ***
fenceopen                   -0.100184   0.260407  -0.385   0.7004    
Transdepthshallow           -0.455646   0.240476  -1.895   0.0581 .  
ripripped:fenceopen          0.117017   0.300422   0.390   0.6969    
fenceopen:Transdepthshallow  0.127042   0.300451   0.423   0.6724    
ripripped:Transdepthshallow  0.005616   0.294003   0.019   0.9848






#SUBSET PERENNIALS IN YEAR TWO ONLY + compute densities per su:=========
data<- read.csv("TopEmergenceGood.csv")
names(data)

ggnative<- data[data$TST==1.5 & data$plot2=="control",]#only records from spring II 
length(as.character(levels(droplevels(ggnative$su))))#588 SU-s that is survey plots

ggnative2<-ggnative[ggnative$newTST== 1.5 ,]
length(as.character(levels(droplevels(ggnative2$su))))#588 SU-s that is survey plots

ggnative3<-ggnative2[ggnative2$nat=="native",]#native that germinated only in year two
length(as.character(levels(droplevels(ggnative3$su))))#584 SU-s that is survey plots
dim(ggnative3)#7621   44
levels(droplevels(ggnative3$nat))#"native"

#ALTERNATIVELy:
#ggnative3<-data2[data2$newTST== 1.5 & data2$TST== 1.5 & data2$nat=="native" & data2$plot2=="control" , ]#only natives

ggnative<- data2[data2$TST==1.5 & data2$plot2=="control",]#only records from spring II 
length(as.character(levels(droplevels(ggnative$su))))#588 SU-s that is survey plots

ggnative2<-ggnative[ggnative$newTST== 1.5 ,]
length(as.character(levels(droplevels(ggnative2$su))))#588 SU-s that is survey plots

ggnative3<-ggnative2[ggnative2$nat=="native",]#native that germinated only in year two
length(as.character(levels(droplevels(ggnative3$su))))#584 SU-s that is survey plots
dim(ggnative3)#7621   44
levels(droplevels(ggnative3$nat))#"native"

a2<-aggregate(ggnative3$totCnt.DontUse,
              by = list(su=ggnative3$su,site=ggnative3$site,plot=ggnative3$plot,
                        Transdepth=ggnative3$Transdepth, rip=ggnative3$rip,
                        fence=ggnative3$fence,plot2=ggnative3$plot2,longevity=ggnative3$longevity), FUN = "sum")

#finding and merging the zero plots:ddplyr and duplicate did not work for some reason
a2$year<-"two"
colnames(a2)[9]<-"sum"

a2.perenial<-a2[a2$longevity=="perennial",]# 578 by 10
dim(a2.perenial)# = 578 by 10 => missing zero plots out of 588
#finding and merging the zero plots:ddplyr, duplicate funcition did not work for some reason

qa2p<-aggregate(ggnative2$totCnt.DontUse, by = list(su=ggnative2$su,site=ggnative2$site,plot=ggnative2$plot,
                                                    Transdepth=ggnative2$Transdepth, rip=ggnative2$rip,
                                                    fence=ggnative2$fence,plot2=ggnative2$plot2), FUN = "sum")
dim(qa2p)# 588  10
colnames(qa2p)[8]<-"sum"
qa2p$sum<-0#assigning zeros for clarity and further merging
qa2p$longevity<-"perennial"
qa2p$year<-"two"
head(qa2p)#all native controls YEAR II (spring 2013)

#MErging zero plots with reminder of survey plots
mxx<-merge(a2.perenial,qa2p, all.x = TRUE,all.y = TRUE, by="su")
dim(mxx)# 588  19
nxx<-mxx[is.na(mxx$site.x),]#NA-s in x = plots missing
nxx
oxx<-nxx[,c(1,11:19)]#cutting the col of missing plots
oxx#
dim(oxx)#10 by 10
#changing the corresponding colnames for rbind:
names(a2.perenial)#"su" ,"site","plot","Transdepth","rip","fence","plot2","longevity","sum"        "year"
colnames(oxx)<-c("su" ,"site","plot","Transdepth","rip","fence","plot2","sum","longevity","year")
perennials.year.two<-rbind(a2.perenial,oxx)#data.frame':	433 obs. of  10 variables:
str(perennials.year.two)#588 obs. of  10 variables:

#STATS on native perennials in YEAR TWO======
PER.glmer.spr13<-glmer(sum ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth
                       +(1|site/plot),family = poisson(link="log"), data=perennials.year.two)
summary(PER.glmer.spr13)
#STATS output:
Fixed effects:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   2.9800     0.2386  12.488   <2e-16 ***
  ripripped                     0.2402     0.2830   0.849    0.396    
fenceopen                    -0.2732     0.3073  -0.889    0.374    
Transdepthshallow            -0.1413     0.2836  -0.498    0.618    
ripripped:fenceopen           0.1194     0.3534   0.338    0.735    
fenceopen:Transdepthshallow   0.3286     0.3534   0.930    0.352    
ripripped:Transdepthshallow  -0.4220     0.3465  -1.218    0.223 

library(broom)#way to save stats output in a csv file:
glmer.per.output.random=tidy(PER.glmer.spr13, effects = "ran_pars")
glmer.per.output=tidy(PER.glmer.spr13, effects ="fixed")
glmer.per.output
#write.table(glmer.per.output,file="glmerOutputPerennials.spr13.csv",sep=",")


#computing 95% CI-s for both years==============
library(Rmisc)
germ<-rbind(a1,a2)#merging year one and two SUs
str(germ)#'data.frame': 1971 obs. of  10 variables:
range(germ$sum)#0.0 0.0 105.5
germ$log10<-log(germ$sum + 1)
range(germ$log10)#  0.000000 4.668145


palog10.depth<-summarySE(germ, measurevar="log10", groupvars=c("Transdepth","longevity","year"))
colnames(palog10.depth)[1]<-"Site.Treatment"

palog10.rip<-summarySE(germ, measurevar="log10", groupvars=c("rip","longevity","year"))
colnames(palog10.rip)[1]<-"Site.Treatment"

palog10.fence<-summarySE(germ, measurevar="log10", groupvars=c("fence","longevity","year"))
colnames(palog10.fence)[1]<-"Site.Treatment"

palog10=rbind(palog10.depth,palog10.rip,palog10.fence)
str(palog10)#24 obs. of  8 variables:
#write.table(palog10, file="BACKLogSingleTreatmentFile.csv", sep=",",row.names=FALSE)



#FIGURE 1- effect of site-scale treatments on plant emergence densities================
#Re-Load  data to be sure all works well [Topsoil Emergence DATA:only spr12 & spr13 seasons included:
data<- read.csv("TopEmergenceGood.csv")
str(data)#40924 obs. of  22 variables: = all good.

#Load Trait data to split our species density data into growth forms"
library(dplyr)
traitURL <- "https://sites.google.com/site/pawelwaryszak/forestdale-experiment/chapter-6/traits.all_19oct15.csv?attredirects=0&d=1"
Traits <- read.csv(url(traitURL)) # read in data for traits
dataCut<-data[,1:15]
data2 <- dplyr::right_join(dataCut,Traits, by = "specCode")
head(data2)
str(data2)#40929 obs. of  42 variables:
n<-levels(droplevels(data2$su))#numner of su-s = sampling units
length(n)#853

#SUBSET PERENNIALS ONLY FROM YEAR ONE:======
a1.perennial<-a1[a1$longevity=="perennial",]
#checking for empty plots and merging them in:
dim(a1.perennial)#426 rows by 10 cols  -> 426-433 = 7 missing zero su-s
mm<-merge(a1.perennial,qa1, all.x = TRUE,all.y = TRUE, by="su") #finding & merging in the missing su-s
mm
nn<-mm[is.na(mm$site.x),]#NA-s in x = su-s missing
nn
oo<-nn[,c(1,11:19)]#cutting the col of missing su-s
oo# (double check)
dim(oo) #= 7 rows  by 10 cols
#changing the corresponding colnames for rbind:
names(a1.perennial)#"su" ,"site","cluster","Transdepth","rip","fence","plot2","longevity","Density"        "year"
colnames(oo)<-c("su" ,"site","cluster","Transdepth","rip","fence","plot2","Density","longevity","year")
perennials.year.one<-rbind(a1.perennial,oo)#data.frame':	433 obs. of  10 variables:
str(perennials.year.one)#433 obs. of  10 variables:
levels(droplevels((perennials.year.one$longevity)))# "perennial" only


#SUBSET PERENNIALS IN YEAR TWO ONLY + compute densities per su:=========
data<- read.csv("TopEmergenceGood.csv")
names(data)

ggnative<- data[data$TST==1.5 & data$plot2=="control",]#only records from spring II 
length(as.character(levels(droplevels(ggnative$su))))#588 SU-s that is survey plots

ggnative2<-ggnative[ggnative$newTST== 1.5 ,]
length(as.character(levels(droplevels(ggnative2$su))))#588 SU-s that is survey plots

ggnative3<-ggnative2[ggnative2$nat=="native",]#native that germinated only in year two
length(as.character(levels(droplevels(ggnative3$su))))#584 SU-s that is survey plots
dim(ggnative3)#7621   44
levels(droplevels(ggnative3$nat))#"native"

#ALTERNATIVELy:
#ggnative3<-data2[data2$newTST== 1.5 & data2$TST== 1.5 & data2$nat=="native" & data2$plot2=="control" , ]#only natives

ggnative<- data2[data2$TST==1.5 & data2$plot2=="control",]#only records from spring II 
length(as.character(levels(droplevels(ggnative$su))))#588 SU-s that is survey plots

ggnative2<-ggnative[ggnative$newTST== 1.5 ,]
length(as.character(levels(droplevels(ggnative2$su))))#588 SU-s that is survey plots

ggnative3<-ggnative2[ggnative2$nat=="native",]#native that germinated only in year two
length(as.character(levels(droplevels(ggnative3$su))))#584 SU-s that is survey plots
dim(ggnative3)#7621   44
levels(droplevels(ggnative3$nat))#"native"

a2<-aggregate(ggnative3$totCnt.DontUse,
              by = list(su=ggnative3$su,site=ggnative3$site,plot=ggnative3$plot,
                        Transdepth=ggnative3$Transdepth, rip=ggnative3$rip,
                        fence=ggnative3$fence,plot2=ggnative3$plot2,longevity=ggnative3$longevity), FUN = "sum")

#finding and merging the zero plots:ddplyr and duplicate did not work for some reason
a2$year<-"two"
colnames(a2)[9]<-"sum"

a2.perenial<-a2[a2$longevity=="perennial",]# 578 by 10
dim(a2.perenial)# = 578 by 10 => missing zero plots out of 588
#finding and merging the zero plots:ddplyr, duplicate funcition did not work for some reason

qa2p<-aggregate(ggnative2$totCnt.DontUse, by = list(su=ggnative2$su,site=ggnative2$site,plot=ggnative2$plot,
                                                    Transdepth=ggnative2$Transdepth, rip=ggnative2$rip,
                                                    fence=ggnative2$fence,plot2=ggnative2$plot2), FUN = "sum")
dim(qa2p)# 588  10
colnames(qa2p)[8]<-"sum"
qa2p$sum<-0#assigning zeros for clarity and further merging
qa2p$longevity<-"perennial"
qa2p$year<-"two"
head(qa2p)#all native controls YEAR II (spring 2013)

#MErging zero plots with reminder of survey plots
mxx<-merge(a2.perenial,qa2p, all.x = TRUE,all.y = TRUE, by="su")
dim(mxx)# 588  19
nxx<-mxx[is.na(mxx$site.x),]#NA-s in x = plots missing
nxx
oxx<-nxx[,c(1,11:19)]#cutting the col of missing plots
oxx#
dim(oxx)#10 by 10
#changing the corresponding colnames for rbind:
names(a2.perenial)#"su" ,"site","plot","Transdepth","rip","fence","plot2","longevity","sum"        "year"
colnames(oxx)<-c("su" ,"site","plot","Transdepth","rip","fence","plot2","sum","longevity","year")
perennials.year.two<-rbind(a2.perenial,oxx)#data.frame':	433 obs. of  10 variables:
str(perennials.year.two)#588 obs. of  10 variables:


#computing 95% CI-s for both years==============
library(Rmisc)
germ<-rbind(a1,a2)#merging year one and two SUs
str(germ)#'data.frame': 1971 obs. of  10 variables:
range(germ$sum)#0.0 0.0 105.5
germ$log10<-log(germ$sum + 1)
range(germ$log10)#  0.000000 4.668145


palog10.depth<-summarySE(germ, measurevar="log10", groupvars=c("Transdepth","longevity","year"))
colnames(palog10.depth)[1]<-"Site.Treatment"

palog10.rip<-summarySE(germ, measurevar="log10", groupvars=c("rip","longevity","year"))
colnames(palog10.rip)[1]<-"Site.Treatment"

palog10.fence<-summarySE(germ, measurevar="log10", groupvars=c("fence","longevity","year"))
colnames(palog10.fence)[1]<-"Site.Treatment"

palog10=rbind(palog10.depth,palog10.rip,palog10.fence)
str(palog10)#24 obs. of  8 variables:
#write.table(palog10, file="BACKLogSingleTreatmentFile.csv", sep=",",row.names=FALSE)



#LOG10 & BACK-transforming :=============
bt.log<-function(meanlog=NULL,sdlog=NULL,n=NULL,alpha=0.05){
  if(is.null(meanlog)|is.null(sdlog)|is.null(n)) stop ("meanlog,sdlog or n are missing")
  if(!is.numeric(meanlog)|!is.numeric(sdlog)|!is.numeric(n)) stop ("meanlog, sdlog, or n are not numeric.")
  sdlog<-sdlog^2   
  G<-function(t){
    j<-2
    s<-(((n-1)**3)/((n+1)*(n**2)*2))*t**2
    a<-1+(((n-1)/n)*t)+s
    while (s>0.000001){
      j<-j+1
      b<-(((n-1)**2)/(n*(n+2*j-3)*j))*t
      s<-s*b
      a<-a+s
    }
    a
  }
  var1<-exp(2*meanlog)*(G(2*sdlog)-G(((n-2)/(n-1))*sdlog))
  outpt<-unlist(list(btmean=exp(meanlog)*G(0.5*sdlog),
                     approx.bt.mean=exp(meanlog+sdlog/2),var=var1,sd=sqrt(var1),
                     var.mean=var1/n,sd.mean=sqrt(var1/n),
                     median=exp(meanlog),LCI=exp(meanlog+sdlog/2-sqrt((sdlog/n)+(sdlog^2)/(2*(n-1)))*qt(1-alpha/2,n-1)),
                     UCI=exp(meanlog+sdlog/2+sqrt((sdlog/n)+(sdlog^2)/(2*(n-1)))*qt(1-alpha/2,n-1))))
  return(outpt)
}

paa<-palog10[1,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p1<-cbind(paa,pp)
p1

paa<-palog10[2,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p2<-cbind(paa,pp)
p2

paa<-palog10[3,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p3<-cbind(paa,pp)
p3

paa<-palog10[4,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p4<-cbind(paa,pp)
p4

paa<-palog10[5,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p5<-cbind(paa,pp)
p5

paa<-palog10[6,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p6<-cbind(paa,pp)
p6

paa<-palog10[7,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p7<-cbind(paa,pp)
p7

paa<-palog10[8,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p8<-cbind(paa,pp)
p8

paa<-palog10[9,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p9<-cbind(paa,pp)
p9

paa<-palog10[10,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p10<-cbind(paa,pp)
p10

paa<-palog10[11,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p11<-cbind(paa,pp)
p11

paa<-palog10[12,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p12<-cbind(paa,pp)
p12

paa<-palog10[13,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p13<-cbind(paa,pp)
p13

paa<-palog10[14,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p14<-cbind(paa,pp)
p14

paa<-palog10[15,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p15<-cbind(paa,pp)
p15

paa<-palog10[16,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p16<-cbind(paa,pp)
p16

paa<-palog10[17,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p17<-cbind(paa,pp)
p17

paa<-palog10[18,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p18<-cbind(paa,pp)
p18

paa<-palog10[19,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p19<-cbind(paa,pp)
p19

paa<-palog10[20,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p20<-cbind(paa,pp)
p20

paa<-palog10[21,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p21<-cbind(paa,pp)
p21

paa<-palog10[22,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p22<-cbind(paa,pp)
p22

paa<-palog10[23,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p23<-cbind(paa,pp)
p23

paa<-palog10[24,]
p<-bt.log(meanlog=paa$log10,sdlog=paa$sd,n=paa$N)
p<-as.data.frame(p)
pp<-t(p)#transpose
pp
p24<-cbind(paa,pp)
p24
#Alternative way of Applying bt.log:
#pallog10[1,10:15]
#dat<-pallog10[,1:7]
#x<-t(apply(dat[,c('log10','sd','N')], 1, function(x) bt.log(x[1],x[2],x[3])))#t does transpoing rows into columns
#x[1,]

pallog10<-rbind(p1,p2,p3,p4,p5,p6,p7,p8,p9,p10,p11,p12,p13,p14,p15,p16,p17,p18,p19,p20,p21,p22,p23,p24)#binding all single results
pallog10$backMean<-pallog10$btmean-1 #renaming colnames
pallog10$backUCI<-pallog10$UCI-1
pallog10$backLCI<-pallog10$LCI-1
str(pallog10)#data.frame':	24 obs. of  20 variables:

#Put in Filter names: Function do not work most likely due to uncorrect colnames
#See: http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name
write.table(pallog10, file="BACKLog24SingleTreatment.csv", sep=",",row.names=FALSE)# I had to add extra Filter column in excel as R does not handle well
#changing the factor levels in the produced data frame.
#See: http://stackoverflow.com/questions/28549045/dplyr-select-error-found-duplicated-column-name
#Otherwise the below script should have worked:
#bt<- pallog10[, c("Streat","longevity","year","log10","backMean","ci")]
#bt$Filter<-bt$Site.Treatment #Below does not work
#bt<- pallog10[, c("Streat","longevity","year","log10","backMean","ci")]
#levels(bt$Filter)[bt$Filter =="fenced" | bt$Filter=="open"]  <- "BIOTIC"
#levels(bt$Filter)[bt$Filter=="ripped" | bt$Filter=="unripped"]  <-"ABIOTIC"
#levels(bt$Filter)[bt$Filter=="deep" | bt$Filter=="shallow"]    <- "DISPERSAL"


#PLOT-ing Figure 1 = Backlog of 95% CIlog-values for Site-scale treatments ======
Emdata <- read.csv("BACKLog24SingleTreatment.csv")#data.frame':	24 obs. of  21 variables:
str(Emdata)#Emdata for 'Emergence back-logged data':	24 obs. of  21 variables: = one extrea Filter column compared to pallog10 data frame.
library(ggplot2)
pd <- position_dodge(.5)#setting the gaps between elements on graph
AllCtrllog10s.CI1<-ggplot(Emdata, aes(x=year, y=backMean, shape=longevity))
AllCtrllog10s.CI2<-AllCtrllog10s.CI1 +geom_errorbar(aes(ymin=log10-backLCI, ymax=log10+backUCI),width=.50,position=pd,size=1.4)
AllCtrllog10s.CI2
AllCtrllog10s.CI3<-AllCtrllog10s.CI2+ geom_point(position=pd,size=6)
AllCtrllog10s.CI3
AllCtrllog10s.CI3a<-AllCtrllog10s.CI3 + geom_line(position=pd) + scale_colour_manual(values = c("green", "red")) +scale_shape_manual(values=c(0,15))
AllCtrllog10s.CI3a
AllCtrllog10s.CI4<-AllCtrllog10s.CI3a+facet_grid(.~Filter+Site.Treatment)+theme_bw()
AllCtrllog10s.CI4


#with grid bigger fond +8:

AllCtrllog10s.CI5<- AllCtrllog10s.CI4 +theme(axis.text.x=element_text(vjust=0.5,size=20),
                                             axis.text.y=element_text(size=20),
                                             axis.title.x=element_blank(),
                                             axis.title.y=element_text(size=24),
                                             panel.grid.minor.x = element_blank(),
                                             strip.text=element_text(size=24),
                                             legend.position = c(.1, .85),
                                             legend.text = element_text(size = 20),
                                             legend.title = element_text(face = "italic",size=20))
#plot.title = element_text(lineheight=1.2, face="bold",size=20))

AllCtrllog10s.CI5
AllCtrllog10s.CI7<- AllCtrllog10s.CI5 +  scale_y_continuous("Plant Density (m\u00B2)")
AllCtrllog10s.CI7

ggsave(filename="fig1d.pdf", dpi=600) # way to save it to fit journal and A4 format easily
#GGplotting rip per site in year one:======
site.rip<-summarySE(germ, measurevar="log10", groupvars=c("rip","longevity","year","site"))
colnames(site.rip)[1]<-"Site.Treatment"
library(ggplot2)
pd <- position_dodge(.5)

AllCtrllog10s.CI1<-ggplot(site.rip, aes(x=site, y=log10, shape=longevity))
AllCtrllog10s.CI2<-AllCtrllog10s.CI1 +geom_errorbar(aes(ymin=log10-ci, ymax=log10+ci),width=.50,position=pd,size=1.4)
AllCtrllog10s.CI2
AllCtrllog10s.CI3<-AllCtrllog10s.CI2+ geom_point(position=pd,size=6)
AllCtrllog10s.CI3
AllCtrllog10s.CI3a<-AllCtrllog10s.CI3 + geom_line(position=pd,width=20)  + scale_colour_manual(values = c("green", "red")) +scale_shape_manual(values=c(0,15))
AllCtrllog10s.CI3a
AllCtrllog10s.CI4<-AllCtrllog10s.CI3a+facet_grid(year~site+Site.Treatment)+theme_bw()
AllCtrllog10s.CI4



#FIGURE 2- effect of plot-scale treatments on plant emergence densities================
data<- read.csv("TopEmergenceGood.csv")
str(data)#40924 obs. of  23 variables:
levels(data$plot2)#"control",heat","herbicide","plastic","pr2", "shade","shade.semi"    "smoke"         "smoke.plastic"
#change if needed to be consistent to: "CTRL" "HEAT" "HERB" "PLAS" "SHAD" "SHSE" "SMOK" "SMPL"
levels(droplevels(data$dataStart))#"aut13"     "spr12"     "spr12only"
library(dplyr)
library(ggplot2)
library(lmerTest)
library(broom)
library(Rmisc)
#merging count data with trait data:
#traitURL <- "https://sites.google.com/site/pawelwaryszak/forestdale-experiment/chapter-6/traits.all_19oct15.csv?attredirects=0&d=1"
#Traits <- read.csv(url(traitURL)) # read in data for traits 
#data2 <- dplyr::right_join(data,Traits, by = "specCode")
#str(data2)#############48649 obs. of  44 variables:

#PERRENIALS plot-TREATMENT ONLY YEAR ONE:=========
gsmall<- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
gsmall1<-gsmall[gsmall$TST== 0.5,]#only in spring2012=tst05
gsmall2<-gsmall1[gsmall1$TST== 0.5 & gsmall1$nat=="native" ,]#only natives
levels(droplevels(gsmall2$nat))#native 
levels(droplevels(gsmall1$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(gsmall2)#10333    23
n1<-levels(droplevels(gsmall1$su))
length(n1)#673
n2<-levels(droplevels(gsmall2$su))
length(n2)#673

#native IN YEAR TWO ONLY = compute densities per plot = su:==========
ggsmall<- data[data$TST==1.5,]#only records from spring II 
ggsmall2<-ggsmall[ggsmall$newTST== 1.5 ,]
#We need to remove shade cause not involved in germination:
ggsmall3<-ggsmall2[!ggsmall2$plot2=="shade" & !ggsmall2$plot2== "shade.semi",]
dim(ggsmall3)#rows x cols dimenention = 19530    23
levels(droplevels(ggsmall3$plot2))#"control", "heat" ,"herbicide","plastic","smoke","smoke.plastic"
ggsmall4<-ggsmall3[ggsmall3$nat=="native",]
dim(ggsmall4)# 10408    44

n<-rbind(gsmall3,ggsmall4)#binding two springs together (spr12 & spr13)
str(n)#19931 obs. of  23 variables:
n$year<-as.factor(ifelse(n$TST=="0.5", "one", "two"))#adding year factor
n.fenced<-n[n$fence=="fenced",]#subsetting only the fenced values as plot-treatments were applied
#only withing the fenced areas. So we need to disregard controls from outside the fence.
names(n.fenced)
n2<-aggregate(n.fenced$count1m2,              
              by = list(su=n.fenced$su,site=n.fenced$site,ClusterType=n.fenced$ClusterType,
                        Transdepth=n.fenced$Transdepth, rip=n.fenced$rip,year=n.fenced$year,
                        comb2=n.fenced$comb2,plot2=n.fenced$plot2,longevity=n.fenced$longevity), FUN = "sum")
str(n2)#data.frame':  1834 obs. of  10 variables: only fenced.
#n2$log10<-log(n2$x+1)



#SummarySE on BOTH YEARS in native:============
fig2<-summarySE(n2, measurevar="log10", groupvars=c("comb2","plot2","longevity","year"))
fig2
fig3<-fig2[ -c(67,68),]#removingshalow.unripped heat as it was a field mistake
dim(fig3)#82  9

#BACK-transforming function:============
bt.log<-function(meanlog=NULL,sdlog=NULL,n=NULL,alpha=0.05){
  if(is.null(meanlog)|is.null(sdlog)|is.null(n)) stop ("meanlog,sdlog or n are missing")
  if(!is.numeric(meanlog)|!is.numeric(sdlog)|!is.numeric(n)) stop ("meanlog, sdlog, or n are not numeric.")
  sdlog<-sdlog^2   
  G<-function(t){
    j<-2
    s<-(((n-1)**3)/((n+1)*(n**2)*2))*t**2
    a<-1+(((n-1)/n)*t)+s
    while (s>0.000001){
      j<-j+1
      b<-(((n-1)**2)/(n*(n+2*j-3)*j))*t
      s<-s*b
      a<-a+s
    }
    a
  }
  var1<-exp(2*meanlog)*(G(2*sdlog)-G(((n-2)/(n-1))*sdlog))
  outpt<-unlist(list(btmean=exp(meanlog)*G(0.5*sdlog),
                     approx.bt.mean=exp(meanlog+sdlog/2),var=var1,sd=sqrt(var1),
                     var.mean=var1/n,sd.mean=sqrt(var1/n),
                     median=exp(meanlog),LCI=exp(meanlog+sdlog/2-sqrt((sdlog/n)+(sdlog^2)/(2*(n-1)))*qt(1-alpha/2,n-1)),
                     UCI=exp(meanlog+sdlog/2+sqrt((sdlog/n)+(sdlog^2)/(2*(n-1)))*qt(1-alpha/2,n-1))))
  return(outpt)
}

#APPLY bt.log function across rows of data:

dat<-fig3
x<-t(apply(dat[,c('log10','sd','N')], 1, function(x) bt.log(x[1],x[2],x[3])))#t does transposing rows into columns
y<-fig3
xy<-cbind(y,x)

xy$backMean<-xy$btmean.log10-1
xy$backUCI<-xy$UCI.log10-1
xy$backLCI<-xy$LCI.log10-1
levels(xy$plot2)#"control","heat","herbicide","plastic","shade","shade.semi","smoke","smoke.plastic"

#renaming factors to make them look nicer on a plot:
xy$season<-as.factor(ifelse(xy$year=="one", "I", "II"))
xy$plot2 <- factor(xy$plot2, levels = c("control","herbicide", "smoke",  "smoke.plastic", "plastic",  "heat"))
xy$comb2.short<-xy$comb2
levels(xy$comb2.short)[levels(xy$comb2.short)=="deep.ripped"] <- "DR"
levels(xy$comb2.short)[levels(xy$comb2.short)=="deep.unripped"] <- "DU"
levels(xy$comb2.short)[levels(xy$comb2.short)=="shallow.ripped"] <- "SR"
levels(xy$comb2.short)[levels(xy$comb2.short)=="shallow.unripped"] <- "SU"
str(xy)#82 obs. of  23 variables:
#Adding Filter Column:
xy$FilterPlot2<-xy$plot2 #Below does not work
levels(xy$FilterPlot2)[xy$FilterPlot2=="smoke"]   <- "DISPERSAL" # and so on.... did not work
#so i added a FilterPlot2 column in excel file:

#Load data from ready file if in trouble:
xy2<-read.csv("backlogSMALLplots.csv")
str(xy2)#82 obs. of  26 variables:
#Change levels order to make it look nicer on a figure:
xy2$season<-as.factor(ifelse(xy2$year=="one", "I", "II"))#optional shortening of time names
names(xy2)
du <- xy2[ xy2$comb2.short=="DU" &  xy2$plot2 !="smoke.plastic" , ] #only deep.unripped (DU)
#and removing smoke.plastic as it is confusing for the  reader.
str(du)#18 obs. of  24 variables:
du$FilterPlot2 <- factor(du$FilterPlot2, levels = c("CONTROL","ABIOTIC", "BIOTIC",  "DISPERSAL"))
du

#DRAWING A PLOT:
library(ggplot2)
pd <- position_dodge(.6)
Fig2.CI1<-ggplot(du, aes(x=year, y=backMean, shape=longevity))
Fig2.CI2<-Fig2.CI1 +geom_errorbar(aes(ymin=backMean-backLCI, ymax=backMean+backUCI),width=.50,position=pd,size=1.4)
Fig2.CI2
Fig2.CI3<-Fig2.CI2+ geom_point(position=pd,size=6)
Fig2.CI3
Fig2.CI3a<-Fig2.CI3 + geom_line(position=pd)+scale_shape_manual(values=c(0,15))#+ scale_colour_manual(values = c("green", "red"))
Fig2.CI3a
Fig2.CI4<-Fig2.CI3a+facet_grid(.~ FilterPlot2 + plot2)+theme_bw()
Fig2.CI4
#with grid bigger fond:
Fig2.CI5<- Fig2.CI4 +theme(axis.text.x=element_text(vjust=0.5,size=20),
                           axis.text.y=element_text(size=20),
                           axis.title.x=element_blank(),
                           axis.title.y=element_text(size=24),
                           panel.grid.minor.x = element_blank(),
                           strip.text=element_text(size=24),
                           legend.position = c(.1, .85),
                           legend.text = element_text(size = 20),
                           legend.title = element_text(face = "italic",size=20))
#plot.title = element_text(lineheight=1.2, face="bold",size=20))

Fig2.CI5
Fig2.CI7<- Fig2.CI5 +  scale_y_continuous("Plant Density (m\u00B2)")
Fig2.CI7



#INVASIVES STATS emergence per su, (spr12)======
library(dplyr)
library(ggplot2)
library(Rmisc)
library(lme4)
library(broom)

data<- read.csv("TopEmergenceGood.csv")
str(data)#40924 obs. of  22 variables:
data$rip<-factor(data$rip, levels = c("unripped","ripped")) #reshuffling levels to show rip effect in stats tables.

#Subset: SITE-TREATMENT ONLY (CONTROLS ONLY YEAR ONE)
w<- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
w1<-w[w$plot2=="control" & w$TST== 0.5,]#only controls in spring2012=tst05
w2<-w1[w1$TST== 0.5 & w1$nat=="invasive" ,]#invasive
levels(droplevels(w2$nat))#double check 
levels(droplevels(w1$plot2))#control only  (double check if we go controls only)
dim(w2)#4965   23 (looking at the dimension of our new data)
n1<-levels(droplevels(w1$su))
length(n1)#433 = number of levels in su (sampling unit)
n2<-levels(droplevels(w2$su))
length(n2)#433 all good! (double check on presence of the redudant levels (zero values))

#Alternatively:
#w2<-data[data$TST== 0.5 & data$nat=="invasive" & data$plot2=="control" ,]#only natives
#dim(w2)#4965   23

#in the first spring after topsoil transfer - spring 2012:
w1<-aggregate(w2$count4m2,
              by = list(su=w2$su,site=w2$site,cluster=w2$cluster,
                        Transdepth=w2$Transdepth, rip=w2$rip,
                        fence=w2$fence,plot2=w2$plot2,longevity=w2$longevity), FUN = "sum")
dim(w1)# 866   9 data dimensions of all annuals & perennials in spring 2012
w1$year<-"one" # create a column "year" for latter merging and computations.
colnames(w1)[9]<-"Density" # create a column "year"Density" for latter merging and computations.
#STATS:
invasive.glmer.spr12<-glmer(Density ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth
                       +(1|site/cluster),family = poisson(link="log"), data=w1)
summary(invasive.glmer.spr12)
#OUTPUT:
############################Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  2.54557    0.27186   9.364  < 2e-16 ***
ripripped                   -0.94334    0.20961  -4.500 6.78e-06 ***
fenceopen                    0.06094    0.22709   0.268    0.788    
Transdepthshallow            0.11859    0.20925   0.567    0.571    
ripripped:fenceopen         -0.12333    0.26320  -0.469    0.639    
fenceopen:Transdepthshallow -0.06543    0.26301  -0.249    0.804    
ripripped:Transdepthshallow  0.12735    0.25784   0.494    0.621 
#Saving output in a tidy way:
glmer.per.output.random=tidy(invasive.glmer.spr12, effects = "ran_pars")
glmer.per.output=tidy(invasive.glmer.spr12, effects ="fixed")
glmer.per.output#easy to save in excel file csv.

#WEEDS PLOTS & TABLES ============
#These codes below are also in seperate R.file = WeedsEst.R

#Plotting Weeds: Effects of Treatments on weed densities YEAR I:=======
library(dplyr)
library(tidyr)
data<- read.csv("TopEmergenceGood.csv")
names(data)

#Group by treatmet and sampling units:
year1data<-data[data$TST== 0.5 & data$nat=="invasive",] #TST = time since transfer
length(levels(year1data$su))# 853 =too many levels, as it remembers all of them
year1data$su <-factor(year1data$su) #factor is the way to drop emtpy levels
length(levels(year1data$su))#673 = YAY!
#Let us compute plat densities per plot (su), per site-treatment:
d1.depth<-summarise(group_by(year1data,su,Transdepth),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("DISPERSAL"))
dim(d1.depth)#674   4
colnames(d1.depth)[2]<- "Site.Treatment"

d1.rip<-summarise(group_by(year1data,su,rip),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("ABIOTIC"))
dim(d1.depth)#674   4
colnames(d1.rip)[2]<- "Site.Treatment"

d1.fence<-summarise(group_by(year1data,su,fence),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("BIOTIC"))
dim(d1.fence)#674   4
colnames(d1.fence)[2]<- "Site.Treatment"

d1<-bind_rows(d1.depth,d1.rip,d1.fence)
dim(d1)#2022    4

#We need to split su into site, cluster,plot:
weeds1 <- d1  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

dim(weeds1)#673   9 =  YAY! These are densities of weeds in year one per 1m2
#Plotting Weeds: Effects of Treatments on weed densities YEAR II:=======
library(dplyr)
library(tidyr)
data<- read.csv("TopEmergenceGood.csv")
names(data)

#Group by treatmet and sampling units:
year2data<-data[data$TST== 1.5 & data$nat=="invasive",] #TST = time since transfer
length(levels(year2data$su))# 853 =too many levels, as it remembers all of them
year2data$su <-factor(year2data$su) #factor is the way to drop emtpy levels
length(levels(year2data$su))#846 = 8 missing =  NAY! These are densities of weeds in year one per 1m2,
# There were Some empty plots, not infested. We skip them as all plots were infested
#Some weeds were not recorded early in the veg season.


#Let us compute plat densities per plot (su), per site-treatment:
d2.depth<-summarise(group_by(year2data,su,Transdepth),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("DISPERSAL"))
dim(d2.depth)#847   4
colnames(d2.depth)[2]<- "Site.Treatment"

d2.rip<-summarise(group_by(year2data,su,rip),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("ABIOTIC"))
dim(d2.depth)#847   4
colnames(d2.rip)[2]<- "Site.Treatment"

d2.fence<-summarise(group_by(year2data,su,fence),Weed.Density.1m2 = sum(count1m2)) %>% mutate(Filter=as.character("BIOTIC"))
dim(d2.fence)#847   4
colnames(d2.fence)[2]<- "Site.Treatment"

d2<-bind_rows(d2.depth,d2.rip,d2.fence)
dim(d2)#2541    4

#We need to split su into site, cluster,plot:
weeds2 <- d2  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

#Drawing the sjmplot of coeficients: fit model
library(lme4)
library(lmerTest)
library(sjstats)
library(sjPlot)
library(nlme)
library(sjmisc)
library(car)
library(ggplot2)

fit.d1a<-glmer(Weed.Density.1m2~rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth+(1|site/cluster),family = poisson(link="log"), data=weeds1)
fit.d1b<-lmer(Weed.Density.1m2~rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth+(1|site/cluster), data=weeds1)
AIC(fit.d1a,fit.d1b)#b [NORMAL error DISTR] is better so let us proceed:

fit.d1 <- lmer(Weed.Density.1m2 ~Transdepth+rip+fence+(1|site)+(1|cluster),data = weeds1)
summary(fit.d1)#WOW!
#Deep topsoil reduces weeds as does ripping while 
#Deep increases native densities:
Fixed effects:  Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)     117.372     16.631   7.348   7.058 0.000160 ***
  Transdepth=deep  -21.354      7.958   7.968  -2.683 0.027884 *  
  rip=ripped       -44.094      7.958   7.968  -5.541 0.000555 ***
  fence=open         4.331      8.486   8.150   0.510 0.623353

sjp.lmer(fit.d1, type = "fe", title = "Weed Densities (1m2) in Year One (Spring 2012)",
         show.legend = TRUE)

#95% Confidence for WEEDS Densities both years:============
dim(weeds1)
names(weeds1)
weeds1$Year<-"one"
as.data.frame(weeds1)

dim(weeds2)
names(weeds2)
as.data.frame(weeds2)
weeds2$Year<-"two"

identical(names(weeds1), names(weeds2)) # TRUE - YAY! after computing sums
#we can merge two years together now
weeds1and2<-rbind(weeds1,weeds2)#does not like it:
str(weeds1and2)

#Time to comute 05% CI using summarySE
library(Rmisc)
all.weeds<-summarySE(weeds1and2, measurevar="Weed.Density.1m2",
                     groupvars=c("Site.Treatment","Filter","Year"))
all.weeds$nat<-"invasive" #it looks like summarySE does not like dplyr produce -> NAs produced
all.weeds
all.weeds<-all.weeds[1:12,]
all.weeds
library(ggplot2)
pd <- position_dodge(.5)#setting the gaps between elements on graph
AllCtrllog10s.CI1<-ggplot(all.weeds, aes(x=Year, y=Weed.Density.1m2, shape=nat, color= Year))
AllCtrllog10s.CI2<-AllCtrllog10s.CI1 +geom_errorbar(aes(ymin=Weed.Density.1m2-ci, ymax=Weed.Density.1m2+ci),width=.50,position=pd,size=1.4)
AllCtrllog10s.CI2
AllCtrllog10s.CI3<-AllCtrllog10s.CI2+ geom_point(position=pd,size=6)
AllCtrllog10s.CI3
AllCtrllog10s.CI3a<-AllCtrllog10s.CI3 + geom_line(position=pd) + scale_colour_manual(values = c("lightgreen", "darkgreen")) +scale_shape_manual(values=0)
AllCtrllog10s.CI3a
AllCtrllog10s.CI4<-AllCtrllog10s.CI3a+facet_grid(.~Filter+Site.Treatment)+theme_bw()
AllCtrllog10s.CI4


#with grid bigger fond +8:

AllCtrllog10s.CI5<- AllCtrllog10s.CI4 +theme(axis.text.x=element_text(vjust=0.5,size=20),
                                             axis.text.y=element_text(size=20),
                                             axis.title.x=element_blank(),
                                             axis.title.y=element_text(size=24),
                                             panel.grid.minor.x = element_blank(),
                                             strip.text=element_text(size=24),
                                             legend.position = "none")

AllCtrllog10s.CI5
AllCtrllog10s.CI7<- AllCtrllog10s.CI5 +  scale_y_continuous("Weed Density (1m\u00B2)")
AllCtrllog10s.CI7

ggsave(filename="Rplot.Weed.Year1.png", dpi=600) # way to save it to fit journal and A4 format easily



#Stats HTML Table of YEAR ONE (site) WEEDS:=====
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
data<- read.csv("TopEmergenceGood.csv")
names(data)
#Let us subset the year one emergence data:
year1data<-data[data$TST== 0.5 & data$nat=="invasive",] #TST = time since transfer
year1data$su <-factor(year1data$su) #factor is the way to drop emtpy levels
length(levels(year1data$su))#673 = YAY!

#Summarize it by all Site.Treatments:
d1<-summarise(group_by(year1data, su,Transdepth, rip, fence, nat),Weed.Density.1m2 = sum(count1m2))
head(d1)
#change the factor level to look consistent on a figure
d1$rip<- factor(d1$rip, levels = c( "unripped","ripped"))
d1$nat<- factor(d1$nat, levels = c("native", "invasive"))
d1$Transdepth<- factor(d1$Transdepth, levels = c("shallow","deep"))
dim(d1)#674   6

#We need to split su into site, cluster,plot:
weeds1 <- d1  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

fit.d1a<-glmer(Weed.Density.1m2~rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth+(1|site/cluster),
               family = poisson(link="log"), data=weeds1)
fit.d1b<-lmer(Weed.Density.1m2~rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth+(1|site/cluster), data=weeds1)
AIC(fit.d1a,fit.d1b)#b [NORMAL error DISTR] is better so let us proceed:

fit.d1 <- lmer(Weed.Density.1m2 ~Transdepth+rip+fence+rip*fence+fence*Transdepth+rip*Transdepth+(1|site)+(1|cluster),data = weeds1)
summary(fit.d1)#WOW!
#Deep topsoil reduces weeds as does ripping while Deep increases native densities:
######################Estimate Std. Error      df t value Pr(>|t|)    
(Intercept)               116.688     17.536   8.211   6.654 0.000142 ***
Transdepthdeep            -28.603     12.527   4.993  -2.283 0.071311 .  
ripripped                 -39.264     12.527   4.993  -3.134 0.025877 *  
fenceopen                  12.226     14.678   5.171   0.833 0.441656    
ripripped:fenceopen       -26.395     16.948   5.170  -1.557 0.178201    
Transdepthdeep:fenceopen   10.610     16.948   5.170   0.626 0.557907    
Transdepthdeep:ripripped    7.570     15.893   5.054   0.476 0.653741
library(broom)
d1.output<-tidy(fit.d1)#, effects ="fixed")# WE need to round them:
d1.output

d1.output %>% mutate_each(funs(round(.,2)), -term) %>% as.data.frame()
#write.table(d1.output,row.names = FALSE, file="StatsOutput_Weeds_YearOne.doc",sep=",")

#Drawing the sjmplot of coeficients: fit model:
library(sjstats)
library(sjPlot)
library(nlme)
library(sjmisc)

sjp.lmer(fit.d1, type = "fe", title = "Weed Densities (1m2) in Year One (Spring 2012)",
         show.legend = TRUE)
sjt.lmer(fit.d1)

sjt.lmer(fit.d1,pred.labels = c("Dipersal Filter (deep)","Abiotic Filter (ripped)","Biotic Filter (unfenced)",
                                "Ripped:Unfenced","Deep:Unfenced","Deep:Ripped"),file= "StatsTable_Weeds_YearOne_SiteTreatms.html",
         show.header = TRUE,string.dv = "Response",show.re.var = FALSE,show.dev = FALSE,show.icc = TRUE,
         depvar.labels = c("Weed Densities (1m&#178;) in Year One (Spring 2012)"))

#Stats HTML Table of YEAR TWO (site) WEEDS:=====
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
data<- read.csv("TopEmergenceGood.csv")
names(data)
#Let us subset the year one emergence data:
year2data<-data[data$TST== 1.5 & data$nat=="invasive",] #TST = time since transfer
year2data$su <-factor(year2data$su) #factor is the way to drop emtpy levels
length(levels(year2data$su))#673 = YAY!

#Summarize it by all Site.Treatments:
d2<-summarise(group_by(year2data, su,Transdepth, rip, fence, nat),Weed.Density.1m2 = sum(count1m2))
head(d2)
#change the factor level to look consistent on a figure
d2$rip<- factor(d2$rip, levels = c( "unripped","ripped"))
d2$nat<- factor(d2$nat, levels = c("native", "invasive"))
d2$Transdepth<- factor(d2$Transdepth, levels = c("shallow","deep"))
dim(d2)# 847   6

#We need to split su into site, cluster,plot:
weeds2 <- d2  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

#Drawing the sjmplot of coeficients: fit model

fit.d2a<-glmer(Weed.Density.1m2~rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth+(1|site/cluster),family = poisson(link="log"), data=weeds2)
fit.d2b<-lmer(Weed.Density.1m2~rip+fence+Transdepth+rip*fence+fence*Transdepth+rip*Transdepth+(1|site/cluster), data=weeds2)
AIC(fit.d2a,fit.d2b)#b [NORMAL error DISTR] is better so let us proceed:

fit.d2 <- lmer(Weed.Density.1m2 ~Transdepth+rip+fence+rip*fence+fence*Transdepth+rip*Transdepth+(1|site)+(1|cluster),data = weeds2)
summary(fit.d2)#WOW!
#STATS OUTPUT:
(Intercept)               175.289     17.335   9.444  10.112 2.25e-06 ***
Transdepthdeep              8.215     16.797   4.966   0.489   0.6456    
ripripped                 -10.703     16.869   5.075  -0.634   0.5533    
fenceopen                  -8.411     19.508   4.982  -0.431   0.6844    
ripripped:fenceopen       -43.109     22.536   4.989  -1.913   0.1141    
Transdepthdeep:fenceopen   47.362     22.536   4.988   2.102   0.0897 .  
Transdepthdeep:ripripped  -18.432     21.304   5.035  -0.865   0.4262  

library(broom)
d2.output<-tidy(fit.d2, effects ="fixed")# WE need to round them:
d2.output

write.table(d2.output,row.names = FALSE, file="StatsOutput_Weeds_YearTwo.csv",sep=",")

library(sjstats)
library(sjPlot)
library(nlme)
library(sjmisc)

sjp.lmer(fit.d2, type = "fe", title = "Weed Densities (1m2) in Year One (Spring 2013)",
         show.legend = TRUE)
sjt.lmer(fit.d2) #THE TABLE is COOL but t-values are missing :/

sjt.lmer(fit.d2,pred.labels = c("Dipersal Filter (deep)","Abiotic Filter (ripped)","Biotic Filter (unfenced)",
                                "Ripped:Unfenced","Deep:Unfenced","Deep:Ripped"),file= "StatOutputTable_WeedsYear2.html",
         show.header = TRUE,string.dv = "Response",show.re.var = FALSE,show.dev = FALSE,show.icc = FALSE, show.fstat = TRUE, show.se = TRUE,show.r2 = TRUE,
         depvar.labels = c("Weed Densities (1m&#178;) in Year Two (Spring 2013)"))


#Stats HTML Table of YEAR ONE (plot2) WEEDS:=====
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
data<- read.csv("TopEmergenceGood.csv")
names(data)
#Let us subset the year one emergence data:
year1data<-data[data$TST== 0.5 & data$nat=="invasive",] #TST = time since transfer
year1data$su <-factor(year1data$su) #factor is the way to drop emtpy levels
length(levels(year1data$su))#673 = YAY!

#Summarize it by all Site.Treatments:
d1<-summarise(group_by(year1data, su, plot2, Transdepth,rip),Weed.Density.1m2 = sum(count1m2))
head(d1)
dim(d1)#674   6

#We need to split su into site, cluster,plot:
weeds1 <- d1  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)
weeds1
#We remove unrelated variables: shade, shade.semi, smoke.plastic
w1<-filter(weeds1, plot2 == "smoke"|plot2 == "herbicide"|plot2 == "control"|plot2 == "plastic")
dim(w1)#577   9

w1$rip<- factor(w1$rip, levels = c( "unripped","ripped"))
w1$Transdepth<- factor(w1$Transdepth, levels = c("shallow","deep"))

#Drawing the sjmplot of coeficients: fit model
#Fence removed as plot2 were located within fence.

fit.d1a<-glmer(Weed.Density.1m2~Transdepth*rip+plot2+(1|site/cluster),family = poisson(link="log"), data=w1)
fit.d1b<-lmer(Weed.Density.1m2~Transdepth*rip+plot2+(1|site/cluster), data=w1)
AIC(fit.d1a,fit.d1b)#b [NORMAL error DISTR] is better so let us proceed:

fit.d1 <- lmer(Weed.Density.1m2 ~Transdepth*rip+plot2+(1|site)+(1|cluster),data = w1)
summary(fit.d1)#WOW!
#STATS OUTPUT:Fixed effects:
##########################Estimate Std. Error     df   t value Pr(>|t|)    
(Intercept)                124.466     16.590   6.680   7.503 0.000173 ***
Transdepth_deep            -21.806      8.975   7.800  -2.430 0.041967 *  
rip_ripped                 -46.194      8.975   7.800  -5.147 0.000949 ***
plot2_herbicide            -31.188      7.437 137.080  -4.194  4.9e-05 ***
plot2_plastic              -14.521      7.437 137.080  -1.953 0.052905 .  
plot2_smoke                 -5.771      7.437 137.080  -0.776 0.439058    
Transdepth_deep:rip_ripped   1.700     12.689   7.790   0.134 0.896799 #Conclusion:
#Any disturbance reduces weed density. Ripping>Herbicide>Transdepth>plastic

library(sjstats)
library(sjPlot)
library(nlme)
library(sjmisc)

sjp.lmer(fit.d1, type = "fe", title = "Weed Densities (1m2) in Year One (Spring 2012)")
sjt.lmer(fit.d1)

sjt.lmer(fit.d1,pred.labels = c("Dispersal [Volume]","Abiotic [Rip]","Biotic [herbicide)",
                                "Abiotic [plastic]","Dispersal [smoke]","Abiotic:Dispersal [Rip:Volume]"),file= "StatOutputTable_Weed_YearOne_Plot2.html",
         show.header = TRUE,string.dv = "Response",show.re.var = FALSE,show.dev = FALSE,show.icc = FALSE,show.se = TRUE,
         depvar.labels = c("Weed Densities (1m&#178;) in Year One (Spring 2012)"))

library(broom)#getting t-values broom-way into manuscript table:
p1.output<-tidy(fit.d1, effects ="fixed")# WE need to round them:
p1.output
#write.table(p1.output,row.names = FALSE, file="StatsOutput_Weeds_YearOne_Plot2.csv",sep=",")

#Stats HTML Table of YEAR TWO (plot2) WEEDS:=====
library(lme4)
library(lmerTest)
library(dplyr)
library(tidyr)
data<- read.csv("TopEmergenceGood.csv")
names(data)
#Let us subset the year one emergence data:
year2data<-data[data$TST== 1.5 & data$nat=="invasive",] #TST = time since transfer
year2data$su <-factor(year2data$su) #factor is the way to drop emtpy levels
length(levels(year2data$su))#673 = YAY!

#Summarize it by all Site.Treatments:
d2<-summarise(group_by(year2data, su,Transdepth, rip, plot2),Weed.Density.1m2 = sum(count1m2))
head(d2)
#change the factor level to look consistent on a figure
d2$rip<- factor(d2$rip, levels = c( "unripped","ripped"))
d2$Transdepth<- factor(d2$Transdepth, levels = c("shallow","deep"))
dim(d2)#847   5

#We need to split su into site, cluster,plot:
weeds2 <- d2  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

#Removing the not important plot-scale treatments.
w2<-filter(weeds2, plot2 == "smoke"|plot2 == "herbicide"|plot2 == "control"|plot2 == "plastic")
dim(w2)#726   8

w1$rip<- factor(w1$rip, levels = c( "unripped","ripped"))
w1$Transdepth<- factor(w1$Transdepth, levels = c("shallow","deep"))


#Drawing the sjmplot of coeficients: fit model

fit.d2a<-glmer(Weed.Density.1m2~Transdepth*rip+plot2+(1|site/cluster),family = poisson(link="log"), data=w2)
fit.d2b<-lmer(Weed.Density.1m2~Transdepth*rip+plot2+(1|site/cluster), data=w2)
AIC(fit.d2a,fit.d2b)#b [NORMAL error DISTR] is better so let us proceed:

fit.d2 <- lmer(Weed.Density.1m2 ~Transdepth*rip+plot2+(1|site)+(1|cluster),data = w2)
summary(fit.d2)#WOW!
#STATS OUTPUT:
#######################Estimate Std. Error     df t value Pr(>|t|)    
(Intercept)                173.74      18.09  11.63   9.603 7.14e-07 ***
Transdepthdeep              34.66      17.87   8.18   1.940   0.0875 .  
ripripped                  -30.39      17.82   8.09  -1.705   0.1261    
plot2herbicide              18.07      15.92 141.19   1.135   0.2581    
plot2plastic                19.57      15.92 141.19   1.230   0.2209    
plot2smoke                 -15.18      15.92 141.19  -0.953   0.3420    
Transdepthdeep:ripripped   -28.95      25.25   8.16  -1.146   0.2841

library(sjstats)
library(sjPlot)
library(nlme)
library(sjmisc)

sjp.lmer(fit.d2, type = "fe", title = "Weed Densities (1m2) in Year One (Spring 2013)",
         show.legend = TRUE)
sjt.lmer(fit.d2)

sjt.lmer(fit.d2,pred.labels = c("Dispersal [Volume]","Abiotic [Rip]","Biotic [herbicide)",
                                "Abiotic [plastic]","Dispersal [smoke]","Abiotic:Dispersal [Rip:Volume]"),file= "StatOutputTable_Weeds_YearTwo_plot2.html",
         show.header = TRUE,string.dv = "Response",show.re.var = FALSE,show.dev = FALSE,show.icc = FALSE, show.ci = FALSE, show.se = TRUE,
         depvar.labels = c("Weed Densities (1m&#178;) in Year Two (Spring 2013)"))

library(broom)#getting t-values broom-way into manuscript table:
p2.output<-tidy(fit.d2, effects ="fixed")# WE need to round them:
p2.output
#write.table(p2.output,row.names = FALSE, file="StatsOutput_Weeds_YearTwo_Plot2.csv",sep=",")
