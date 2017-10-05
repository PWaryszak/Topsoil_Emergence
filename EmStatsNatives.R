#STATS on Emergence Data ("TopEmergenceGood.csv")
#FIGURE 1- effect of site-scale treatments on perennial plant emergence densities
#LOAD R-PACKAGES & DATA ================
#install.packages("lme4", "tidyverse", "Rmisc", "lmerTest", "broom") #run this first if no packages present on your R.
library(tidyverse)
library(Rmisc)
library(lme4)
library(lmerTest)
library(broom)#Re-Load  data to be sure all works well [Topsoil Emergence DATA:only spr12 & spr13 seasons included:
data<- read.csv("TopEmergenceGood.csv")#our density data
str(data)#40924 obs. of  22 variables: = all good.


#STATS Emergence Densities YEAR_ONE_ALL_TREATMETNS=========
gsmall<- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
gsmall1<-gsmall[gsmall$TST== 0.5,]#only in spring2012=tst05
EmDensities2012<-gsmall1[gsmall1$TST== 0.5 & gsmall1$nat=="native" ,]#only natives

levels(droplevels(EmDensities2012$nat))#native 
levels(droplevels(gsmall1$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(EmDensities2012)#10333    23

#Compute the total number of su (sampling units) in this year study:
n1.levels <- an1<-aggregate(gsmall1$count4m2,              
                            by = list(su=gsmall1$su, site=gsmall1$site, cluster=gsmall1$cluster,
                                      Transdepth=gsmall1$Transdepth, rip=gsmall1$rip,
                                      fence=gsmall1$fence, plot2=gsmall1$plot2), FUN = "sum")
length(levels(droplevels(n1.levels$su)))#673 = total number of su-s (sampling units)
head(n1.levels)
colnames(n1.levels)[8]<-"sum4m2"
n1.levels$year<-"one"

#compute densities per plot = su in year one. Poission takes only integers
#so we have to compute per true su size = 4 m2 (count4m2):
EmDensities2012<-EmDensities2012[ EmDensities2012$plot2 != "shade",] #shade applied after spring surveys

n1<-aggregate(EmDensities2012$count4m2,              
              by = list(su=EmDensities2012$su, site=EmDensities2012$site, cluster=EmDensities2012$cluster,
                        Transdepth=EmDensities2012$Transdepth, rip=EmDensities2012$rip,
                        fence=EmDensities2012$fence, plot2=EmDensities2012$plot2,
                        longevity=EmDensities2012$longevity), FUN = "sum")
str(n1)#938 obs. of  9 variables:
colnames(n1)[9]<-"sum4m2"
n1$year<-"one"
head(n1)

n1.perennials <- n1 [ n1$longevity=="perennial",c( "su", "site","cluster","Transdepth","rip","fence","plot2","sum4m2","year","longevity")]
str(n1.perennials)#665 obs. of  10 variables:

empty.plots <-  dplyr::anti_join(n1.levels, n1.perennials, by = "su")
empty.plots #all plots in a1 that do not have match in a1.perennial
dim(empty.plots)#8 rows
empty.plots$longevity <- "perennial" #assigning longevity column
empty.plots$sum4m2 <- 0 #assigning zero to perennials absent from empty plots.

perennials.year.one<-rbind(n1.perennials,empty.plots)#
str(perennials.year.one)#673 obs. of  10 variables:
levels(droplevels((perennials.year.one$longevity)))# double check. YES = "perennial" only
perennials.year.one$rip <- factor( perennials.year.one$rip, levels = c("unripped","ripped"))#changing levels to show Coef-s in relation to ripped effect
perennials.year.one<-perennials.year.one[ perennials.year.one$plot2 != "shade.semi",] #shade was survival-related treatment applied after spring surveys
perennials.year.one<-perennials.year.one[ perennials.year.one$plot2 != "shade",] #shade was survival-related treatment applied after spring surveys
perennials.year.one$plot2<-factor(perennials.year.one$plot2)
levels(droplevels(perennials.year.one$plot2))

#STATS Spring2012 all interactions========
PER.glmer.spr12.interactions<-glmer(sum4m2 ~ rip+fence+Transdepth+rip*fence+fence*Transdepth
                       +rip*Transdepth + plot2 + rip*plot2 +Transdepth*plot2
                       +(1|site/cluster),family = poisson(link="log"), data=perennials.year.one)
summary(PER.glmer.spr12.interactions)

#OUTPUT:
#####################################Estimate Std. Error z value Pr(>|z|)    
(Intercept)                           3.91640    0.17579  22.279  < 2e-16 ***
ripripped                            -0.82995    0.20366  -4.075 4.60e-05 ***
fenceopen                            -0.04860    0.23529  -0.207   0.8363    
Transdepthshallow                    -0.40124    0.20337  -1.973   0.0485 *  
plot2herbicide                        0.54026    0.05715   9.453  < 2e-16 ***
plot2plastic                          0.39358    0.05827   6.754 1.43e-11 ***
plot2smoke                            0.28980    0.05883   4.926 8.39e-07 ***
plot2smoke.plastic                    0.41714    0.05820   7.168 7.63e-13 ***
ripripped:fenceopen                  -0.06629    0.27299  -0.243   0.8081    
fenceopen:Transdepthshallow           0.01368    0.27279   0.050   0.9600    
ripripped:Transdepthshallow           0.12560    0.25604   0.491   0.6237    
ripripped:plot2herbicide             -0.70879    0.08199  -8.645  < 2e-16 ***
ripripped:plot2plastic               -0.44690    0.08092  -5.523 3.34e-08 ***
ripripped:plot2smoke                 -0.09820    0.07803  -1.258   0.2082    
ripripped:plot2smoke.plastic         -0.53254    0.08223  -6.477 9.38e-11 ***
Transdepthshallow:plot2herbicide     -0.63677    0.07349  -8.664  < 2e-16 ***
Transdepthshallow:plot2plastic       -0.61864    0.07453  -8.301  < 2e-16 ***
Transdepthshallow:plot2smoke         -0.53800    0.07371  -7.299 2.89e-13 ***
Transdepthshallow:plot2smoke.plastic -0.69490    0.07532  -9.226  < 2e-16 ***


#STATS Spring2012 with no interactions========
#shown above all interaction with plot2 (plot-scale treatments) showed negative effect
#They all failed to deliver postive effect on emergence densities so we  cut them out from main table
#an possibly may go into an appendix in our emergence paper.
PER.glmer.spr12<-glmer(sum4m2 ~ rip+fence+Transdepth+rip*fence+fence*Transdepth
                                    +rip*Transdepth + plot2 +(1|site/cluster),family = poisson(link="log"), data=perennials.year.one)
summary(PER.glmer.spr12)
#OUTPUT:
##############################Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  4.100627   0.166991  24.556  < 2e-16 ***
ripripped                   -1.014463   0.188089  -5.394 6.91e-08 ***
fenceopen                   -0.229316   0.218675  -1.049 0.294334    
Transdepthshallow           -0.660138   0.188029  -3.511 0.000447 ***
plot2herbicide               0.032282   0.035520   0.909 0.363435    
plot2plastic                -0.042324   0.036104  -1.172 0.241086    
plot2smoke                  -0.005729   0.035813  -0.160 0.872915    
plot2smoke.plastic          -0.070679   0.036335  -1.945 0.051749 .  
ripripped:fenceopen          0.113958   0.253291   0.450 0.652774    
fenceopen:Transdepthshallow  0.266291   0.253282   1.051 0.293093    
ripripped:Transdepthshallow  0.137902   0.238695   0.578 0.563445  
#Creating clean Stats_Output Table:
glmer.per.output=tidy(PER.glmer.spr12, effects ="fixed")
glmer.per.output
#write.table(glmer.per.output,file="glmerOutputPerennials.spr12.csv",sep=",")



#STATS on YEAR TWO (Spring 2013) emergence: Native PERENNIALS, ALL TREATMENTS=====
#Re-Load data:
data<- read.csv("TopEmergenceGood.csv")
str(data)#40924 obs. of  23 variables:
#compute densities per plot = su:
ggsmall<- data[data$TST==1.5,]#only records from spring II 
ggsmall2<-ggsmall[ggsmall$newTST== 1.5 ,]

#We need to remove shade cause not involved in germination:
ggsmall3<-ggsmall2[!ggsmall2$plot2=="shade" & !ggsmall2$plot2== "shade.semi",]
dim(ggsmall3)#19530    23
levels(droplevels(ggsmall3$plot2))#"control", "heat" ,"herbicide","plastic","smoke","smoke.plastic"
length(levels(droplevels(ggsmall3$su)))# 804

#Run aggregate to get total number of su in year two:
n2.levels <-aggregate(ggsmall3$count4m2,              
                      by = list(su=ggsmall3$su, site=ggsmall3$site, fence=ggsmall3$fence,
                                Transdepth=ggsmall3$Transdepth, rip=ggsmall3$rip,
                                cluster=ggsmall3$cluster, plot2=ggsmall3$plot2), FUN = "sum")
length(levels(droplevels(n2.levels$su)))#804= total number of su-s (sampling units) in year two
#number of plots went up in year two compared to year one
#as we increased number of observation units (su) due to high mortality after year one surveys,
colnames(n2.levels)[8]<-"sum4m2"
n2.levels$year<-"two"
head(n2.levels)

EmDensities2013<-ggsmall3[ggsmall3$nat=="native",] #subset native seedlings only (our focus)
dim(EmDensities2013)# 10408    44

#compute densities per plot = su in year one:
EmDensities2013<-ggsmall4[ggsmall4$fence=="fenced",]#subsetting only the fenced values as plot-treatments were applied
str(EmDensities2013)#6743 obs. of  23 variables:

n2<-aggregate(EmDensities2013$count4m2,              
              by = list(su=EmDensities2013$su, site=EmDensities2013$site, fence=EmDensities2013$fence,
                        Transdepth=EmDensities2013$Transdepth, rip=EmDensities2013$rip,
                        cluster=EmDensities2013$cluster, plot2=EmDensities2013$plot2,
                        longevity=EmDensities2013$longevity), FUN = "sum")
str(n2)#992 obs. of  8 variables:
colnames(n2)[9]<-"sum4m2"
n2$year<-"two"
head(n2)

n2.perennials <- n2 [ n2$longevity=="perennial",c( "su", "site","Transdepth","rip","fence","cluster","plot2","sum4m2","year","longevity")]
str(n2.perennials)#506 obs. of  9 variables:

#Join su-s (sampling units) from empty.plots to n2.perennials to account for empty plots in our data:
empty.plots2 <-  dplyr::anti_join(n2.levels, n2.perennials, by = "su")
empty.plots2 #all plots in a1 that do not have match in a1.perennial
empty.plots2$longevity <- "perennial"
empty.plots2$sum4m2 <- 0

perennials.year.two<-rbind(n2.perennials,empty.plots2)#
str(perennials.year.two)#804 obs. of  10 variables:
levels(droplevels((perennials.year.two$longevity)))# double check. YES = "perennial" only
perennials.year.two$rip <- factor( perennials.year.two$rip, levels = c("unripped","ripped"))#changing levels to show Coef-s in relation to ripped effect


#STATS Spring 2013 all interactions========

PER.glmer.spr13.interactions<-glmer(sum4m2 ~ rip+fence+Transdepth+rip*fence+fence*Transdepth
                                    +rip*Transdepth + plot2 + rip*plot2 +Transdepth*plot2
                                    +(1|site/cluster),family = poisson(link="log"), data=perennials.year.two)
summary(PER.glmer.spr13.interactions)
######################################Estimate Std. Error z value Pr(>|z|)    
Estimate Std. Error z value Pr(>|z|)    
(Intercept)                           2.90924    0.21861  13.308  < 2e-16 ***
ripripped                             0.13637    0.24999   0.545 0.585413    
fenceopen                            -0.13835    0.28962  -0.478 0.632857    
Transdepthshallow                    -0.18894    0.25054  -0.754 0.450760    
plot2heat                             0.96190    0.06743  14.266  < 2e-16 ***
plot2herbicide                        0.57190    0.07118   8.035 9.39e-16 ***
plot2plastic                          0.32586    0.07421   4.391 1.13e-05 ***
plot2smoke                            0.56773    0.07074   8.025 1.01e-15 ***
plot2smoke.plastic                    0.76955    0.06893  11.164  < 2e-16 ***
ripripped:fenceopen                   0.09547    0.33452   0.285 0.775332    
fenceopen:Transdepthshallow           0.24805    0.33452   0.741 0.458397    
ripripped:Transdepthshallow          -0.16583    0.31457  -0.527 0.598073    
ripripped:plot2herbicide             -0.56668    0.08971  -6.316 2.68e-10 ***
ripripped:plot2plastic               -0.34599    0.09070  -3.815 0.000136 ***
ripripped:plot2smoke                 -0.31718    0.08635  -3.673 0.000240 ***
ripripped:plot2smoke.plastic         -0.61112    0.08754  -6.981 2.92e-12 ***
Transdepthshallow:plot2heat          -0.45984    0.10675  -4.308 1.65e-05 ***
Transdepthshallow:plot2herbicide     -0.79666    0.09431  -8.447  < 2e-16 ***
Transdepthshallow:plot2plastic       -0.57038    0.09400  -6.068 1.29e-09 ***
Transdepthshallow:plot2smoke         -0.75873    0.09081  -8.355  < 2e-16 ***
Transdepthshallow:plot2smoke.plastic -1.03030    0.09420 -10.938  < 2e-16 ***

  
#Creating clean Stats_Output Table:
glmer.per.outputs.spr12i=tidy(PER.glmer.spr12.interactions, effects ="fixed")
glmer.per.outputs.spr12i $ season <-"Spring 2012"
glmer.per.outputs.spr13i=tidy(PER.glmer.spr13.interactions, effects ="fixed")
glmer.per.outputs.spr13i $ season <-"Spring 2013"

Output.All.interactions<- rbind(glmer.per.outputs.spr12i,glmer.per.outputs.spr13i)
#write.csv(Output.All.interactions, file="StatsNativesGlmerInteractions.csv")  


#STATS Spring2013 with no interactions========
#shown above all interaction with plot2 (plot-scale treatments) showed negative effect
#They all failed to deliver postive effect on emergence densities so we  cut them out from main table
#an possibly may go into an appendix in our emergence paper.
PER.glmer.spr13<-glmer(sum4m2 ~ rip+fence+Transdepth+rip*fence+fence*Transdepth
                         +rip*Transdepth + plot2 +(1|site/cluster),family = poisson(link="log"), data=perennials.year.two)
summary(PER.glmer.spr13)
############################Estimate Std. Error z value Pr(>|z|)    
(Intercept)                  3.124338   0.220338  14.180  < 2e-16 ***
ripripped                   -0.054631   0.249395  -0.219  0.82661    
fenceopen                   -0.350533   0.290180  -1.208  0.22705    
Transdepthshallow           -0.513926   0.249928  -2.056  0.03975 *  
plot2heat                    0.543782   0.046358  11.730  < 2e-16 ***
plot2herbicide               0.009271   0.043536   0.213  0.83136    
plot2plastic                -0.074506   0.044543  -1.673  0.09439 .  
plot2smoke                   0.123545   0.042258   2.924  0.00346 ** 
plot2smoke.plastic           0.129877   0.042191   3.078  0.00208 ** 
ripripped:fenceopen          0.280573   0.334860   0.838  0.40210    
fenceopen:Transdepthshallow  0.567122   0.334845   1.694  0.09032 .  
ripripped:Transdepthshallow -0.153959   0.315997  -0.487  0.62610    
---