#Emergence Stats with SpeciesID as random factors
#Look at the frequency of seedlings/Traits
library(tidyverse)
library(gdata)
library(lme4)
library(lmerTest)

#Across all seasons after topsoil transfer:
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
dim(BigDF2)#790625     38 variables with REMNANT DATA
levels(BigDF2$season)#"Autumn2013" "Autumn2014" "remnant"    "Spring2012" "Spring2013"

BigDF2<- BigDF2[!BigDF2$season=="remnant",] #remove remnant data
BigDF3 <- BigDF2 [ BigDF2$count > 0, ] #Remove zero values rows
BigDF3 <- drop.levels(BigDF3)
BigDF4 <- BigDF3 [ BigDF3$season=="Spring2012" | BigDF3$season=="Autumn2014", ] #Remove zero values from count of species
BigDF4 <- drop.levels(BigDF4)


#STATS Emergence in Spring 2012=====
spr12 <- BigDF4[ BigDF4$season== "Spring2012" & BigDF4$nat== "native",]##NATIVE ONLY!
spr12$Sum4m2 <- spr12$count * 4

spr12.glmer <- glmer(Sum4m2 ~ rip+fence+Transdepth+rip*fence+fence*Transdepth
                                    +rip*Transdepth +(1|site) +(1|block) +(1|specCode),
                     family = poisson(link="log"), data = spr12)
summary(spr12.glmer)
#Fixed effects:
#                               Estimate Std. Error z value Pr(>|z|)    
(Intercept)                   -0.11883    0.09386  -1.266    0.205    
ripunripped                    0.82650    0.01496  55.243  < 2e-16 ***
  fenceopen                      0.10088    0.02161   4.667 3.05e-06 ***
  Transdepthshallow              0.11945    0.01768   6.757 1.41e-11 ***
  ripunripped:fenceopen         -0.32900    0.02209 -14.891  < 2e-16 ***
  fenceopen:Transdepthshallow   -0.17022    0.02013  -8.458  < 2e-16 ***
  ripunripped:Transdepthshallow -0.17549    0.01956  -8.970  < 2e-16 ***