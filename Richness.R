#LOAD DATA:
library(tidyverse)
library(vegan)
library(Rmisc)
data<- read.csv("TopsoilEmergenceData.csv")#our density data
dim(data)#38247 obs. of  24 variables:

#Spring 2012 Richness=========
#Subset native plant species data for TST=0.5 (Spring 2012):
df05 <- data %>% filter(TST==0.5 & nat=="native" ) %>% 
  select("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence") %>%
  na.omit()#su = sampling unit

#Change sp data to wide format to compute richness:
veg.wide05<-spread(df05,key = specCode, value = count4m2, fill = 0)
dim(veg.wide05)#673 155

#Compute Richness/Diversity indices:
veg.wide05$DIVshannon<-diversity(veg.wide05[,6:155])
veg.wide05$DIVsimpson<-diversity(veg.wide05[,6:155], index= "simpson")
veg.wide05$Density<-rowSums(veg.wide05[,6:155])
veg.wide05$Richness<-specnumber(veg.wide05[,6:155])

#Spring 2013 Richness=========
#Subset native, perennials species data for TST=1.5 (Spring 2013):
df1.5 <- data %>% filter(TST==1.5 & nat=="native") %>% 
  select("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence")%>%
  na.omit()#su = sampling unit

#Change sp data to wide format to compute:
veg.wide1.5<-spread(df1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#848 126

#Compute Diversity indices:
veg.wide1.5$DIVshannon<-diversity(veg.wide1.5[,6:126])
veg.wide1.5$DIVsimpson<-diversity(veg.wide1.5[,6:126], index= "simpson")
veg.wide1.5$Density<-rowSums(veg.wide1.5[,6:126])
veg.wide1.5$Richness<-specnumber(veg.wide1.5[,6:126])

#Run Two Models (Spring 2012 and 2013):===========
library(lme4)
library(lmerTest)
veg.wide05 $ su <- as.character(veg.wide05 $ su)
veg.wide05$rip <- factor( veg.wide05$rip, levels = c("unripped","ripped"))#changing levels to show Coef-s in relation to ripped effect

veg.wide05.site<-veg.wide05 %>%
    separate(col = "su",into=c("site","cluster","plot"), sep="\\.", remove = F) %>%
  filter(plot2 == "smoke" | plot2 == "heat" | plot2 == "herbicide" | plot2== "control")

#Spring 2012 Model:
glmer.spr12<-glmer(Richness ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+
                     rip*Transdepth + plot2 + rip*plot2 +Transdepth*plot2+
                     (1|site/cluster),family = poisson(link="log"),
                   data = veg.wide05.site)

#Spring 2013 Model:
veg.wide1.5 $ su <- as.character(veg.wide1.5 $ su)
veg.wide1.5$rip <- factor( veg.wide1.5$rip, levels = c("unripped","ripped"))#changing levels to show Coef-s in relation to ripped effect

veg.wide1.5.site<-veg.wide1.5 %>%
  separate(col = "su",into=c("site","cluster","plot"), sep="\\.", remove = F) %>%
  filter(plot2 == "smoke" | plot2 == "heat" | plot2 == "herbicide" | plot2== "control")

glmer.spr13<-glmer(Richness ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+
                     rip*Transdepth + plot2 + rip*plot2 +Transdepth*plot2+
                     (1|site/cluster),family = poisson(link="log"),
                   data = veg.wide1.5.site)

#Create Summary Table of that model:
library("sjPlot")
library("TMB")
tab_model (glmer.spr12,glmer.spr13,
           show.intercept = T, show.se = T, show.est = T,
           transform = NULL, show.fstat = T, show.stat = T,
           auto.label = FALSE, show.ci = FALSE,
           title = "Richness as Response Variable (Two Models for Spring 2012 and 2013)")

#Resources: https://strengejacke.github.io/sjPlot/articles/tab_model_estimates.html
#http://127.0.0.1:22024/library/sjPlot/doc/tab_mixed.R

#Spring SW-index 2012 Model:
glmer.spr12_SW<-glmer(DIVshannon ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+
                     rip*Transdepth + plot2 + rip*plot2 +Transdepth*plot2+
                     (1|site/cluster),family = poisson(link="log"),
                   data = veg.wide05.site)

#Spring SW-index 2013 Model:
glmer.spr13_SW<-glmer(DIVshannon ~ rip+fence+Transdepth+rip*fence+fence*Transdepth+
                     rip*Transdepth + plot2 + rip*plot2 +Transdepth*plot2+
                     (1|site/cluster),family = poisson(link="log"),
                   data = veg.wide1.5.site)

tab_model (glmer.spr12_SW,glmer.spr13_SW,
           show.intercept = T, show.se = T, show.est = T,
           transform = NULL, show.fstat = T, show.stat = T,
           auto.label = FALSE, show.ci = FALSE,
           title = "Shannon-Wiener Diversity Index as Response Variable (Two Models for Spring 2012 and 2013)")


#Richness SummarySE for spring2012 emergence:=====
veg.wide05$season <- "Spring 2012"

#Compute means +95%CI for the ggplot for spring 2012 emergence indices:
d1<-summarySE(veg.wide05, measurevar="Richness", groupvars=c("Transdepth","season"))
d1$Filter <- "DISPERSAL"
colnames(d1)[1]<-"Site.Treatment"

d2<-summarySE(veg.wide05, measurevar="Richness", groupvars=c("rip","season"))
d2$Filter <- "ABIOTIC"
colnames(d2)[1]<-"Site.Treatment"

d3<-summarySE(veg.wide05, measurevar="Richness", groupvars=c("fence","season"))
d3$Filter <- "BIOTIC"
colnames(d3)[1]<-"Site.Treatment"

#Richness SummarySE for spring2013 emergence:=====
#Compute means +95%CI for the ggplot for spring 2013 emergence indices:
veg.wide1.5$season <- "Spring 2013"
d4<-summarySE(veg.wide1.5, measurevar="Richness", groupvars=c("Transdepth","season"))
d4$Filter <- "DISPERSAL"
colnames(d4)[1]<-"Site.Treatment"

d5<-summarySE(veg.wide1.5, measurevar="Richness", groupvars=c("rip","season"))
d5$Filter <- "ABIOTIC"
colnames(d5)[1]<-"Site.Treatment"

d6<-summarySE(veg.wide1.5, measurevar="Richness", groupvars=c("fence","season"))
d6$Filter <- "BIOTIC"
colnames(d6)[1]<-"Site.Treatment"

EmData<-rbind(d1,d2,d3,d4,d5,d6)#merging year two and one
EmData
EmData$Scale <- "Site" #to reduce facetting to one level only on x-axis.

#Richness Figure, all years:=====
pd <- position_dodge(.5)
Fig_Richness <- 
  ggplot(EmData, aes(x=Site.Treatment, y=Richness,shape=Filter, color=season)) +
  geom_errorbar(aes(ymin=Richness-ci, ymax=Richness+ci),width=.30,position=pd,size=1.4)+
  geom_point(position=pd,size=3)+ 
  #geom_line(position=pd) +
  scale_colour_manual(values = c("green", "red")) +
  facet_grid(season~Filter,  scales="free", drop = T)+theme_bw()+
  scale_y_continuous("Richness of native seeedlings", limits = c(10,25))+
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=18),
        legend.position = "none")
Fig_Richness
#ggsave(Fig_Richness, filename = "Fig1_SiteScale_emergenceRichness.jpg", width = 180, height = 120, units = "mm")

