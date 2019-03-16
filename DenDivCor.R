#LOAD DATA:
#Correlation between density and Richness indices:
library(tidyverse)
library(vegan)
library(Rmisc)
data<- read.csv("TopsoilEmergenceData.csv")#our density data
str(data)#38247 obs. of  25 variables:


#Pearson's Correalation Test for Spring 2012=========
#Subset native, perennials species data for TST=0.5 (Spring 2012):
df05 <- data %>%
  filter(TST==0.5 & nat=="native" & longevity=="perennial" )%>% 
  select("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "comb3")%>%
  na.omit()#su = sampling unit

#Change sp data to wide format to compute:
veg.wide05<-spread(df05,key = specCode, value = count4m2, fill = 0)
dim(veg.wide05)#665 124

#Compute Diversity indices:
veg.wide05$DIVshannon<-diversity(veg.wide05[,6:124])
veg.wide05$DIVsimpson<-diversity(veg.wide05[,6:124], index= "simpson")
veg.wide05$Density<-rowSums(veg.wide05[,6:124])
veg.wide05$Richness<-specnumber(veg.wide05[,6:124])

#Run Correlation Test:
summary(cor(veg.wide05$DIVshannon,veg.wide05$Density))
cor05<-cor.test(veg.wide05$DIVshannon,veg.wide05$Density)
cor05$estimate # 0.7239812 
cor05$p.value # P< 0.001
cor05$statistic # t = 27.02393 

#Produce Fig Richness~Density for reviewer:
ggplot(veg.wide05, aes(Density,Richness, color = comb3)) + 
  geom_point()+
  ggtitle("Relationship between density and richness (Spring 2012)")+
  labs(x = "Native Density",y="Native Richness",colour="Treatments:") +
  theme_bw(base_size = 12)


#Pearson's Correalation Test for Spring 2013=========
#Subset native, perennials species data for TST=1.5 (Spring 2013):
data<- read.csv("TopsoilEmergenceData.csv")#our density data

df1.5 <- data %>% filter(TST==1.5 & nat=="native" & longevity=="perennial")
str(df1.5)#7289 obs. of  25 variables:

#produce species matrix:
sp.data1.5<- df1.5 %>% 
  select("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence")%>%
  na.omit()#su = sampling unit
str(sp.data1.5)#7289 obs.

#Change sp data to wide format to compute:
veg.wide1.5<-spread(sp.data1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#836 100
head(veg.wide1.5)

#Compute Diversity indices:
veg.wide1.5$DIVshannon<-diversity(veg.wide1.5[,6:100])
veg.wide1.5$DIVsimpson<-diversity(veg.wide1.5[,6:100], index= "simpson")
veg.wide1.5$Density<-rowSums(veg.wide1.5[,6:100])

summary(cor(veg.wide1.5$DIVshannon,veg.wide1.5$Density))
cor1.5<-cor.test(veg.wide1.5$DIVshannon,veg.wide1.5$Density)
cor1.5$estimate # 0.650716
cor1.5$p.value # P< 0.001
cor1.5$statistic # t = 24.74854 

#Total Species Richness=======
#Natives only:
sp<- data[data$nat == "native",]
levels(droplevels(sp$specCode))
length(levels(droplevels(sp$specCode))) # 165 species!

#n of Natives in Spring 2012 mergence event
sp12<- data[data$nat == "native" & data$EmergenceSeason == "Spring2012" ,]
levels(droplevels(sp12$specCode))
length(levels(droplevels(sp12$specCode))) # 150 species!

#n of Natives in Spring 2013 mergence event

sp13<- data[data$nat == "native" & data$EmergenceSeason == "Spring2013" ,]
levels(droplevels(sp13$specCode))
length(levels(droplevels(sp13$specCode))) # 121 species!

#n of Natives in Spring 2012 mergence event
sp12<- data[data$nat == "native" & data$EmergenceSeason == "Spring2012" ,]
levels(droplevels(sp12$specCode))
length(levels(droplevels(sp12$specCode))) #150 species!

#n of Natives in Spring 2013 mergence event

sp13<- data[data$nat == "native" & data$EmergenceSeason == "Spring2013" ,]
levels(droplevels(sp13$specCode))
length(levels(droplevels(sp13$specCode))) #121 species!

#All plant species, including weeds ("invasive"):
levels(droplevels(data$specCode))
length(levels(droplevels(data$specCode))) # 247 species!

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
#Spring 2013 data:
df1.5 <- data %>% filter(TST==1.5 & nat=="native" & longevity=="perennial")
str(df1.5)#7289 obs. of  25 variables:

sp.data1.5<- df1.5 %>%
  select("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence")%>%
  na.omit()#su = sampling unit
str(sp.data1.5)#7289 obs. of  3 variables:

#Change sp data to wide format to compute:
veg.wide1.5<-spread(sp.data1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#836  100

#Compute Diversity indices:
veg.wide1.5$DIVshannon<-diversity(veg.wide1.5[,6:100])
veg.wide1.5$DIVsimpson<-diversity(veg.wide1.5[,6:100], index= "simpson")
veg.wide1.5$Density<-rowSums(veg.wide1.5[,6:100])
veg.wide1.5$Richness<-specnumber(veg.wide1.5[,6:100])

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

#Richness ggplot for all years emergence richness:=====
pd <- position_dodge(.5)
Fig_Richness <- 
  ggplot(EmData, aes(x=Site.Treatment, y=Richness,shape=Filter, color=season)) +
  geom_errorbar(aes(ymin=Richness-ci, ymax=Richness+ci),width=.30,position=pd,size=1.4)+
  geom_point(position=pd,size=3)+ 
  #geom_line(position=pd) +
  scale_colour_manual(values = c("green", "red")) +
  facet_grid(season~Filter,  scales="free", drop = T)+theme_bw()+
  scale_y_continuous("Richness of native perennial plants", limits = c(4,16))+
  theme(axis.text.y=element_text(size=18),
                   axis.text.x=element_text(size=18),
                   axis.title.x=element_blank(),
                   axis.title.y=element_text(size=18),
                   panel.grid.minor.x = element_blank(),
                   strip.text=element_text(size=18),
                   legend.position = "none")
Fig_Richness
#ggsave(Fig_Richness, filename = "Fig1_SiteScale_emergenceRichness.jpg", width = 180, height = 120, units = "mm")


#Shannon-Wienner indices SummarySE=====
#Spring 2012:
veg.wide05$season <- "Spring 2012"

#Compute means +95%CI for the ggplot for spring 2012 emergence indices:
d1<-summarySE(veg.wide05, measurevar="DIVshannon", groupvars=c("Transdepth","season"))
d1$Filter <- "DISPERSAL"
colnames(d1)[1]<-"Site.Treatment"

d2<-summarySE(veg.wide05, measurevar="DIVshannon", groupvars=c("rip","season"))
d2$Filter <- "ABIOTIC"
colnames(d2)[1]<-"Site.Treatment"

d3<-summarySE(veg.wide05, measurevar="DIVshannon", groupvars=c("fence","season"))
d3$Filter <- "BIOTIC"
colnames(d3)[1]<-"Site.Treatment"


#Spring2013:
df1.5 <- data %>% filter(TST==1.5 & nat=="native" & longevity=="perennial")
str(df1.5)#7289 obs. of  25 variables:

sp.data1.5<- df1.5 %>%
  select("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence")%>%
  na.omit()#su = sampling unit
str(sp.data1.5)#7289 obs. of  3 variables:

#Change sp data to wide format to compute:
veg.wide1.5<-spread(sp.data1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#836  100

#Compute Diversity indices:
veg.wide1.5$DIVshannon<-diversity(veg.wide1.5[,6:100])
veg.wide1.5$DIVsimpson<-diversity(veg.wide1.5[,6:100], index= "simpson")
veg.wide1.5$Density<-rowSums(veg.wide1.5[,6:100])
veg.wide1.5$Richness<-specnumber(veg.wide1.5[,6:100])

#Compute means +95%CI for the ggplot for spring 2013 emergence indices:
veg.wide1.5$season <- "Spring 2013"
d4<-summarySE(veg.wide1.5, measurevar="DIVshannon", groupvars=c("Transdepth","season"))
d4$Filter <- "DISPERSAL"
colnames(d4)[1]<-"Site.Treatment"

d5<-summarySE(veg.wide1.5, measurevar="DIVshannon", groupvars=c("rip","season"))
d5$Filter <- "ABIOTIC"
colnames(d5)[1]<-"Site.Treatment"

d6<-summarySE(veg.wide1.5, measurevar="DIVshannon", groupvars=c("fence","season"))
d6$Filter <- "BIOTIC"
colnames(d6)[1]<-"Site.Treatment"

EmData<-rbind(d1,d2,d3,d4,d5,d6)#merging year two and one
EmData
EmData$Scale <- "Site" #to reduce facetting to one level only on x-axis.

#Shannon-Wienner ggplot:=====
pd <- position_dodge(.5)
Fig_DIVshannon <- 
  ggplot(EmData, aes(x=Site.Treatment, y=DIVshannon,shape=Filter, color=season)) +
  geom_errorbar(aes(ymin=DIVshannon-ci, ymax=DIVshannon+ci),width=.30,position=pd,size=1.4)+
  geom_point(position=pd,size=3)+ 
  #geom_line(position=pd) +
  scale_colour_manual(values = c("green", "red")) +
  facet_grid(season~Filter,  scales="free", drop = T)+theme_bw()+
  scale_y_continuous("Diversity of native perennial plants", limits = c(1.5,2.25))+
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=18),
        legend.position = "none")

Fig_DIVshannon
#ggsave(Fig_DIVshannon, filename = "Fig1_SiteScale_emergenceDIVshannon.jpg", width = 180, height = 120, units = "mm")

#COOLer way to plot correlations:========
#install.packages("GGally")
library(GGally)
#All Sites:
pm <- ggpairs(veg.wide05[,c("DIVshannon","DIVsimpson", "Density")],
              upper = list(continuous = wrap(ggally_points, size = 2, color = "red")),
              lower=list(continuous = wrap(ggally_points, size = 2, color = "blue")),
              title= "Spring 2012 Correlations. Diversity & Density of Native Perennials")

cool_theme<-  theme(legend.position = "none", 
                    plot.title = element_text(face="bold",size=14, hjust=0.5),
                    panel.grid.major = element_blank(), 
                    axis.ticks = element_blank(), 
                    panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA))

pm2<- pm + cool_theme 
pm2 
#ggsave(filename="DivCorrNatPerennialSpring2012.jpeg", dpi=600) 

#Plot-scale Richness SummarySE for spring2012 emergence:=====
#Produce spring2012 subset with plot2 != "control":
Spring2012_Data <- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
Spring2012_Data <- Spring2012_Data[Spring2012_Data$TST== 0.5,]#only in spring2012=tst05
Spring2012_Data <- Spring2012_Data[Spring2012_Data$TST== 0.5 & Spring2012_Data$nat=="native" & Spring2012_Data$longevity=="perennial" ,]#only natives 
#check:
levels(droplevels(Spring2012_Data$nat))#native 
levels(droplevels(Spring2012_Data$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(Spring2012_Data)#7490   24

#Select these few columns to produce species matrix:
sp.data05<- Spring2012_Data %>% 
  select(c("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence"))%>%
  na.omit()
str(sp.data05)#7487 obs. of  7 variables:
#Change sp data to wide format to compute:
veg.wide05<-spread(sp.data05,key = specCode, value = count4m2, fill = 0)
dim(veg.wide05)#665 124

veg.wide05.DU <- veg.wide05[ veg.wide05$Transdepth == "deep" & veg.wide05$rip == "unripped",]
dim(veg.wide05.DU)#168 124
veg.wide05.DU$season <- "Spring 2012"

#Compute Diversity indices:
veg.wide05.DU$DIVshannon<-diversity(veg.wide05.DU[,6:124])
veg.wide05.DU$DIVsimpson<-diversity(veg.wide05.DU[,6:124], index= "simpson")
veg.wide05.DU$Density<-rowSums(veg.wide05.DU[,6:124])
veg.wide05.DU$Richness<-specnumber(veg.wide05.DU[,6:124])


#Compute means +95%CI for the ggplot for spring 2012 emergence indices:
du12<-summarySE(veg.wide05.DU, measurevar="Richness", groupvars=c("plot2","season"))
du12

#Remove plastic and shade as agreed. No effect:
du12<- du12  [  du12$plot2 !="smoke.plastic" ,]
du12 <- du12 [ du12$plot2 !="plastic" ,]
du12<- du12  [  du12$plot2 !="shade" ,]
du12 <- du12 [ du12$plot2 !="shade.semi" ,]

colnames(du12)[1] <- "Treatment"
du12$Filter <- c("CONTROL","BIOTIC", "DISPERSAL")

#Plot-scale Richness SummarySE for spring2013 emergence:=====
Spring2013_Data <- data[data$TST==1.5,]#only records from spring II
Spring2013_Data <- Spring2013_Data[Spring2013_Data$newTST== 1.5,]#only in spring2013=tst05
Spring2013_Data <- Spring2013_Data[Spring2013_Data$newTST== 1.5 & Spring2013_Data$nat=="native" & Spring2013_Data$longevity=="perennial" ,]#only natives 
#check:
levels(droplevels(Spring2013_Data$nat))#native 
levels(droplevels(Spring2013_Data$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(Spring2013_Data)#7290   24

#Select these few columns to produce species matrix:
sp.data1.5<- Spring2013_Data %>% 
  select(c("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence"))%>%
  na.omit()
str(sp.data1.5)#7289 obs. of  7 variables:
#Change sp data to wide format to compute:
veg.wide1.5<-spread(sp.data1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#836 100

veg.wide1.5.DU <- veg.wide1.5[ veg.wide1.5$Transdepth == "deep" & veg.wide1.5$rip == "unripped",]
dim(veg.wide1.5.DU)#224 100
veg.wide1.5.DU$season <- "Spring 2013"

#Compute Diversity indices:
veg.wide1.5.DU$DIVshannon<-diversity(veg.wide1.5.DU[,6:100])
veg.wide1.5.DU$DIVsimpson<-diversity(veg.wide1.5.DU[,6:100], index= "simpson")
veg.wide1.5.DU$Density<-rowSums(veg.wide1.5.DU[,6:100])
veg.wide1.5.DU$Richness<-specnumber(veg.wide1.5.DU[,6:100])


#Compute means +95%CI for the ggplot for spring 2012 emergence indices:
du13<-summarySE(veg.wide1.5.DU, measurevar="Richness", groupvars=c("plot2","season"))
du13

#Remove plastic and shade as agreed. No effect:
du13<- du13  [  du13$plot2 !="smoke.plastic" ,]
du13 <- du13 [ du13$plot2 !="plastic" ,]
du13<- du13  [  du13$plot2 !="shade" ,]
du13 <- du13 [ du13$plot2 !="shade.semi" ,]

colnames(du13)[1] <- "Treatment"
du13$Filter <- c("CONTROL","DISPERSAL","BIOTIC", "DISPERSAL")


EmData<-rbind(du12,du13)#merging year two and one
EmData
EmData$Scale <- "Plot" #to reduce facetting to one level only on x-axis.

#Richness ggplot for all years emergence richness:=====
pd <- position_dodge(.5)
Fig_Richness_plot <- 
  ggplot(EmData, aes(x=Treatment, y=Richness,shape=Filter, color=season)) +
  geom_errorbar(aes(ymin=Richness-ci, ymax=Richness+ci),width=.30,position=pd,size=1.4)+
  geom_point(position=pd,size=3)+ 
  #geom_line(position=pd) +
  scale_colour_manual(values = c("green", "red")) +
  facet_grid(season~Filter,  scales="free", drop = T)+theme_bw()+
  scale_y_continuous("Richness of native perennial plants", limits = c(4,25))+
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=18),
        legend.position = "none")

Fig_Richness_plot
#ggsave(Fig_Richness_plot, filename = "FigS2_PlotScale_emergenceRichness.jpg", width = 180, height = 120, units = "mm")





#SummarySE for Shannon-Wienner indices of spring2012 emergence:=====
Spring2012_Data <- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
Spring2012_Data <- Spring2012_Data[Spring2012_Data$TST== 0.5,]#only in spring2012=tst05
Spring2012_Data <- Spring2012_Data[Spring2012_Data$TST== 0.5 & Spring2012_Data$nat=="native" & Spring2012_Data$longevity=="perennial" ,]#only natives 
#check:
levels(droplevels(Spring2012_Data$nat))#native 
levels(droplevels(Spring2012_Data$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(Spring2012_Data)#7490   24

#Select these few columns to produce species matrix:
sp.data05<- Spring2012_Data %>% 
  select(c("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence"))%>%
  na.omit()
str(sp.data05)#7487 obs. of  7 variables:
#Change sp data to wide format to compute:
veg.wide05<-spread(sp.data05,key = specCode, value = count4m2, fill = 0)
dim(veg.wide05)#665 124

veg.wide05.DU <- veg.wide05[ veg.wide05$Transdepth == "deep" & veg.wide05$rip == "unripped",]
dim(veg.wide05.DU)#168 124
veg.wide05.DU$season <- "Spring 2012"

#Compute Diversity indices:
veg.wide05.DU$DIVshannon<-diversity(veg.wide05.DU[,6:124])
veg.wide05.DU$DIVsimpson<-diversity(veg.wide05.DU[,6:124], index= "simpson")
veg.wide05.DU$Density<-rowSums(veg.wide05.DU[,6:124])
veg.wide05.DU$Richness<-specnumber(veg.wide05.DU[,6:124])


#Compute means +95%CI for the ggplot for spring 2012 emergence indices:
du12<-summarySE(veg.wide05.DU, measurevar="DIVshannon", groupvars=c("plot2","season"))
du12

#Remove plastic and shade as agreed. No effect:
du12<- du12  [  du12$plot2 !="smoke.plastic" ,]
du12 <- du12 [ du12$plot2 !="plastic" ,]
du12<- du12  [  du12$plot2 !="shade" ,]
du12 <- du12 [ du12$plot2 !="shade.semi" ,]

colnames(du12)[1] <- "Treatment"
du12$Filter <- c("CONTROL","BIOTIC", "DISPERSAL")

#SummarySE for Shannon-Wienner indices of spring2013 emergence:=====
Spring2013_Data <- data[data$TST==1.5,]#only records from spring II
Spring2013_Data <- Spring2013_Data[Spring2013_Data$newTST== 1.5,]#only in spring2013=tst05
Spring2013_Data <- Spring2013_Data[Spring2013_Data$newTST== 1.5 & Spring2013_Data$nat=="native" & Spring2013_Data$longevity=="perennial" ,]#only natives 
#check:
levels(droplevels(Spring2013_Data$nat))#native 
levels(droplevels(Spring2013_Data$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(Spring2013_Data)#7290   24

#Select these few columns to produce species matrix:
sp.data1.5<- Spring2013_Data %>% 
  select(c("su", "specCode", "count4m2", "plot2", "Transdepth", "rip", "fence"))%>%
  na.omit()
str(sp.data1.5)#7289 obs. of  7 variables:
#Change sp data to wide format to compute:
veg.wide1.5<-spread(sp.data1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#836 100

veg.wide1.5.DU <- veg.wide1.5[ veg.wide1.5$Transdepth == "deep" & veg.wide1.5$rip == "unripped",]
dim(veg.wide1.5.DU)#224 100
veg.wide1.5.DU$season <- "Spring 2013"

#Compute Diversity indices:
veg.wide1.5.DU$DIVshannon<-diversity(veg.wide1.5.DU[,6:100])
veg.wide1.5.DU$DIVsimpson<-diversity(veg.wide1.5.DU[,6:100], index= "simpson")
veg.wide1.5.DU$Density<-rowSums(veg.wide1.5.DU[,6:100])
veg.wide1.5.DU$Richness<-specnumber(veg.wide1.5.DU[,6:100])


#Compute means +95%CI for the ggplot for spring 2012 emergence indices:
du13<-summarySE(veg.wide1.5.DU, measurevar="DIVshannon", groupvars=c("plot2","season"))
du13

#Remove plastic and shade as agreed. No effect:
du13<- du13  [  du13$plot2 !="smoke.plastic" ,]
du13 <- du13 [ du13$plot2 !="plastic" ,]
du13<- du13  [  du13$plot2 !="shade" ,]
du13 <- du13 [ du13$plot2 !="shade.semi" ,]

colnames(du13)[1] <- "Treatment"
du13$Filter <- c("CONTROL","DISPERSAL","BIOTIC", "DISPERSAL")


EmData<-rbind(du12,du13)#merging year two and one
EmData
EmData$Scale <- "Plot" #to reduce facetting to one level only on x-axis.

#DIVshannon ggplot for all years emergence richness:=====
pd <- position_dodge(.5)
Fig_DIVshannon_plot <- 
  ggplot(EmData, aes(x=Treatment, y=DIVshannon,shape=Filter, color=season)) +
  geom_errorbar(aes(ymin=DIVshannon-ci, ymax=DIVshannon+ci),width=.30,position=pd,size=1.4)+
  geom_point(position=pd,size=3)+ 
  #geom_line(position=pd) +
  scale_colour_manual(values = c("green", "red")) +
  facet_grid(season~Filter,  scales="free", drop = T)+theme_bw()+
  scale_y_continuous("Diversity of native perennial plants", limits = c(1,2.7))+
  theme(axis.text.y=element_text(size=18),
        axis.text.x=element_text(size=18),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=18),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=18),
        legend.position = "none")

Fig_DIVshannon_plot
#ggsave(Fig_DIVshannon_plot, filename = "FigS2_PlotScale_emergenceDIVshannon.jpg", width = 180, height = 120, units = "mm")


#Arrange plots====
library(grid)
library(gridExtra)

PlotRichDiv_Fig <- grid.arrange(Fig_Richness_plot, Fig_DIVshannon_plot)
PlotRichDiv_Fig
ggsave(PlotRichDiv_Fig, filename = "FigS2_PlotScale_RichnessDIVshannon.jpg", width = 180, height = 240, units = "mm")

SiteRichDiv_Fig <- grid.arrange(Fig_Richness, Fig_DIVshannon)
SiteRichDiv_Fig
ggsave(SiteRichDiv_Fig, filename = "FigS2_SiteScale_RichnessDIVshannon.jpg", width = 180, height = 240, units = "mm")
