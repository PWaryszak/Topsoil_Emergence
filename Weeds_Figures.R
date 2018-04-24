#METADATA for TopsoilEmergenceData.csv----------
#EmData = R-File for Emergence Data Analysis & Visualization.
#It contains data on emergence of plant species on restoration sites (Details in Pawel Waryszak 's PhD thesis)
#TST = Time since Topsoil transfer, 0.5 = spring 2012 (half a year after transfer) & 1.5 = Spring 2013
#newTST - relates to when the seedling was first recorded (important for survial chapter)
#count4m2 = total Count per 4 square meters vegetation survey plot.
#count1m2 = totCnt/4 to express it per 1 square m.
#cluster = block of 4m2 plots, here called "sub"

#WEEDS YEAR I Site-Scale:=======
library(tidyverse)
#setwd("~/OneDrive/Documents/Murdoch/All Chapters Together/Manuscripts/Germination Manuscript")
data<- read.csv("TopsoilEmergenceData.csv")

#Group by treatmet and sampling units:
#Transdepth:
d1.depth <- filter(data, TST == 0.5 & nat == "invasive")%>%
group_by(su, Transdepth) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

dim(d1.depth)#673   3
colnames(d1.depth)[2]<- "Site.Treatment"
d1.depth$Filter <- "DISPERSAL"


#rip:
d1.rip <- filter(data, TST == 0.5 & nat == "invasive")%>%
  group_by(su, rip) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

dim(d1.rip)#673   3
colnames(d1.rip)[2]<- "Site.Treatment"
d1.rip$Filter <- "ABIOTIC"

d1.fence<- filter(data, TST == 0.5 & nat == "invasive")%>%
  group_by(su, fence) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

dim(d1.fence)#674   3
colnames(d1.fence)[2]<- "Site.Treatment"
d1.fence$Filter <- "BIOTIC"

d1<-bind_rows(d1.depth,d1.rip,d1.fence)
dim(d1)#2019    4

#We need to split su into site, cluster,plot:
weeds1 <- d1  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)
dim(weeds1)#2019    7

#WEEDS YEAR II Site-Scale:==========
#Transdepth:
d2.depth <- filter(data, newTST == 1.5 & nat == "invasive")%>%
  group_by(su, Transdepth) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

dim(d2.depth)#846   3
colnames(d2.depth)[2]<- "Site.Treatment"
d2.depth$Filter <- "DISPERSAL"


#rip:
d2.rip <- filter(data, newTST == 1.5 & nat == "invasive")%>%
  group_by(su, rip) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

dim(d2.rip)#846   3
colnames(d2.rip)[2]<- "Site.Treatment"
d2.rip$Filter <- "ABIOTIC"

d2.fence<- filter(data, newTST == 1.5 & nat == "invasive")%>%
  group_by(su, fence) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

dim(d2.fence)#846   3
colnames(d2.fence)[2]<- "Site.Treatment"
d2.fence$Filter <- "BIOTIC"

d2<-bind_rows(d2.depth,d2.rip,d2.fence)
dim(d2)#2538    4

#We need to split su into site, cluster,plot:
weeds2 <- d2  %>%  separate( col=su,  c("site", "cluster", "plot"), remove = FALSE)

#WEEDS Densities both years:============
weeds1$Year<-"Spring2012"
as.data.frame(weeds1)

weeds2$Year<-"Spring2013"
as.data.frame(weeds2)

identical(names(weeds1), names(weeds2)) # TRUE - YAY! after computing sums
#we can merge two years together now
weeds1and2<-rbind(weeds1,weeds2)#does not like it:
str(weeds1and2)#4563 obs. of  8 vari

#Compute 95% CI using summarySE:
library(Rmisc)
all.weeds<-summarySE(weeds1and2, measurevar="Weed.Density.1m2",
                     groupvars=c("Site.Treatment","Filter","Year"))
all.weeds$nat<-"invasive" #it looks like summarySE does not like dplyr produce -> NAs produced
all.weeds
all.weeds<-all.weeds[1:12,]
all.weeds

#GGPLOT:
pd <- position_dodge(.5)

Weed.Fig1 <-  ggplot(all.weeds, aes(x=Site.Treatment, y=Weed.Density.1m2, shape = Site.Treatment, color = Year))+
  geom_errorbar(aes(ymin=Weed.Density.1m2-ci, ymax=Weed.Density.1m2+ci),width=.2,position=pd,size=1.4)+
  geom_point(position=pd,size=6)+ 
  geom_line(position=pd) +
  scale_colour_manual(values = c("black", "red")) +
  scale_shape_manual(values=c(2,22,4,25,8,11))+
  facet_grid(.~Filter,  scales="free", drop = T)+
  labs(y=expression(Weed~density~(m^{-2})), color = "Emergence Season", shape = "Treatment")+
  theme_bw()+
  theme(axis.text.y=element_text(size=17),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24),
        axis.text.x = element_text(size=13),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=17),
        legend.position = "right") +
  scale_y_continuous(limits = c(55,187))

Weed.Fig1
#ggsave(Weed.Fig1, filename = "Fig_SiteScale_WEEDS3.jpg", width = 180, height = 120, units = "mm")

#################################################


#################################################
# INVASIVES YEAR ONE plot2:=========
#Transdepth:

w <- filter(data, newTST == 0.5 & nat == "invasive" & comb2 == "deep.unripped" & fence == "fenced") %>%
  group_by(su, plot2) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))


colnames(w)[3]<-"sum1m2"
w$year<-"Spring2012"


#INVASIVE YEAR TWO plot2=======
#compute densities per plot = su:
ww <- filter(data, newTST == 1.5 & nat == "invasive" & comb2 == "deep.unripped" & fence == "fenced") %>%
  group_by(su, plot2) %>% na.omit() %>%
  summarise(Weed.Density.1m2 = sum(count1m2))

colnames(ww)[3]<-"sum1m2"
ww$year<-"Spring2013"


#BIND both years:
www<-rbind(w ,ww)#binding two springs together (spr12 & spr13)
str(www)#70 obs. of  4 vars

#SummarySE on BOTH YEARS in INVASIVE
library(Rmisc)
WeedsFig2data<-summarySE(www, measurevar="sum1m2", groupvars=c("plot2","year"))
WeedsFig2data

#Define filters:
WeedsFig2data2 <- WeedsFig2data %>%  mutate(Filter = recode(plot2, 
                                                          plastic = "ABIOTIC",
                                                          control = "CONTROL", heat = "DISPERSAL",
                                                          herbicide = "BIOTIC", plastc = "ABIOTIC",
                                                          shade = "ABIOTIC", shade.semi = "ABIOTIC",
                                                          smoke = "DISPERSAL", smoke.plastic = "DISPERSAL"))



WeedsFig2data3 <- subset (WeedsFig2data2, WeedsFig2data2$plot2 != "shade")#remove shade as designed for survival not emergence
WeedsFig2data3 <- subset (WeedsFig2data3, WeedsFig2data3$plot2 != "shade.semi")#remove shade.semi (stolen shade)
WeedsFig2data3$Filter<- factor(WeedsFig2data3$Filter, levels = c("CONTROL", "ABIOTIC", "BIOTIC", "DISPERSAL"))
WeedsFig2data3$plot2<- factor(WeedsFig2data3$plot2, levels = c("control", "herbicide", "heat", 
                                                             "plastic", "smoke","smoke.plastic"))

#WeedFig-both years, plot-scale treatments============
pd <- position_dodge(.5)
#Remove plastic as agreed. No effect:
WeedsFig2data4 <- WeedsFig2data3 [  WeedsFig2data3$plot2 !="smoke.plastic" ,]
WeedsFig2data5 <- WeedsFig2data4 [  WeedsFig2data4$plot2 !="plastic" ,]
WeedsFig2data5
WeedsFig2 <- ggplot(WeedsFig2data5, aes(x=plot2, y=sum1m2, shape=plot2, color=year))+
  geom_errorbar(aes(ymin=sum1m2-ci, ymax=sum1m2+ci),width=.2,position=pd,size=1.4)+
  geom_point(position=pd,size=6)+ 
  geom_line(position=pd) +
  scale_colour_manual(values = c("black", "red")) +
  scale_shape_manual(values=c(16,22,8,11))+
  facet_grid(.~Filter, scales="free", drop = T)+
  labs(y=expression(Weed~density~(m^{-2})), color = "Emergence Season", shape = "Treatment")+
  theme_bw()+
  theme(axis.text.y=element_text(size=17),
        #axis.text.x=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=22),
        axis.text.x = element_text(size=17),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=17),
        legend.position = "right") +

  scale_y_continuous(limits = c(30, 260))


WeedsFig2
#ggsave(WeedsFig2, filename = "WeedsFig_Plot2emergenceNEW3.jpg", width = 180, height = 110, units = "mm")
