#FIGURE 2- effect of plot-scale treatments on native perennial plant emergence densities
#LOAD DATA for FIG2 ================
library(tidyverse)
library(Rmisc)
#Re-Load data:
data<- read.csv("TopsoilEmergenceData.csv")
str(data)#38247 obs. of  24 variables:
levels(data$plot2)#"control",heat","herbicide","plastic","pr2", "shade","shade.semi"    "smoke" "smoke.plastic"
#change if needed to be length-consistent to: "CTRL" "HEAT" "HERB" "PLAS" "SHAD" "SHSE" "SMOK" "SMPL"
levels(droplevels(data$dataStart))#"aut13"     "spr12"     "spr12only"

#Emergence Densities YEAR ONE:=========
Spring2012_Data<- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
Spring2012_Data1<-Spring2012_Data[Spring2012_Data$TST== 0.5,]#only in spring2012=tst05
Spring2012_Data2<-Spring2012_Data1[Spring2012_Data1$TST== 0.5 & Spring2012_Data1$nat=="native" ,]#only natives

levels(droplevels(Spring2012_Data2$nat))#native 
levels(droplevels(Spring2012_Data1$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(Spring2012_Data2)#10277    24

#Compute the total number of su (sampling units) in this year study:
n1.levels <- an1<-aggregate(Spring2012_Data1$count1m2,              
                            by = list(su=Spring2012_Data1$su, site=Spring2012_Data1$site, 
                                      Transdepth=Spring2012_Data1$Transdepth, rip=Spring2012_Data1$rip,
                                      comb2=Spring2012_Data1$comb2, plot2=Spring2012_Data1$plot2), FUN = "sum")
length(levels(droplevels(n1.levels$su)))#673 = total number of su-s (sampling units)
colnames(n1.levels)[7]<-"sum1m2"
n1.levels$year<-"one"
head(n1.levels)

#compute densities per plot = su in year one:
n.fenced<-Spring2012_Data2[Spring2012_Data2$fence=="fenced",]#subsetting only the fenced values as plot-treatments were applied
str(n.fenced)#7511 obs. of  23 variables:

n1<-aggregate(n.fenced$count1m2,              
              by = list(su=n.fenced$su, site=n.fenced$site, 
                        Transdepth=n.fenced$Transdepth, rip=n.fenced$rip,
                        comb2=n.fenced$comb2, plot2=n.fenced$plot2,
                        longevity=n.fenced$longevity), FUN = "sum")
str(n1)#938 obs. of  8 variables:
colnames(n1)[8]<-"sum1m2"
n1$year<-"one"
head(n1)
n1.perennials <- n1 [ n1$longevity=="perennial",c( "su", "site","Transdepth","rip","comb2","plot2","sum1m2","year","longevity")]
str(n1.perennials)#476 obs. of  9 variables:


#Emergence Densities YEAR TWO=======
#compute densities per plot = su:
Spring2013_Data<- data[data$TST==1.5,]#only records from spring II 
Spring2013_Data2<-Spring2013_Data[Spring2013_Data$newTST== 1.5 ,]

#We need to remove shade cause not involved in germination:
Spring2013_Data3 <- Spring2013_Data2[!Spring2013_Data2$plot2=="shade" & !Spring2013_Data2$plot2== "shade.semi",]

levels(droplevels(Spring2013_Data3$plot2))#"control", "heat" ,"herbicide","plastic","smoke","smoke.plastic"
length(levels(droplevels(Spring2013_Data3$su)))# 804

Spring2013_Data4<-Spring2013_Data3[Spring2013_Data3$nat=="native",] #subset native seedlings only (our focus)
dim(Spring2013_Data4)# 10100    24

#compute densities per plot = su in year one:
n2.fenced<-Spring2013_Data4[Spring2013_Data4$fence=="fenced",]#subsetting only the fenced values as plot-treatments were applied
str(n2.fenced)#6507 obs. of  24 variables:

n2<-aggregate(n2.fenced$count1m2,              
              by = list(su=n2.fenced$su, site=n2.fenced$site, 
                        Transdepth=n2.fenced$Transdepth, rip=n2.fenced$rip,
                        comb2=n2.fenced$comb2, plot2=n2.fenced$plot2,
                        longevity=n2.fenced$longevity), FUN = "sum")
str(n2)#992 obs. of  8 variables:
colnames(n2)[8]<-"sum1m2"
n2$year<-"two"
nrow(n2[n2$sum1m2==0,])#1 = that many plots were empty of native perennials in year two:

n2.perennials <- n2 [ n2$longevity=="perennial",c( "su", "site","Transdepth","rip","comb2","plot2","sum1m2","year","longevity")]
str(n2.perennials)#506 obs. of  9 variables:

#BIND both years:
n<-rbind(n1.perennials ,n2.perennials )#binding two springs together (spr12 & spr13)
str(n)#982 obs. of  9 variables:


#SummarySE on BOTH YEARS in native:============
DU <- n [n$comb2=="deep.unripped",] #these are our controls
str(DU)#262 obs. of  9 variables:

fig2data<-summarySE(DU, measurevar="sum1m2", groupvars=c("comb2","plot2","longevity","year"))
fig2data
dim(fig2data)#13 9
AllDU<-summarySE(DU, measurevar="sum1m2", groupvars=c("comb2","longevity","year"))
AllDU

#Drawing Figure 2 perennials only, both years, (plot-scale treats)==============
fig2data$Scale <- "Site" #to reduce facetting to one level only on x-axis.
fig2data$year2 <- ifelse(fig2data$year == "one", "Spring 2012", "Spring 2013") #to make it look better on figure
#Define filters:
fig2data2 <- fig2data %>%  mutate(Filter = recode(plot2, 
                         plastic = "ABIOTIC",
                         control = "CONTROL", heat = "DISPERSAL",
                         herbicide = "BIOTIC", plastc = "ABIOTIC",
                         shade = "ABIOTIC", shade.semi = "ABIOTIC",
                         smoke = "DISPERSAL", smoke.plastic = "DISPERSAL"))



fig2data3 <- subset (fig2data2, fig2data2$plot2 != "shade")#remove shade as designed for survival not emergence
fig2data3 <- subset (fig2data3, fig2data3$plot2 != "shade.semi")#remove shade.semi (stolen shade)
fig2data3
fig2data3$Filter<- factor(fig2data3$Filter, levels = c("CONTROL", "ABIOTIC", "BIOTIC", "DISPERSAL"))
fig2data3$plot2<- factor(fig2data3$plot2, levels = c("control", "herbicide", "heat", 
                                                     "plastic", "smoke","smoke.plastic"))

#Table of Emergence Densiteis in Year I & II=======
fig2data3
############comb2         plot2 longevity year  N    sum1m2        sd        se       ci Scale       year2    Filter
#3  deep.unripped       control perennial  one 60 15.583333  9.448306 1.2197710 2.440756  Site Spring 2012   CONTROL
#4  deep.unripped       control perennial  two 76  7.273026  6.196394 0.7107751 1.415937  Site Spring 2013   CONTROL
#6  deep.unripped          heat perennial  two 20 11.762500  8.603654 1.9238355 4.026634  Site Spring 2013 DISPERSAL
#9  deep.unripped     herbicide perennial  one 12 18.020833 10.111570 2.9189588 6.424585  Site Spring 2012    BIOTIC
#10 deep.unripped     herbicide perennial  two 11  9.545455  6.202639 1.8701659 4.166989  Site Spring 2013    BIOTIC
#13 deep.unripped       plastic perennial  one 12 15.666667  7.516144 2.1697240 4.775530  Site Spring 2012   ABIOTIC
#14 deep.unripped       plastic perennial  two 11  8.045455  4.711446 1.4205545 3.165193  Site Spring 2013   ABIOTIC
#21 deep.unripped         smoke perennial  one 12 15.250000  8.202688 2.3679121 5.211739  Site Spring 2012 DISPERSAL
#22 deep.unripped         smoke perennial  two 12  9.000000  7.842918 2.2640554 4.983152  Site Spring 2013 DISPERSAL
#25 deep.unripped smoke.plastic perennial  one 12 16.375000  7.257050 2.0949298 4.610910  Site Spring 2012 DISPERSAL
#26 deep.unripped smoke.plastic perennial  two 12 11.062500  9.571575 2.7630758 6.081489  Site Spring 2013 DISPERSAL

#Drawing Fig 2, plot-scale treatments============
pd <- position_dodge(.5)
Fig1<-ggplot(fig2data3, aes(x=Scale, y=sum1m2, shape=year2, color=year2))
Fig2<-Fig1 +geom_errorbar(aes(ymin=sum1m2-ci, ymax=sum1m2+ci),width=.2,position=pd,size=1.4)
Fig3<-Fig2+ geom_point(position=pd,size=6)
Fig3a<-Fig3 + geom_line(position=pd) + scale_colour_manual(values = c("green", "red")) +scale_shape_manual(values=c(15,15))
Fig4<-Fig3a+facet_grid(year2~Filter + plot2)+theme_bw()
#Changing the font size:
Fig5<- Fig4 +theme(axis.text.y=element_text(size=20),
                   axis.text.x=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_text(size=24),
                   panel.grid.minor.x = element_blank(),
                   strip.text=element_text(size=20),
                   legend.position = "none")
Fig6<- Fig5 +  scale_y_continuous("Plant Density (m\u00B2)", limits = c(4,25))
#Fig6

#Testing NEW ARRANGMENTS:
pd <- position_dodge(.5)
#Remove plastic as agreed. No effect:
fig2data4 <- fig2data3 [  fig2data3$plot2 !="smoke.plastic" ,]
fig2data5 <- fig2data4 [  fig2data4$plot2 !="plastic" ,]

Fig2 <- ggplot(fig2data5, aes(x=plot2, y=sum1m2, shape=Filter, color=year2))+
 geom_errorbar(aes(ymin=sum1m2-ci, ymax=sum1m2+ci),width=.2,position=pd,size=1.4)+
 geom_point(position=pd,size=6)+ 
 geom_line(position=pd) +
 scale_colour_manual(values = c("green", "red")) +
 scale_shape_manual(values=c(16,15,15,15))+
 facet_grid(year2~Filter,  scales="free", drop = T)+theme_bw()+
  theme(axis.text.y=element_text(size=18),
         #axis.text.x=element_blank(),
         axis.title.x=element_blank(),
         axis.title.y=element_text(size=24),
         axis.text.x = element_text(size=18),
         panel.grid.minor.x = element_blank(),
         strip.text=element_text(size=20),
         legend.position = "none") +
  labs(y=expression(Plant~density~(m^{-2})))+
  scale_y_continuous(limits = c(4,25))


ggsave(Fig2, filename = "Fig2_Plot2emergenceNEW.jpg", width = 160, height = 100, units = "mm")
