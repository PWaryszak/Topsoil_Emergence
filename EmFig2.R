#FIGURE 2- effect of plot-scale treatments on native perennial plant emergence densities
#LOAD DATA for FIG2 ================
library(tidyverse)
library(Rmisc)
#Re-Load data:
data<- read.csv("TopsoilEmergenceData.csv")
str(data)#40924 obs. of  23 variables:
levels(data$plot2)#"control",heat","herbicide","plastic","pr2", "shade","shade.semi"    "smoke"         "smoke.plastic"
#change if needed to be length-consistent to: "CTRL" "HEAT" "HERB" "PLAS" "SHAD" "SHSE" "SMOK" "SMPL"
levels(droplevels(data$dataStart))#"aut13"     "spr12"     "spr12only"

#Emergence Densities YEAR ONE:=========
gsmall<- data[data$dataStart=="spr12" | data$dataStart=="spr12only",]#only records from spring I, dim(g1) = 658476     39
gsmall1<-gsmall[gsmall$TST== 0.5,]#only in spring2012=tst05
gsmall2<-gsmall1[gsmall1$TST== 0.5 & gsmall1$nat=="native" ,]#only natives

levels(droplevels(gsmall2$nat))#native 
levels(droplevels(gsmall1$plot2))#"control","herbicide", "plastic" ,"shade","shade.semi"    "smoke"         "smoke.plastic"
dim(gsmall2)#10333    23

#Compute the total number of su (sampling units) in this year study:
n1.levels <- an1<-aggregate(gsmall1$count1m2,              
                            by = list(su=gsmall1$su, site=gsmall1$site, 
                                      Transdepth=gsmall1$Transdepth, rip=gsmall1$rip,
                                      comb2=gsmall1$comb2, plot2=gsmall1$plot2), FUN = "sum")
length(levels(droplevels(n1.levels$su)))#673 = total number of su-s (sampling units)
colnames(n1.levels)[7]<-"sum1m2"
n1.levels$year<-"one"
head(n1.levels)

#compute densities per plot = su in year one:
n.fenced<-gsmall2[gsmall2$fence=="fenced",]#subsetting only the fenced values as plot-treatments were applied
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

empty.plots <-  dplyr::anti_join(n1.levels, n1.perennials, by = "su")
empty.plots #all plots in a1 that do not have match in a1.perennial
empty.plots$longevity <- "perennial"
empty.plots$sum1m2 <- 0

perennials.year.one<-rbind(n1.perennials,empty.plots)#
str(perennials.year.one)#673 obs. of  9 variables:
levels(droplevels((perennials.year.one$longevity)))# double check. YES = "perennial" only




#Emergence Densities YEAR TWO=======
#compute densities per plot = su:
ggsmall<- data[data$TST==1.5,]#only records from spring II 
ggsmall2<-ggsmall[ggsmall$newTST== 1.5 ,]

#We need to remove shade cause not involved in germination:
ggsmall3<-ggsmall2[!ggsmall2$plot2=="shade" & !ggsmall2$plot2== "shade.semi",]
dim(ggsmall3)#19530    23
levels(droplevels(ggsmall3$plot2))#"control", "heat" ,"herbicide","plastic","smoke","smoke.plastic"
length(levels(droplevels(ggsmall3$su)))# 804

#Run aggregate to get total number of su in year two:
n2.levels <-aggregate(ggsmall3$count1m2,              
                            by = list(su=ggsmall3$su, site=ggsmall3$site, 
                                      Transdepth=ggsmall3$Transdepth, rip=ggsmall3$rip,
                                      comb2=ggsmall3$comb2, plot2=ggsmall3$plot2), FUN = "sum")
length(levels(droplevels(n2.levels$su)))#804= total number of su-s (sampling units) in year two
#number of plots went up in year two compared to year one
#as we increased nuber of observation units (su) due to high mortality after year one surveys,
colnames(n2.levels)[7]<-"sum1m2"
n2.levels$year<-"one"
head(n2.levels)

ggsmall4<-ggsmall3[ggsmall3$nat=="native",] #subset native seedlings only (our focus)
dim(ggsmall4)# 10408    44

#compute densities per plot = su in year one:
n2.fenced<-ggsmall4[ggsmall4$fence=="fenced",]#subsetting only the fenced values as plot-treatments were applied
str(n2.fenced)#6743 obs. of  23 variables:

n2<-aggregate(n2.fenced$count1m2,              
              by = list(su=n2.fenced$su, site=n2.fenced$site, 
                        Transdepth=n2.fenced$Transdepth, rip=n2.fenced$rip,
                        comb2=n2.fenced$comb2, plot2=n2.fenced$plot2,
                        longevity=n2.fenced$longevity), FUN = "sum")
str(n2)#992 obs. of  8 variables:
colnames(n2)[8]<-"sum1m2"
n2$year<-"two"
head(n2)

n2.perennials <- n2 [ n2$longevity=="perennial",c( "su", "site","Transdepth","rip","comb2","plot2","sum1m2","year","longevity")]
str(n2.perennials)#506 obs. of  9 variables:

empty.plots2 <-  dplyr::anti_join(n2.levels, n2.perennials, by = "su")
empty.plots2 #all plots in a1 that do not have match in a1.perennial
empty.plots2$longevity <- "perennial"
empty.plots2$sum1m2 <- 0

perennials.year.two<-rbind(n2.perennials,empty.plots2)#
str(perennials.year.two)#804 obs. of  9 variables:
levels(droplevels((perennials.year.two$longevity)))# double check. YES = "perennial" only

#BIND both years:
n<-rbind(perennials.year.one,perennials.year.two)#binding two springs together (spr12 & spr13)
str(n)#1477 obs. of  9 variables:


#SummarySE on BOTH YEARS in native:============
DU <- n [n$comb2=="deep.unripped",] #these are our controls
str(DU)#384 obs. of  9 variables:

fig2<-summarySE(DU, measurevar="sum1m2", groupvars=c("comb2","plot2","longevity","year"))
fig2
dim(fig2data)#13  11

#Drawing Figure 2 perennials only, both years, (plot-scale treats)==============
range(fig2data$sum1m2)# 7.273026 18.093750
fig2data$Scale <- "Site" #to reduce facetting to one level only on x-axis.
fig2data$year2 <- ifelse(fig2data$year == "one", "Spring 2012", "Spring 2013") #to make it look better on figure
#Define filters:
fig2data2 <- fig2data %>%  mutate(Filter = recode(plot2, 
                         plastic = "ABIOTIC",
                         control = "CONTROL", heat = "DISPERSAL",
                         herbicide = "BIOTIC", plastc = "ABIOTIC",
                         shade = "ABIOTIC", shade.semi = "ABIOTIC",
                         smoke = "DISPERSAL", smoke.plastic = "DISPERSAL"))



fig2data3 <- subset (fig2data2, fig2data2$plot2 != "shade")
fig2data3 <- subset (fig2data3, fig2data3$plot2 != "shade.semi")
fig2data3
fig2data3$Filter<- factor(fig2data3$Filter, levels = c("CONTROL", "ABIOTIC", "BIOTIC", "DISPERSAL"))
fig2data3$plot2<- factor(fig2data3$plot2)
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
Fig4
#Changing the font size:
Fig5<- Fig4 +theme(axis.text.y=element_text(size=20),
                   axis.text.x=element_blank(),
                   axis.title.x=element_blank(),
                   axis.title.y=element_text(size=24),
                   panel.grid.minor.x = element_blank(),
                   strip.text=element_text(size=20),
                   legend.position = "none")
Fig5
Fig6<- Fig5 +  scale_y_continuous("Plant Density (m\u00B2)", limits = c(4,25))
Fig6
