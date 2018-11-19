#Supplementary Site-Effect FIGURE  effect of site on perennial plant emergence densities
#LOAD DATA ================
library(tidyverse)
library(Rmisc)
data<- read.csv("TopsoilEmergenceData.csv")#our density data
str(data)#40924 obs. of  22 variables: = all good.

#SUBSET PERENNIALS IN YEAR ONE ONLY + compute densities per su:=========
#Aggregate data to compute sums per 1 m2 in year one:
gnative1<-data[data$plot2=="control" & data$TST== 0.5,]#only controls in spring2012=tst05
dim(gnative1)#  11406    24 = that many in site-level treatments (coded "Control")
gnative2<-gnative1[gnative1$TST== 0.5 & gnative1$nat=="native" ,]#only natives
dim(gnative2)#6411   24 - that many natives

n2<-levels(droplevels(gnative2$su))
length(n2)#433 all good! = number of surveyed plots (observation units)

#compute densities per plot = su:
a1<-aggregate(gnative2$count1m2,
              by = list(su=gnative2$su,site=gnative2$site,plot=gnative2$plot,
                        Transdepth=gnative2$Transdepth, rip=gnative2$rip,
                        fence=gnative2$fence,plot2=gnative2$plot2,longevity=gnative2$longevity),
              FUN = "sum")

dim(a1)# 834 by 9, range(a1$sum1m2) =0.25 71.75
a1$year<-"one"
colnames(a1)[9]<-"sum1m2"

#Check for empty plots to use them in computing mean densities per su:
a1.perennial<-a1[a1$longevity=="perennial",] #these su of a1 that contain perennials
empty.plots <-  dplyr::anti_join(a1,a1.perennial, by = "su")
empty.plots #all plots in a1 that do not have match in a1.perennial
#           su  site plot Transdepth      rip  fence   plot2 longevity sum1m2 year
#1  AnkE.SRFC.a4  AnkE   a4    shallow   ripped fenced control    annual  17.25  one
#2  AnkE.SRFC.b4  AnkE   b4    shallow   ripped fenced control    annual  33.25  one
#3  AnkM.SRFC.a3  AnkM   a3    shallow   ripped fenced control    annual  31.00  one
#4  AnkM.SROC.a1  AnkM   a1    shallow   ripped   open control    annual  55.25  one
#5  AnkM.SUFC.a3  AnkM   a3    shallow unripped fenced control    annual  50.00  one
#6 ForSE.DROC.c1 ForSE   c1       deep   ripped   open control    annual   1.00  one
#7 ForSE.DROC.c2 ForSE   c2       deep   ripped   open control    annual   8.00  one

#filling in empty plots with zero values for perennials and merging them with a1.perennial:
empty.plots$longevity <- "perennial"
empty.plots$sum1m2 <- 0

#Creating final emergence densities data.frame:
perennials.year.one<-rbind(a1.perennial,empty.plots)#data.frame':	433 obs. of  10 variables:
str(perennials.year.one)#433 obs. of  10 variables:
levels(droplevels((perennials.year.one$longevity)))# double check. YES = "perennial" only

#computing 95% CI-s for year ONE==============
t<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("Transdepth","longevity","year", "site"))
colnames(t)[1]<-"Site.Treatment"
t$Filter<- "DISPERSAL"
t

r<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("rip","longevity","year","site"))
colnames(r)[1]<-"Site.Treatment"
r$Filter  <- "ABIOTIC"
r

f<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("fence","longevity","year", "site"))
colnames(f)[1]<-"Site.Treatment"
f$Filter  <- "BIOTIC"
f

combo<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("Transdepth","rip","longevity","year", "site"))
combo

#SUBSET PERENNIALS IN YEAR TWO ONLY + compute densities per su:=========
ggnative<- data[data$TST==1.5 & data$plot2=="control",]#only records from spring II 
ggnative2<-ggnative[ggnative$nat=="native",]#native that germinated only in year two
length(as.character(levels(droplevels(ggnative2$su))))#584 su-s that is survey plots
dim(ggnative2)#7446   24

a2<-aggregate(ggnative2$count1m2,
              by = list(su=ggnative2$su,site=ggnative2$site,plot=ggnative2$plot,
                        Transdepth=ggnative2$Transdepth, rip=ggnative2$rip,
                        fence=ggnative2$fence,plot2=ggnative2$plot2,longevity=ggnative2$longevity),
              FUN = "sum")

a2$year<-"two"
colnames(a2)[9]<-"sum1m2"

a2.perenial<-a2[a2$longevity=="perennial",]# subsetting su-s where perennials were detected
dim(a2.perenial)# = 578  10

#finding and merging empty plots
a2.perennial<-a2[a2$longevity=="perennial",] #these su of a2 that contain perennials
empty.plots2 <-  dplyr::anti_join(a2,a2.perennial, by = "su")
empty.plots2 #all plots in a1 that do not have match in a1.perennial
#su  site plot Transdepth      rip  fence   plot2 longevity sum1m2 year
#1  AnkM.SRFC.a3  AnkM   a3    shallow   ripped fenced control    annual   8.75  two
#2  AnkM.SROC.a1  AnkM   a1    shallow   ripped   open control    annual   8.50  two
#3 ForSW.SUFC.a4 ForSW   a4    shallow unripped fenced control    annual   0.50  two
#4 ForSW.SUFC.c3 ForSW   c3    shallow unripped fenced control    annual   0.50  two
#5 ForSW.SUFC.c4 ForSW   c4    shallow unripped fenced control    annual   1.00  two

#filling in empty plots with zero values for perennials and merging them with a1.perennial:
empty.plots2$longevity <- "perennial"
empty.plots2$sum1m2 <- 0

#MErging zero plots with reminder of survey plots
perennials.year.two<-rbind(a2.perenial, empty.plots2)
str(perennials.year.two)#584 obs. of  10 variables:

#computing 95% CI-s for year TWO==============
t2<-summarySE(perennials.year.two, measurevar="sum1m2", groupvars=c("Transdepth","longevity","year", "site"))
colnames(t2)[1]<-"Site.Treatment"
t2$Filter<- "DISPERSAL"
t2


r2<-summarySE(perennials.year.two, measurevar="sum1m2", groupvars=c("rip","longevity","year", "site"))
colnames(r2)[1]<-"Site.Treatment"
r2$Filter  <- "ABIOTIC"
r2

f2<-summarySE(perennials.year.two, measurevar="sum1m2", groupvars=c("fence","longevity","year", "site"))
colnames(f2)[1]<-"Site.Treatment"
f2$Filter  <- "BIOTIC"
f2

#NEW Supp SITE FIG (20nov2018):====
Fig1 <-   ggplot(EmData, aes(x=site, y=sum1m2, shape=Site.Treatment, color=year2))+
  geom_errorbar(aes(ymin=sum1m2-ci, ymax=sum1m2+ci),width=.2,position=pd,size=1.4)+
  geom_point(position=pd,size=3)+ 
  geom_line(position=pd) +
  facet_grid(year2~Filter+Site.Treatment,  scales="free", drop = T)+theme_bw()+
  theme(axis.text.y=element_text(size=17),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=20),
        axis.text.x = element_text(size=10, angle = 90),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=12),
        legend.position = "none") +
  labs(y=expression(Plant~density~(m^{-2})))+
  scale_y_continuous(limits = c(0,21))

Fig1
ggsave(Fig1, filename = "SiteFig1b.jpg", width = 200, height = 100, units = "mm")
