#LOAD DATA:
#Correlation between density and Richness indices:
library(tidyverse)
library(vegan)
library(Rmisc)
data<- read.csv("TopsoilEmergenceData.csv")#our density data
str(data)#38247 obs. of  25 variables:

#Pearson's Correalation Test for Spring 2012=========
#Subset native, perennials species data for TST=0.5 (Spring 2012):
df05 <- data %>% filter(TST==0.5 & nat=="native" & longevity=="perennial")
str(df05)#7487 obs. of  25 variables:

sp.data05<- df05 %>% select(c("su", "specCode", "count4m2"))
str(sp.data05)#7489 obs. of  3 variables:

#Change sp data to wide format to compute:
veg.wide05<-spread(sp.data05,key = specCode, value = count4m2, fill = 0)
dim(veg.wide05)#665 120

#Compute Diversity indices:
veg.wide05$DIVshannon<-diversity(veg.wide05[,2:120])
veg.wide05$DIVsimpson<-diversity(veg.wide05[,2:120], index= "simpson")
veg.wide05$Density<-rowSums(veg.wide05[,2:120])

summary(cor(veg.wide05$DIVshannon,veg.wide05$Density))
cor05<-cor.test(veg.wide05$DIVshannon,veg.wide05$Density)
cor05$estimate # 0.7239812 
cor05$p.value # P< 0.001
cor05$statistic # t = 27.02393 


#Pearson's Correalation Test for Spring 2013=========
#Subset native, perennials species data for TST=1.5 (Spring 2013):
data<- read.csv("TopsoilEmergenceData.csv")#our density data

df1.5 <- data %>% filter(TST==1.5 & nat=="native" & longevity=="perennial")
str(df1.5)#7289 obs. of  25 variables:

sp.data1.5<- df1.5 %>% select(c("su", "specCode", "count4m2"))#su = sampling unit
str(sp.data1.5)#7289 obs. of  3 variables:

#Change sp data to wide format to compute:
veg.wide1.5<-spread(sp.data1.5,key = specCode, value = count4m2, fill = 0)
dim(veg.wide1.5)#836  96
head(veg.wide1.5)

#Compute Diversity indices:
veg.wide1.5$DIVshannon<-diversity(veg.wide1.5[,2:96])
veg.wide1.5$DIVsimpson<-diversity(veg.wide1.5[,2:96], index= "simpson")
veg.wide1.5$Density<-rowSums(veg.wide1.5[,2:96])

summary(cor(veg.wide1.5$DIVshannon,veg.wide1.5$Density))
cor1.5<-cor.test(veg.wide1.5$DIVshannon,veg.wide1.5$Density)
cor1.5$estimate # 0.650716
cor1.5$p.value # P< 0.001
cor1.5$statistic # t = 24.74854 

#Correlation Pair Plots Zuur's way:==========
#Here are some functions that we took from the pairs help file and
#modified, or wrote ourselves. To cite these, use the r citation: citation()

panel.cor <- function(x, y, digits=1, prefix="", cex.cor = 6)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(0, 1, 0, 1))
  r1=cor(x,y,use="pairwise.complete.obs")
  r <- abs(cor(x, y,use="pairwise.complete.obs"))
  txt <- format(c(r1, 0.123456789), digits=digits)[1]
  txt <- paste(prefix, txt, sep="")
  if(missing(cex.cor)) { cex <- 0.9/strwidth(txt) } else {
    cex = cex.cor}
  text(0.5, 0.5, txt, cex = cex * r)
}



panel.smooth2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                        cex = 1, col.smooth = "black", span = 2/3, iter = 3, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok))
    lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
          col = 1, ...)
}



panel.lines2=function (x, y, col = par("col"), bg = NA, pch = par("pch"),
                       cex = 1, ...)
{
  points(x, y, pch = pch, col = col, bg = bg, cex = cex)
  ok <- is.finite(x) & is.finite(y)
  if (any(ok)){
    tmp=lm(y[ok]~x[ok])
    abline(tmp)}
}



panel.hist <- function(x, ...)
{
  usr <- par("usr"); on.exit(par(usr))
  par(usr = c(usr[1:2], 0, 1.5) )
  h <- hist(x, plot = FALSE)
  breaks <- h$breaks; nB <- length(breaks)
  y <- h$counts; y <- y/max(y)
  rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}





#Cor Plot in Spring 2012 Season:====
#Computes automaticaly corelations between densities and diversity indices:

pairs(veg.wide05[,c("DIVshannon", "Density","DIVsimpson")],
      lower.panel = panel.cor,)

#Cor Plot in Spring 2013 Season:====

pairs(veg.wide1.5[,c("DIVshannon", "Density","DIVsimpson")],
      lower.panel = panel.cor,
      main="Spring 2012 Correlations. Diversity & Density of Native Perennials:")
png(filename = "NatPErSpring2012.jpg", width = 600, height = 400)
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

#Total Species Richness=======
#Natives only:
#All:
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


#SUBSET PERENNIALS IN YEAR ONE ONLY + compute densities per su:=========
#Aggregate data to compute sums per 1 m2 in year one:
gnative1<-data[data$plot2=="control" & data$TST== 0.5,]#only controls in spring2012=tst05
dim(gnative1)# 11406    24= that many in site-level treatments (coded "Control")
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

dim(a1)# 834 by 9 = CONTAINS sum per plot per 2 longevities (ann & per) in spring 2012
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


#Computing 95% CI-s of emergence densities for year ONE==============
t<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("Transdepth","longevity","year"))
colnames(t)[1]<-"Site.Treatment"
t$Filter<- "DISPERSAL"
t
##Site.Treatment longevity year   N    sum1m2       sd        se        ci    Filter
#1           deep perennial  one 217 10.084101 7.714270 0.5236788 1.0321748 DISPERSAL
#2        shallow perennial  one 216  6.646991 6.133361 0.4173224 0.8225671 DISPERSAL

r<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("rip","longevity","year"))
colnames(r)[1]<-"Site.Treatment"
r$Filter  <- "ABIOTIC"
r
##Site.Treatment longevity year   N    sum1m2       sd        se        ci  Filter
#1         ripped perennial  one 217  4.868664 3.814344 0.2589346 0.5103619 ABIOTIC
#2       unripped perennial  one 216 11.886574 7.996175 0.5440708 1.0723957 ABIOTIC

f<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("fence","longevity","year"))
colnames(f)[1]<-"Site.Treatment"
f$Filter  <- "BIOTIC"
f
##Site.Treatment longevity year   N   sum1m2       sd        se        ci Filter
#1         fenced perennial  one 241 8.564315 7.915374 0.5098740 1.0043996 BIOTIC
#2           open perennial  one 192 8.125000 6.124152 0.4419726 0.8717741 BIOTIC

combo<-summarySE(perennials.year.one, measurevar="sum1m2", groupvars=c("Transdepth","rip","longevity","year"))
combo
######Transdepth  rip longevity year   N    sum1m2       sd        se        ci
#1       deep   ripped perennial  one 109  5.919725 4.311494 0.4129662 0.8185707
#2       deep unripped perennial  one 108 14.287037 8.116256 0.7809871 1.5482159
#3    shallow   ripped perennial  one 108  3.807870 2.889031 0.2779972 0.5510970
#4    shallow unripped perennial  one 108  9.486111 7.139288 0.6869783 1.3618544

#SUBSET PERENNIALS IN YEAR TWO ONLY + compute densities per su:=========
data<- read.csv("TopsoilEmergenceData.csv")
names(data)

ggnative<- data[data$TST==1.5 & data$plot2=="control",]#only records from spring II 

ggnative2<-ggnative[ggnative$nat=="native",]#native that germinated only in year two
length(as.character(levels(droplevels(ggnative2$su))))#585 su-s that is survey plots
dim(ggnative2)#8848   23
levels(droplevels(ggnative3$nat))#"native"

a2<-aggregate(ggnative2$count1m2,
              by = list(su=ggnative2$su,site=ggnative2$site,plot=ggnative2$plot,
                        Transdepth=ggnative2$Transdepth, rip=ggnative2$rip,
                        fence=ggnative2$fence,plot2=ggnative2$plot2,longevity=ggnative2$longevity), FUN = "sum")

a2$year<-"two"
colnames(a2)[9]<-"sum1m2"

a2.perenial<-a2[a2$longevity=="perennial",]# subsetting su-s where perennials were detected
dim(a2.perenial)# = 580 => missing zero plots out of 585 -580 = 5
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
str(perennials.year.two)#585 obs. of  10 variables:

#computing 95% CI-s for year TWO==============
t2<-summarySE(perennials.year.two, measurevar="sum1m2", groupvars=c("Transdepth","longevity","year"))
t2
#Transdepth longevity year   N   sum1m2        sd        se        ci
#1       deep perennial  two 292 7.370719 6.159829 0.3604767 0.7094721
#2    shallow perennial  two 293 5.940273 5.632230 0.3290384 0.6475876
colnames(t2)[1]<-"Site.Treatment"
t2$Filter<- "DISPERSAL"


r2<-summarySE(perennials.year.two, measurevar="sum1m2", groupvars=c("rip","longevity","year"))
r2
#rip longevity year   N    sum1m2        sd        se        ci
#1   ripped perennial  two 292 6.408390 5.582400 0.3266852 0.6429654
#2 unripped perennial  two 293 6.899317 6.275606 0.3666248 0.7215622
colnames(r2)[1]<-"Site.Treatment"
r2$Filter  <- "ABIOTIC"

f2<-summarySE(perennials.year.two, measurevar="sum1m2", groupvars=c("fence","longevity","year"))
f2
#fence longevity year   N    sum1m2        sd        se        ci
#1 fenced perennial  two 297 6.826599 6.034479 0.3501560 0.6891107
#2   open perennial  two 288 6.476562 5.845545 0.3444521 0.6779726
colnames(f2)[1]<-"Site.Treatment"
f2$Filter  <- "BIOTIC"

#Drawing Figure 1 perennials only, both years==============
EmData<-rbind(t,r,f,t2,r2,f2)#merging year two and two SUs
str(EmData)#data.frame':	12 obs. of  9 variables:
range(EmData$sum)#4.854839 11.863426
EmData$Scale <- "Site" #to reduce facetting to one level only on x-axis.
EmData$year2 <- ifelse(EmData$year == "one", "Spring 2012", "Spring 2013") #to make it look better on figure

pd <- position_dodge(.5)
Fig1<-ggplot(EmData, aes(x=Scale, y=sum1m2, shape=year2, color=year2))
Fig2<-Fig1 +geom_errorbar(aes(ymin=sum1m2-ci, ymax=sum1m2+ci),width=.30,position=pd,size=1.4)
Fig3<-Fig2+ geom_point(position=pd,size=6)
Fig3a<-Fig3 + geom_line(position=pd) + scale_colour_manual(values = c("green", "red")) +scale_shape_manual(values=c(15,15))
Fig4<-Fig3a+facet_grid(year2~Filter+Site.Treatment)+theme_bw()
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
Fig6<- Fig5 +  scale_y_continuous("Plant Density (m\u00B2)", limits = c(4,14))
Fig6

#ggsave(Fig6, filename="fig1Perennials3.png", width = 140, height = 200, units = "mm") # way to save it to fit journal and A4 format easily
