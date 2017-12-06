#Rainfall DATA, Run First:#####
#Libraries of functions need to be loaded:
if (!require(tidyverse)) library(tidyverse)
if (!require(gridExtra)) library(gridExtra)

#Data can be loaded off internet:
rainfall<- read.csv(url("https://raw.githubusercontent.com/PWaryszak/Topsoil_Emergence/master/Rainfall/rainfall2.csv"))
#"rainfall2.csv" was compiled of BOM data from: http://www.bom.gov.au/climate/data/stations/
str(rainfall)#24 obs. of  16 variables:

#Manipulate data to long format = Turn wide format of rain to long format:
rain<- rainfall [ , 2:15]#subset variables of interest
head(rain)#check it out

rain.long <- gather(rain, key = Month, value = Rainfall, Jan:Dec, factor_key=TRUE)
head(rain.long)#check it out
str(rain.long)#288 obs. of  4 variables:


#Plot Rain in 2012 +CumSum====
rain2012<-rain.long[rain.long$Year==2012,]
rain2012$CumSum<-cumsum(rain2012$Rainfall)

Cum2012<- summarise(group_by(rain2012,Year,Month), MeanRain = mean(Rainfall)) %>% 
  transform(CumSum=cumsum(MeanRain)) %>%
  as.data.frame
Cum2012# Output below:
####Year Month MeanRain CumSum
1  2012   Jan     33.2   33.2
2  2012   Feb     18.6   51.8
3  2012   Mar      0.1   51.9
4  2012   Apr     65.8  117.7
5  2012   May     77.0  194.7
6  2012   Jun    162.4  357.1
7  2012   Jul     36.8  393.9
8  2012   Aug    112.6  506.5
9  2012   Sep    110.8  617.3
10 2012   Oct     17.5  634.8
11 2012   Nov     61.4  696.2
12 2012   Dec     50.9  747.1

747.1/5 #149.42

#Plot Rainfall in 2012: (More Info on web: https://rpubs.com/MarkusLoew/226759)
Rain2012.Plot<-ggplot(Cum2012, aes(x=Month, y=MeanRain))
Rain2012.Plot1<-Rain2012.Plot+ geom_bar(position=pd, stat="identity", fill = "blue")+
  ggtitle("Rainfall (mm) at restoration sites in 2012")+
  ylab("Monthly Rainfall (mm)")+ theme_light()+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15,color = "blue"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=17,color = "blue"),
        legend.position = "none",
        strip.text = element_text(size=15),
        plot.title = element_text(lineheight=1.2, face="bold",size=20, hjust = 0.5))

Rain2012.Plot1
Rain2012.Plot2 <-Rain2012.Plot1 + geom_line(aes(y = CumSum/5))
Rain2012.Plot3 <- Rain2012.Plot2  +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Cumulative Rainfall (mm)"))+
  geom_point(aes(y = CumSum/5, x= Month, size = 2,colour = "red")) +
  geom_line(aes(x= as.numeric(Month),y= CumSum/5, colour="red"))
 

Plot2012<-Rain2012.Plot3 + theme(axis.text.y.right =  element_text(color = "red"),
                       axis.title.y.right =  element_text(color = "red"))
Plot2012


#Plot Rain in 2013 +CumSum====
rain2013<-rain.long[rain.long$Year==2013,]
rain2013$CumSum<-cumsum(rain2013$Rainfall)

Cum2013<- summarise(group_by(rain2013,Year,Month), MeanRain = mean(Rainfall)) %>% 
  transform(CumSum=cumsum(MeanRain)) %>%
  as.data.frame
Cum2013# Output below:
####Year Month MeanRain CumSum
1  2013   Jan     15.0   15.0
2  2013   Feb      3.5   18.5
3  2013   Mar     74.3   92.8
4  2013   Apr     21.5  114.3
5  2013   May    154.3  268.6
6  2013   Jun     42.4  311.0
7  2013   Jul    154.5  465.5
8  2013   Aug    168.9  634.4
9  2013   Sep    186.2  820.6
10 2013   Oct     39.4  860.0
11 2013   Nov     13.8  873.8
12 2013   Dec      3.7  877.5

#Plot Rainfall in 2012:
Rain2013.Plot<-ggplot(Cum2013, aes(x=Month, y=MeanRain))
Rain2013.Plot1<-Rain2013.Plot+ geom_bar(position=pd, stat="identity", fill = "blue")+
  ggtitle("Rainfall (mm) at restoration sites in 2013")+
  ylab("Monthly Rainfall (mm)")+ theme_light()+
  theme(axis.text.x=element_text(size=15),
        axis.text.y=element_text(size=15,color = "blue"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=17,color = "blue"),
        legend.position = "none",
        strip.text = element_text(size=15),
        plot.title = element_text(lineheight=1.2, face="bold",size=20, hjust = 0.5))

Rain2013.Plot1
Rain2013.Plot2 <-Rain2013.Plot1 + geom_line(aes(y = CumSum/5))
Rain2013.Plot3 <- Rain2013.Plot2  +
  scale_y_continuous(sec.axis = sec_axis(~.*5, name = "Cumulative Rainfall (mm)",breaks = c(0,200,400,600,800)))+
  geom_point(aes(y = CumSum/5, x= Month, size = 2,colour = "red")) +
  geom_line(aes(x= as.numeric(Month),y= CumSum/5, colour="red"))


Plot2013<-Rain2013.Plot3 + theme(axis.text.y.right =  element_text(color = "red"),
                                 axis.title.y.right =  element_text(color = "red"))
Plot2013


#MERGE Plot2012 with Plot2012:
c <- grid.arrange(Plot2012,Plot2013)
c
#ggsave( c, filename = "CumulativeRainfall3.png", width = 12, height = 8)

#Plot Rainfall, No CumSum:=====
rain2012_2013<- rain.long [  rain.long$Year== 2012 | rain.long$Year== 2013, ]
rain2012_2013$Precipitation <- "Precipitation"
rain2012_2013$Year<-as.factor(rain2012_2013$Year)

pd<-position_dodge(0.9)
f<-ggplot(rain2012_2013, aes(x=Precipitation, y=Rainfall, fill = Year))
f1<-f + geom_bar(position=pd, stat="identity")+facet_grid(Year~Month)+
  ggtitle("Monthly rainfall (mm) at restoration sites (2012-2013)")+
  ylab("Rainfall (mm)")+ theme_light()+
  theme(axis.text.x=element_blank(),
        axis.text.y=element_text(size=15),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=17),
        legend.position = "none",
        strip.text = element_text(size=15),
        plot.title = element_text(lineheight=1.2, face="bold",size=20, hjust = 0.5))


f1
#USe ggsave if you want to save as image file.
#ggsave(filename = "RainfallRestorationSites.png", width = 8, height = 4)


#Summarize Rainfal by Month by Year in 2012 & 2013====
Av <- summarise( group_by(rain2012_2013,Year,Month), Mean_Rainfall = mean(Rainfall)) %>% 
  as.data.frame
Av

####Year Month Mean_Rainfall
1  2012   Jan        33.2
2  2012   Feb        18.6
3  2012   Mar         0.1
4  2012   Apr        65.8
5  2012   May        77.0
6  2012   Jun       162.4
7  2012   Jul        36.8
8  2012   Aug       112.6
9  2012   Sep       110.8
10 2012   Oct        17.5
11 2012   Nov        61.4
12 2012   Dec        50.9
13 2013   Jan        15.0
14 2013   Feb         3.5
15 2013   Mar        74.3
16 2013   Apr        21.5
17 2013   May       154.3
18 2013   Jun        42.4
19 2013   Jul       154.5
20 2013   Aug       168.9
21 2013   Sep       186.2
22 2013   Oct        39.4
23 2013   Nov        13.8
24 2013   Dec         3.7

