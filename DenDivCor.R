#LOAD DATA:
#Correlation between density and Richness indices:
library(tidyverse)
library(vegan)
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
cor1.5$estimate # 0.6507165 
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
      lower.panel = panel.cor,)

#COOLer way to plot correlations:========
install.packages("GGally")
library(GGally)
#All Sites:
pm <- ggpairs(veg.wide05[,c("DIVshannon","DIVsimpson", "Density")],
              upper=list(params=list(size=12)),
              lower=list(combo="points", params=c(colour="blue")),
              title= "Correlations between the Diversity indices & Densities")
pm

cool_theme<-  theme(legend.position = "none", 
                    plot.title = element_text(lineheight=1.2, face="bold",size=36),
                    panel.grid.major = element_blank(), 
                    axis.ticks = element_blank(), 
                    panel.border = element_rect(linetype = "dashed", colour = "black", fill = NA))

pm2<- pm + cool_theme 
pm2 
#ggsave(filename="DivCorr1.jpeg", dpi=600) 

#Species Richness=======
sp<- data[data$nat == "native",]
levels(droplevels(sp$specCode))
length(levels(droplevels(sp$specCode))) #165 species!
