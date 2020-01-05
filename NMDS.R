#LOAD LIBRARIES AND DATA:======
library(vegan)
library(MASS)
library(tidyverse)
library(gridExtra)
library(grid)

#WEB:
https://jonlefcheck.net/2012/10/24/nmds-tutorial-in-r/
https://chrischizinski.github.io/rstats/vegan-ggplot2/

#Spring 2012 NMDS =======
veg.nmds <- read.csv ("vegNMDS_Spring2012_250runs.csv")

#NMDS for Fence in Spring 2012:
fence_spr12 <- filter(veg.nmds, Treatment == "fenced" |Treatment == "open")
#Draw a hull:
grp.a <- fence_spr12[fence_spr12$Treatment == "fenced", ][chull(fence_spr12[fence_spr12$Treatment =="fenced", c("MDS1", "MDS2")]), ]
grp.b <- fence_spr12[fence_spr12$Treatment == "open", ][chull(fence_spr12[fence_spr12$Treatment =="open", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

f1 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=fence_spr12,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("fenced" = "red", "open" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
        #axis.text.y = element_blank(), # remove y-axis text
        #axis.ticks = element_blank(),  # remove axis ticks
        #axis.title.x = element_text(size=18), # remove x-axis labels
        axis.title.y = element_text(size=18),
        axis.title.x = element_text(size=18),
        legend.position = "top",
        legend.text = element_text(size=18),
        legend.title  = element_text(size=18),
        panel.background = element_blank(), 
        panel.grid.major = element_blank(),  #remove major-grid labels
        panel.grid.minor = element_blank(),  #remove minor-grid labels
        plot.background = element_blank())  
f1  
#Rip in Spring2012:
rip_spr12 <- filter(veg.nmds, Treatment == "ripped" |Treatment == "unripped")
#Draw a hull:
grp.a <- rip_spr12[rip_spr12$Treatment == "ripped", ][chull(rip_spr12[rip_spr12$Treatment =="ripped", c("MDS1", "MDS2")]), ]
grp.b <- rip_spr12[rip_spr12$Treatment == "unripped", ][chull(rip_spr12[rip_spr12$Treatment =="unripped", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

r1 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=rip_spr12,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("ripped" = "red", "unripped" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

r1
#Transdepth Spring2012:
Transdepth_spr12 <- filter(veg.nmds, Treatment == "deep" |Treatment == "shallow")
#Draw a hull:
grp.a <- Transdepth_spr12[Transdepth_spr12$Treatment == "deep", ][chull(Transdepth_spr12[Transdepth_spr12$Treatment =="deep", c("MDS1", "MDS2")]), ]
grp.b <- Transdepth_spr12[Transdepth_spr12$Treatment == "shallow", ][chull(Transdepth_spr12[Transdepth_spr12$Treatment =="shallow", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

t1 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=Transdepth_spr12,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("deep" = "red", "shallow" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  
t1

grid.arrange(t1,f1,r1, ncol=3) #SAved as "NatWeeds.jpeg" on 30/6/2019


#Spring 2013 NMDS =======
veg.nmds <- read.csv ("vegNMDS_Spring2013_250runs.csv")
#On Fence in Spring 2013:
fence_spr13 <- filter(veg.nmds, Treatment == "fenced" |Treatment == "open")
#Draw a hull:
grp.a <- fence_spr13[fence_spr13$Treatment == "fenced", ][chull(fence_spr13[fence_spr13$Treatment =="fenced", c("MDS1", "MDS2")]), ]
grp.b <- fence_spr13[fence_spr13$Treatment == "open", ][chull(fence_spr13[fence_spr13$Treatment =="open", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

f1 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=fence_spr13,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("fenced" = "red", "open" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  
f1  
#Rip in Spring2013:
rip_spr13 <- filter(veg.nmds, Treatment == "ripped" |Treatment == "unripped")
#Draw a hull:
grp.a <- rip_spr13[rip_spr13$Treatment == "ripped", ][chull(rip_spr13[rip_spr13$Treatment =="ripped", c("MDS1", "MDS2")]), ]
grp.b <- rip_spr13[rip_spr13$Treatment == "unripped", ][chull(rip_spr13[rip_spr13$Treatment =="unripped", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

r1 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=rip_spr13,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("ripped" = "red", "unripped" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

r1
#Transdepth Spring2013:
Transdepth_spr13 <- filter(veg.nmds, Treatment == "deep" |Treatment == "shallow")
#Draw a hull:
grp.a <- Transdepth_spr13[Transdepth_spr13$Treatment == "deep", ][chull(Transdepth_spr13[Transdepth_spr13$Treatment =="deep", c("MDS1", "MDS2")]), ]
grp.b <- Transdepth_spr13[Transdepth_spr13$Treatment == "shallow", ][chull(Transdepth_spr13[Transdepth_spr13$Treatment =="shallow", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

t1 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=Transdepth_spr13,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("deep" = "red", "shallow" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "", shape = "")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  
t1

grid.arrange(t1,f1,r1, ncol=3) #SAved as "NatWeeds.jpeg" on 30/6/2019



#AUTUMN 2014 NEW PAPER======
library(vegan)
library(MASS)
library(tidyverse)

#DATA:
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
dim(BigDF2)#data.frame':  787500 obs. of  39 variables, 791058 obs. of  38 variables with REMNANT DATA
du.ctrl<-big2.du[big2.du$plot2=="CTRL",]

#Sites scale treatments only (fence, rip, Transdepth):
s01<-BigDF2[BigDF2$plot2=="CTRL",] #looking at site-scale treatments only = "CTRL"
dim(s01)#533324     38

#subsetting  only few columns that will be needed for vegan:
s02<-s01[, c( "specCode", "count","su","season", "Transdepth","rip", "fence")]

#Gather  "Transdepth","rip", "fence" into one column "Treatment"
s03 <- gather(s02, key = "Treatment_Type", value = "Treatment", Transdepth:fence)
dim(s03)#1599972       6

s04<-s03[s03$season== "Autumn2014",] #pick only one spring
dim(s04)#377223      6
s04$binary<-ifelse(s04$count > 0, 1,0)#turning count int binary presence/absence data
names(s04)#

a1<- s04[ c("specCode","su","season","Treatment", "binary")]

a1.wide<-tidyr::spread(a1, specCode, binary, fill = TRUE)#producing species matrix...
dim(a1.wide)#1491  256
species<-a1.wide[,4:256]

#Remove Singletons:
occur.cols<-apply(species,2,sum)#sum species occurances in each column
a1.good.matrix<-species[ , !occur.cols <= 1 ] #removing all 0 or 1-sum columns.
dim(a1.good.matrix) #1491   52, singleton columns removed.

veg<-a1.good.matrix #this a good subset of the species only matrix
env<-as.data.frame(a1.wide[,c("Treatment","season")])#it subsets main treatments for env fit matrix
veg.env<-cbind(veg,env)
names(veg.env)

#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#1 12 = no zeros! YAY!
veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.

#Computing MDS:
MDS <- metaMDS(veg.env1[ , 1:52], distance = "bray")#computing distances in veg matrix
MDS$stress #0.00266 Stress type ?

env<- as.data.frame(veg.env1[,53])#Treatment data
colnames(env)[1]<-"Treatment"

coordinates<-as.data.frame(MDS$points[,1:2])#site scores in NMDS
veg.nmds<-cbind(coordinates, env)
write.csv (veg.nmds, file = "vegNMDS_Autumn2014.csv")

#NMDS for Fence in Autumn 2014:
fence_aut14 <- filter(data.scores, Treatment == "fenced" |Treatment == "open")
#Draw a hull:
grp.a <- fence_aut14[fence_aut14$Treatment == "fenced", ][chull(fence_aut14[fence_aut14$Treatment =="fenced", c("NMDS1", "NMDS2")]), ]
grp.b <- fence_aut14[fence_aut14$Treatment == "open", ][chull(fence_aut14[fence_aut14$Treatment =="open", c("NMDS1", "NMDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

#Vegan MDS in GGPLOT New PAPER =======
veg.nmds <- read.csv ("vegNMDS_Autumn2014.csv")

#NMDS for Fence in Spring 2012:
fence_aut14 <- filter(veg.nmds, Treatment == "fenced" |Treatment == "open")
#Draw a hull:
grp.a <- fence_aut14[fence_aut14$Treatment == "fenced", ][chull(fence_aut14[fence_aut14$Treatment =="fenced", c("MDS1", "MDS2")]), ]
grp.b <- fence_aut14[fence_aut14$Treatment == "open", ][chull(fence_aut14[fence_aut14$Treatment =="open", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

f2 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=fence_aut14,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("fenced" = "red", "open" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "Biotic Filter Manipulation:", shape = "Biotic Filter Manipulation:")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  

#Rip in Autumn2014:
rip_aut14 <- filter(veg.nmds, Treatment == "ripped" |Treatment == "unripped")
#Draw a hull:
grp.a <- rip_aut14[rip_aut14$Treatment == "ripped", ][chull(rip_aut14[rip_aut14$Treatment =="ripped", c("MDS1", "MDS2")]), ]
grp.b <- rip_aut14[rip_aut14$Treatment == "unripped", ][chull(rip_aut14[rip_aut14$Treatment =="unripped", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

r2 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=rip_aut14,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("ripped" = "red", "unripped" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "Abiotic Filter Manipulation:", shape = "Abiotic Filter Manipulation:")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  


r2
#Transdepth in Autumn2014:
Transdepth_aut14 <- filter(veg.nmds, Treatment == "deep" |Treatment == "shallow")
#Draw a hull:
grp.a <- Transdepth_aut14[Transdepth_aut14$Treatment == "deep", ][chull(Transdepth_aut14[Transdepth_aut14$Treatment =="deep", c("MDS1", "MDS2")]), ]
grp.b <- Transdepth_aut14[Transdepth_aut14$Treatment == "shallow", ][chull(Transdepth_aut14[Transdepth_aut14$Treatment =="shallow", c("MDS1", "MDS2")]), ]
hull.data <- rbind(grp.a, grp.b)  #combine grp.a and grp.b

t2 <- ggplot() + 
  geom_polygon(data=hull.data,aes(x=MDS1,y=MDS2,fill=Treatment, group=Treatment),alpha=0.30) + # add the convex hulls
  geom_point(data=Transdepth_aut14,aes(x=MDS1,y=MDS2,shape=Treatment,colour=Treatment),size=4)+ # add the point markers
  scale_colour_manual(values=c("deep" = "red", "shallow" = "blue")) +
  labs(x = "NMDS 1",y="NMDS 2", colour = "Abiotic Filter Manipulation:", shape = "Abiotic Filter Manipulation:")+
  guides(fill = FALSE, size = FALSE)+
  #coord_equal() +
  theme_bw() +
  theme(#axis.text.x = element_blank(),  # remove x-axis text
    #axis.text.y = element_blank(), # remove y-axis text
    #axis.ticks = element_blank(),  # remove axis ticks
    #axis.title.x = element_text(size=18), # remove x-axis labels
    axis.title.y = element_text(size=18),
    axis.title.x = element_text(size=18),
    legend.position = "top",
    legend.text = element_text(size=18),
    legend.title  = element_text(size=18),
    panel.background = element_blank(), 
    panel.grid.major = element_blank(),  #remove major-grid labels
    panel.grid.minor = element_blank(),  #remove minor-grid labels
    plot.background = element_blank())  


library(gridExtra)
library(grid)
grid.arrange(t2,f2,r2, ncol=3) #SAved as "NatWeeds.jpeg" on 30/6/2019



#Two MDS points form outliers that cause messy clustering on sp2 plot
#Let us find them and remove them
names(veg.nmds)
veg.nmds2<-veg.nmds[- which.max(veg.nmds$MDS1) , ]
dim(veg.nmds2)#208 3
x<-veg.nmds2[- which.min(veg.nmds2$MDS1), ]

dim(x)# 207 3
#plot veg.nmds3 in GGPLOT:
nmds3plot<-ggplot(x, aes(MDS1, MDS2))+ geom_point(aes(color = Season, shape =Season), size= 4)+theme_bw()
nmds3plot
x3<-nmds3plot+theme(  axis.text.y=element_text(size=18),
                      axis.title.x=element_blank(),
                      axis.text.x=element_text(size=18),
                      axis.title.y=element_blank(),
                      panel.grid.minor.x = element_blank(),
                      legend.position = "bottom",
                      legend.text = element_text(size = 14),
                      legend.title = element_text(size = 14),
                      strip.text.x=element_text(size=28),
                      plot.title = element_text(lineheight=1.2, face="bold",size=26))

x4<- x3 +scale_color_manual(values = c("green", "blue"))+ scale_shape_manual(values=c(17,15))
x4

#Start/End Season NMDS========
#subsetting  only few columns that will be needed for vegan:
a<-du.ctrl[, c( "specCode", "count","su","season", "comb2","plot2")]
a$binary<-ifelse(a$count > 0, 1,0)#turning count int binary presence/absence data
names(a)# "specCode","count","su","season","comb2" ,"plot2", "binary"  
str(a)#data.frame':	133837 obs. of  7 variables:

#NMDS start-and-end season as per Morgan's comment on thesis======
# = Comparing "start" (spr12) and "end" season (aut14):
#We work on deep.unripped (a) only as it was most successful treatment in terms of native densities
levels(a$season)
a1<-a[a$season== "Spring2012" | a$season== "Autumn2014",]
str(a1)#data.frame':	58949 obs. of  7 variables:
a1<- a1[ c("specCode","su","season", "binary")]

a1.wide<-tidyr::spread(a1, specCode, binary, fill = TRUE)#producing species matrix...
dim(a1.wide)#233 255
species<-a1.wide[,3:255]

#Remove Singletons:
occur.cols<-apply(species,2,sum)#sum species occurances in each column
a1.good.matrix<-species[ , !occur.cols <= 1 ] #removing all 0 or 1-sum columns.
dim(a1.good.matrix) #233 124, singleton columns removed.

veg<-a1.good.matrix #this a good subset of the species only matrix
env<-as.data.frame(a1.wide[,"season"])#it subsets main treatments for env fit matrix
colnames(env1)[1] <- "season"
veg.env<-cbind(veg,env)
names(veg.env)

#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#0 48
str(veg.env)#233 obs. of  126 variables:

veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.
dim(veg.env1)#209 obs. of  126 variables:

#Computing MDS:
MDS1 <- metaMDS(veg.env1[ , 1:124], distance = "bray")#computing distances
# Stress:   
MDS1$stress * 100 # = 6.88% , Stress type 1, weak ties.

plot(MDS1$points[,1:2])
env1<- as.data.frame(veg.env1[,125])#env data
colnames(env1)[1]<-"Season"
veg1<- veg.env1[ , 1:124]

coordinates<-as.data.frame(MDS1$points[,1:2])
veg.nmds<-cbind(coordinates, env1)
dim(veg.nmds)#209 3
veg.nmds$Season<- factor(veg.nmds$Season, levels = c("Spring2012", "Autumn2014"))
#Vegan MDS in GGPLOT:
sp1<-ggplot(data = veg.nmds, aes(MDS1, MDS2)) + geom_point(aes(color = Season), size= 4)
sp2<-sp1+theme_bw()+ggtitle("NMDS for start & end season")
sp2

#Two MDS points form outliers that cause messy clustering on sp2 plot
#Let us find them and remove them
names(veg.nmds)
veg.nmds2<-veg.nmds[- which.max(veg.nmds$MDS1) , ]
dim(veg.nmds2)#208 3
x<-veg.nmds2[- which.min(veg.nmds2$MDS1), ]

dim(x)# 207 3
#plot veg.nmds3 in GGPLOT:
nmds3plot<-ggplot(x, aes(MDS1, MDS2))+ geom_point(aes(color = Season, shape =Season), size= 4)+theme_bw()
nmds3plot
x3<-nmds3plot+theme(  axis.text.y=element_text(size=18),
                        axis.title.x=element_blank(),
                        axis.text.x=element_text(size=18),
                        axis.title.y=element_blank(),
                        panel.grid.minor.x = element_blank(),
                        legend.position = "bottom",
                        legend.text = element_text(size = 14),
                        legend.title = element_text(size = 14),
                        strip.text.x=element_text(size=28),
                        plot.title = element_text(lineheight=1.2, face="bold",size=26))

x4<- x3 +scale_color_manual(values = c("green", "blue"))+ scale_shape_manual(values=c(17,15))
x4

#on Producing eclipse:
#https://stackoverflow.com/questions/13794419/plotting-ordiellipse-function-from-vegan-package-onto-nmds-plot-created-in-ggplo

#ANOSIM as per Morgan's request===============

veg<-a1.good.matrix #this a good subset of the species only matrix (see above how we got here)
env<-as.data.frame(a1.wide[,"season"])#it subsets main treatments for env fit matrix
colnames(env)[1] <- "season"
veg.env<-cbind(veg,env)
names(veg.env)
#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#0 48
str(veg.env)#233 obs. of  126 variables:

veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.
str(veg.env1)#209 obs. of  126 variables:
veg.ANOSIM <- veg.env1[, -c(125,126)]
env.ANOSIM <- veg.env1[, c(125,126)] #column n 125 = season

topsoil.dist <- vegdist(veg.ANOSIM, binary = TRUE)
attach(env.ANOSIM)
topsoil.ANOSIM <-  anosim(topsoil.dist, season)
summary(topsoil.ANOSIM)
#OUTPUT:
Call:  anosim(dat = topsoil.dist, grouping = season), Dissimilarity: binary bray 
#ANOSIM statistic R: 0.4565 
#Significance: 0.001 
#Number of permutations: 999


#Joining Remnant/Topsoil DATA=======
#Remnant Banksia Woodland data:
REMNspecies <- read.csv("REMNspecies.csv")
names(REMNspecies)
#changing from wide to long format to fit big2trait format:
# The arguments to gather():
# - data: Data object
# - key: Name of new key column (made from names of data columns)
# - value: Name of new value column
# - ...: Names of source columns that contain values
b <- gather(REMNspecies,specCode,count, acacpulc:xantprei)
str(b)#2424 obs. of  6 variables:
b$binary<-ifelse(b$count > 0, 1,0)#new column with 0,1 values only
range(b$binary)# 0 1
names(b)
ab<-rbind(a,b)#joining remnant data with my topsoil data
ab.matrix<-tidyr::spread(ab, specCode, binary)#producing species matrix...
head(ab.matrix)
ab.matrix[is.na(ab.matrix)] <- 0 #replaces NA=s with zero
range(ab.matrix$count)#[1]   0 272
dim(ab.matrix)#4844  287

#VEGAN:
veg<-ab.matrix[,c(6:287)] #this subsets the species only matrix
env<-ab.matrix[,c("comb2","plot2","season")]#it subsets main treatments for env fit matrix
#removing zero rows
veg$RowSum<-rowSums(veg)
range(veg$RowSum)#0 27
str(veg)#4844 obs. of  283 variables:
veg1<-subset(veg, veg$RowSum != 0)#'data.frame':  4291 obs. of  283 variables: M
#MDS runs slowely with veg1 with too many low rows. Worth looking at: 
#veg2<-subset(veg, veg$RowSum > 2)#

#NMDS ALL seasons==========
MDS <- metaMDS(veg1[1:282])#computing distances
plot(MDS$points[,1:2])
cut.env<-rownames(veg1)#subsettng variables that fit veg1 for color-coding the plot
ab.env<-ab.matrix[cut.env,]#subsetting only the rows computed in MDS
dim(ab.env)#
ab.env.matrix<-ab.env[,c(1:5)]
names(ab.env.matrix)

coordinates<-as.data.frame(MDS$points[,1:2])
veg.nmds<-cbind(veg1,coordinates, season=ab.env$season)
names(veg.nmds)
veg.nmds$stage<-veg.nmds$season
#Seperating season in two:
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="aut11"] <- "Ref.Aut2011"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="spr11"] <- "Ref.Spr2011"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Spring2012"] <- "Off.Spr2012"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Autumn2013"] <- "Off.Aut2013"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Spring2013"] <- "Off.Spr2013"
levels(veg.nmds$stage)[levels(veg.nmds$stage)=="Autumn2014"] <- "Off.Aut2014"

#Vegan MDS in GGPLOT:
#all
sp1<-ggplot(data = veg.nmds, aes(MDS1, MDS2)) + geom_point(aes(color = stage), size= 4)
sp2<-sp1+theme_bw()+ggtitle("NMDS for all seasons")
sp2



#NMDS Autumns=============== 
veg.aut<-ab.matrix[ab.matrix$season=="Autumn2013"|ab.matrix$season=="Autumn2014"|ab.matrix$season=="aut11",]
dim(veg.aut)#1296  287
veg.aut$RowSum <- rowSums(veg.aut[ ,c(6:287)])
veg1.aut<-subset(veg.aut, veg.aut$RowSum !=0)#that is minimum 1 in a row
dim(veg1.aut)#1011 288
range(veg1.aut$RowSum)#1 to 16

#Computing MDS:
veg1.aut.species<- veg1.aut[ , -c(1:5, 288)] #subset veg matrix only
MDS1.aut <- metaMDS(veg1.aut.species)#computing distances in veg matrix only
MDS1.aut$stress * 100 # =  0.4264156%

cut.env1.aut<-rownames(veg1.aut)#producing extra factors that fit veg1 for color-coding the plot
ab.env1.aut<-ab.matrix[cut.env1.aut,]#subsetting only the rows computed in FD
dim(ab.env1.aut)#1011 287
ab.env.matrix4<-ab.env1.aut[,c(1:5)]
names(ab.env.matrix4)#"count"  "su"     "season" "comb2"  "plot2" 

coordinates.veg1.aut<-as.data.frame(MDS1.aut$points[,1:2])
nmds.veg1.aut<-cbind(coordinates.veg1.aut, season=ab.env1.aut$season)
names(nmds.veg1.aut)#"MDS1"   "MDS2"   "season"
nmds.veg1.aut$Stage<-nmds.veg1.aut$season

#Renaming seasons into:
levels(nmds.veg1.aut$Stage)[levels(nmds.veg1.aut$Stage)=="aut11"] <- "Ref.Aut2011"
levels(nmds.veg1.aut$Stage)[levels(nmds.veg1.aut$Stage)=="Autumn2013"] <- "Off.Aut2013"
levels(nmds.veg1.aut$Stage)[levels(nmds.veg1.aut$Stage)=="Autumn2014"] <- "Off.Aut2014"
table(nmds.veg1.aut$Stage)#empty factor levels shown.
nmds.veg1.aut$Stage<-factor(nmds.veg1.aut$Stage, levels = c("Ref.Aut2011","Off.Aut2013","Off.Aut2014"))
#write.csv(nmds.veg1.aut, file = "nmds.veg1.aut.csv")#it takes too much time to run MDS so its output is saved here

#GGPLOT NMDS Autumns =============== 
nmds.veg1.aut<-read.csv("nmds.veg1.aut.csv")
nmds.plot1<-ggplot(data = nmds.veg1.aut, aes(MDS1, MDS2)) + geom_point(aes(color = Stage))
nmds.plot2<-sp1+theme_bw()+ggtitle("NMDS in three Autumn seasons")
nmds.plot2
#1 outliers to be removed:
min(nmds.veg1.aut$MDS1)#[1] -141.5723
v1<-nmds.veg1.aut[-which.min(nmds.veg1.aut$MDS1),]
dim(v1)#1010    5
v1$Stage <- factor(v1$Stage, levels = c("Ref.Aut2011", "Off.Aut2013", "Off.Aut2014"))
sp1<-ggplot(v1, aes(MDS1, MDS2)) + geom_point(aes(color = Stage, shape=Stage), size= 6)
sp2<-sp1+theme_classic()
sp2
sp3 <- sp2 + theme(axis.text.y=element_text(size=18),
                  axis.title.x=element_blank(),
                  axis.text.x=element_text(size=18),
                  axis.title.y=element_blank(),
                  panel.grid.minor.x = element_blank(),
                  legend.position = "bottom",
                  legend.text = element_text(size = 14),
                  legend.title = element_text(size = 14,face="bold"),
                  strip.text.x=element_text(size=28),
                  plot.title = element_text(lineheight=1.2, face="bold",size=26))

sp3
#ggsave(filename="nmdsveg1b.jpeg", dpi=600, width=140, height=140, unit= "mm") 
#above is used in Trait Chapter as fig 3:

#ANOSIM Autumns=============
levels(ab.matrix$season)#ignore remnant = 0 rows.
aut<-ab.matrix[ab.matrix$season== "aut11"|ab.matrix$season== "Autumn2013" | ab.matrix$season== "Autumn2014",]
species.aut<-aut[,6:287]

env.aut<-as.data.frame(aut[,"season"])#it subsets main treatments for env fit matrix
colnames(env.aut)[1] <- "season"
veg.env.aut<-cbind(species.aut,env.aut)
names(veg.env.aut)
#removing zero rows
veg.env.aut$RowSum<-rowSums(veg.env.aut[,c(1:182)])
range(veg.env$RowSum)#0 48
str(veg.env.aut)#1296 obs. of  284 variables:

veg.env1.aut<-subset(veg.env.aut, veg.env.aut$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.
str(veg.env1.aut)#805 obs. of  284 variables:
veg.ANOSIM.aut <- veg.env1.aut[, -c(283,284)]
env.ANOSIM.aut<- veg.env1.aut[, c(283,284)]) #
env.ANOSIM.aut$season<-factor(env.ANOSIM.aut$season)

topsoil.dist.aut <- vegdist(veg.ANOSIM.aut, binary = TRUE)
attach(env.ANOSIM.aut)
table(env.ANOSIM.aut$season)
topsoil.ANOSIM.aut <-  anosim(topsoil.dist.aut, season)
plot(topsoil.ANOSIM.aut)
summary(topsoil.ANOSIM.aut)
#OUTPUT:anosim(dat = topsoil.dist.aut, grouping = season)
#ANOSIM statistic R: 0.04431 
#Significance: 0.001 

#NMDS Springs================
veg.spr<-ab.matrix[ab.matrix$season=="Spring2012"|ab.matrix$season=="Spring2013"|ab.matrix$season=="spr11",]
dim(veg.spr)#3548  287 - more than aut which is expected as more seedlings stay alive in spring
veg.spr$RowSum <- rowSums(veg.spr[ ,c(6:287)])
veg1.spr<-subset(veg.spr, veg.spr$RowSum !=0)#that is minimum 1 in a row. Vegdist does not take 0 rows
dim(veg1.spr)#3280  288
range(veg1.spr$RowSum)#1 to 27

#Computing MDS:
veg1.spr.species<- veg1.spr[ , -c(1:5, 288)] #subset veg matrix only
MDS1.spr <- metaMDS(veg1.spr.species)#computing distances in veg matrix only
MDS1.spr$stress * 100 # =  %

cut.env1.spr<-rownames(veg1.spr)#producing extra factors that fit veg1 for color-coding the plot
ab.env1.spr<-ab.matrix[cut.env1.spr,]#subsetting only the rows computed in FD
dim(ab.env1.spr)#1011 287
ab.env.matrix4<-ab.env1.spr[,c(1:5)]
names(ab.env.matrix4)#"count"  "su"     "season" "comb2"  "plot2" 

coordinates.veg1.spr<-as.data.frame(MDS1.spr$points[,1:2])
nmds.veg1.spr<-cbind(coordinates.veg1.spr, season=ab.env1.spr$season)
names(nmds.veg1.spr)#"MDS1"   "MDS2"   "season"
nmds.veg1.spr$Stage<-factor(nmds.veg1.spr$season)
table(nmds.veg1.spr$Stage)
#Spring2012 Spring2013      spr11 
#1444       1710        126 
#Renaming seasons into:
levels(nmds.veg1.spr$Stage)[levels(nmds.veg1.spr$Stage)=="spr11"] <- "Ref.spr2011"
levels(nmds.veg1.spr$Stage)[levels(nmds.veg1.spr$Stage)=="Spring2012"] <- "Off.spr2012"
levels(nmds.veg1.spr$Stage)[levels(nmds.veg1.spr$Stage)=="Spring2013"] <- "Off.spr2013"
table(nmds.veg1.spr$Stage)#empty factor levels shown.
nmds.veg1.spr$Stage<-factor(nmds.veg1.spr$Stage, levels = c("Ref.spr2011","Off.spr2012","Off.spr2013"))
#write.csv(nmds.veg1.spr, file = "nmds.veg1.spr.csv")#it takes too much time to run MDS so its output is saved here

#GGPLOT NMDS Springs==========
spspr1<-ggplot(data = nmds.veg1.spr, aes(MDS1, MDS2)) + geom_point(aes(color = Stage), size= 4)
spspr2<-spspr1+theme_bw()+ggtitle("NMDS in three Spring seasons")
spspr2#vivid outliers prsent to be removed:


#nmds.veg1.spr <- read.csv("nmds.veg1.spr.csv")# Ouput of ~6h of MDS1.spr <- metaMDS(veg1.spr.species)#computing distances in veg matrix only

veg.spr.No_Outlier<-nmds.veg1.spr[- which.min(nmds.veg1.spr$MDS1), ]

spspr1<-ggplot(veg.spr.No_Outlier, aes(MDS1, MDS2)) + geom_point(aes(color = Stage, shape=Stage), size= 4)
spspr2<-spspr1+theme_bw()     #+ggtitle("NMDS in three Spring seasons")
spspr3 <- spspr2 + theme(  axis.text.y=element_text(size=18),
                     axis.title.x=element_blank(),
                     axis.text.x=element_text(size=18),
                     axis.title.y=element_blank(),
                     panel.grid.minor.x = element_blank(),
                     legend.position = "bottom",
                     legend.text = element_text(size = 14),
                     legend.title = element_text(size = 14,face="bold"),
                     strip.text.x=element_text(size=28),
                     plot.title = element_text(lineheight=1.2, face="bold",size=26))

spspr3
#ggsave(filename="nmdsVeg4SpringB.jpeg", dpi=600, width=140, height=140, unit= "mm") 

#ANOSIM Springs=========
veg.spr<-ab.matrix[ab.matrix$season=="Spring2012"|ab.matrix$season=="Spring2013"|ab.matrix$season=="spr11",]
dim(veg.spr)#3548  287
species.spr<-veg.spr[,6:287]

env.spr<-as.data.frame(veg.spr[,"season"])#it subsets main treatments for env fit matrix
colnames(env.spr)[1] <- "season"
veg.env.spr<-cbind(species.spr,env.spr)

#removing zero rows from veg.env.spr
veg.env.spr$RowSum<-rowSums(veg.env.spr[,c(1:182)])
range(veg.env$RowSum)#0 48
str(veg.env.spr)#3548 obs. of  284 variables::

veg.env1.spr<-subset(veg.env.spr, veg.env.spr$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.
str(veg.env1.spr)#'data.frame':	2690 obs. of  284 variables:
veg.ANOSIM.spr <- veg.env1.spr[, -c(283,284)]
env.ANOSIM.spr<- veg.env1.spr[, c(283,284)] #
env.ANOSIM.spr$season<-factor(env.ANOSIM.spr$season)

topsoil.dist.spr <- vegdist(veg.ANOSIM.spr, binary = TRUE)#dissimilariy distances
attach(env.ANOSIM.spr)#we show R where to look for season
table(env.ANOSIM.spr$season)#quick look at the n of obs per season
#Spring2012 Spring2013      spr11 
#1239       1348            103 

topsoil.ANOSIM.spr <-  anosim(topsoil.dist.spr, season)
summary(topsoil.ANOSIM.spr)
#OUTPUT:anosim(dat = topsoil.dist.spr, grouping = season)
#ANOSIM statistic R: 0.01014 
#Significance: 0.001 

#NMDS DATA Spring 2012 binary:=========
:
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
dim(BigDF2)#data.frame':  787500 obs. of  39 variables, 791058 obs. of  38 variables with REMNANT DATA

#Sites scale treatments only (fence, rip, Transdepth):
s01<-BigDF2[BigDF2$plot2=="CTRL",] #looking at site-scale treatments only = "CTRL"
dim(s01)#533324     38

#subsetting  only few columns that will be needed for vegan:
s02<-s01[, c( "specCode", "count","su","season", "Transdepth","rip", "fence")]

#Gather  "Transdepth","rip", "fence" into one column "Treatment"
s03 <- gather(s02, key = "Treatment_Type", value = "Treatment", Transdepth:fence)
dim(s03)#1599972       6 

s04<-s03[s03$season== "Spring2012",] #pick only one spring
s04$binary<-ifelse(s04$count > 0, 1,0)#turning count int binary presence/absence data

a1<- s04[ c("specCode","su","season","Treatment", "binary")]

a1.wide<-tidyr::spread(a1, specCode, binary, fill = TRUE)#producing species matrix...
dim(a1.wide)#3063  256
species<-a1.wide[,4:256]

#Remove Singletons:
occur.cols<-apply(species,2,sum)#sum species occurances in each column
a1.good.matrix<-species[ , !occur.cols <= 1 ] #removing all 0 or 1-sum columns.
dim(a1.good.matrix) #3063  216, singleton columns removed.

veg<-a1.good.matrix #this a good subset of the species only matrix
env<-as.data.frame(a1.wide[,c("Treatment","season")])#it subsets main treatments for env fit matrix
veg.env<-cbind(veg,env)
names(veg.env)

#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#1 48 = no zeros! YAY!
#veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.

#Computing MDS:
MDS <- metaMDS(veg.env[ , 1:187], distance = "bray", try = 250)#computing distances in veg matrix
MDS$stress # Stress = 0.2310054

plot(MDS$points[,1:2])
env<- as.data.frame(veg.env[,188])#Treatment data
colnames(env)[1]<-"Treatment"
coordinates<-as.data.frame(MDS$points[,1:2])#site scores or:
veg.nmds<-cbind(coordinates, env)
#write.csv (veg.nmds, file = "vegNMDS_Spring2012_250runs.csv")


data.scores <- as.data.frame(scores(MDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Treatment <- env$Treatment  #  add the grp variable created earlier
head(data.scores)  #look at the data


species.scores <- as.data.frame(scores(MDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  


#NMDA DATA Spring2013 on binary:====
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
dim(BigDF2)#data.frame':  787500 obs. of  39 variables, 791058 obs. of  38 variables with REMNANT DATA
#Sites scale treatments only (fence, rip, Transdepth):
s01<-BigDF2[BigDF2$plot2=="CTRL",] #looking at site-scale treatments only = "CTRL"

#subsetting  only few columns that will be needed for vegan:
s02<-s01[, c( "specCode", "count","su","season", "Transdepth","rip", "fence")]

#Gather  "Transdepth","rip", "fence" into one column "Treatment"
s03 <- gather(s02, key = "Treatment_Type", value = "Treatment", Transdepth:fence)

s04<-s03[s03$season== "Spring2013",] #pick only one spring
s04$binary<-ifelse(s04$count > 0, 1, 0)#turning count int binary presence/absence data

a1<- s04[ c("specCode","su","season","Treatment", "binary")]

a1.wide<-tidyr::spread(a1, specCode, binary, fill = TRUE)#producing species matrix...
dim(a1.wide)#1764  256
species<-a1.wide[,4:256]

#Remove Singletons:
occur.cols<-apply(species,2,sum)#sum species occurances in each column
a1.good.matrix<-species[ , !occur.cols <= 1 ] #removing all 0 or 1-sum columns.
dim(a1.good.matrix) #1764  171singleton columns removed.

veg<-a1.good.matrix #this a good subset of the species only matrix
env<-as.data.frame(a1.wide[,c("Treatment","season")])#it subsets main treatments for env fit matrix
veg.env<-cbind(veg,env)
names(veg.env)

#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#1 49 = no zeros! YAY!
#veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.

#Computing MDS:
MDS <- metaMDS(veg.env[ , 1:171], distance = "bray", try = 250)#computing distances in veg matrix
MDS$stress # Stress = 0.2535987

plot(MDS$points[,1:2])
env<- as.data.frame(veg.env[,172])#Treatment data
colnames(env)[1]<-"Treatment"
coordinates<-as.data.frame(MDS$points[,1:2])#site scores or:
veg.nmds<-cbind(coordinates, env)
#write.csv (veg.nmds, file = "vegNMDS_Spring2013_250runs.csv")


data.scores <- as.data.frame(scores(MDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Treatment <- env$Treatment  #  add the grp variable created earlier
head(data.scores)  #look at the data


species.scores <- as.data.frame(scores(MDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  


#NMDS DATA Spring 2012 on counts:========
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
dim(BigDF2)#data.frame':  787500 obs. of  39 variables, 791058 obs. of  38 variables with REMNANT DATA

#Sites scale treatments only (fence, rip, Transdepth):
s01<-BigDF2[BigDF2$plot2=="CTRL",] #looking at site-scale treatments only = "CTRL"
dim(s01)#533324     38

#subsetting  only few columns that will be needed for vegan:
s02<-s01[, c( "specCode", "count","su","season", "Transdepth","rip", "fence")]

#Gather  "Transdepth","rip", "fence" into one column "Treatment"
s03 <- gather(s02, key = "Treatment_Type", value = "Treatment", Transdepth:fence)
dim(s03)#1599972       6 

s04<-s03[s03$season== "Spring2012",] #pick only one spring

a1<- s04[ c("specCode","su","season","Treatment", "count")]

a1.wide<-tidyr::spread(a1, specCode, count, fill = TRUE)#producing species matrix...
dim(a1.wide)#[1] 1299  256
species<-a1.wide[,4:256]

#Remove Singletons:
occur.cols<-apply(species,2,sum)#sum species occurances in each column
a1.good.matrix<-species[ , !occur.cols <= 1 ] #removing all 0 or 1-sum columns.
dim(a1.good.matrix) #3063  216, singleton columns removed.

veg<-a1.good.matrix #this a good subset of the species only matrix
env<-as.data.frame(a1.wide[,c("Treatment","season")])#it subsets main treatments for env fit matrix
veg.env<-cbind(veg,env)
names(veg.env)

#removing zero rows
veg.env$RowSum<-rowSums(veg)
range(veg.env$RowSum)#1.25 375.00
#veg.env1<-subset(veg.env, veg.env$RowSum != 0)#removing zero rows as metaMDS does not run on zero rows.

#Computing MDS:
MDS <- metaMDS(veg.env[ , 1:163], distance = "bray", try = 250)#computing distances in veg matrix
MDS$stress # Stress =  0.2146225

plot(MDS$points[,1:2])
env<- as.data.frame(veg.env[,164])#Treatment data
colnames(env)[1]<-"Treatment"
coordinates<-as.data.frame(MDS$points[,1:2])#site scores or:
veg.nmds.counts.spr12<-as.data.frame(cbind(coordinates, env))
#write.csv (veg.nmds.counts.spr12, file = "vegNMDS_Spring2012_OnCounts250runs.csv", row.names = F)


data.scores <- as.data.frame(scores(MDS))  #Using the scores function from vegan to extract the site scores and convert to a data.frame
data.scores$site <- rownames(data.scores)  # create a column of site names, from the rownames of data.scores
data.scores$Treatment <- env$Treatment  #  add the grp variable created earlier
head(data.scores)  #look at the data


species.scores <- as.data.frame(scores(MDS, "species"))  #Using the scores function from vegan to extract the species scores and convert to a data.frame
species.scores$species <- rownames(species.scores)  # create a column of species, from the rownames of species.scores
head(species.scores)  

