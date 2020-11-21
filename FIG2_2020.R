#LOAD R-libraries and show your session info:==========
#This code checks if required packages are loaded on your computer. 
#And loads them accordingly. Package Versions noted as well:

if (!require('tidyverse')) install.packages('tidyverse'); library('tidyverse')#for data manipulation and plotting
#tidyverse_1.3.0, ggplot2_3.3.2, dplyr_1.0.2

if (!require('vegan')) install.packages('vegan'); library('vegan') #For species indices
#vegan_2.5-6 

if (!require('egg')) install.packages('egg'); library('egg')##for tagging facets in ggplot
#egg_0.4.5

sessionInfo()
#R version 4.0.3 (2020-10-10)
#Platform: x86_64-w64-mingw32/x64 (64-bit)
#Running under: Windows 10 x64 (build 18363)



#LOAD DATA:
#Use the big data that is freely available at Mendeley repository here 
#(download data first to your directory):

f <-read.csv("big2trait.csv")# %>% #load data from your directory

#Load only autumn season and spread to wide format to compute  densities.
fi <- f %>%
  filter(season == "Spring2012" | season == "Spring2013") %>%
  filter (nat=="native" & plot2 =="CTRL") %>%  #Pick only site-level treatments = CTRL
  select(su, specCode, count, plot2, Transdepth, rip, fence, season,comb2) %>%
  spread(key = specCode, value = count, fill = 0)#spread to wide format to cpmpute indices later on

#Create natives only vector:
nat <- f %>%
  select(nat,longevity, specCode) %>%
  filter(nat =="native" & longevity == "perennial") 

#Create a vector with names of all native species in our data:
native_species <- as.character(unique(nat$specCode))

#subset native_species vector from fi that are only native:
fin <- fi[names(fi)[names(fi) %in% native_species] ] 

#Compute total densities (Richness) and summarise by treatment:
fina <- fin %>%
  mutate(Richness = specnumber(fin[,(which(names(fin)=='acaccycl'):which(names(fin)=='xanthueg'))]),
         Diversity = diversity(fin[,(which(names(fin)=='acaccycl'):which(names(fin)=='xanthueg'))]))

#summarise the total densities and plot:
f1 <- select(fi, Transdepth, rip, fence, season, comb2)
f2 <- select(fina, Richness,Diversity)
final <- cbind(f1,f2)

max(final$Richness[final$season=="Spring2012"]) #30
max(final$Richness[final$season=="Spring2013"]) #29

#Richness of Native Perennials======
#Summarise Richness for Transdepth:
Richness_Transdepth <-  final %>%
  group_by(season,Transdepth) %>%
  summarise(AV = mean(Richness,na.rm=T),
            SD = sd(Richness, na.rm = T),
            N = length(Richness),
            SE = SD/sqrt(N),
            CI = 1.96 * SE) %>%
  rename(Treatment=Transdepth)%>%
  mutate(Filter = "DISPERSAL")%>%
  mutate(season2 = ifelse(season=="Spring2012", "Spring 2012", "Spring 2013"))

#Summarise Richness for rip:
Richness_rip <-  final %>%
  group_by(season,rip) %>%
  summarise(AV = mean(Richness,na.rm=T),
            SD = sd(Richness, na.rm = T),
            N = length(Richness),
            SE = SD/sqrt(N),
            CI = 1.96 * SE) %>%
  rename(Treatment=rip)%>%
  mutate(Filter = "ABIOTIC")%>%
  mutate(season2 = ifelse(season=="Spring2012", "Spring 2012", "Spring 2013"))

#Summarise Richness for fence:
Richness_fence <-  final %>%
  group_by(season,fence) %>%
  summarise(AV = mean(Richness,na.rm=T),
            SD = sd(Richness, na.rm = T),
            N = length(Richness),
            SE = SD/sqrt(N),
            CI = 1.96 * SE) %>%
  rename(Treatment=fence)%>%
  mutate(Filter = "BIOTIC")%>%
  mutate(season2 = ifelse(season=="Spring2012", "Spring 2012", "Spring 2013"))


rich3 <- rbind(Richness_Transdepth,Richness_rip,Richness_fence)
rich3$index <- "Richness"


#Diversity of Native Perennials======
#Summarise Diversity for Transdepth:
Diversity_Transdepth <-  final %>%
  group_by(season,Transdepth) %>%
  summarise(AV = mean(Diversity,na.rm=T),
            SD = sd(Diversity, na.rm = T),
            N = length(Diversity),
            SE = SD/sqrt(N),
            CI = 1.96 * SE) %>%
  rename(Treatment=Transdepth)%>%
  mutate(Filter = "DISPERSAL")%>%
  mutate(season2 = ifelse(season=="Spring2012", "Spring 2012", "Spring 2013"))

#Summarise Diversity for rip:
Diversity_rip <-  final %>%
  group_by(season,rip) %>%
  summarise(AV = mean(Diversity,na.rm=T),
            SD = sd(Diversity, na.rm = T),
            N = length(Diversity),
            SE = SD/sqrt(N),
            CI = 1.96 * SE) %>%
  rename(Treatment=rip)%>%
  mutate(Filter = "ABIOTIC")%>%
  mutate(season2 = ifelse(season=="Spring2012", "Spring 2012", "Spring 2013"))

#Summarise Diversity for fence:
Diversity_fence <-  final %>%
  group_by(season,fence) %>%
  summarise(AV = mean(Diversity,na.rm=T),
            SD = sd(Diversity, na.rm = T),
            N = length(Diversity),
            SE = SD/sqrt(N),
            CI = 1.96 * SE) %>%
  rename(Treatment=fence)%>%
  mutate(Filter = "BIOTIC")%>%
  mutate(season2 = ifelse(season=="Spring2012", "Spring 2012", "Spring 2013"))


div3 <- rbind(Diversity_Transdepth,Diversity_rip,Diversity_fence)
div3$index <- "Diversity"



#PLOT Richness and Diversity together=========
rich_div <- rbind(rich3,div3)
#write.csv(rich_div, file = "Emergence_Richness_Diversity_2020.csv", row.names = F)

pd<-position_dodge(width = 0.4)

fig2 <- ggplot(rich_div, aes(x=Treatment, y=AV, color=season2))+ #shapes removed to keep figures simple: , shape=Filter #guides(shape=F)+
  geom_errorbar(aes(ymin=AV-SE, ymax=AV+SE),width=.3,position=pd,size=1.4)+
  geom_point(position=pd, size=3.5) + 
  geom_line(position=pd)+
  facet_grid(index~Filter,  scales="free", drop = T)+
  
  theme_bw()+
  scale_colour_manual(values = c("#0072B2", "#E69F00")) +
  theme(axis.text.y=element_text(size=14 , color = "black"),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24),
        axis.text.x = element_text(size=14, color = "black"),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=20),
        legend.position = "bottom") +
  labs(y="Emergence indices" ,color = "Season: ")

#Fix tag_facet to keep facet strips: WEB = https://stackoverflow.com/questions/56064042/using-facet-tags-and-strip-labels-together-in-ggplot2
tag_facet2 <- function(p, open = "(", close = ")", tag_pool = letters, x = -Inf, y = Inf, 
                       hjust = -0.5, vjust = 1.5, fontface = 2, family = "", ...) {
  
  gb <- ggplot_build(p)
  lay <- gb$layout$layout
  tags <- cbind(lay, label = paste0(open, tag_pool[lay$PANEL], close), x = x, y = y)
  p + geom_text(data = tags, aes_string(x = "x", y = "y", label = "label"), ..., hjust = hjust, 
                vjust = vjust, fontface = fontface, family = family, inherit.aes = FALSE)
}

tag_facet2(fig2)

#Save in your directory as desired:
ggsave(filename = "Fig2_2020_InColor4.jpg", width = 180, height = 120, units = "mm")

#Fig 2 Caption:
#Mean richness and diversity (Shannon-Wiener index) of native perennial seedlings emerging
#under three site-scale filter manipulation treatments for the two spring seasons following 
#topsoil transfer (±95% CI, 4 m-2): spring 2012 and spring 2013. Treatments were: abiotic; 
#ripped and unripped, biotic; fenced and open, dispersal; deep and shallow topsoil
#(n=192 in spring 2012, n=288 in spring 2013).







------------------------------------------------------------------
------------------------------------------------------------------

#EXTRA INFO======
#Overall Mean Richness:
Richness_min_max <-  final %>%
  group_by(season) %>%
  summarise(Min_rich = min(Richness,na.rm=T),
            Min_div = min(Diversity,na.rm=T),
            Max_rich = max(Richness,na.rm=T),
            Max_div = max(Diversity,na.rm=T))


#Richness All Native:
#Use the big data:
f <-read.csv("big2trait.csv")# %>%   #the entire data frame (site,plot,trait)

#Load only autumn season and spread to wide format to compute  densities.
fi <- f %>%
  filter(season == "Spring2012" | season == "Spring2013") %>%
  filter (nat=="native" & plot2 =="CTRL") %>%  #Pick only site-level treatments = CTRL
  select(su, specCode, count, plot2, Transdepth, rip, fence, season,comb2) %>%
  spread(key = specCode, value = count, fill = 0)#spread to wide format to cpmpute indices later on

#Create natives only vector:
nat <- f %>%
  select(nat,longevity, specCode) %>%
  filter(nat =="native") 

#Create a vector with names of all native species in our data:
native_species <- as.character(unique(nat$specCode))

#subset native_species vector from fi that are only native:
fin <- fi[names(fi)[names(fi) %in% native_species] ] 

#Compute total densities (Richness) and summarise by treatment:
fina <- fin %>%
  mutate(Richness = specnumber(fin[,(which(names(fin)=='acaccycl'):which(names(fin)=='xanthueg'))]),
         Diversity = diversity(fin[,(which(names(fin)=='acaccycl'):which(names(fin)=='xanthueg'))]))

#summarise the total densities and plot:
f1 <- select(fi, Transdepth, rip, fence, season, comb2)
f2 <- select(fina, Richness,Diversity)
final <- cbind(f1,f2)

table(final$Richness, final$season)
max(final$Richness[final$season=="Spring2012"])
max(final$Richness[final$season=="Spring2013"])
