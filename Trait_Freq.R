#Look at the frequency of seedlings/Traits
#WEB: https://github.com/dcomtois/summarytools
library(tidyr)
library(dplyr)
library(ggplot2)
library(gdata)

#Across all seasons after topsoil transfer:
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
str(BigDF2)#data.frame':  787500 obs. of  39 variables,791058 obs. of  38 variables with REMNANT DATA
levels(BigDF2$season)#"Autumn2013" "Autumn2014" "remnant"    "Spring2012" "Spring2013"

BigDF2<- BigDF2[!BigDF2$season=="remnant",] #remove remnant data
BigDF3 <- BigDF2 [ BigDF2$count > 0, ] #Remove zero values rows
BigDF3 <- drop.levels(BigDF3)
#Work on 2-years plots.
#In 2013 we added few extra plots to keep balanced design (omitted here).

#Freq of Spr12 vs Aut14 (start and end season):
#NATIVE AND PERENNIAL ONLY!
BigDF4 <- BigDF3 [ BigDF3$season=="Spring2012" | BigDF3$season=="Autumn2014", ] #Remove zero values from count of species
BigDF4 <- drop.levels(BigDF4)

#Compute Perry's PFT as per Fowler's paper--------
#NATIVES vs ALIENS:==========
nat_Percent <- filter (BigDF4, plot2=="CTRL") %>% #only sie -scale treatments
  group_by(su,nat, .drop = F) %>% #CHOOSE YOUR PFT HERE!
  summarise(Spr2012 = sum(count[season=="Spring2012"]),
            Aut2014 = sum(count[season == "Autumn2014"])) %>%
   mutate (Transdepth =ifelse(grepl('D',su), 'deep', 'shallow')) %>%
   mutate (rip =ifelse(grepl('R',su), 'ripped', 'unripped')) %>%
   mutate (fence =ifelse(grepl('F',su), 'fenced', 'open')) %>%
  gather(season,  count1m2, Spr2012:Aut2014) %>% #Fill seasons with count1m2
  spread (nat,  count1m2) %>% #move native & aliens to cols to compute % 
  mutate(total = invasive + native,
         Nat_Perc = native/ total *100, #Percent of natives per plot
         Inv_Perc = invasive/total *100)#Percent of invasives per plot
nat_Percent

ggplot (nat_Percent, aes(x = season, y = Inv_Perc)) +
           geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
           theme_bw() +
           facet_grid(.~fence)

ggplot (nat_Percent, aes(x = season, y = Inv_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~rip)

ggplot (nat_Percent, aes(x = season, y = Inv_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~Transdepth)

#RESPROUTERS vs SEEDERS:==============
resprouter_Percent <- filter (BigDF4, plot2=="CTRL" & resprouter != "NA") %>% #only sie -scale treatments
  group_by(su,resprouter, .drop = F) %>% #CHOOSE YOUR PFT HERE!
  summarise(Spr2012 = sum(count[season=="Spring2012"]),
            Aut2014 = sum(count[season == "Autumn2014"])) %>%
  mutate (Transdepth =ifelse(grepl('D',su), 'deep', 'shallow')) %>%
  mutate (rip =ifelse(grepl('R',su), 'ripped', 'unripped')) %>%
  mutate (fence =ifelse(grepl('F',su), 'fenced', 'open'))%>%
  gather(season,  count1m2, Spr2012:Aut2014)%>%
  spread (resprouter,  count1m2) %>%
  mutate(total = no + yes, #levels of variable "nat"
         Resp_Perc = yes/ total *100,
         NoResp_Perc = no/total *100)
resprouter_Percent

ggplot (resprouter_Percent, aes(x = season, y = Resp_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~fence)

ggplot (resprouter_Percent, aes(x = season, y = Resp_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~rip)

ggplot (resprouter_Percent, aes(x = season, y = Resp_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~Transdepth)

#WOODY vs NON_WOODY:=============
growCategory_Percent <- filter (BigDF4, plot2=="CTRL" & growCategory != "NA") %>% #only sie -scale treatments
  group_by(su,growCategory, .drop = F) %>% #CHOOSE YOUR PFT HERE!
  summarise(Spr2012 = sum(count[season=="Spring2012"]),
            Aut2014 = sum(count[season == "Autumn2014"])) %>%
  mutate (Transdepth =ifelse(grepl('D',su), 'deep', 'shallow')) %>%
  mutate (rip =ifelse(grepl('R',su), 'ripped', 'unripped')) %>%
  mutate (fence =ifelse(grepl('F',su), 'fenced', 'open'))%>%
  gather(season,  count1m2, Spr2012:Aut2014)%>%
  spread (growCategory,  count1m2) %>%
  mutate(total = woody + grass + herb + succulent, #levels of variable "nat"
         woody_Perc = woody/ total *100, 
         grass_Perc = grass/total *100,
         herb_Perc = herb/total *100,
         succulent_Perc = succulent/total *100)
growCategory_Percent

ggplot (growCategory_Percent, aes(x = season, y = woody_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~fence)

ggplot (growCategory_Percent, aes(x = season, y = grass_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~rip)

ggplot (growCategory_Percent, aes(x = season, y = herb_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~Transdepth)

ggplot (growCategory_Percent, aes(x = season, y = succulent_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~fence)

growCategory_means <- gather(growCategory_Percent,growCategory, Percent,woody_Perc:succulent_Perc) %>%
  group_by(growCategory, season, rip) %>%
  summarise(Mean_Percent = mean(Percent, na.rm=T),
            SD_Percent = sd (Percent, na.rm=T))
growCategory_means

pd <- position_dodge(.5)

ggplot (growCategory_means, aes(x = season, y = Mean_Percent, shape= season)) +
  geom_point(size =3) +
  
  theme_bw() +
  facet_grid(rip ~ growCategory)


library(Rmisc)
n_rip<-summarySE(nat_Percent, measurevar="Nat_Perc", groupvars=c("rip","season"), na.rm = T)
i_rip <-summarySE(nat_Percent, measurevar="Inv_Perc", groupvars=c("rip","season"), na.rm = T)

n_Transdepth<-summarySE(nat_Percent, measurevar="Nat_Perc", groupvars=c("Transdepth","season"), na.rm = T)
i_Transdepth <-summarySE(nat_Percent, measurevar="Inv_Perc", groupvars=c("Transdepth","season"), na.rm = T)



#Remove Sp aut14 > spr12--------------
#plots species that were detected in Autumn 2014 but not in Spring 2012:
Rmv <-  BigDF4  %>% group_by(specCode, su) %>%
       filter(nat=="native" & longevity == "perennial") %>%
       summarise(Spr2012 = sum(season=="Spring2012"),
                 Aut2014 = sum(season == "Autumn2014")) %>%
       filter ( Spr2012 < Aut2014)
Rmv
#write.csv(Rmv, file = "SpeciesNotDetected.csv")

#Mortality by Treatment X Species-------------
Freq_depth <- BigDF4 %>% group_by(specCode,Transdepth, .drop = F) %>%
  filter(nat=="native" & longevity == "perennial") %>%
  summarise(Spr2012 = sum(season=="Spring2012"),
            Aut2014 = sum(season == "Autumn2014")) %>%
  mutate(  Mortality = (Spr2012 - Aut2014)/Spr2012 * 100) %>%
  filter (Mortality >= 0) #negative mortality due to detection of sp in aut14 but not in spr12
table(Freq_depth$Mortality)

Freq_rip <- BigDF4 %>% group_by(specCode,rip, .drop = F) %>%
  filter(nat=="native" & longevity == "perennial") %>%
  summarise(Spr2012 = sum(season=="Spring2012"),
            Aut2014 = sum(season == "Autumn2014")) %>%
  mutate(Mortality = (Spr2012 - Aut2014)/Spr2012 * 100)
Freq_rip

Freq_fence <- BigDF4 %>% group_by(specCode,fence) %>%
  filter(nat=="native" & longevity == "perennial") %>%
  summarise(Spr2012 = sum(season=="Spring2012"),
            Aut2014 = sum(season == "Autumn2014")) %>%
  mutate( Mortality = (Spr2012 - Aut2014)/Spr2012 * 100)
View(Freq_fence)

table(Freq_fence$Mortality)

#Freq of Natvies vs Invasive:
#install.packages("summarytools")
library("summarytools")
descr(BigDF3$count)
ctable(BigDF3$specCode, BigDF3$season, method = "render")

run1 <- with(BigDF3, stby(list(x = nat, y = Transdepth), season, ctable))
print(run1, method = "browser")
run2 <- with(BigDF3, stby(list(x = nat, y = rip), season, ctable))
print(run2, method = "browser")

write