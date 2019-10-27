#Look at the frequency of seedlings/Traits in 3 treatments (Transdepth, Rip, Fence):
#WEB: https://github.com/dcomtois/summarytools
library(tidyr)
library(dplyr)
library(ggplot2)
library(gdata)
library(lme4)
library(lmerTest)
library(officer)
library(flextable)
library(broom)#Re-Load  data to be sure all works well [Topsoil Emergence DATA:only spr12 & spr13 seasons included:

#Across all seasons after topsoil transfer:
BigDF2<-read.csv("big2trait.csv")#the entire data frame (site,plot,trait)
dim(BigDF2)#791058 obs. of  38 variables, including REMNANT DATA
levels(BigDF2$season)#"Autumn2013" "Autumn2014" "remnant"    "Spring2012" "Spring2013"
BigDF2<- BigDF2[!BigDF2$season=="remnant",] #remove remnant data
BigDF3 <- BigDF2 [ BigDF2$count > 0 , ] #Remove zero values rows, 
BigDF3 <- drop.levels(BigDF3)

#Work on 2-years plots.
#In 2013 we added few extra plots to keep balanced design (omitted here).
BigDF3 <- BigDF2 [ BigDF2$count > 0 & BigDF2$plot2 == "CTRL", ] #Remove zero value rows, work on CTRL only

#Freq of Spr12 vs Aut14 (start and end season):
#NATIVE AND PERENNIAL ONLY!
BigDF4 <- BigDF3 [ BigDF3$season=="Spring2012" | BigDF3$season=="Autumn2014", ] #Remove zero values from count of species
BigDF4 <- drop.levels(BigDF4)#10109    38
dim(BigDF4)
#Compute Perry's PFT as per Fowler's paper--------
#NATIVES vs ALIENS (%):==========
nat_Percent <- group_by(BigDF4, su,  nat, .drop = F) %>% #CHOOSE YOUR PFT HERE!
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

ggplot (nat_Percent, aes(x = season, y = Inv_Perc)) +
           geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
           theme_bw() +  facet_grid(.~fence)

#Native mean +SD percent=
nat_Perc_mean <- gather(nat_Percent,TRF, Treatment, Transdepth:fence) %>%
  group_by(season,  Treatment) %>%
  summarize( mean_nat = mean (Nat_Perc, na.rm=T),
             n_nat = length(Nat_Perc),
             sd_nat = sd(Nat_Perc, na.rm=T),
             se_nat = sd_nat/sqrt(n_nat))%>%
  select(season, Treatment, mean_nat, se_nat)

#Annual vs Perennial PFT:=============
longevity_Percent <- filter (BigDF4, plot2=="CTRL" & longevity != "NA") %>% #only sie -scale treatments
  group_by(su,longevity, .drop = F) %>% #CHOOSE YOUR PFT HERE!
  summarise(Spr2012 = sum(count[season=="Spring2012"]),
            Aut2014 = sum(count[season == "Autumn2014"])) %>%
  mutate (Transdepth =ifelse(grepl('D',su), 'deep', 'shallow')) %>%
  mutate (rip =ifelse(grepl('R',su), 'ripped', 'unripped')) %>%
  mutate (fence =ifelse(grepl('F',su), 'fenced', 'open'))%>%
  gather(season,  count1m2, Spr2012:Aut2014)%>%
  spread (longevity,  count1m2) %>%
  mutate(total = annual + perennial, #levels of variable "nat"
         annual_Perc = annual/ total *100, 
         perennial_Perc = perennial/total *100)

ggplot (longevity_Percent, aes(x = season, y = annual_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~fence)

longevity_means <- gather(longevity_Percent,longevity, Percent,annual_Perc:perennial_Perc) %>%
  gather(Treatment, Level, Transdepth:fence)%>%
  group_by(longevity, season, Level) %>%
  summarise(Mean_Percent = mean(Percent, na.rm=T),
            SD_Percent = sd (Percent, na.rm=T))
lon_Perc_mean <- gather(longevity_Percent,TRF, Treatment, Transdepth:fence) %>%
  group_by(season,  Treatment) %>%
  summarize( mean_lon = mean (annual_Perc, na.rm=T),
             sd_lon = sd(annual_Perc, na.rm=T),
             n_lon = length(annual_Perc),
             se_lon = sd_lon/sqrt(n_lon)) %>%
  select(season, Treatment, mean_lon, se_lon)

pd <- position_dodge(.5)
ggplot (longevity_means, aes(x = season, y = Mean_Percent, shape= season)) +
  geom_point(size =3) +
  theme_bw() +
  facet_grid(rip ~ longevity)

#RESPROUTERS vs SEEDERS:==============
resprouter_Perc <- filter (BigDF4, plot2=="CTRL" & resprouter != "NA") %>% #only sie -scale treatments
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

ggplot (resprouter_Perc, aes(x = season, y = Resp_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~fence)

resprouter_Perc_mean <- gather(resprouter_Perc, TRF,Treatment,Transdepth:fence) %>%
  group_by(season, Treatment) %>%
  summarize( mean_resprouter = mean (Resp_Perc, na.rm=T),
             n_resprouter = length(Resp_Perc),
             sd_resprouter = sd(Resp_Perc, na.rm=T),
             se_resprouter = sd_resprouter/sqrt(n_resprouter)) %>%
  select(season, Treatment, mean_resprouter, se_resprouter)

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
  mutate(total = woody + grass + herb + succulent, #levels of variable
         woody_Perc = woody/ total *100, 
         grass_Perc = grass/total *100,
         herb_Perc = herb/total *100,
         succulent_Perc = succulent/total *100)
growCategory_Percent

grow_woody <- select(growCategory_Percent,Transdepth,rip,fence,season, woody_Perc )
grow_Perc_mean <- gather(growCategory_Percent,TRF,Treatment,c(Transdepth,rip,fence)) %>%
  group_by(season, Treatment)%>%
  summarize( mean_woody = mean (woody_Perc, na.rm=T),
             n_woody = length(woody_Perc),
             sd_woody = sd(woody_Perc, na.rm=T),
             se_woody = sd_woody/sqrt(n_woody)) %>%
  select(season, Treatment, mean_woody, se_woody)

growCategory_means <- gather(growCategory_Percent,growCategory, Percent,woody_Perc:succulent_Perc) %>%
  group_by(growCategory, season, rip) %>%
  summarise(Mean_Percent = mean(Percent, na.rm=T),
            SD_Percent = sd (Percent, na.rm=T))

pd <- position_dodge(.5)
ggplot (growCategory_means, aes(x = season, y = Mean_Percent, shape= season)) +
  geom_point(size =3) +
  theme_bw() +
  facet_grid(rip ~ growCategory)

ggplot (growCategory_Percent, aes(x = season, y = woody_Perc)) +
  geom_violin(adjust = 1L, scale = "area", fill = "#35b779") +
  theme_bw() +
  facet_grid(.~fence)

#TABLE of PFT %======
nat_Perc_mean #Mean values +- SE of natives percentages
nat_Perc_mean$ mean_nat <- round(nat_Perc_mean$mean_nat, 1)
nat_Perc_mean$ se_nat <- round(nat_Perc_mean$se_nat, 1)
t1 <- unite_(nat_Perc_mean, "Mean_SE", c("mean_nat","se_nat"), sep = "+-")%>%
  spread(Treatment, Mean_SE ) %>%
  mutate (Trait = "natives")

grow_Perc_mean#Mean values +- SE of woody percentages
grow_Perc_mean$ mean_woody <- round(grow_Perc_mean$mean_woody, 1)
grow_Perc_mean$ se_woody <- round(grow_Perc_mean$se_woody, 1)
t2 <- unite_(grow_Perc_mean, "Mean_SE", c("mean_woody","se_woody"), sep = "+-")%>%
  spread(Treatment, Mean_SE ) %>%
  mutate (Trait = "woody")

resprouter_Perc_mean#Mean values +- SE of resprouters percentages
resprouter_Perc_mean$ mean_resprouter <- round(resprouter_Perc_mean$mean_resprouter, 1)
resprouter_Perc_mean$ se_resprouter <- round(resprouter_Perc_mean$se_resprouter, 1)
t3 <- unite_(resprouter_Perc_mean, "Mean_SE", c("mean_resprouter","se_resprouter"), sep = "+-")%>%
  spread(Treatment, Mean_SE ) %>%
  mutate (Trait = "resprouter")

lon_Perc_mean#Mean values +- SE of annual percentages
lon_Perc_mean$ mean_lon <- round(lon_Perc_mean$mean_lon, 1)
lon_Perc_mean$ se_lon <- round(lon_Perc_mean$se_lon, 1)
t4 <- unite_(lon_Perc_mean, "Mean_SE", c("mean_lon","se_lon"), sep = "+-")%>%
  spread(Treatment, Mean_SE ) %>%
  mutate (Trait = "annual")

tbl <- rbind (t1,t2,t3,t4)
#write.csv(tbl, file = "TraitPercentageTable.csv")
              
              
#Remove! Sp aut14 > spr12--------------
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
Spr12_Freq<-ctable(BigDF3$specCode[BigDF3$season=="Spring2012"], BigDF3$Transdepth[BigDF3$season=="Spring2012"], method = "render")
a<- as.data.frame(Spr12_Freq$cross_table)
a1 <- spread(a, Transdepth, Freq)
Spr12_Freq

run1 <- with(BigDF3, stby(list(x = specCode, y = Transdepth), season, ctable))
print(run1, method = "viewer")
print(run1, file = "run1.txt")
?print.list #see options for print methods

print(run1, methods
run2 <- with(BigDF3, stby(list(x = nat, y = rip), season, ctable))
print(run2, method = "browser")


#STATS on Plant functional groups=====
#On Pawel's Data extract (should be the same as Joe's = "Jandakot_Pawel_DataExtract_28Jul2019.csv"). SPRING 2012 EMERGENCE:
#Taking species into accoung as random factor:
spr12_PAV <- filter(BigDF4, season == "Spring2012") %>%
  gather(treatment, treat_, Transdepth:fence)
glmer_spr12_PAV<-glmer(count*4 ~ treat_*nat #multiply by 4 to avoid decimals (counts is per 1m2, but in the field recorded per 4m2 )
                                    +(1|site) +(1|plot) +(1|specCode),
                                    family = poisson(link="log"),
                                 data=spr12_PAV)
#STATS OUTPUT:
summary(glmer_spr12_PAV)
#Generalized linear mixed model fit by maximum likelihood (Laplace Aproximation) [glmerMod]
Family: poisson  ( log )
Formula: count * 4 ~ treat_ * nat + (1 | site) + (1 | plot) + (1 | specCode)
Data: spr12_PAV

AIC       BIC    logLik  deviance  df.resid 
458247.9  458371.4 -229108.9  458217.9     27975 

Scaled residuals: 
  Min      1Q  Median      3Q     Max 
-10.833  -1.439  -0.319   0.658  47.855 

Random effects:
  Groups   Name        Variance Std.Dev.
specCode (Intercept) 0.42959  0.6554  
plot     (Intercept) 0.04900  0.2214  
site     (Intercept) 0.05191  0.2278  
Number of obs: 27990, groups:  specCode, 187; plot, 12; site, 6

Fixed effects:
  Estimate Std. Error z value Pr(>|z|)    
(Intercept)                 3.220412   0.145780  22.091  < 2e-16 ***
  treat_fenced              0.013564   0.005620   2.414   0.0158 *  
  treat_open                0.042236   0.005953   7.095 1.29e-12 ***
  treat_ripped              0.065646   0.006415  10.232  < 2e-16 ***
  treat_shallow             0.047253   0.006065   7.792 6.62e-15 ***
  treat_unripped            0.004369   0.005379   0.812   0.4166    
natnative                  -2.741533   0.111132 -24.669  < 2e-16 ***
  treat_fenced:natnative   -0.057770   0.011335  -5.097 3.46e-07 ***
  treat_open:natnative     -0.222777   0.012541 -17.764  < 2e-16 ***
  treat_ripped:natnative   -0.375656   0.013994 -26.844  < 2e-16 ***
  treat_shallow:natnative  -0.255774   0.012038 -21.248  < 2e-16 ***
  treat_unripped:natnative -0.014804   0.010888  -1.360   0.1740 

