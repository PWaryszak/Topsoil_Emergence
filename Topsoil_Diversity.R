#DATA:========
library(tidyverse) # load this package if not done yet 
library(vegan) # load this package if not done yet  

#Diversity:=========
#Trait data that fits our topsoil emergence data
#traitURL <- "https://sites.google.com/site/pawelwaryszak/Pawels-PhD-Thesis/chapter-6/traits.all_19oct15.csv?attredirects=0&d=1"
#traits <- read.csv(url(traitURL)) # read in data for traits Ind <- c(as.character(traits[traits[,2]=="native",1]), "count") # index of native species and text 'count'
traits <- read.csv("traits.all_19oct15.csv")
levels(traits$specCode)#253

#Re-Load data:
data<- read.csv("TopsoilEmergenceData.csv")
str(data)#38247 obs. of  24 variables:
#Join density data with trait data:
MyTraits <- select(traits,specCode, Nfix, growCategory,maxHgt,seedStorage)
DivData <- left_join(data, MyTraits, by = "specCode")
str(DivData)#38247 obs. of  28 variables:

#double check on richness value with spread function:
topsoil_sp <- DivData %>% 
  
  filter(comb2 == "deep.unripped") %>% #the most succesful site-treatment
  
  select(plot2, EmergenceSeason, su, specCode, count1m2) %>%
  
  spread(specCode, count1m2, fill = 0)


Species <- select( topsoil_sp, acaccycl:xanthueg )

topsoil_sp$Richness <- specnumber(Species)
table(topsoil_sp$Richness) 


topsoil_sp_means <- select(topsoil_sp,su, plot2, EmergenceSeason, Richness)%>%
  
  group_by(su, plot2, EmergenceSeason) %>%
  
  summarise(RichnessMean = mean(Richness), Richness_SD = sd(Richness))

topsoil_sp_means #the same values!!! as in topsoil_sums !!!



###
topsoil_sums <- DivData %>% 
  
  filter(comb2 == "deep.unripped") %>% #the most succesful site-treatment
  
  group_by(plot2, EmergenceSeason, su) %>%
  
  summarise(Total_per_plot = sum(count1m2),
            Richness = specnumber(count1m2),
            RichnessNatives = specnumber(count1m2[nat=="native"]),
            TotalNatives_per_plot = sum(count1m2[nat=="native"]),
            TotalInvasives_per_plot = sum(count1m2[nat=="invasive"]),
            RichnessInvasives = specnumber(count1m2[nat=="invasive"]))

#Compute Means on above diversiy/density values
topsoil_means <-  group_by(topsoil_sums, plot2, EmergenceSeason)%>%
  
  summarise(Mean_RichNat = round(mean(RichnessNatives, na.rm=T),1),
            Mean_RichNat_SD = round(sd(RichnessNatives, na.rm=T),1),

            Mean_RichInv = round(mean(RichnessInvasives, na.rm=T),1),
            Mean_RichInv_SD = round(sd(RichnessInvasives, na.rm=T),1),
            
            Mean_Inv_Density = round(mean(TotalInvasives_per_plot, na.rm=T),1),
            Mean_Inv_Density_SD = round(sd(TotalInvasives_per_plot, na.rm=T),1),
            
            Mean_Nat_Density = round(mean(TotalNatives_per_plot, na.rm=T),1),
            Mean_Nat_Density_SD = round(sd(TotalNatives_per_plot, na.rm=T),1),
            
            Mean_InvasiveDensity = round(mean(TotalInvasives_per_plot, na.rm=T),1),
            Mean_InvasiveDensity_SD = round(sd(TotalInvasives_per_plot, na.rm=T),1),
            N = n())
            

topsoil_means_SD <- unite(topsoil_means, "Mean_RichNat", c("Mean_RichNat", "Mean_RichNat_SD"), sep = " ± ") %>%
  
  unite( "Mean_RichInv", c("Mean_RichInv", "Mean_RichInv_SD"), sep = " ± ") %>%
  unite( "Mean_Inv_Density", c("Mean_Inv_Density", "Mean_Inv_Density_SD"), sep = " ± ") %>%
  unite( "Mean_Nat_Density", c("Mean_Nat_Density", "Mean_Nat_Density_SD"), sep = " ± ") %>%
  unite( "Mean_InvasiveDensity", c("Mean_InvasiveDensity", "Mean_InvasiveDensity_SD"), sep = " ± ")

#write.csv(topsoil_means_SD, file = "TABLE1.csv")

#Percent of Nfixers:=======
Nfixers <- DivData %>% na.omit() %>%
  filter(comb2 == "deep.unripped") %>% #the most succesful site-treatment
  
  group_by(plot2, EmergenceSeason, su) %>%
  
  summarise(Nyes = sum(count1m2[Nfix == "yes"]),
            Total = sum(count1m2),
            NyesPerc = Nyes/Total*100)

Nfixers_means <- group_by(Nfixers , plot2, EmergenceSeason) %>%
  
  summarise(MeanPercNfix = round (mean(NyesPerc,  na.rm=T) ,1), 
            SDNfix = round( sd(NyesPerc,  na.rm=T) ,1 ),
                      N = length(NyesPerc), se  = SDNfix / sqrt(N))

Nfixers_means_SD <- Nfixers_means %>%
  unite( "MeanPercNfix", c("MeanPercNfix", "SDNfix"), sep = " ± " )

#Percent of seedStorage type = soil:=======
seeds <- DivData %>% na.omit() %>% 
  
  filter(comb2 == "deep.unripped") %>% #the most succesful site-treatment
  
  group_by(plot2, EmergenceSeason, su) %>%
  filter (seedStorage == "canopy" | seedStorage=="soil") %>%
  summarise(Canopyyes = sum(count1m2[seedStorage == "canopy"]),
            Total = sum(count1m2),
            CanopyPerc = Canopyyes/Total*100) %>%
  group_by( plot2, EmergenceSeason) %>%
  summarise(MeanPercCanopy = round(mean(CanopyPerc),1), SD_Canopy = round(sd(CanopyPerc),1))

seeds_means_SD <- seeds %>%
  unite( "MeanPercCanopy", c("MeanPercCanopy", "SD_Canopy"), sep = " ± " )

#Percent of perennials:=====
longevity <- DivData %>% na.omit() %>%
  group_by(plot2, EmergenceSeason, su) %>%
  
  filter(comb2 == "deep.unripped") %>% #the most succesful site-treatment
  
  summarise(Long = sum(count1m2[longevity=="perennial"]),
            Total = sum(count1m2),
            LongPerePerc = Long/Total*100 ) %>%
  
  group_by( plot2, EmergenceSeason) %>%
  summarise(MeanPerePerc = round(mean(LongPerePerc),1), SD_Long = round(sd(LongPerePerc),1))

longevity_means_SD <- longevity %>%
  unite( "MeanPerePerc", c("MeanPerePerc", "SD_Long"), sep = " ± " )


#MERGE ALL DATA:====

DivData_Means_SD <- cbind (topsoil_means_SD,
                           Nfixers_means_SD,
                           seeds,
                           longevity_means_SD)
names(DivData_Means_SD)
D2 <- DivData_Means_SD[ ,c("plot2","EmergenceSeason" ,"Mean_RichNat" ,"Mean_RichInv" ,
                           "MeanPercNfix", "MeanPercCanopy","MeanPerePerc")]
names(D2)
#write.csv(D2, file = "TABLE_TopsoilSpeciesIndexData_NEAL.csv")
