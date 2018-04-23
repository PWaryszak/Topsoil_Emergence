#DATA:========
library(tidyverse) # load this package if not done yet 
library(vegan) # load this package if not done yet  

#Joining Remnant/remnant DATA=======
#Remnant Banksia Woodland data:
REMNspecies <- read.csv("REMNspecies.csv")

#changing from wide to long format to fit big2trait format:
b <- gather(REMNspecies,specCode,count, acacpulc:xantprei)
str(b)#2424 obs. of  6 variables:
b$binary<-ifelse(b$count > 0, 1,0)#Count is of unclear unit so turning into 0,1 values only

#joining remnant data with species trait data:
traits2 <- read.csv("REMNtraits.csv")
MyTraits2 <- select(traits2,specCode, Nfix, growCategory,maxHgt,seedStorage,nat,longevity)
DivData2 <- left_join(b, MyTraits2, by = "specCode")
str(DivData2)#	2424 obs. of  11 variables:

#Compute Means on above diversiy/density values (Recorded on 100m2 plots)
#to convert to count1m2 divide by 100

remnant_sums <- DivData2 %>% 
  
  group_by(plot2, season, su) %>%
  
  summarise(Total_per_plot = sum(count/100),
            Richness = specnumber(count/100),
            RichnessNatives = specnumber(count[nat=="native"]),
            TotalNatives_per_plot = sum(count[nat=="native"]/100),
            TotalInvasives_per_plot = sum(count[nat=="invasive"]/100),
            PercentNatives = TotalNatives_per_plot/Total_per_plot*100,
            RichnessInvasives = specnumber(count[nat=="invasive"]))

remnant_means <-  group_by(remnant_sums, plot2, season)%>%
  
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
            
            MeanPercentNatives = round ( mean(PercentNatives, na.rm=T), 1),
            MeanPercentNatives_SD = round ( sd(PercentNatives, na.rm=T), 1),
            
            N = n())


remnant_means_SD <- unite(remnant_means, "Mean_RichNat", c("Mean_RichNat", "Mean_RichNat_SD"), sep = " ± ") %>%
  
  unite( "Mean_RichInv", c("Mean_RichInv", "Mean_RichInv_SD"), sep = " ± ") %>%
  unite( "Mean_Inv_Density", c("Mean_Inv_Density", "Mean_Inv_Density_SD"), sep = " ± ") %>%
  unite( "Mean_Nat_Density", c("Mean_Nat_Density", "Mean_Nat_Density_SD"), sep = " ± ") %>%
  unite( "Mean_InvasiveDensity", c("Mean_InvasiveDensity", "Mean_InvasiveDensity_SD"), sep = " ± ") %>%
  unite( "MeanPercentNatives", c("MeanPercentNatives","MeanPercentNatives_SD"), sep = " ± " )

remnant_means_SD
#write.csv(remnant_means_SD, file = "TABLE_NEW2.csv")

#Percent of Nfixers:=======
Nfixers <- DivData2 %>% na.omit() %>%

  group_by(plot2, season, su) %>%
  
  summarise(Nyes = sum(count[Nfix == "yes"]),
            Total = sum(count),
            NyesPerc = Nyes/Total*100)

Nfixers_means <- group_by(Nfixers , plot2, season) %>%
  
  summarise(MeanPercNfix = round (mean(NyesPerc,  na.rm=T) ,1), 
            SDNfix = round( sd(NyesPerc,  na.rm=T) ,1 ),
            N = length(NyesPerc), se  = SDNfix / sqrt(N))

Nfixers_means_SD <- Nfixers_means %>%
  unite( "MeanPercNfix", c("MeanPercNfix", "SDNfix"), sep = " ± " )

Nfixers_means_SD

#Percent of seedStorage type = soil:=======
seeds <- DivData2 %>% na.omit() %>% 
  
  group_by(plot2, season, su) %>%
  filter (seedStorage == "canopy" | seedStorage=="soil") %>%
  
  summarise(Canopyyes = sum(count[seedStorage == "canopy"]),
            Total = sum(count),
            CanopyPerc = Canopyyes/Total*100) %>%
  
  group_by( plot2, season) %>%
  summarise(MeanPercCanopy = round(mean(CanopyPerc),1), SD_Canopy = round(sd(CanopyPerc),1))

seeds_means_SD <- seeds %>%
  unite( "MeanPercCanopy", c("MeanPercCanopy", "SD_Canopy"), sep = " ± " )

seeds_means_SD


#Percent of perennials:=====
longevity <- DivData2 %>% na.omit() %>%
  group_by(plot2, season, su) %>%
  
  summarise(Long = sum(count[longevity=="perennial"]),
            Total = sum(count),
            LongPerePerc = Long/Total*100 ) %>%
  
  group_by( plot2, season) %>%
  summarise(MeanPerePerc = round(mean(LongPerePerc),1), SD_Long = round(sd(LongPerePerc),1))

longevity_means_SD <- longevity %>%
  unite( "MeanPerePerc", c("MeanPerePerc", "SD_Long"), sep = " ± " )


#MERGE ALL DATA:====

DivData2_Means_SD <- cbind (remnant_means_SD,
                           Nfixers_means_SD,
                           seeds,
                           longevity_means_SD)
Ref_Data <- DivData2_Means_SD[ ,c("plot2","season" ,"Mean_RichNat" ,"Mean_RichInv" ,
                           "MeanPercNfix", "MeanPercCanopy","MeanPerePerc")]
colnames(Ref_Data)[2]<- "EmergenceSeason"

#Seee Topsoil_Diversity R-file to find D2 object:
TopRef <- rbind(D2, Ref_Data)
#write.csv(TopRef, file = "TABLE_Topsoil_Reference_Sp_indices_NEAL.csv")
