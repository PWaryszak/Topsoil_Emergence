#LOAD Final Densities DATA:
library(tidyverse)
Fin <- read.csv("FinalDensityAll.csv") #data of final densities in the last survey season (Autumn2014)

#Convert data into Mean +CI for the figure
Final <-  filter (Fin, treat =="control") %>%
      gather( key = "Treatment_Type", value = "Treatment", rip:Transdepth) %>%
      group_by (Treatment) %>%
      summarise(Mean = mean(sum1m2, na.rm = T),
                 sd =  sd(sum1m2, na.rm = T),
                 N    = length(sum1m2),
                 se   = sd / sqrt(N),
                 CI = 1.96 *se * Mean) %>%
  mutate ( season = "Autumn 2014") %>% #create extra x factor for facetting
  mutate (Filter = ifelse(Treatment =="ripped"|Treatment =="unripped", "ABIOTIC",
                         ifelse(Treatment == "fenced" |Treatment =="open", "BIOTIC", "DISPERSAL")))

  Final$Filter <- factor(Final$Filter, levels = c("ABIOTIC","BIOTIC", "DISPERSAL"))

#Draw a figure:
pd<-position_dodge(0.5)
  
FinFig <- ggplot(Final, aes(x=Treatment, y=Mean, shape=Filter))+
  geom_errorbar(aes(ymin=Mean-se, ymax=Mean+se),width=.2,position=pd,size=1.4)+
  geom_point(position=pd, size=4) + 
  geom_line(position=pd)+
  facet_grid(season~Filter,  scales="free", drop = T)+
  theme_bw()+
  theme(axis.text.y=element_text(size=17),
        axis.title.x=element_blank(),
        axis.title.y=element_text(size=24),
        axis.text.x = element_text(size=17),
        panel.grid.minor.x = element_blank(),
        strip.text=element_text(size=20),
        legend.position = "none") +
  labs(y=expression(Final~native~density~(m^{-2})))

FinFig

#ggsave(FinFig, filename = "FinalDensityFig.jpg", width = 180, height = 100, units = "mm")
