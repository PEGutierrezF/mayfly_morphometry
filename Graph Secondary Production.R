



#--------------------------------------------
# Production graph
# 06 Jun 2020
#PEGF
#--------------------------------------------
#

library(gg.gap)
library("ggpubr")
require("nlme")
library(ggplot2)
library(dplyr)
library("reshape2")
library("forcats")
library("gridExtra")
library("BiocManager")

biomass <- read.csv("Biomass.csv")
biomass

SecondaryProduction <- biomass %>% select(Month,Year, Production)
SecondaryProduction


p1 <- ggplot(SecondaryProduction, aes(x=Month,
                              y=Production, 
                              color=Year)) +
  geom_point(aes(), size = 4, fill="white")+
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+
  theme_bw() +
  geom_line(aes(group = Year)) +
  
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) + 
  theme(legend.position = c(0.87, 0.86)) +
  theme(legend.text = element_text(color = "black", size = 12))+
  
 # ylim(0,11.5) +
  
  xlab('')+ ylab("Production ("*mg~AFDM~m^-2*")") +
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) +
  theme(axis.text.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1

