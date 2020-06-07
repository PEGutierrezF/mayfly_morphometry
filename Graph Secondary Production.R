



#--------------------------------------------
# Production graph
# 06 Jun 2020
#PEGF
#--------------------------------------------
#

library(ggplot2)
library(ggthemes)
library(patchwork)
library(colorblindr)
library(cowplot)
library(colorspace)
library(wesanderson)
library(ggsci)

Biomass <- read.csv("Biomass.csv")
Biomass


biomass$Month = factor(Biomass$Month, levels = month.abb)
p1 <- ggplot(biomass, aes(x=Month,
                              y=Production, 
                              color= factor(Year))) +
  geom_point(aes(), size = 4, fill="white") +
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+
  theme_bw() +
  geom_line(aes(group = Year)) +
  
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) + 
  theme(legend.position = c(0.9, 0.83)) +
  theme(legend.text = element_text(color = "black", size = 12))+
  
 # ylim(0,11.5) +
  
  xlab('')+ ylab("Production ("*mg~AFDM~m^-2*")") +
  
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) +
  theme(axis.text.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1


# Emergent biomass --------------------------------------------------------

biomass
biomass$Month = factor(biomass$Month, levels = month.abb)
p2 <- ggplot(biomass, aes(x=Month,
                          y=Emergent, 
                          color= factor(Year))) +
  geom_point(aes(), size = 4, fill="white") +
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+
  theme_bw() +
  geom_line(aes(group = Year)) +
  
  theme(legend.position="none") +
  ylim(0,3) +
  
  xlab('Month')+ ylab("Biomass ("*mg~AFDM~m^-2*")") +
  
  theme(axis.title.x = element_text(size = 12, angle = 0)) + # axis x
  theme(axis.title.y = element_text(size = 12, angle = 90)) + # axis y
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
p2

Graph <- p1/p2 + plot_annotation(tag_levels = 'A')
Graph

Graph + ggsave("Figure 2a.jpeg",width=6, height=6,dpi=600)
