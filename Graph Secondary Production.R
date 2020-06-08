



#--------------------------------------------
# Production graph
# 06 Jun 2020
#PEGF
#--------------------------------------------
#

library(gg.gap)
library(ggpubr)
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

Biomass$Month = factor(Biomass$Month, levels = month.abb)
p1 <- ggplot(Biomass, aes(x=Month,
                              y=First, 
                              color=Type)) +
  geom_point(aes(), size = 4, fill="white")+
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+
  theme_bw() +
  geom_line(aes(group = Type)) +
  
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) + 
  theme(legend.position = c(0.87, 0.86)) +
  theme(legend.text = element_text(color = "black", size = 12))+
  
  scale_y_continuous(limits = c(0, 10), breaks = seq(0, 10, by = 2))+
  
  xlab('')+ ylab('')+
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) +
  theme(axis.text.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1


# Biomass 2003 ------------------------------------------------------------

biomass2003$Month = factor(biomass2003$Month, levels = month.abb)
p2 <- ggplot(Biomass, aes(x=Month,
                              y=Second, 
                              color=Type)) +
  geom_point(aes(), size = 4, fill="white") +
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+
  theme_bw() +
  geom_line(aes(group = Type)) +
  xlab('Month') + ylab('') + # #Axis 
  
  theme(legend.position="none") +
  
  scale_y_continuous(limits = c(0, 17), breaks = seq(0, 17, by = 4))+
  
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black"))+ #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))+# #subaxis y
  
  theme(axis.title.x = element_text(color="black", size=12))+ # #Axis x
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(p2)

##### Figure 1 #####

Figure1 <- ggarrange(p1 + rremove("x.text") , p2 , align = "v",
                     labels = c("A", "B"),font.label = list(size = 12,face= "plain",color = "black"),
                     ncol = 1, nrow = 2)


Figure1. <-annotate_figure(Figure1,
                           left = text_grob("Biomass ("*mg~AFDM~m^-2*")", rot = 90,
                                            color = "Black", face = "bold", size = 12))
Figure1.
Figure1. + ggsave("Figure 2.jpeg",width=6, height=6,dpi=600)


