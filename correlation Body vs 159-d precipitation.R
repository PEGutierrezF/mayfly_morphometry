



# section lable -----------------------------
# 22 Mar 2020
#PEGF
#--------------------------------------------
#

BiocManager::install("annotatr")

library(gg.gap)
library("ggpubr")
require("nlme")
library(ggplot2)
library(dplyr)
library("reshape2")
library("forcats")
library("gridExtra")
library("BiocManager")

##### Biomass ##########

# Biomass 2002 ------------------------------------------------------------

biomass2002<- read.csv("biomass2002.csv")
biomass2002

biomass2002$Month = factor(biomass2002$Month, levels = month.abb)

p1 <- ggplot(biomass2002, aes(x=Month,
                               y=Biomass, 
                               color=Type)) +
  geom_point(aes(), size = 4, fill="white")+
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+
  theme_bw() +
  geom_line(aes(group = Type)) +
  
  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) + 
  theme(legend.position = c(0.87, 0.86)) +
  theme(legend.text = element_text(color = "black", size = 12))+
  
  ylim(0,11.5) +
  
  xlab('')+ ylab('')+
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) +
  theme(axis.text.x = element_blank())+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
p1


# Biomass 2003 ------------------------------------------------------------

biomass2003 <- read.csv("biomass2003.csv")
biomass2003

biomass2003$Month = factor(biomass2003$Month, levels = month.abb)

p2 <- ggplot(biomass2003, aes(x=Month,
                           y=Biomass, 
                           color=Type)) +
  geom_point(aes(), size = 4, fill="white") +
    scale_color_manual(values=c("#2166ac", "#FF7F00"))+
    theme_bw() +
  geom_line(aes(group = Type)) +
  xlab('Month') + ylab('') + # #Axis 
  
  theme(legend.position="none") +
  
  ylim(0,25) +

  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black"))+ #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))+# #subaxis y
  
  theme(axis.title.x = element_text(color="black", size=12))+ # #Axis x
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
print(p2)

##### Figure 1 #####

Figure1 <- ggarrange(p1 + rremove("x.text") , p2 , 
                     labels = c("A", "B"),font.label = list(size = 12,face= "plain",color = "black"),
                     ncol = 1, nrow = 2)


Figure1. <-annotate_figure(Figure1,
                left = text_grob("Biomass ("*mg~AFDM~m^-2*")", rot = 90,
                                 color = "Black", face = "bold", size = 12))

Figure1. + ggsave("Figure 2.jpeg",width=6, height=6,dpi=600)


# Correlation between body length and 159-d precipitation -----------------

correlation<- read.csv("Correlation.csv")
correlation

cor.test(correlation$Prec159,correlation$Length,method = "spearman", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales

c2 <- ggplot(correlation, aes(x=Prec159,
                           y=Length)) +
  geom_point() + 
  geom_smooth(method=lm,se=TRUE,colour="black", size=0.5) +
  
  xlab("159-d precipitation (mm)") + ylab("Body length (mm)") +
  theme(axis.title.y = element_text(size = 12, angle = 90)) +
  theme(axis.title.x = element_text(size = 12, angle = 00)) +

  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
print(c2)

c2 + ggsave("Figure3.jpeg",width=6, height=4,dpi=600)


