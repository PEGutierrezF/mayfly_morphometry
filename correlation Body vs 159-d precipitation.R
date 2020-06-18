



#--------------------------------------------
# Correlation body length vs hydrology
# 17 Jun 2020
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


