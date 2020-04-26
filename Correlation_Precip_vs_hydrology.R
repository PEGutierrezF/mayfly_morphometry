



#--------------------------------------------
# Correlation Precipitation vs hydrology 
# 25 Apr 2020
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

Rainfall<- read.csv("Precip_vs_hydrology.csv")
Rainfall


p1 <- ggplot(Rainfall, aes(x=Month,
                               y=Precipitation,
                               colour = as.factor(Year))) +
  geom_point(aes(shape =as.factor(Year)), size = 4) +
  geom_line(aes(group =as.factor(Year)))+
  scale_color_manual(values=c("#D55E00", "#0072B2"))+
  
  xlab('Month')+ ylab('Total precipitation (mm)') +
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

print(p1)


# Correlation Precipitation vs Hydrology ----------------------------------

# 2002 correlation --------------------------------------------------------

FirstY <- Rainfall[1:12,]
FirstY
shapiro.test(FirstY$MeanPrecipitation)
shapiro.test(FirstY$Discharge)

cor.test(FirstY$MeanPrecipitation,FirstY$Discharge,method = "pearson", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales


# 2003 correlation --------------------------------------------------------

SecondY <- correlations[13:24,]
SecondY
shapiro.test(FirstY$Precipitation)
shapiro.test(FirstY$Temperature)
cor.test(SecondY$Temperature,SecondY$Precipitation,method = "pearson", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales


# All years ---------------------------------------------------------------

cor.test(Rainfall$MeanPrecipitation,Rainfall$Discharge,method = "pearson", exact=F)

p2 <- ggplot(Rainfall, aes(x=MeanPrecipitation,
                               y=Discharge)) +
  geom_point() + 
  geom_smooth(method=lm,se=FALSE) +
  
  xlab('Precipitation (mm)')+ ylab('Discharge (m/S^3') +
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

print(p2)


(p1 / p2)





