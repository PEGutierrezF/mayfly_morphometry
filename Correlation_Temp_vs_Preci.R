



#--------------------------------------------
# Correlation Temperature vs Precipitation 
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


correlations<- read.csv("Temp_vs_Preci.csv")
correlations


# 2002 correlation --------------------------------------------------------

FirstY <- correlations[1:12,]
FirstY
shapiro.test(FirstY$Precipitation)
shapiro.test(FirstY$Temperature)

cor.test(FirstY$Temperature,FirstY$Precipitation,method = "pearson", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales


# 2003 correlation --------------------------------------------------------

SecondY <- correlations[13:24,]
SecondY
shapiro.test(FirstY$Precipitation)
shapiro.test(FirstY$Temperature)
cor.test(SecondY$Temperature,SecondY$Precipitation,method = "pearson", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales


# All years ---------------------------------------------------------------

cor.test(correlations$Temperature,correlations$Precipitation,method = "pearson", exact=F)
shapiro.test(correlations$Temperature)
shapiro.test(correlations$Precipitation)

# Graph -------------------------------------------------------------------

p1 <- ggplot(correlations, aes(x=Precipitation,
                         y=Temperature,
                         colour = as.factor(Year))) +
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)

print(p1)



p2 <- ggplot(correlations, aes(x=Precipitation,
                               y=Temperature)) +
  geom_point() + 
  geom_smooth(method=lm,se=FALSE)

print(p2)

TemperaturevsPrecipitation <- p1/p2
TemperaturevsPrecipitation + ggsave("Correlation_Temp_vs_Precip.jpeg",width=6, height=8,dpi=600)


