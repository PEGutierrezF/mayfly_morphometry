



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

Rainfall$Month = factor(Rainfall$Month, levels = month.abb)
r1 <- ggplot(Rainfall, aes(x=Month,
                               y=Precipitation,
                               colour = as.factor(Year))) +
  
  geom_point(aes(), size = 4) +
  geom_line(aes(group =as.factor(Year)))+
  scale_color_manual(values=c("#2166ac", "#FF7F00"))+

  xlab('Month')+ ylab("Total monthly \n precipitation (mm)") +
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  ylim(0,1100) +

  theme(legend.title = element_blank()) +
  theme(legend.key=element_blank()) + 
  theme(legend.position = c(0.9, 0.83)) +
  theme(legend.text = element_text(color = "black", size = 12))+
  
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

print(r1)


# Correlation Precipitation vs Hydrology ----------------------------------

# 2002 correlation --------------------------------------------------------

FirstY <- Rainfall[1:12,]
FirstY
shapiro.test(FirstY$Daily)
shapiro.test(FirstY$Discharge)

cor.test(FirstY$Daily,FirstY$Discharge,method = "spearman", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales


# 2003 correlation --------------------------------------------------------

SecondY <- Rainfall[13:24,]
SecondY
shapiro.test(FirstY$Daily)
shapiro.test(FirstY$Discharge)
cor.test(SecondY$Daily,SecondY$Discharge,method = "spearman", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales


# All years ---------------------------------------------------------------

cor.test(Rainfall$Daily,Rainfall$Discharge,method = "spearman", exact=F)

r2 <- ggplot(Rainfall, aes(x=Daily,
                               y=Discharge)) +
  geom_point() + 
  geom_smooth(method=lm,se=TRUE,colour="black", size=0.5) +
  
  xlab('Precipitation (mm)')+ ylab(expression(Discharge~(m~s^-3))) +
  theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis x
  theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) + #subaxis y
  
  theme(legend.title = element_blank()) +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
        panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))

print(r2)


r3 <- (r1 / r2) + plot_annotation(tag_levels = 'A')
r3
r3 + ggsave("Figure1.jpeg",width=6, height=6,dpi=600)






