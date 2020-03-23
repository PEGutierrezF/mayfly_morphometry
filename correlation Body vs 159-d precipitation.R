



# section lable -----------------------------
# 22 Mar 2020
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

##### Biomass ##########

# Biomass 2002 ------------------------------------------------------------

biomass2002<- read.csv("biomass2002.csv")
biomass2002

biomass2002$Month = factor(biomass2002$Month, levels = month.abb)

p <- ggplot(biomass2002, aes(Month,Biomass, color=Type))
p2 <- p + geom_point(aes(shape =Type), size = 4, fill="white") +
            scale_shape_manual(values=c(16, 17))+
            scale_color_manual(values=c("#D55E00", "#0072B2"))+
            theme_bw()
p3 <- p2 + geom_line(aes(group = Type))
p4 <- p3 + xlab('')+ ylab('')
p5 <- p4 + theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))
p6 <- p5 + theme(legend.title = element_blank()) 
p7 <- p6 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
p8 <- p7 + scale_y_continuous(breaks=seq(0,12,2))
p8


# Biomass 2003 ------------------------------------------------------------

biomass2003 <- read.csv("biomass2003.csv")
biomass2003

biomass2003$Month = factor(biomass2003$Month, levels = month.abb)

p10 <- ggplot(biomass2003, aes(Month,Biomass,color=Type))
p11 <- p10 + geom_point(aes(shape =Type), size = 4, fill="white") +
              scale_shape_manual(values=c(16, 17))+
              scale_color_manual(values=c("#D55E00", "#0072B2"))+
              theme_bw()
p12 <- p11 + geom_line(aes(group = Type))
p13 <- p12 + xlab('Month') + ylab('') # #Axis 
p14 <- p13 + theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) #subaxis x
p15 <- p14 + theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) # #subaxis y
p16 <- p15 + theme(axis.title.x = element_text(color="black", size=12)) # #Axis x
p17 <- p16 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
p17

##### Figure 1 #####

Figure1 <- ggarrange(p8 + rremove("x.text") , p17 , 
          labels = c("A", "B"),font.label = list(size = 12, color = "black"),
          ncol = 1, nrow = 2,
          common.legend = TRUE, legend = "right")


annotate_figure(Figure1,
                left = text_grob("Biomass ("*mg~AFDM/m^2*")", rot = 90,
                color = "Black", face = "bold", size = 12))
Figure1 + ggsave("Figure 1.TIFF",width=6, height=4,dpi=600)


# Correlation between body length and 159-d precipitation -----------------

correlation<- read.csv("Correlation.csv")
correlation

cor.test(correlation$Prec159,correlation$Length,method = "spearman", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales

p20 <- ggscatter(correlation, x = "Prec159", y = "Length", 
          add = "reg.line", conf.int = TRUE, size = 2,
          cor.coef = F, cor.method = "spearman",
          xlab = "159-d precipitation (mm)", ylab = "Body length (mm)")
p20
p21 <- p20 + theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) #subaxis x
p22 <- p21 + theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) #subaxis y
p23 <- p22 + theme(axis.title.x = element_text(color="black", size=12)) # #Axis x
p24 <- p23 + theme(axis.title.y = element_text(color="black", size=12)) # #Axis y
p25 <- p24 + theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5))
p25


Figure2 <- p25 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
                   panel.background = element_blank(), axis.line = element_line(colour = "black"))
Figure2 + ggsave("Figure 2.TIFF",width=6, height=4,dpi=600)


