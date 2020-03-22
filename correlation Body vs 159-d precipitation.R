
library(gg.gap)
library("ggpubr")
require("nlme")
library(ggplot2)
library(dplyr)
library("reshape2")
library("forcats")
library("gridExtra")

##### Biomass ##########
setwd ("D:/Curriculum/02_ Articulos/31- Gutierrez-F & Ramirez 2020 PeerJ/New Results/Body_length_Analisys")



# Biomass 2002 ------------------------------------------------------------

biomass2002.frm <- read.csv("biomass2002.csv")
attach(biomass2002.frm)
head(biomass2002.frm)


biomass2002.frm$Month = factor(biomass2002.frm$Month, levels = month.abb)

p <- ggplot(biomass2002.frm, aes(Month,Biomass, color=Type))
p  
p2 <- p + geom_point(aes(shape =Type), size = 4, fill="white") +
            scale_shape_manual(values=c(16, 17))+
          scale_color_manual(values=c("#C90751", "#56B4E9"))+
             theme_bw()
p2
p3 <- p2 + geom_line(aes(group = Type))
p3
p4 <- p3 + xlab('')+ ylab('')
p5 <- p4 + theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black"))
p6 <- p5 + theme(legend.title = element_blank()) 
p7 <- p6 + theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
              panel.background = element_blank(), axis.line = element_line(colour = "black"))
p7
p8 <- p7 + scale_y_continuous(breaks=seq(0,12,2))
p8


# Biomass 2003 ------------------------------------------------------------

biomass2003.frm <- read.csv("biomass2003.csv")
attach(biomass2003.frm)
head(biomass2003.frm)


biomass2003.frm$Month = factor(biomass2003.frm$Month, levels = month.abb)


p10 <- ggplot(biomass2003.frm, aes(Month,Biomass,color=Type))
p10  
p11 <- p10 + geom_point(aes(shape =Type), size = 4, fill="white") +
              scale_shape_manual(values=c(16, 17))+
              scale_color_manual(values=c("#C90751", "#56B4E9"))+
                theme_bw()

p11
p12 <- p11 + geom_line(aes(group = Type))
p12
p13 <- p12 + xlab('Month') + ylab('') # #Axis 
p14 <- p13 + theme(axis.text.x=element_text(angle=0, size=10, vjust=0.5, color="black")) #subaxis x
p15 <- p14 + theme(axis.text.y=element_text(angle=0, size=10, vjust=0.5, color="black")) # #subaxis y
p16 <- p15 + theme(axis.title.x = element_text(color="black", size=12)) # #Axis x
p16
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


correlation.frm <- read.csv("Correlation.csv")
attach(correlation.frm)
head(correlation.frm)

cor.test(Prec_159, Length,  method = "spearman", exact=F) # "exact f" para quitar el error porque no quiere correr porque hay varios numeros iguales

p20 <- ggscatter(correlation.frm, x = "Prec_159", y = "Length", 
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


