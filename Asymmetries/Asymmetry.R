
library(lme4)
library(nlme)
library(lmerTest)


##### First Wing #####

# First Wing Area Female
# FWAF= First Wing Area Female

FWAF.frm=read.csv("FirstWAreaFemale.csv")
attach(FWAF.frm)
FWAF.frm

shapiro.test(ValueFWAF)
V1 <- 1/sqrt(ValueFWAF)
shapiro.test(V1)

A.mod=aov(V1~ Side * Individual, data=FWAF.frm)
summary(A.mod)

fit1<-lmer(V1~  Side + (1|Individual), REML=TRUE, data= FWAF.frm)
fit1
summary(fit1)
anova(fit1)
rand(fit1)

# First Wing Length Female
# FWLF= First Wing Length Female

FWLF.frm=read.csv("Asymmetries/FirstWLengthFemale.csv")
attach(FWLF.frm)
FWLF.frm

shapiro.test(ValuesFWLF)
boxplot(ValuesFWLF~Side,data=FWLF.frm)

B.mod=aov(ValuesFWLF~ Side * Individual, data=FWLF.frm)
summary(B.mod)


fit2 <- lme4::lmer(ValuesFWLF ~ Side + (1|Individual) + (1|Side:Individual), data= FWLF.frm, REML= FALSE)
anova(fit2)
summary(fit2)
rand(fit2)

install.packages("Metrics")
library(Metrics)
mse(FWLF.frm$ValuesFWLF, predict(fit2,FWLF.frm))


#  Measurement error ------------------------------------------------------

Error_FWLF.frm=read.csv("FirstWLengthFemaleERROR.csv")
attach(Error_FWLF.frm)
Error_FWLF.frm

shapiro.test(ValuesFWLF)
boxplot(ValuesFWLF~Side,data=FWLF.frm)

B.mod=aov(ValuesFWLF~ Side * Individual, data=Error_FWLF.frm)
summary(B.mod)

hist(ValuesFWLF, data=Error_FWLF.frm)



# First Wing Length Male
# FWAM = First Wing Length Male

FWAM.frm=read.csv("FirstWAreaMale.csv")
attach(FWAM.frm)
FWAM.frm

shapiro.test(ValueFWAM)
V3<-1/(ValueFWAM)
shapiro.test(V3)

C.mod=aov(V3~ Side * Individual + (1 | Individual) ,data=FWAM.frm)
summary(C.mod)


# First Wing Length Male
# FWAM = First Wing Length Male

FWLM.frm=read.csv("FirstWLengthMale.csv")
attach(FWLM.frm)
FWLM.frm

shapiro.test(ValueFWLM)

D.mod=aov(ValueFWLM ~ Side * Individual + (1 | Individual) ,data=FWLM.frm)
summary(D.mod)

##### Hind Wing #####

# Hind Wing Area Female
# HWAF= Hind Wing Area Female

HWAF.frm=read.csv("HindWAreaFemale.csv")
attach(HWAF.frm)
HWAF.frm

E.mod=aov(ValueHWAF~ Side * Individual + (1 | Individual) ,data=HWAF.frm)
summary(E.mod)

# Hind Wing Area Female
# HWLF= Hind Wing Length Female

HWLF.frm=read.csv("HindWLengthFemale.csv")
attach(HWLF.frm)
HWLF.frm

shapiro.test(ValueHWLF)
V4<-1/(ValueHWLF)
shapiro.test(V4)

F.mod=aov(V4~ Side*Individual + (1 | Individual), data=HWLF.frm)
summary(F.mod)

# Hind Wing Area Male
# HWAM= Hind Wing Area Male


HWAM.frm=read.csv("HindWAreaMale.csv")
attach(HWAM.frm)
HWAM.frm

G.mod=aov(ValueHWAM~ Side * Individuo + (1|Individuo),data=HWAM.frm)
summary(G.mod) 


# Hind Wing Length Male
# HWLM= Hind Wing Length Male


HWLM.frm=read.csv("HindWLengthMale.csv")
attach(HWLM.frm)
HWLM.frm

shapiro.test(ValueHWLM)
V5 <- 1/(ValueHWLM)
shapiro.test(V5)

H.mod=aov(V5~Side * factor(Individual) * (1|Individual),data=HWLM.frm)
summary(H.mod)



###### Forceps #####

Forceps.frm=read.csv("Forceps.csv")
attach(Forceps.frm)
Forceps.frm

shapiro.test(Value)

F.mod1=aov(Value~Side*factor(Indiv) + (1|Indiv),data=Forceps.frm)
summary(F.mod1)


fit10<-lmer(Value~ Side * Individual + (1|Individual),REML=TRUE, data=Forceps.frm)
summary(fit10)
anova(fit10)
rand(fit10)

boxplot(Value~Side,data=Forceps.frm)
hist(Value)

