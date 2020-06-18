
library(lme4)
library(nlme)
library(lmerTest)
library(sjstats)

# var(resid(f0))*99/90
# https://stats.stackexchange.com/questions/114944/mixed-model-with-lmer-variance-of-residuals-should-give-the-same-as-level-1-var
# mse 
# https://strengejacke.github.io/sjstats/reference/rmse.html


##### First Wing #####

# First Wing Area Female --------------------------------------------------
# FWAF= First Wing Area Female

FWAF.frm=read.csv("Asymmetries/FirstWAreaFemale.csv")
attach(FWAF.frm)
FWAF.frm
head(FWAF.frm)

shapiro.test(ValueFWAF)
V1 <- 1/sqrt(ValueFWAF)
shapiro.test(V1)

mod1 <- lmerTest::lmer(V1 ~ Side*Individual + (1|Individual) + (1|Individual:Side), 
                       data= FWAF.frm, REML= FALSE, control =lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
anova(mod1)
mse(mod1)
var(resid(mod1))*99/90
summary(mod1)
rand(mod1)


# First Wing Length Female ------------------------------------------------
# FWLF= First Wing Length Female

FWLF.frm=read.csv("Asymmetries/FirstWLengthFemale.csv")
attach(FWLF.frm)
FWLF.frm

shapiro.test(ValuesFWLF)
boxplot(ValuesFWLF~Side,data=FWLF.frm)

mod2 <- lmerTest::lmer(ValuesFWLF ~ Side*Individual + (1|Individual) + (1|Individual:Side), 
                   data= FWLF.frm, REML= FALSE, control =lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
anova(mod2)
mse(mod2)
var(resid(mod2))*99/90
summary(mod2)
rand(mod2)


# First Wing Area Male --------------------------------------------------
# FWAM = First Wing Length Male

FWAM.frm=read.csv("Asymmetries/FirstWAreaMale.csv")
attach(FWAM.frm)
FWAM.frm
head(FWAM.frm)

shapiro.test(ValueFWAM)
V3<-1/(ValueFWAM)
shapiro.test(V3)

mod3 <- lmerTest::lmer(V3 ~ Side*Individual + (1|Individual) + (1|Individual:Side), 
                       data= FWAM.frm, REML= FALSE, control =lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
anova(mod3)
mse(mod3)
var(resid(mod3))*99/90
summary(mod3)
rand(mod3)



# First Wing Length Male ------------------------------------------------
# FWAM = First Wing Length Male

FWLM.frm=read.csv("Asymmetries/FirstWLengthMale.csv")
attach(FWLM.frm)
FWLM.frm

shapiro.test(ValueFWLM)

mod4 <- lmerTest::lmer(ValueFWLM ~ Side*Individual + (1|Individual) + (1|Individual:Side), 
                       data= FWLM.frm, REML= FALSE, control =lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
anova(mod4)
mse(mod4)
var(resid(mod4))*99/90
summary(mod4)
rand(mod4)


####################
###### Forceps #####
####################

Forceps.frm=read.csv("Asymmetries/Forceps.csv")
attach(Forceps.frm)
Forceps.frm

shapiro.test(Value)
boxplot(Value~Side,data=Forceps.frm)


# p value used = lmerTest::lmer
fit2 <- lmerTest::lmer(Value ~ Side * Individual + (1|Individual) + (1|Individual:Side), 
                   data= Forceps.frm, REML= FALSE, control =lmerControl(check.conv.singular = .makeCC(action = "ignore",  tol = 1e-4)))
anova(fit2)
mse(fit2)
var(resid(fit2))*99/90
summary(fit2)
rand(fit2)

ranef (fit2)



