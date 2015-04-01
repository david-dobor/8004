library(lme4)
library(nlme)


grass <- read.table("grass.dat", 
   col.names=c("Block","Culti","Innoc","Yield"))
grass$Block <- as.factor(grass$Block)
grass$Culti <- as.factor(grass$Culti)
grass$Innoc <- as.factor(grass$Innoc)


# Multistratum models may be fitted using aov() if
# the design is balanced and all factors are random. 
# Models are specified by a model formula
#
#  response ~ mean.formula + Error(strata.formular)

summary(grass.aov <- aov(Yield ~ Culti*Innoc + 
          Error(Block/Culti),data=grass))


o1=lm(Yield~Culti*Innoc+Block*Culti,data=grass)
anova(o1)



# Use the lme functions to fit the split-plot
# model with random block effects.  

options(contrasts=c(factor="contr.treatment",
                    ordered="contr.poly"))

o2=lmer(Yield~Culti*Innoc+(1|Block)+(1|Block:Culti),data=grass)
anova(o2)
ranef(o2)
plot(o2,resid(.,type='pearson',scaled=T)~fitted(.))
plot(o2,Yield~fitted(.))

qqnorm((ranef(o2)[[1]])[,1])
qqline((ranef(o2)[[1]])[,1])

qqnorm((ranef(o2)[[2]])[,1])
qqline((ranef(o2)[[2]])[,1])


o3=lme(Yield~Culti*Innoc,random=~1|Block/Culti,data=grass)
anova(o3)



