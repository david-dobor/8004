#Example code for simulating data from our
#classic split plot example.

block=factor(rep(1:4,each=12))
geno=factor(rep(rep(1:3,each=4),4))
fert=rep(seq(0,150,by=50),12)

X=model.matrix(~geno+fert+I(fert^2)+geno:fert)
beta=c(125,15,-10,.4,-0.0015,0,.2)

Z1=model.matrix(~0+block)
Z2=model.matrix(~0+geno:block)
Z=cbind(Z1,Z2)

#The code below generates the random effects
#and random errors and assembles the response
#vector. The function set.seed is used to
#control the random number generator so that
#the same random effects and errors will be
#generated each time this code is called.
set.seed(532)
u=c(rnorm(4,0,6),rnorm(12,0,7))
e=rnorm(48,0,6)
y=X%*%beta+Z%*%u+e
y=round(y,1)

d=data.frame(block,geno,fert,y)
View(d)

#ANOVA-based analysis
o=lm(y~block+geno+block:geno+factor(fert)+geno:factor(fert))
anova(o)
a=as.matrix(anova(o))
a
#ANOVA estimates of variance components:
#Estimate of sigma^2_e
MSE=a[6,3]
MSE

#Estimate of sigma^2_w
MSBlockGeno=a[4,3]
(MSBlockGeno-MSE)/4 

#Save the square roots of these estimates
#for comparison with REML estimates computed
#later.
sige=sqrt(MSE)
sigw=sqrt((MSBlockGeno-MSE)/4)

#F test for genotype main effects
MSGeno=a[2,3]
Fstat=MSGeno/MSBlockGeno
Fstat

pval=1-pf(Fstat,a[2,1],a[4,1])
pval


#95% confidence interval for geno 2 - geno 1
gmeans=tapply(y,geno,mean)
gmeans

est=gmeans[2]-gmeans[1]
names(est)=NULL

#We showed previously that the variance of
#the difference between genotype means
#is 2*E(MS_block*geno)/(nblocks*nferts)

#Thus, we compute a standard error as
se=sqrt(2*MSBlockGeno/(4*4))
lower=est-qt(.975,a[4,1])*se
upper=est+qt(.975,a[4,1])*se
c(estimate=est,se=se,lower=lower,upper=upper)



#REML analysis via lme
library(nlme)
#Below I create f and g factors to shorten
#code and the names that R assigns to the
#elements of beta hat.
f=factor((fert+50)/50)
f

g=geno
o=lme(y~g*f,random=~1|block/g)
o

#Note that the REML estimates of standard deviation
#match the ANOVA estimates computed
#from lm output.
sigw
sige

#The ANOVA table computed from lme output
#automatically gives the correct tests for
#genotype, fertilizer, and
#genotype by fertilizer interaction for
#the balanced data case.
anova(o)

#The GLS estimate of the fixed effect
#parameter beta is obtained as follows.
fixed.effects(o)

#The estimated variance covariance matrix of
#the GLS estimator is obtained as follows.
vcov(o) 

























