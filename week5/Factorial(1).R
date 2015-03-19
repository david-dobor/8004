time=factor(rep(c(3,6),each=5))
temp=factor(rep(c(20,30,20,30),c(2,3,4,1)))
y=c(2,5,9,12,15,6,6,7,7,16)
d=data.frame(time,temp,y)
d


## plotting
means=tapply(d$y,list(d$time,d$temp),mean)
x.axis<-unique(d$time)

matplot(c(3,6),c(2,16),type="n",xlab="Time",ylab="Mean
Response",main="Amount of Active Ingredient")

matlines(x.axis,means,type="b")

## end of plotting

options(contrasts=c("contr.treatment","contr.treatment")) # default R constraints

#######################################################
## other full rank design matrices
##options(contrasts=c("contr.sum","contr.sum")) # sum constraints
##options(contrasts=c("contr.SAS","contr.SAS")) # SAS constraints
####################################################

## linear model fitting
o=lm(y~time+temp+time:temp,data=d)
# or  o=lm(y~time*temp,data=d)


model.matrix(o)
coef(o)
vcov(o)





#Cell means are
#
#          temp 20   temp 30
#          -------------------------
# time 3   mu        mu+temp30
# time 6   mu+time6  mu+time6+temp30+time6:temp30
#
#Time main effects?
#
#   (mu+mu+temp30)/2
#  -(mu+time6+mu+time6+temp30+time6:temp30)/2
#   ----------------------------
#  -time6-time6:temp30/2
#
#  H0:time6+time6:temp30/2=0


test=function(lmout,C,d=0){
  b=coef(lmout)
  V=vcov(lmout)
  dfn=nrow(C)
  dfd=lmout$df
  Cb.d=C%*%b-d
  Fstat=drop(t(Cb.d)%*%solve(C%*%V%*%t(C))%*%Cb.d/dfn)
  pvalue=1-pf(Fstat,dfn,dfd)
  list(Fstat=Fstat,pvalue=pvalue)
}


o$coef



#Cell means are
#
#          temp 20   temp 30
#          -------------------------
# time 3   mu        mu+temp30
# time 6   mu+time6  mu+time6+temp30+time6:temp30
#
#Time main effects?
#
#   (mu+mu+temp30)/2
#  -(mu+time6+mu+time6+temp30+time6:temp30)/2
#   ----------------------------
#  -time6-time6:temp30/2
#
#  H0:time6+time6:temp30/2=0


Ctime=matrix(c(
0,1,0,.5
),nrow=1,byrow=T)


#Temperature main effect?

Ctemp=matrix(c(
0,0,1,.5
),nrow=1,byrow=T)

Ctimetempint=matrix(c(
0,0,0,1
),nrow=1,byrow=T)


test(o,Ctime)
test(o,Ctemp)
test(o,Ctimetempint)

#The R function anova will produce tests for
#the presence of time main effects,
#temp main effects, and time-by-temp interaction.
#However, these are "Type I Tests" that do not
#in general match the "Type III Tests" above.

anova(o)

## the Anova in car package does it properly
library(car)
Anova(o,type='II')



Coverall=matrix(c(
0,1,0,0,
0,0,1,0,
0,0,0,1
),nrow=3,byrow=T)

test(o,Coverall)


#Other choices for C can provide the same test.

Coverall=matrix(c(
0,0,1,0,
0,0,1,1,
0,1,0,0
),nrow=3,byrow=T)

test(o,Coverall)


#Any difference between the 3 month, 20 degree mean
#and the average of the other three treatment means?
#
#          temp 20   temp 30
#          -------------------------
# time 3   mu        mu+temp30
# time 6   mu+time6  mu+time6+temp30+time6:temp30
#
#
# 2*time6/3+2*temp30/3+time6:temp30/3

C=matrix(c(
0,2/3,2/3,1/3
),nrow=1,byrow=T)

test(o,C)

############# Reduced versus full

full=lm(y~time+temp+time:temp,data=d)
reduced=lm(y~1,data=d)

#############  Reduced versus full comparison function

rvsf=function(reduced,full)
{
  sser=deviance(reduced)
  ssef=deviance(full)
  dfer=reduced$df
  dfef=full$df
  dfn=dfer-dfef
  Fstat=(sser-ssef)/dfn/
        (ssef/dfef)
  pvalue=1-pf(Fstat,dfn,dfef)
  list(Fstat=Fstat,dfn=dfn,dfd=dfef,pvalue=pvalue)
}

rvsf(reduced,full)
anova(reduced,full)


Cinteraction=matrix(c(
0,0,0,1
),nrow=1,byrow=T)

test(full,Cinteraction)
anova(full)


additive=lm(y~time+temp,data=d)
model.matrix(additive)
coef(additive)

rvsf(additive,full)
anova(additive,full)










