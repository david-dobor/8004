library(lme4)
library(nlme)


temp <- read.table("weight2.dat", 
       col.names=c("Subj","Program","S1",
       "S2","S3","S4","S5","S6","S7"))
temp

attach(temp)
weight <- data.frame(rbind(
		cbind.data.frame(Subj,Program, Time=2,Strength=S1),
		cbind.data.frame(Subj,Program, Time=4,Strength=S2),
		cbind.data.frame(Subj,Program, Time=6,Strength=S3),
		cbind.data.frame(Subj,Program, Time=8,Strength=S4),
		cbind.data.frame(Subj,Program, Time=10,Strength=S5),
		cbind.data.frame(Subj,Program, Time=12,Strength=S6),
		cbind.data.frame(Subj,Program, Time=14,Strength=S7)))

detach()

weight$Subj <- as.factor(weight$Subj)
weight$Program <- as.factor(weight$Program)
weight$Timef <- as.factor(weight$Time)


i <- order(weight$Subj,weight$Time)
weight <- weight[i,]


# Delete the list called i         
rm(i)  


# Compute sample means

means <- tapply(weight$Strength,
    list(weight$Time,weight$Program),mean)
means


#  Make a profile plot of the means


x.axis <- unique(weight$Time)

par(fin=c(6.0,6.0),pch=18,mkh=.1,mex=1.5,
            cex=1.2,lwd=3)
matplot(c(2,14), c(79,85.7), type="n", 
     xlab="Time(Days)", ylab="Strength",
     main= "Observed Strength Means")   
matlines(x.axis,means,type='l',lty=c(1,3,7))
matpoints(x.axis,means, pch=c(16,17,15))    
legend('topleft',legend=c("RI program",
    'WI Program','Controls'),lty=c(1,3,7),col=c(1,2,3),pch=c(16,17,15),bty='n')

m1=lmer(Strength~Program*Timef+(1|Subj),data=weight)
summary(m1)
anova(m1)
confint(m1)

m2=lme(Strength~Program*Timef,
        random=~1|Subj,data=weight)








