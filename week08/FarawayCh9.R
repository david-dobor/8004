library(faraway)
library(lme4)

# The acuity of vision for seven subjects was tested. 
# The response is the lag in milliseconds between a 
# light flash and a response in the cortex of the eye. 
# Each eye is tested at four different powers of lens.
data(vision)
?vision

# start by making a few plots
vision$npower <- rep(1:4,14)
xyplot(acuity~npower|subject,vision,type="1",groups=eye,lty=1:2,layout=c(4,2))

# Decide how to model the data. 
# The power is a fixed effect.
# The subjects are random effects.
# The eye factor is nested within subjects.
# y_{ijk} = \mu + P_j + s_i + e_{ik} + \epsilon_{ijk}

mmod <- lmer(acuity~power+(1|subject)+(1|subject:eye),vision)
summary(mmod)

# The random effects structure we have used here induces
# a correlation between measurements on the same subject 
# and another between measurements on the same eye.

# Compute these two correlations as:
4.64^2/(4.64^2+3.21^2+4.07^2)            #ans: 0.4448393

(4.64^2+3.21^2)/(4.64^2+3.21^2+4.07^2)   #ans: 0.6577401
# there is a stronger correlation between observations 
# on the same eye than between the left and right eyes 
# of the same individual.


## We can check for a power effect:
anova(mmod)





















