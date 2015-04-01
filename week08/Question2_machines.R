################################################################
########################## Question 2 ##########################
################################################################
# from the table:
MSE <- 280/60
MSB_A <- 644/12
MSA <- 1966/2

############################ part(a)############################
sigma2_c <- MSE                   # ans: 4.666667
sigma2_u <- 1/5 * (MSB_A - MSE)   # ans: 9.8
sigma2_a <- 1/25 * (MSA - MSB_A)  # ans: 37.17333

############################ part(b)############################
# for sigma2_c:
alpha <- .05
lower.limit <- 60 * MSE / qchisq(1 - alpha/2, df=60)
upper.limit <- 60 * MSE / qchisq(alpha/2, df=60)
CI_error <- sqrt(c(lower.limit, upper.limit))

# for sigma2_u:
v <- (sigma2_u)^2 / ( ((-MSE/5)^2 / 60)  + (MSB_A/5)^2 / 12 )
lower.limit <- (v*sigma2_u) / qchisq(1 - alpha/2, df=v)
upper.limit <- (v*sigma2_u)/ qchisq(alpha/2, df=v)
CI_u <- sqrt(c(lower.limit, upper.limit))

# for sigma2_a:
v <- (sigma2_a)^2 / ( ((MSA/25)^2 / 2)  + (-MSB_A/25)^2 / 12 )
lower.limit <- (v*sigma2_a) / qchisq(1 - alpha/2, df=v)
upper.limit <- (v*sigma2_a)/ qchisq(alpha/2, df=v)
CI_a <- sqrt(c(lower.limit, upper.limit))

############################ part(c)############################
# 95\% Confidence Interval for \mu:
qt(0.975,df=2)
CI_mu <- c( 35 - qt(0.975,df=2) * sqrt( MSA / 75 ), 35 + qt(0.975,df=2) * sqrt( MSA / 75 ))
CI_mu








