library(MASS)
data(Boston)

?Boston
Y=as.matrix(Boston$medv)
X=as.matrix(Boston[,c('crim','nox','rm','age','dis')])

# part (a)
#png("scatter_a.png")
dfrm <- data.frame(Y, X)
plot(dfrm)
# dev.off()

X=cbind(rep(1,dim(Boston)[1]),X)

# part (b)
qrx <- qr(X)
qrx$rank

# part (c)
beta.hat <- solve(t(X) %*% X) %*% t(X) %*% Y
beta.hat
Y.hat <- X %*% beta.hat;
head(Y.hat)
e <- Y - Y.hat
head(e)

# part (d)
png("resids_d.png")
plot(Y.hat, e, main="Residuals Against the Fitted Means", xlab="Fitted Means", ylab = "Residuals")
dev.off()

# part (e)
png("normplot_e.png")
qqnorm(e, ylab = "Sample Quantiles for the Residuals", main="Normal Q-Q Plot for the Residuals")
dev.off()

# part (f)
sigma.squared.hat <- (t(e) %*% e) / (506 - 6)
sigma.squared.hat

# part (f)
m1=lm(medv~crim+nox+rm+age+dis,data=Boston)
m1$coeff # agrees with beta.hat computed earlier
r <- m1$residuals  # agrees with e computed earlier
m1$df.residual     # agrees with 506 - rank(X)
fitted(m1)
residuals(m1)
df.residual(m1)
