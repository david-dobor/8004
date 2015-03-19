## homework assignment 5
# data from ch 15 (prob 15.9)

#part (a):
y <- c(73,102,118,104,81,107,100,87,
       98,74,56,111,95,88,82,77,86,92,
       94,79,96,98,102,102,
       90,76,90,64,86,51,72,90,95,78,
       107,95,97,80,98,74,
       49,82,73,86,81,97,106)

protein <- factor(c(rep("1",34), rep("2", 13)))  #factor A
meat <- factor(c(rep("1", 8), c(rep("2", 10)), c(rep("3", 6), c(rep("1", 10), c(rep("2", 6),c(rep("3", 7))))))) #factor b: 1: beef, 2: cereal, 3: pork

d <- data.frame(y, protein, meat)

# (b):
means <- tapply(d$y, list(d$meat, d$protein), mean)
means

# (c):
x.axis <- unique(d$meat)
matplot(c(1,3),c(70,110), type="n", xlab="Meat", ylab="Mean Response", main="Weight Gain")
matlines(x.axis, means, type="b")

# (d):
options(conrasts=c("contr.sum", "contr.sum"))

lm.out1 <- lm(y~protein*meat, data=d)
summary.aov(lm.out1, ssType=1)

lm.out2 <- lm(y~meat*protein, data=d)
summary.aov(lm.out2, ssType=1)

# (e)

