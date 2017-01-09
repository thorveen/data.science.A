#
#
#
# understanding model uitput
#
#


#
# ANOVA
#

# make a data set and test the anova
weight.t <- c(4, 10, 3)
sd.t <- c(1,1, 1)
n.t <- c(10, 10, 16)
names.t <- c("bird", "mammals", "fish")
animal.t <- rep( names.t, n.t)

# make data sets
data.t <- c( rnorm( n.t[1], weight.t[1], sd.t[1]), rnorm(n.t[2], weight.t[2], sd.t[2]), rnorm(n.t[3], weight.t[3], sd.t[3]) )
d <- as.data.frame(cbind(animal.t, data.t), stringsAsFactors = FALSE)
str(d)
colnames(d) <- c("animal", "weight")
d$weight <- as.numeric(d$weight)

ylim.extra <- 0.15 * diff(range(d$weight))
ylim.1 <- c(min(d$weight) - ylim.extra, max(d$weight) + ylim.extra) 
xlim.1 <- c(0, length(names) + 1)
jitter.1 <- 8
pch.1 <- 16
col.1 <- c("red", "blue", "black")

plot(NA, ylim = ylim.1, xlim = xlim.1, xaxt = 'n ', las = 1, ylab = "weight", xlab = "species")
axis(1, c(1,2,3), names.t)
for(i in 1:length(names.t)){
  points(jitter( rep(i, length(d$weight[d$animal == names.t[i]])) , jitter.1), d$weight[d$animal == names.t[i]], pch = pch.1, col = col.1[i])
  lines( c(i - 0.3, i + 0.3), rep( mean(d$weight[d$animal == names.t[i]]), 2))
}

# add horizontal lines for grand mean
lines(xlim.1, rep(mean(d$weight),2), ltw = 3, col = "grey")

par(mfrow=c(1,1))

# --- get ---
# get the means
mean.animals <- tapply(d$weight, list(d$animal), mean)

d$animal <-factor(d$animal)
m1 <- lm(d$weight ~ d$animal)
plot(m1)
hist(resid(m1))

summary(m1)

#  coefficents
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# (Intercept)       4.0015     0.3501  11.429 7.51e-12 ***
# d$animalfish     -0.6327     0.4952  -1.278    0.212    
# d$animalmammals   6.1598     0.4952  12.440 1.08e-12 ***

levels.d <- levels(d$animal)

# (Intercept) first group vs 0
bird <- d$weight[d$animal == levels.d[1]]
t.test(bird)

# d$animalfish
# one sample t test of difference vs 0?
t.test( d$weight[d$animal == levels.d[2]] - mean(bird))
mean(d$weight[d$animal == levels.d[2]] - mean(bird))
# t = -1.8271, df = 9, p-value = 0.101

# or two sample t-test?
t.test(d$weight[d$animal != "mammals"] ~ d$animal[d$animal != "mammals"])
#t = 1.2722, df = 17.983, p-value = 0.2195
# looks like two sample t test

# check for level 1 to 3
t.test(d$weight[d$animal != "fish"] ~ d$animal[d$animal != "fish"])
# t = -12.373, df = 17.986, p-value = 3.109e-10

# --------- dummy variables
model.matrix(m1)

# ---------- means model
m1.means <- lm(d$weight ~ d$animal - 1)
model.matrix(m1.means)
summary(m1.means)

# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
# d$animalbird      4.0015     0.3501  11.429 7.51e-12 ***
# d$animalfish      3.3688     0.3501   9.622 3.23e-10 ***
# d$animalmammals  10.1614     0.3501  29.022  < 2e-16 ***

# series of on sample t tests?
t.test(d$weight[d$animal == "bird"])
# t = 11.21, df = 9, p-value = 1.372e-06
t.test(d$weight[d$animal == "fish"])
# t = 9.7282, df = 9, p-value = 4.498e-06
t.test(d$weight[d$animal == "mammals"])
# t = 29.282, df = 9, p-value = 3.083e-10

# indeed what it does. Differences 

# Add a second explanatory var
d$sex <- rep( c("male", "female"), sum(n.t) / 2)

m2 <- lm(d$weight ~ d$animal + d$sex )
summary(m2)

m2.1 <- lm(d$weight ~  d$sex + d$animal)
summary(m2.1)

m2.2 <- lm(d$weight ~  d$sex )
summary(m2.2)
anova(m2.1, m2.2)
# significantly reduces model fit, should not exclude term animal

m2.3 <- lm(d$weight ~ d$animal)
summary(m2.3)
anova(m2.1, m2.3)
# not significantly affects model fit, exclude the term sex

# type III through car package 
Anova(m2.3, type = 3)


# AIC model reduction
AIC(m2.1)
AIC(m2.3)
# smaller is better
library(MASS)
stepAIC(m2.1)
H# helps you do the model reduction

# =============================
# regression analysis
# =============================

r <- read.csv("data.regression.csv", stringsAsFactors = FALSE)
str(r)
plot(r$age, r$hormone, pch = 16, col = "red" )

# simplest model, only fit an intercept.
mr1.0 <- lm(r$hormone ~ 1)
abline(mr1.0, col = "grey", lty = "dashed", ltw = 2)
summary(mr1.0)
# intercept should be the mean
mean(r$hormone)
# how much variance is there?
sum(mr1.0$residuals^2)


# how much better does fitting the slope help explain variance?
mr1.2 <- lm(r$hormone ~ r$age)
abline(mr1.2, col = "grey")
summary(mr1.2)
sum(mr1.2$residuals^2)  # much better!

# official test through anova
anova(mr1.0, mr1.2)



c()


