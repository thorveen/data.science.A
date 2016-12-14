#
#
#
#
# data used from Dwyer, Fensham et al. 2010 
# example from Buckley Chapter 6 in Ecological Statistics, Gordon Fox
#

wd <- getwd()
list.files()
# load the daat from day 12
d <- read.csv(paste(wd, "/day 13/death.tree.csv", sep = ""), stringsAsFactors = FALSE)
str(d)
head(d)

# how many sites are there?
length(unique(d$site))
# 3
#"let's simplify this and get one site. # 2
d1 <- d[d$site == 2,]

# how many records are death (0) and survived (1)? 
table(d1$live)

# let's look into the realtionship between death and diamter
plot(d1$diam, d1$live)
# hard to tell apart.
# make symbol fill and semitransparant

col.t <- c("#000000")
alpha.t <- 77

plot(d1$diam, d1$live, col = paste(col.t, alpha.t, sep = ""), pch = 16, ylab = "death-survive",xlab = "diameter (cm)")
# as we know we only have 0' and 1's we can 'jitter the y values a little'
jitter.val <- 0.2




plot(d1$diam, jitter(d1$live, jitter.val), col = paste(col.t, alpha.t, sep = ""), pch = 16, las = 1,ylab = "death-survive",xlab = "diameter (cm)")

# check this graph, what patter does this look like?


# choose a link function. 0 and 1 data 
# binomial



glm.0 <- glm(live ~ diam, data = d1, family = binomial)
# same as family = binomial(link = "logit")

summary(glm.0)
# note that the estimates are still on the logit scale

# dispersion = residual deviance / residual DF => should be close to 1
glm.0$deviance / glm.0$df.residual
# 1.179295 => very close to 1
# or coef()
coef(glm.0)


# let's plot the fitted lines (this is already transformed onto the original scale)




lines(d1$diam, glm.0$fitted.values)

# small steps to make it nice and smooth



diam.range.for.predict <- seq(min(d1$diam), max(d1$diam), 0.1)
pred.response <- predict(glm.0, list(diam = diam.range.for.predict), type = "response")
lines(diam.range.for.predict, pred.response, col = "red")

# manually do model reduction



glm.1 <- glm(live ~ 1, data = d1, family = binomial)
summary(glm.1)

anova(glm.0, glm.1, test = "Chisq")
# significant, so do not exclude diameter from the model

# what about the AIC scores?
# 328.31 vs 354.16

# what is the diameter at which 50% of the individuals expected to die?
summary(glm.0)
# - intercept/ slope
- (-1.72924)/ 0.09611

# think of a way to do this more nicely using model output



library(MASS)
dose.p(glm.0, p = 0.5)

# add a line to show this, both horizontal and vertical

x.5 <- dose.p(glm.0, p = 0.5)[1]
y.5 <- 0.5
# where to start on the X and the Y
x.start <- min(d1$diam)
y.start <- 0
# vertical lines, same x, different y
lines(rep(x.5, 2), c(y.start, y.5), col = "grey", lty = "dashed")
# horizontal
lines(c(x.start, x.5), rep(y.5, 2), col = "grey", lty = "dashed")


