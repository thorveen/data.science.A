#
#
# example to do a binomial GLM
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

# method two
xt <- factor(d$site)
str(xt)
levels(xt) <- c("forest", "near.hotel", "forgot.where")
length(unique(xt))

# 3
#"let's simplify this and get one site. # 2
d1 <- d[d$site == 2,]

# how many records are death (0) and survived (1)? 
length(d1$live[d1$live == 0])
length(d1$live[d1$live == 1])

table(d1$live)




# let's look into the relationship between death and diamwter
plot(d1$diam, d1$live)


# hard to tell apart.
# make symbol fill and semitransparant
?jitter

# fancy graph
jitter.t <- c(0.2, 0.5)
col.t <- c("#FF0000", "#00FF00")
alpha.t <- c(50, "FF")
plot(d1$diam, jitter(d1$live, jitter.t[1]), pch = 16, col = paste(col.t[1], alpha.t[1], sep =""), xlab ="diameter", ylab = "survival" ,las = 1)

# check this graph, what patter does this look like?


# choose a link function. 0 and 1 data 
# binomial

glm.0 <- glm(live ~ diam, data = d1, family = binomial)
# same as family = binomial(link = "logit")

# check what the model output looks like
summary(glm.0)

# plot the linear values (these are the transformed values from the model output above)
x.r <- range(d1$diam)
y.val <- c((-1.72924 + 0.09611 * x.r[1]), (-1.72924 + 0.09611 * x.r[2]) )
lines(x.r, y.val)
# note that the estimates are still on the logit scale

# the variance of the data may not match that assumed under the distribution (link function)
# selected. Therefore check for over (more than predicted) and under(less variance than expected)
# dispersal. To calculate the dispersion ration:
#  residual deviance / residual DF => should be close to 1
glm.0$deviance / glm.0$df.residual
# 1.179295 => very close to 1, good! If deviation are large, you have a problem
# 
# if overdispersed you need to choose other link function
# also know as 'quasi xxx' function.
# e.g. quasibinomial or quasipoisson

# let's plot the fitted lines (this is already transformed onto the original scale)
str(glm.0)            # what is stored in the model object we saved.
glm.0$fitted.values   # gives the the fitted values from the model
# add fitted lines to the plot
lines(d1$diam, glm.0$fitted.values, col = "red", lwd = 3)
# sometimes the fitted line is not very smooth depeding on the space between fitted values
# generate predcited values with small steps to make it nice and smooth using predict()
# range of x values for which I want to predicted value
diam.range.for.predict <- seq(min(d1$diam), max(d1$diam), 0.1)
pred.response <- predict(glm.0, list(diam = diam.range.for.predict), type = "response")
lines(diam.range.for.predict, pred.response, col = "red", lwd = 1)
#in this case the lines are very similar so was probably not worth it.

# ---- model reduction -----
# manually do model reduction
glm.1 <- glm(live ~ 1, data = d1, family = binomial)
summary(glm.1)
# comapre the full model, glm.0 with the reduced model (glm.1)
anova(glm.0, glm.1, test = "Chisq")
# significant, so do not exclude diameter from the model


# --- EXTRA ----
# what about the AIC scores?
# 328.31 vs 354.16

# what is the diameter at which 50% of the individuals expected to die?
summary(glm.0)
# - intercept/ slope
- (-1.72924)/ 0.09611

# or use

library(MASS)
dose.p(glm.0, p = 0.5)

# think of a way to do this more nicely using model output
# add a line to show this, both horizontal and vertical

x.5 <- dose.p(glm.0, p = 0.5)[1]
y.5 <- 0.5
# where to start on the X and the Y
x.start <- min(d1$diam)
y.start <- 0
# vertical lines, same x, different y
lines(rep(x.5, 2), c(y.start, y.5), lty = "dashed", lwd = 0.5)
# horizontal
lines(c(x.start, x.5), rep(y.5, 2), lty = "dashed", lwd = 0.5)

# ----the end -------




