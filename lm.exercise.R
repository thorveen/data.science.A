#
# two exercises to better understand the lm output 
# 1) regression analysis
# 2) anova analysis
#


# ==============================================================
# 1) regression analysis
# ==============================================================

getwd()
# --- a
# read the data "data.regression.csv" and make a plot
# pch = 16 and make the dots red
list.files()
d <- read.csv("data.regression.csv", stringsAsFactors = FALSE)
str(d)

plot(d$age, d$hormone, pch = 16, col = "red", las= 1, xlab = "age", ylab = "hormone level")

# --- b
# plot the simplest model, intercept only
m0 <- lm(d$hormone ~ 1)
summary(m0)
abline(m0)


# calculate the mean and plot is as a grey line, of thickness 2 and dashed (lty = ....)
mean(d$hormone)
lines(range(d$age), rep(mean(d$hormone), 2), col = "grey", lwd = 2, lty = "dashed")

# check the mean with the model output
m0$coefficients - mean(d$hormone)

# get an idea of how much variance is explianed using the residuals
str(m0)
rs.sq.m0 <- sum(m0$residuals^2)


# --- c
# do  linear regression and fit the slope as well
m1 <- lm(d$hormone ~ d$age)
summary(m1)
# plot the fitted line and make it grey
abline(m1, col = "purple")

# use the same measure as above to check if more or less variance is explained
(rs.sq.m1 <- sum(m1$residuals^2)) # 2.656717
rs.sq.m0                          # 44.45072

# --- d
# use the anova() function on both models and interpret what this says
# anova(model1, model2)

anova(m0, m1)

# ---
model.matrix(m1)

# ==============================================================
# 2) anova analysis
# ==============================================================
# run the code below to make a test data set
# ---- test data set ----
# make a data set and test the anova
weight.t <- c(4, 10, 3)
sd.t <- c(1,1, 1)
n.t <- c(10, 10, 15)
names.t <- c("bird", "mammals", "fish")
animal.t <- rep( names.t, n.t)

# make data sets
data.t <- c( rnorm( n.t[1], weight.t[1], sd.t[1]), rnorm(n.t[2], weight.t[2], sd.t[2]), rnorm(n.t[3], weight.t[3], sd.t[3]) )
d <- as.data.frame(cbind(animal.t, data.t), stringsAsFactors = FALSE)
str(d)
colnames(d) <- c("animal", "weight")
d$weight <- as.numeric(d$weight)
# -----------

# --- a
# make a plot where each group is seperated (like stripchart)
# points() and make different colour for each of the groups


# add the eman for each group and the overal mean


# --- b
# run an anova (lm()) and check the output and link it with the figure


# check the dummy variables using model.matrix()


# --- c
# what do the different t tests respresent?
# Think of a way to figure this out and perform the tests


# --- d 
# perform a means model and interpret the output



# --- e what tests are done? redo them



# lets add a second vexplanatory variable, sex
# e.g. to data frame d: d$sex <- rep( c("male", "female"), sum(n.t) / 2)
# --- f
# check models with and without the variables using summary

# --- g
# anova fits the variables sequentially ("type I sum of squares") in R as default
# The order of variables matter! But only of the design is not balanced.
# other programs do sequentially fitting ("type III sum of squares")
# you can use the car package to test this, or you can do it manualy 
# check this effect but re-arranging the order of the explanatory variables.



# --- g 
# use anova() to compare models 



# load the car package  
# --- h try the Anova() for the Type III method. 
# Anova(model.output, type = 3)
library(car)



