#
#
# GLM assignment
#
#
#

# some general parameters

col.t <- c("#660000")
alpha.t <- 60

# ---- start the assignment questions:
# --- read data and make selection 
list.files()

fish.all <- read.csv("data.fish.csv", stringsAsFactors = FALSE)
str(fish.all)
fish <- fish.all[fish.all$year == 2011,]

# --- a
plot(fish$age, fish$number.eggs, pch = 16, col = paste(col.t[1], alpha.t, sep = ""), las = 1, ylab = "number of eggs", xlab = "age")
# looks like a lot of 0 values
hist(fish$number.eggs)
# yes, indeed. Zero inflated data. Problem.
# problems
# - zero inflated data
# - variance seems to increase with age -> not normals

# --- b 
# count data following a poisson distribution

# --- c
# zero inflated data 
# split the test into two parts: one binomial and one poisson data with zeros excluded

# --- d
fish.1 <- fish[fish$number.eggs > 0,]
length(fish.1$number.eggs[fish.1$number.eggs == 0])
# 0, correct

# === method 1 transform and do LM ===

# --- e
fish.1$log.eggs <- log(fish.1$number.eggs)

m.l0 <- lm(fish.1$log.eggs ~ fish.1$age)
summary(m.l0)
plot(m.l0)

# doesnt look too bad but the QQ plot shows a strange pattern

# model reduction
m.l1 <- lm(fish.1$log.eggs ~ 1)
anova(m.l0, m.l1)
# output shows that age significantly increases the amount of variance
# explianed -> keep it in.

# --- f
plot(fish.1$age, fish.1$log.eggs, pch = 16, col = paste(col.t[1], alpha.t, sep = ""), las = 1, ylab = "log(eggs)", xlab = "age")
abline(m.l0, col = "red")

# === method 2 GLM ===
# --- g


# --- h 
m1 <- glm(number.eggs ~ age, family = "poisson", data = fish.1)
summary(m1)

# --- i
plot(fish.1$age, fish.1$number.eggs, pch = 16, col = paste(col.t[1], alpha.t, sep = ""), las = 1, ylab = "number of eggs", xlab = "age")
# use fitted values
lines(fish.1$age[order(fish.1$age)], m1$fitted.values[order(fish.1$age)])
# use predict
age.predict <- seq(min(fish.1$age), max(fish.1$age), 0.1)
pred.response <- predict(m1, list(age = age.predict), type = "response")
lines(age.predict, pred.response, col = "red")


# --- j
m1$deviance / m1$df.residual

# --- k
mq1 <- glm(number.eggs ~ age, family = "quasipoisson" , data = fish.1)
summary(mq1)
pred.response.quasi <- predict(mq1, list(age = age.predict), type = "response")
lines(age.predict, pred.response.quasi, col = "purple")
 # check manually how different they are
pred.response.quasi - pred.response

# --- l
# 
m.full <- glm(number.eggs ~ age + body.length, family = "poisson", data = fish.1)
m.full.1 <- glm(number.eggs ~ body.length + age , family = "poisson", data = fish.1)
summary(m.full)
summary(m.full.1)
# order does not matter

# effect of age
m.1a <- glm(number.eggs ~ body.length, family = "poisson", data = fish.1)
anova(m.full, m.1a, test = "Chisq")
# check for effect of body length
m.1b <- glm(number.eggs ~ age , family = "poisson", data = fish.1)
anova(m.full, m.1b, test = "Chisq")







  