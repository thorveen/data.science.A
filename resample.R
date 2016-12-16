#
#
# Bootstrap and permutations
#
#

# get working directory to make path to folder to read data from
wd <- getwd()
# path to foler day 14
p.d14 <- paste(wd, "/day 14/", sep = "")

# ---------------------------------------------------------------------------
# --------------- bootstrapping ---------------------------------------------
# ---------------------------------------------------------------------------
# read the data bootstrap.data.csv

# --- a
# load the data and plot the 'value' parameter.
# Anything odd?

d <- read.csv(paste(p.d14, " bootstrap.data.csv", sep = ""), stringsAsFactors = FALSE)
# there appears to be a space in front of the title. Odd.
range(d$value)
br.t <- seq(-4,10, 0.5) # sequence of histogram cut-off points. Thirs number gives bin width
hist(d$value, breaks = br.t)
# explore the distribution by changing the bin width


# --- b
# use bootstrapping to get the sample distribution of the mean difference between
# the two groups. Retain the same sample size as the original sample for each group.
# do this at least 1000 times
str(d)
table(d$group)  # get the sample sizes of the two groups
n.A <- table(d$group)[1]  # sample size group A
n.B <- table(d$group)[2]  # sample size group B
# or other way
#length(d$value[d$group == "A"])

n.iterations <- 1000  # number of times to resample for bootstrap
store.means <- rep(NA, n.iterations)  # empty vector to store the bootstrap means into

# the loop to resample for the bootstrapping
# will be done n.iterations times

for(i in 1:n.iterations){
  print(i)  # iteration number will be shown in console. handy to
  # find out where in the loop it goes wrong. remove if it works well
  
  # resample from each group (A and B), using its sample size
  # and store the new sample
  sample.A <- sample(d$value[d$group == "A"], n.A, replace = TRUE)
  sample.B <- sample(d$value[d$group == "B"], n.B, replace = TRUE)
  
  # take mean of group A minus the mean of group B and store it
  store.means[i] <- mean(sample.A) - mean(sample.B)
}
# Check the distribution
hist(store.means)
#  get the bootstrap se = the sd of the sampling distribution
sd(store.means)# bootstrap se

# mean of the original data set
mean.difference <- mean(d$value[d$group == "A"]) - mean(d$value[d$group == "B"])
# mean and and the bootstrap SE are
# 8.879859 with a SE of 0.6000379


# ---------------------------------------------------------------------------
# ---- permutation test -----------------------------------------------------
# ---------------------------------------------------------------------------
# We have two groups, A and B and we have data along the wavelength spectrum, 
# starting from 1 nm till 201 nm.
# We want to know if the two sets of curves are different from another.
# to do this we will use permutation test 

# --- a
# load the data file permutation.data.csv
# plot the data and give the two groups a different colour
# Do the groups look different?
p <- read.csv(paste(p.d14, " permutation.data.csv", sep = ""), stringsAsFactors = FALSE)
# again the title has an odd space proceeding it.
head(p) # show the first couple of rows
str(p)  # check the structure

# make a selection for group A and B of the original data
A <- p[p$group == "A",]
B <- p[p$group == "B",]

# all the x values ( 1 to 201)
x.values <- c(1:(ncol(p)-1))
xlim.t <- c(1,(ncol(p)-1))  # the range for the x axis
ylim.t <- range(p[,-1])     # the range for the y axis

# make a vector with a different colour for each group
# this can be used to make the lines a different colour
col.t <- p$group
col.t[col.t == "A"] <- "red"
col.t[col.t == "B"] <- "green"

# make an empty plot, with ylim and xlim set as defined above
plot(NA, xlim = xlim.t, ylim = ylim.t)
# go through all the rows and plot the data using lines
# remember to exclude the first column (through the -1) as this is the group name
for(i in 1:nrow(p)) lines(x.values, p[i,-1], col = col.t[i])
# the col = col.t[i] gives the line a colour depending in its group

# --- Now to doing the pemrutation test
# rember we are free to make our own test statistic
# make loop to go through each wavelength and get 
# the difference between group A and B, square it (we are after the magnitude
# of the differences) sum it. This 'sum of squared differences' is our statistics


# ***** work with the original data *****
# make vectore to store means
mean.stor <- rep(NA, (ncol(p) - 1))
# select group A and B
A.t <- p[p$group == "A" , ]
B.t <- p[p$group == "B" , ]
# go through each wavelength (column), starting at column 2
for(i in 2:ncol(p)){
  print(i)  # print iteration number to console
  # not neded if everything goes well, but you can track progress this way
  # i <- 1    # little 'hack' so you can test if all goes well for a specific
  # value of i. not neded if all goes well
  A.tm <- mean(A.t[,i]) # get mean of A
  B.tm <- mean(B.t[,i]) # get mean of B
  # take the differnce between A and B and square it. Store in vector
  mean.stor[i - 1] <- (A.tm - B.tm)^2
}
# our final statistic of the original data is the sum of all the squared differences
real.test.statistic <- sum(mean.stor) 


# ==== let's up a permutation to get null distribution ====
iter <- 1000  # number of iterations for the null distribution
storage.perm <- rep(NA, iter) # storage for your final test statistic

# the main loop, to through all iterations
for(k in 1:iter){
  #k <- 1
  perm.data <- p    # make copy of original data set (not necesarily needed)
  # ramdomly reshuffle the group column
  # this is the crux of the permutation test as this breaks down
  # any relationship within groups
  perm.data$group <- sample(p$group, length(p$group), replace = FALSE )

  # same as abov, split the data frame in group A and B
  A.t <- perm.data[perm.data$group == "A" , ]
  B.t <- perm.data[perm.data$group == "B" , ]
  # loop to go through all wavelength (1 to 201)
  for(i in 2:ncol(perm.data)){
    print(i)
    #i <- 2
    A.tm <- mean(A.t[,i])
    B.tm <- mean(B.t[,i])
    mean.stor[i - 1] <- (A.tm - B.tm)^2
    # end of the loop for one wavelenght
  }
  # one iteration for the permutation test done.
  # store the squared differences between the means
  storage.perm[k] <- sum(mean.stor)
}

# we now have the null distribution, plot it
hist(storage.perm)
# we also have our original test statistic value.
# check how many values or more extrme under the null distribution
# divide this number by number of iterations to get the p value
# multiply by 2 as the test is two-tailed

2 * (length(storage.perm[storage.perm > real.test.statistic]) + 1)/iter

# NOTE here this turns out to be 0, as the curves are much more similar
# within a group, i.e. there is no other sample which would create a larger difference
# see North, BV, Curtis D, Sham PC (2002) A note on the calculation of 
# empirical P values from Monte Carlo procedures. American Journal of Human 
# Genetics, 71, 439– 441 why to do the + 1

















# **** quick code to make the test data sets for the bootstrap ***
# *** and permutation tests ****

# --- bootstrap ---
wd <- getwd()
p.d14 <- paste(wd, "/day 14/", sep = "")

# generate data sets
n.1 <- c(15, 3, 8)
A1 <- runif(n.1[1], 6, 10)
B1 <- c(rnorm(n.1[2], 2, 0.5), rnorm(n.1[3], -2, 1))

group <- rep(c("A", "B", "B"), n.1)
tt <- cbind(group,c(A1, B1)  )
colnames(tt) <- c("group", "value")
write.csv(tt, paste(p.d14, "bootstrap.data.csv", sep = ""), row.names = FALSE)

hist(c(A1, B1)  )

# --- permutation ---

# gaussian
f.gaussian <- function(mean, sd, range){
  #range <- seq(-1, 5, 0.2)
  #mean <- 1
  #sd <- 0.2 
  val <- 1/(sd * sqrt(2 * pi)) * exp(-(range - mean)^2/(2 * sd^2))
  return(val)
}

range <- seq(0, 20, 0.1)
v <- f.gaussian(10, 50, range)
plot(v)

m <- c(9.4, 10)
sd.m <- 0.5
sd <- c(9, 10)
sd2.m <- 0.2
n.2 <- c(12,13)
names <- rep(c("A", "B"), n.2)
storage <- matrix(NA, nrow = sum(n.2), ncol = length(range))
means <- c( rnorm(n.2[1], mean = m[1], sd = sd.m), rnorm(n.2[2], mean = m[2], sd = sd.m))
sds <- c( rnorm(n.2[1], mean = sd[1], sd = sd2.m), rnorm(n.2[2], mean = sd[2], sd = sd2.m))

for(i in 1:sum(n.2)){
  #i <- 3
  storage[i,] <- f.gaussian(means[i], sds[i], range)
  
}

permutation.data <- data.frame(cbind(names, storage))
head(permutation.data)
colnames(permutation.data) <- c("group", c(1:201))
write.csv(permutation.data, paste(p.d14, "permutation.data.csv", sep = ""), row.names = FALSE)


groups <- permutation.data[,1] 
data <- as.numeric(permutation.data[,-1] )
str(data)
ylim.t <- range(storage)
xlim.t <- c(1, ncol(storage))

plot(NA, xlim = xlim.t, ylim = ylim.t)
for(i in 1:ncol(storage)) lines(c(1:ncol(storage)), storage[i,])


