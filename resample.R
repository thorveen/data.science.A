#
# Bootstrap and permutations
#
#


# bootstrapping
# read the data bootstrap.data.csv

# --- a
# load the data and plot the 'value' parameter.
# Anything odd?


# --- b
# use bootstrapping to get the sample distribution of the mean difference between
# the two groups. Retain the same sample size as the original sample for each group.
# do this 1000 times



# --- c 
# plot the sample distribution and get the standard error






# ---- permutation test ----
# We have two groups, A and B and we have data along the wavelength spectrum, starting from 1 nm
# till 201 nm.
# we want to know if the two sets of curves are different from another.
# to do this we will use  apermutatin test approach

# --- a
# load the data file permutation.data.csv
# plot the data and give the two groups a different colour
# Do the groups look different?


# --- b 
# we are now for each wavelength reshuffle the groups (the permutation stage)
# perform a t test and store the t value( this will be used to get our test statistic)
# do this first for one wavelength!

 # --- c
# expand the approach to add a loop, storing the t value for the difference between the (reshuffled)
# groups at a wavelength.
# at the and, sum all of the t values. This is our own, invented test statistic and 
# we have the first value for our sampling distribution of our test statistic
# let's call it SumT

# --- d
# we want to do the above 1000 times! Nest the above permutatin into a for loop.










# ----------- end --------------





















#
wd <- getwd()
p.d14 <- paste(wd, "/day 14/", sep = "")


# generate data sets
n.1 <- c(15, 3, 8)
A1 <- runif(n.1[1], 6, 10)
B1 <- c(rnorm(n.1[2], 2, 0.5), rnorm(n.1[3], -2, 1))

group <- rep(c("A", "B", "B"), n.1)
tt <- cbind(group,c(A1, B1)  )
colnames(tt) <- c("group", "value")
write.csv(tt, paste(p.d14, "bootstrap.data.csv"), row.names = FALSE)

hist(c(A1, B1)  )


# permutation

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
write.csv(permutation.data, paste(p.d14, "permutation.data.csv"), row.names = FALSE)



groups <- permutation.data[,1] 
data <- as.numeric(permutation.data[,-1] )
str(data)
ylim.t <- range(storage)
xlim.t <- c(1, ncol(storage))

plot(NA, xlim = xlim.t, ylim = ylim.t)
for(i in 1:ncol(storage)) lines(c(1:ncol(storage)), storage[i,])
