#
#
# several graphing scripts
# 1) dot plot from scratch
# 2) use polygon()
# 3) multipanel using par(fig())
# 4) strip chart from scratch
# 
#

# ------------------------------------------------------------------------------
# --- 1) dot plot from scratch -----
# Building a graph from scratch
# To show how R allows you to customise your graphs


# --- SIMULATE DATA ---
# vector for the x values: 
# the () around x <- c(1:10) makes R show the output
(x <- c(1:10))

# a set of 10 random numbers between 3 and 10 
(y <- runif(10, 3,10))

# --- start plotting -------
# lets plot this 
plot(x,y)


# what does R really do?
# ADVANCED: trick to get the min and max values for the x and y axis
xlim <- range(x)
ylim <- range(y)

diff(xlim)

# plot an empty plot
plot(NA,  ylim = ylim, xlim = xlim, xlab = "", ylab = "", xaxt = "n", yaxt = "n", bty = "n")  # 

# draw linea round the plotting are (set by the xlim and ylim as defined above)
box()


# R basically provides you with a frame to draw on
# let's draw the same points as in the plot(x,y)
points(x,y)

# add a single point
points(x, y, pch = 19, col = "purple", cex = 3)

# add axis
axis(1) ; axis(2)
# hopefully this gives you an idea about the moudaliry of an R plot.
# many of the different components can be customised
# in the next section I show how you can do some fun changes to your plot
# to make it much better than the standard version


# --- MAKING THE GRAPH ---
# lets use plot and take control over the plotting
plot(x,y)

# how can we manipulate this?
# use ? to see what the plot function has to offer
?plot
# for many tweaks such as line and symbol types, check
?par

# I dislike the orientation of the y labes, change to horizontal (las = 1)
# also, like the circle to be filled (pch = 19) and red (col = red)
plot(x, y, las = 1, pch = 19, col = "red")

# R by default determines the sizes of the axis, but we can set this
# y axis is too short ylim = c(0, 12)
plot(x,y, las = 1, pch = 19, ylim = c(0,12), col = "red")

# don't like the y label, use ylab = (or xlab for the x-axis, main for the main title)
plot(x,y, las = 1, pch = 19, ylim = c(0, 12), col = "red", ylab= "random stuff", main="how graphs work")


#to many tickmarks for my taste. R lets you suppress any axis (xaxt = "n" for xor yaxt for y) and determines yourself 
# how you like it using axis() 
plot(x, y, las = 1, pch = 19, ylim = c(0, 12), col = "red", yaxt = "n")
# ok, axis ticks and labels gone
# let's make my own. First argument gives which axis you want: 1 = x, 2=y,3 top, 4 right side
axis(2,c(2,6, 10), labels = c("low","medium","high"), las = 1)

# lets add some more data
y2 <- runif(10, 0,12)
# make sure this still falls with the range as set by ylim!!!
# lets plot y2 againts x (make sure this has the same number of entries (use length()))
# and make them nice and big (use lwd) 
points(x, y2, pch = 19, col = "purple", cex = 4)

# we can add a line wherever we'd like
lines(c(3, 7.5), c(6, 8), col= "grey", lwd = 3, lty = "dashed")

# this is panel A, so we like to have an A in the left top. at coordinates 1.5, 11.5
# but pretty big! (use cex (1 is standard))
text(1.5, 11.5, "A", cex = 2)

# add a rather random legend (just to show how it works)
legend("topright", c("P", "R"), pch = c(19, 19), col = c("purple", "red"))

# ------------------------------------------------------------------------------
# --- 2) use polygon() ----

# --- function to draw a gaussian curve ---
gaussian <- function(x, offset) {
	1/sqrt(2 * pi) * exp(-0.5 * (x-offset) ^ 2)
	}
# --- end of function declaration ---
	
# --- variables for the figure	
x <- seq(-5, 5, 0.01)					# sequence of x values
colours <- c("#0000CD", "#8B0000")		# colours
alpha <- c("FF")						# transparency
lwd  <- 2.3								# thickness of line
xlim <- c(-5, 5)							# range for x-axis
ylim <- c(0, 0.5)						# range for y-axis

# postition of the mean of the gaussians
offset <- c(-1.5, 1.5)

# --- start the plotting routine ----
plot(NA, ylim = ylim, xlim = xlim,  xlab="",ylab="", type = "l", lwd = lwd, col = paste(colours[1], alpha, sep =""), las = 1)

# first gaussian
polygon(c(x[1], x, x[length(x)]), c(0, gaussian(x, offset[1]), 0), col = paste(colours[1], alpha, sep = ""), border = NA)

# second gaussian
polygon(c(x[1], x, x[length(x)]),c(0, gaussian(x, offset[2]), 0), col = paste(colours[2], alpha, sep = ""), border = NA)


# lets add a point object to the figure
polygon(c(rep(-4, 2), rep(-3, 2)), c(0, 0.2, 0.3, 0), col = "black")
# --- end of code ----


# ------------------------------------------------------------------------------
# --- 3) multipanels -------
# example to show plot area 
# show use of multi panel
#
#

dims <- c(5, 8)
col.t <- c("blue", "red")

# ------------------------------------------------------------------------------
# NOTE: once you define the par(), R remembers this
# so if you make a multipanel with different
# number of rows for example, you got to set it back to
# par(mfrow = c(1,1)) to get the standard 'one plot'

pdf("general.layout2b.pdf", width = dims[1], height = dims[2] )
par(mar = c(4, 4, 3, 2), oma = c(3, 3, 3, 3))
y <- x <- c(1:10)
xlim.t <- range(x)
ylim.t <- range(y)
#plot(NA, xlim = xlim.t, ylim = ylim.t, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
plot(NA, xlim = xlim.t, ylim = ylim.t)

text((0.5 * xlim.t[2]), (0.5 * ylim.t[2]), "PLOT AREA")

mtext("PLOT MARGIN (mar = c(4, 4, 3, 2))", side = 3, line = 1, col = col.t[1])
box("figure", col = col.t[1])
mtext("l = 0", side = 3, line = 0, col = col.t[1], adj = 1)
mtext("l = 1", side = 3, line = 1, col = col.t[1], adj = 1)
mtext("l = 2", side = 3, line = 2, col = col.t[1], adj = 1)
#mtext("l = 3", side = 3, line = 3, col = col.t[1], adj = 1)

mtext("OUTER MARGIN (oma = c(3, 3, 3, 3))", side = 1, line = 1, col = col.t[2], outer = TRUE)
box("outer", col = col.t[2])
mtext("l = 0", side = 1, line = 0, col = col.t[2], adj = 1, outer = TRUE)
mtext("l = 1", side = 1, line = 1, col = col.t[2], adj = 1,outer = TRUE)
mtext("l = 2", side = 1, line = 2, col = col.t[2], adj = 1, outer = TRUE)
#mtext("l = 3", side = 1, line = 2.5, col = col.t[2], adj = 1, outer = TRUE)
box("outer", col = col.t[2])
dev.off()



# ------------------------------------------------------------------------------
# --- multiple panels ---


# two rows, one column
par(mfcol = c(2,1))

par(oma = c(3, 3, 3, 3))

# first plot
par(mar = c(3, 3, 3, 3))
plot(NA, xlim = xlim.t, ylim = ylim.t, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
text((0.5 * xlim.t[2]), (0.5 * ylim.t[2]), "PLOT 1")
box("figure", col = col.t[1])
mtext("PLOT 1 MARGIN (mar = c(3, 3, 3, 3))", side = 3, line = 1, col = col.t[1])


# plot 2
par(mar = c(2, 1.5, 4, 0))
plot(NA, xlim = xlim.t, ylim = ylim.t, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
text((0.5 * xlim.t[2]), (0.5 * ylim.t[2]), "PLOT 2")
box("figure", col = col.t[1])
box("outer", col = col.t[2])
mtext("PLOT 2 MARGIN (mar = c(2, 1.5, 4, 0))", side = 3, line = 1, col = col.t[1])

#
mtext("OUTER MARGIN (oma = c(3, 3, 3, 3))", side = 3, line = 1, col = col.t[2], outer = TRUE)

# --- multipanel using par(fig()) -----

# fig() takes coordinates of the corners of the plotting region (mar)
# relative to the whole plotting region (oma)
# fig  takes c(x1, x2, y1, y2)

# quartz()
par(oma = c(1, 1, 1, 1))

par(fig = c(0, 0.5, 0, 1), mar = c(3, 3, 3, 3), xpd = NA)
plot(NA, xlim = xlim.t, ylim = ylim.t, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
text((0.5 * xlim.t[2]), (0.5 * ylim.t[2]), "PLOT 1")
box("figure", col = col.t[1])
mtext("PLOT 1 MARGIN (mar = c(3, 3, 3, 3))", side = 3, line = 1, col = col.t[1])

par(new = TRUE, fig = c(0.5, 1, 0, 0.5))
plot(NA, xlim = xlim.t, ylim = ylim.t, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
text((0.5 * xlim.t[2]), (0.5 * ylim.t[2]), "PLOT 2")
box("figure", col = col.t[1])
box("outer", col = col.t[2])


par(new = TRUE, fig = c(0.55, 0.95, 0.6, 0.95), mar = c(1, 2, 1, 1))
plot(NA, xlim = xlim.t, ylim = ylim.t, ylab = "", xlab = "", xaxt = "n", yaxt = "n")
text((0.5 * xlim.t[2]), (0.5 * ylim.t[2]), "PLOT 3")
box("figure", col = col.t[1])
box("outer", col = col.t[2])


mtext("OUTER MARGIN (oma = c(1, 1, 1, 1))", side = 3, line = 0, col = col.t[2], outer = TRUE)
# ------------------------------------------------------------------------------


# ------------------------------------------------------------------------------
# --- 4) a strip chart from scratch 

# make some data
y <- runif(10, 2, 10)
gr.t <- c("A", "B") # group names
groups <- rep(gr.t, 5)

d <- data.frame(cbind(groups, y))
d$y <- as.numeric(d$y)
ylim.t <- range(y)
xlim.t <- c(0.5, 2.5)
           

col.t <- c("red", "green")  # colours for each group
wdth.l <- 0.075 # width of the line of the mean of each group
jitter.t <- 2   # amount of jitter on the x axis

plot(NA, xlim = xlim.t, ylim = ylim.t, xaxt = 'n', las = 1)
axis(1,  c(1,2), gr.t)  # make a news x axis and add the two groups as labels
for(i in 1:2){
  #i <- 2
  points(jitter( rep(i, length(d$groups[d$groups == gr.t[i]]) ), jitter.t) , d$y[d$groups == gr.t[i]], pch = 16, col =  col.t[i])
  lines( c((i - wdth.l), (i + wdth.l)), rep( mean(d$y[d$groups == gr.t[i]]), 2) )
}




