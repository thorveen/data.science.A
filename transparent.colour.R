#
# code to show how to add transparency to colour 
# this can be very useful if you have a lot of overlapping data points
# and you want to use the intensity of the colour as a proxy for the data 
# density (much a like in a heat map)
#
# use the hexadecimal colour code which described the colour spectrum in RGB
# in #rrggbb. Each colour can range from 00 - FF
# adn additional two digits covers the 'alpha channel' which determines the transparency
col.t <- "#FF990"     # hexadecimal colour
alpha <- "99"         # called the alpga channel, 
cex.t <- 10           # the size of the symbols
pch.t <- 16           # symbol type

# make a one point plot
plot(1, pch = pch.t ,col = paste(col.t, alpha, sep = ""), cex =  cex.t)
# add one point to show the effect of transparency
points(1.1,1.1, pch = pch.t, col = paste(col.t, alpha, sep = ""), cex =  cex.t)

# -----------