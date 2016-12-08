#
# setting up a folder structure and using paths
# to allow for batch processes and reproducible scripts
# 
#
#

# get working directory
wkdir <- getwd()

# check which files are in the working directory
files <- list.files()
length(files) # how many files
str(files)    # what is the structure of the object files

# want want to have the following folders
folders <- c("script", "data", "figures", "data output")
length(folders) # number of folders

# check if the folders exist, if not, make them
for(i in 1:length(folders)){
  if( file.exists(folders[i]) == FALSE) {
    dir.create(folders[i])
  }
}

# let's make paths to the folders
# this will be used to access files from and write files to
# the folders. I use p. for 'path'
p.script <- paste(wkdir,"/script/", sep = "")
p.data <- paste(wkdir,"/data/", sep = "")
p.data.out <- paste(wkdir,"/data output/", sep = "")
p.fig <- paste(wkdir,"/figures/", sep = "")

# get the path to the files in the folder data (path is p.data)
data.files <-  list.files(p.data)

#read file 'counter' (to which you assign a number)
counter <- 2
d <- read.csv(paste(p.data, data.files[counter], sep = ""))
head(d)  

# make a vector to store regression coefficients into of all the files
slopes <- rep(NA, length(data.files))

# move the example above into a for loop
for(i in 1:length(data.files)){
  #i <- 1  # i is now the object that R uses to keep track of the number of
  # iterations of the loop
  
  # read the data using the names stored in data.files
  d <- read.csv(paste(p.data, data.files[i], sep = ""))
  str(d)
  # do a regression
  m <- lm(d$DNA.concentration ~ d$waterlevel)
  # check the coefficients
  m$coefficients    # second index is the slope
  slopes[i] <- m$coefficients[2]
  
  # plot the data and the fitted slopes
  pdf(paste(p.fig, "figures.regression.", data.files[i], ".pdf", sep = ""))
  plot(d$waterlevel, d$DNA.concentration, pch = 19, las = 1)
  abline(m, col = "red")  # plots fitted regression
  dev.off() # closes the graphics device, need together with pdf()
}

# combine the file names with with slopes and save as .csv
slopes.all <- data.frame(cbind(data.files, slopes), stringsAsFactors = FALSE)
colnames(slopes.all) <- c("file.names", "regression.coeff")

write.csv(slopes.all, paste(p.data.out, "overview.slopes.csv", sep = ""), row.names = FALSE)



