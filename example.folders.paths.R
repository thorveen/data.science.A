#
# setting up a folder structure and using paths
# to allow for batch processes and reproducible scripts
# 
#
#

# get working directory
wkdir <- getwd()

# which files are in the working directory
files <- list.files()
length(files) # how many files
str(files)  # what is the structure of the object files

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
p.fig <- paste(wkdir,"/figures/", sep = "")

# get the path to the files in the folder data (path is p.data)
data.files <-  list.files(p.data)

#read file 'counter' (to which you assign a number)
counter <- 2
d <- read.csv(paste(p.data, data.files[counter], sep = ""))
head(d)  


# let's make a folder structure

y <- runif(10, 2, 10)
x <- runif(10, 4, 8)
n <- c(1:10)
for(i in 1:10){
  pdf( paste(p.fig, "test n = ", n[i], ".pdf", sep = ""))
  x <- runif(1000, 6, 10)
  hist( x, pch = 19, col = "blue", las = 1)
  dev.off()
}
