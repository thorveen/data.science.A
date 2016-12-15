#
#
# code to allocate people to different groups 
# avoid same people multiple times in the same group
#
#

names.data.stars <- c("Jordan", "Laura", "Devon", "Sabine", "Frederik", "Karla", "Samuel", "Alicia", "Rachel", "Anika")
length(names.data.stars)

groups <- rep(LETTERS[1:3], c(3,4,3))
names <- sample(names.data.stars, length(names.data.stars), replace = FALSE)

(new.groups <- data.frame(groups, names))


# for the GLM assignment
BDC.groups <- c("C", "A", "C", "C", "B", "B", "B", "A", "A", "A")
(groups.t <- cbind(names.data.stars, BDC.groups))


# ========= assignment 2 groups ============
 # for the next assignment we have five groups of two
five.groups <- rep(c(LETTERS[1:5]), rep(2, 5))
# sample from these randomly to make new groups
new.t <- sample(five.groups, length(five.groups), replace = FALSE)

# check if these duos have not been together before in a group
gr.t <- unique(five.groups)
for(i in 1:length(gr.t)){
  # i <- 2
  d.t <- which(new.t == gr.t[i])
  if(groups.t[d.t[1], 2] == groups.t[d.t[2], 2]) print(paste("group ", i ," = same group", sep = ""))
}

# run till you get no warning
groups.t2 <- cbind(groups.t, new.t)
colnames(groups.t2)[3] <- "GLM.groups"

# names.data.stars BDC.groups new.t
#  [1,] "Jordan"         "C"        "C"  
#  [2,] "Laura"          "A"        "D"  
#  [3,] "Devon"          "C"        "B"  
#  [4,] "Sabine"         "C"        "E"  
#  [5,] "Frederik"       "B"        "A"  
#  [6,] "Karla"          "B"        "D"  
#  [7,] "Samuel"         "B"        "C"  
#  [8,] "Alicia"         "A"        "B"  
#  [9,] "Rachel"         "A"        "E"  
# [10,] "Anika"          "A"        "A"  


# ========= assignment 3 groups ============
# the last task, assign project buddies for the final project
 # for the next assignment we have five groups of two
five.groups.2 <- rep(c(LETTERS[1:5]), rep(2, 5))
# sample from these randomly to make new groups
new.t2 <- sample(five.groups.2, length(five.groups.2), replace = FALSE)

# check if these duos have not been together before in a group
gr.t <- unique(five.groups)
for(i in 1:length(gr.t)){
  # i <- 1
  d.t <- which(new.t2 == gr.t[i])
  # not together in the first assignment?
  if(groups.t2[ d.t[1], 2] == groups.t2[d.t[2], 2]) print(paste("group ", i ," = same group", sep = ""))
  # not together in the second assignment?
  if(groups.t2[d.t[1], 3] == groups.t2[d.t[2], 3]) print(paste("group ", i ," = same group", sep = ""))
}

groups.t3 <- cbind(groups.t2, new.t2)
colnames(groups.t3)[4] <- "final.assignment"

# this should work
#     names.data.stars BDC.groups GLM.groups final.assignment
#  [1,] "Jordan"         "C"        "C"        "E"             
#  [2,] "Laura"          "A"        "D"        "B"             
#  [3,] "Devon"          "C"        "B"        "D"             
#  [4,] "Sabine"         "C"        "E"        "A"             
#  [5,] "Frederik"       "B"        "A"        "C"             
#  [6,] "Karla"          "B"        "D"        "D"             
#  [7,] "Samuel"         "B"        "C"        "B"             
#  [8,] "Alicia"         "A"        "B"        "E"             
#  [9,] "Rachel"         "A"        "E"        "C"             
# [10,] "Anika"          "A"        "A"        "A"               

# ==== end ======
