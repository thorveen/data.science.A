#
# code to allocate 
#
#

set.seed(4)
names.data.stars <- c("Jordan", " Laura", "Devon", "Sabine", "Fredrik", "Karla", "Samuel", "Alicia", "Rachel", "Anika")
length(names.data.stars)

groups <- rep(LETTERS[1:3], c(3,4,3))
names <- sample(names.data.stars, length(names.data.stars), replace = FALSE)

(new.groups <- data.frame(groups, names))
