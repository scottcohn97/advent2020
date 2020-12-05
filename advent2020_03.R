#' Scott Cohn
#' Advent of Code Day 3
#' Dec 4

library(tidyverse)

# load
slope <- do.call(rbind, strsplit(readLines("data/day03.txt"), ""))

##########
# Part 1 #
##########

# go toboganning
tobogan <- function(matr, x, y){
  # chars freak me out
  matr <- chartr(".", "0", matr)
  matr <- chartr("#", "1", matr)
  
  # times to repeat pattern to reach bottom of slope
  itrs  <- (nrow(matr) - 1) %/% y
  width <- ((itrs * x) %/% ncol(matr)) + 1
  
  # create matrix
  matr <- matrix(matr, ncol = ncol(matr) * width, nrow = nrow(matr))
  
  # init pos
  col = 1
  row = 1
  # init count
  trees <- 0
  # tobogan
  while(row <= nrow(matr) && col <= ncol(matr)){
    if(matr[row, col] == 1){
      # iter tree count
      trees = trees + 1
    }
    # carve the slopes over 3 down 1
    row = row + y
    col = col + x
  }
  # return tree count
  return(trees)
}

# Run 
tobogan(matr = slope, x = 3, y = 1)

##########
# Part 2 #
##########

# give new routes
routes <- data.frame(
  x = c(1,3,5,7,1),
  y = c(1,1,1,1,2)
)

# apply func to df over rows and return product of elements
apply(routes, 1, function(x){ tobogan(slope, x["x"], x["y"]) }) %>% prod()
