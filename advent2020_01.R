#' Scott Cohn
#' Advent of Code Day 1
#' Dec 4

#' Goal: 
#' Find pair that sum to 2020
#' Then multiply those two numbers together

##########
# Part 1 #
##########

# Load data
expense <- read.delim("data/day01.txt", header = FALSE, col.names = c("x"))

# Algo: Sorting and Walking Inward
find2sum <- function(target, list, i = 1){
  # Sort 
  list <- sort(list[[i]])
  
  # set lhs and rhs
  lhs = 1
  rhs = length(list)
  
  # compare ends
  while (list[lhs] < list[rhs]) {
    # define sum
    sum = list[lhs] + list[rhs]
    # if sum is equal to target
    if (sum == target){
      # print puzzle solution
      return(paste("Solution:", list[lhs]*list[rhs]))
    } else if (sum < target){
      # iterate lhs
      lhs = lhs + 1
    } else {
      # iterate rhs
      rhs = rhs - 1
    }
  }
  # otherise no solution
  return("No valid pair")
}

find2sum(target = 2020, list = expense)

##########
# Part 2 #
##########

library(sqldf)

sqldf('
  SELECT 
      (i.x * j.x * k.x) AS solution
  FROM expense i 
  JOIN expense j
  JOIN expense k 
  WHERE i.x + j.x + k.x = 2020 
  LIMIT 1;'
)



