#' Scott Cohn
#' Advent of Code Day 2
#' Dec 4

library(tidyverse)

# Load data
password <- read.delim("data/day02.txt", header = FALSE, 
                       col.names = c("policy", "letter", "pw"), 
                       sep = " ")

##########
# Part 1 #
##########

#' Each line gives the password policy and then the password.
#'  The password policy indicates the lowest and highest number 
#'  of times a given letter must appear for the password to be 
#'  valid. For example, 1-3 a means that the password must contain 
#'  a at least 1 time and at most 3 times.

password %>% 
  # split column allow into min/max
  separate(col = "policy", into = c("min", "max"), sep = "-", convert = TRUE) %>% 
  # remove ":" char
  mutate(letter = str_remove(letter, ":")) %>% 
  # add col with count of letter occurrences in pw
  mutate(count = mapply(str_count, pw, letter)) %>% 
  # T/F if occurrences of letter is valid
  mutate(valid = count >= min & count <= max) %>% 
  # return valid pw
  count(valid)


##########
# Part 2 #
##########

#' Each policy actually describes two positions in the password, 
#' where 1 means the first character, 2 means the second character, 
#' and so on. (Be careful; Toboggan Corporate Policies have no concept 
#' of "index zero"!) Exactly one of these positions must contain the given 
#' letter. Other occurrences of the letter are irrelevant for the purposes 
#' of policy enforcement.

password %>% 
  # split column allow into first/last
  separate(col = "policy", into = c("min", "max"), sep = "-", convert = TRUE) %>% 
  # remove ":" char
  mutate(letter = str_remove(letter, ":")) %>% 
  # check if valid
  mutate(valid = xor(
    substr(pw, min, min) == letter, 
    substr(pw, max, max) == letter)
    ) %>% 
  # return count of valid
  count(valid)
