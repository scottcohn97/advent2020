#' Scott Cohn
#' Advent of Code Day 4
#' Dec 5
library(tidyverse)

# This .txt file is so gross... some cleaning

# read in data and keep blank lines to sep entries
pp_raw <- scan("data/day04.txt", what = "raw", blank.lines.skip = FALSE)

# put groups of obs on same row with space btwn
pp_raw <- scan(text = paste(ifelse(pp_raw == "", "\n", pp_raw), collapse=" "), 
                sep = "\n", what = "list") 

convert_cell <- function(x){
  # split into new col using sep = " "
  split_cell <- unlist(strsplit(stringr::str_trim(x), " "))
  # name objects
  setNames(
    # remove text up to ":" and use as name
    stringr::str_remove(split_cell, "^[a-z]*:"), 
    # match char from prev
    stringr::str_extract(split_cell, "^[a-z]*")
    )
}

passport <- 
  # returns df // applies convert to 
  map_df(pp_raw, convert_cell) %>%
  # determine units
  mutate(hgt_unit = str_extract(hgt, "(cm|in)$"),
         # drop number before/after first number
         hgt = parse_number(hgt)) 
 

##########
# Part 1 #
##########

#' Count the number of valid passports - those that have all 
#' required fields. Treat cid as optional. In your batch file, 
#' how many passports are valid?

passport %>% 
  # select desired columns (cid is optional)
  select(eyr, byr, hcl, ecl, hgt, iyr, pid) %>% 
  # remove rows w and NA
  na.omit() %>% 
  # count whats left
  nrow()


##########
# Part 2 #
##########
  
#' Count the number of valid passports - those that have 
#' all required fields and valid values. Continue to treat 
#' cid as optional. In your batch file, how many passports are valid?
