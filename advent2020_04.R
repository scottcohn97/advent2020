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

#' byr (Birth Year) - four digits; at least 1920 and at most 2002.
#' iyr (Issue Year) - four digits; at least 2010 and at most 2020.
#' eyr (Expiration Year) - four digits; at least 2020 and at most 2030.
#' hgt (Height) - a number followed by either cm or in:
#'   If cm, the number must be at least 150 and at most 193.
#'   If in, the number must be at least 59 and at most 76.
#' hcl (Hair Color) - a # followed by exactly six characters 0-9 or a-f.
#' ecl (Eye Color) - exactly one of: amb blu brn gry grn hzl oth.
#' pid (Passport ID) - a nine-digit number, including leading zeroes.
#' cid (Country ID) - ignored, missing or not.


# apply rules above to respective columns, prefix v
validated_passport <- passport %>% 
  # 1 for valid // 0 for invalid 
  mutate(
    vbyr = ifelse(between(byr, 1920, 2002) & nchar(byr) == 4, 1, 0),
    viyr = ifelse(between(iyr, 2010, 2020) & nchar(iyr) == 4, 1, 0),
    veyr = ifelse(between(eyr, 2020, 2030) & nchar(eyr) == 4, 1, 0),
    vhgt = ifelse((hgt_unit == "cm" & between(hgt, 150, 193)) |
                  (hgt_unit == "in" & between(hgt, 59, 76)) , 1, 0),
    vhcl = ifelse(str_detect(hcl, "^#[0-9a-f]{6}$"), 1, 0),
    vecl = ifelse(str_detect(ecl, "^(amb|blu|brn|gry|grn|hzl|oth)$"), 1, 0),
    vpid = ifelse(str_detect(pid, "^[0-9]{9}$"), 1, 0)
  )

validated_passport %>% 
  # If the last 7 columns are all valid (sum = 7) ...
  mutate(valid = ifelse(rowSums(.[10:16]) == 7, TRUE, FALSE)) %>% 
  # Return distro of valid, not valid, NA
  count(valid)
