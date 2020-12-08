## Day 2 - passwords
library(tidyverse)

## Part 1: must be between min and max instances of the letter in the password
dat <- read_delim("Day02/Day2Data.txt", 
                  delim = c(" "), 
                  col_names = c("min_max", "letter", "password"),
                  col_types = "ccc")

dat_clean <- 
  dat %>% 
  separate(min_max, c("min_count", "max_count"), "-", convert = T) %>% 
  mutate(letter = str_remove(letter, ":"),
         letter_count = map_int(str_extract_all(password, letter), length), 
         valid1 = letter_count >= min_count & letter_count <= max_count) 

dat_clean %>% filter(valid1) %>% nrow()


## Part 2: first location or second location must match the letter, but not both. 
dat_clean <- dat_clean %>% 
  mutate(letter1_valid = str_sub(password, min_count, min_count) == letter,
         letter2_valid = str_sub(password, max_count, max_count) == letter,
         valid2 = xor(letter1_valid, letter2_valid))  ## xor: one or other but not both

dat_clean %>% filter(valid2) %>% nrow()
