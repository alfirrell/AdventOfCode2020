## Day 4: Count valid passports
library(tidyverse)

## Part 1
lines <- read_lines("Day4Data.txt")

passports <- 
  tibble(fields = lines) %>% 
  mutate(batch_id = cumsum(lines == "") + 1) %>% ## blank lines define the separate passports
  group_by(batch_id) %>% 
  summarise(fields = paste0(fields, collapse = " ")) %>% ## paste all the lines together for a single passport
  mutate(valid = str_detect(fields, "ecl:") & 
           str_detect(fields, "pid:") & 
           str_detect(fields, "byr:") & 
           str_detect(fields, "iyr:") & 
           str_detect(fields, "hgt:") & 
           str_detect(fields, "hcl:") & 
           str_detect(fields, "eyr:")) ## cid not required
  
passports %>% 
  summarise(n_valid = sum(valid)) 


## Part 2
# splitting into a matrix for more control

## Split into a column per key (pid, hcl, iyr etc.)
passports_wide <- 
  passports %>% 
  filter(valid) %>% ## filter out the ones already missing a field
  separate_rows(fields, sep = " ") %>% ## row per key:value
  filter(fields != "") %>% 
  ungroup() %>% 
  separate(fields, c("key", "value"), ":") %>% # split key and value into 2 cols
  pivot_wider(batch_id, names_from = "key", values_from = "value") ## pivot

## ... and add extra validation
passports_wide %>% 
  mutate(byr_valid = parse_integer(byr) %>% between(1920, 2002),
         iyr_valid = parse_integer(iyr) %>% between(2010, 2020),
         eyr_valid = parse_integer(eyr) %>% between(2020, 2030),
         hgt_valid = 
           (str_detect(hgt, "^\\d*cm$") & (str_extract(hgt, "\\d*") %>% parse_integer() %>% between(150, 193))) |
           (str_detect(hgt, "^\\d*in$") & (str_extract(hgt, "\\d*") %>% parse_integer() %>% between(59, 76))),
         hcl_valid = str_detect(hcl, "^#[0-9a-f]{6}$"),
         ecl_valid = ecl %in% c("amb", "blu", "brn", "gry", "grn", "hzl", "oth"),
         pid_valid = str_detect(pid, "^\\d{9}$")) %>% 
  mutate_all(replace_na, replace = FALSE) %>% 
  mutate(valid = byr_valid & iyr_valid & eyr_valid & hgt_valid & hcl_valid & ecl_valid & pid_valid) %>%
  summarise(n_valid = sum(valid))

