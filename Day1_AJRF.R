library(tidyverse)

## Part 1: 2 entries which sum to 2020
raw_vals <- read_csv("Day01/Day1Data.csv", col_types = "i", col_names = "val")
raw_vals <- raw_vals$val

expand_grid(val1 = raw_vals,
            val2 = raw_vals) %>% 
  ## keep just one combination of each pair, e.g. 1999-1 but not 1-1999
  ## and only the one(s) which sum to 2020
  filter(val1 > val2,
         val1 + val2 == 2020) %>%
  mutate(product = val1 * val2)

## Part 2: 3 entries which sum to 2020
expand_grid(val1 = raw_vals,
            val2 = raw_vals,
            val3 = raw_vals) %>% 
  ## keep just one combination of each triple (A > B > C)
  ## and only the one(s) which sum to 2020
  filter(val1 > val2,
         val2 > val3,
         val1 + val2 + val3 == 2020) %>% 
  mutate(product = val1 * val2 * val3)

## This might be a bit slow and painful if these get much bigger, as we're 
## creating the full N*N*N-row combinations.  Could do more filtering at 
## each stage to remove the impossible combinations
## e.g. where val1 + val2 > 2020, don't bother looking for val3.
## or use SQL-style inequality joins (where A > B)




