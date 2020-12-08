library(tidyverse)

## Method 2: self-join
raw_vals <- read_csv("Day01/Day1Data.csv", col_types = "i", col_names = "val")

raw_vals %>% 
  mutate(val2 = 2020 - val,
         product = val * val2) %>% 
  filter(val > val2) %>% 
  semi_join(raw_vals, by = c("val2" = "val")) ## keep only valid val2

## Puzzle 2: expand_grid + join
expand_grid(val1 = raw_vals$val,
            val2 = raw_vals$val) %>% 
  filter(val1 > val2) %>%
  mutate(val3 = 2020 - (val1 + val2),
         product = val1 * val2 * val3) %>% 
  filter(val2 > val3) %>% 
  semi_join(raw_vals, by = c("val3" = "val")) ## keep only valid val3
