library(tidyverse)
library(tictoc)
options(scipen = 999)

## Part 1
dat <- read_csv("Day10/Day10Data.txt", col_names = "jolts", col_types = "i")
#test_dat <- read_csv("Day10/TestData.txt", col_names = "jolts", col_types = "i")

add_first_and_last <- function(dat) {
  
  device_j <- max(dat$jolts) + 3
  dat %>% 
    ## add extra rows for the outlet (0) and my device (max + 3)
    bind_rows(tibble(jolts = c(0, device_j))) %>%
    arrange(jolts) 
}

dat_plus_first_and_last <- add_first_and_last(dat)

dat_plus_first_and_last %>% 
  mutate(diff = jolts - lag(jolts, default = 0)) %>% 
  summarise(count_1_diffs = sum(diff == 1),
            count_3_diffs = sum(diff == 3),
            final_product = count_1_diffs * count_3_diffs)

## Part 2: counting the possible arrangements

## New approach: get each changeable chunk and multiply up the combinations
# If there's a gap of 3, it's not removable, so just look at each part between a gap of three (or the end)
#dat <- tibble(jolts = c(1, 4, 5, 6, 7, 10, 11, 12, 15, 16))
#dat <- read_csv("Day10/TestData.txt", col_names = "jolts", col_types = "i")
dat <- read_csv("Day10/Day10Data.txt", col_names = "jolts", col_types = "i")

dat <- dat %>% 
  add_first_and_last() %>%  
  mutate(row_id = row_number())

## Any which already have a gap of 3 between can't be removed
## Nor can first or last
dat <- 
  dat %>% 
  mutate(diff = jolts - lag(jolts),
         diff_ahead = lead(diff),
         is_removable = !(diff==3 | diff_ahead == 3 | (diff == 2 & diff_ahead == 2)),
         is_removable = ifelse(row_id == 1 | row_id == nrow(dat), FALSE, is_removable))

## Only the bits with removable data can be changed
## Looks at each changeable combo, then multiply the combinations
grouped_chunks <- 
  dat %>% 
  mutate(grp_id = cumsum(is_removable != lag(is_removable, default = FALSE)),
         grp_id = if_else(is_removable, grp_id, 0L)) %>% 
  filter(grp_id > 0) %>% 
  group_by(grp_id)

## Q:  How long are these contiguous chunks?
grouped_chunks %>% count() %>% ungroup() %>% summarise(max_n = max(n))
## A: max length 3, so we can be lazy and hard-code-y in the below!

count_combos <- function(chunk) {
  
  if (nrow(chunk) == 1) {
    ret_val <- 2   ## include or exclude
  } else if (nrow(chunk) == 2 & 
             sum(chunk$diff) == 2 & 
             sum(chunk$diff_ahead) == 2) {
    ## If 2 values are both they're 1 apart, could remove both and be fine,
    ## so 4 combos (unchanged, remove one, remove other, remove both)
    
    ret_val <- 4
  } else {
    ## check them!  Up to 3 in a chunk
    jolts <- chunk$jolts
    ## add the one before and after
    one_before <- min(jolts) - chunk$diff[[1]]
    one_after <- max(jolts) + chunk$diff[[nrow(chunk)]]
    jolts_bookended <- c(one_before, jolts, one_after)
    
    ##remove 1-3 values
    new_jolts <- tibble(jolts = jolts_bookended)
    
    ## remove 1
    singles <- 2:(nrow(new_jolts)-1)
    single_is_valid <- map_lgl(singles, function(row) {
      new_jolts %>% 
        slice(-row) %>% 
        mutate(diff = jolts - lag(jolts)) %>% 
        summarise(is_valid = max(diff, na.rm = T) <= 3) %>% 
        pull(is_valid)
    })
    valid_singles <- singles[single_is_valid]
    single_removal_count <- length(valid_singles)
    
    ## remove 2
    # start with removable singles
    doubles <- 
      expand_grid(r1 = valid_singles, r2 = valid_singles) %>% 
      filter(r2 > r1)
    double_is_valid <- pmap_lgl(doubles, function(r1, r2) {
      new_jolts %>% 
        slice(-r1, -r2) %>% 
        mutate(diff = jolts - lag(jolts)) %>% 
        summarise(is_valid = max(diff, na.rm = T) <= 3) %>% 
        pull(is_valid)
    })
    valid_doubles <- doubles[double_is_valid, ]
    double_removal_count <- nrow(valid_doubles)
    
    ## remove 3? - this is the max so that's 3 in a row, which can't work (gap will always be > 3)
    
    ## Add up these, plus 1 for leaving unchanged
    ret_val <- single_removal_count + double_removal_count + 1
  }
    
  as.integer(ret_val)
}

chunks <- 
  grouped_chunks %>% 
  nest() %>% 
  mutate(cnt = map_int(data, count_combos))
  
prod(chunks$cnt)

