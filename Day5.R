library(tidyverse)

# test <- 
#   c("FBFBBFFRLR",
#   "BFFFBBFRRR",
#   "FFFBBBFRRR",
#   "BBFFBBFRLL")

dat <- read_lines("Day5Data.txt")

binaries <- 2^(9:0)

seat_nums <- 
  dat %>% 
  map_int(function(seat) {
    ## If back or right, high half (1) else low half
    seat_vals <- (str_split_fixed(seat, "", n = 10) %in% c("B", "R")) #* fblr
    sum(seat_vals * as.integer(binaries))
  })

max(seat_nums)

## Part II
all_seats <- 0:sum(binaries)
## remove first and last which don't exist
valid_seats <- all_seats[all_seats >= min(seat_nums) &
                           all_seats <= max(seat_nums)]
##And my seat is...
setdiff(valid_seats, seat_nums)
