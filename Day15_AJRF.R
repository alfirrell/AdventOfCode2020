library(tidyverse)
library(tictoc)

line <- read_lines("Day15/Day15Data.txt")
dat <- (line %>% str_split(","))[[1]] %>% as.integer()
# Tests
#dat <- c(0,3,6)
#dat <- c(1,3,2)
#dat <- c(2,3,1)
#tic()

counter <- tibble(number = dat, 
                  last_mention_turn = seq_along(dat),
                  last_but_one_mention_turn = NA)

startup_length <- length(dat)

dict <- vector("list", 100)
dict[dat + 1] <- 1:startup_length  ## add 1 to allow for zeros

first_turn <- 2
final_turn <- 100

for (this_turn in first_turn:final_turn) {
  if (this_turn <= (startup_length + 1)) {
    ## do nothing, store the prev value for next time
    prev_num <- dat[this_turn - 1]
  } 
  
  if (is.null(dict[[prev_num + 1]])) {  ## is new
    dict[[prev_num + 1]] <- this_turn
    prev_num <- 0
  } else {
    turn_diff <- dict[prev_num + 1] - this_turn
    dict[turn_diff] <- this_turn
    prev_num <- turn_diff
  }
}

startTime = time.time()
nums = dict(zip(day15_input, range(1, len(day15_input) + 1)))
n = 2020
i = 1
while i < n:
  if i < len(day15_input):
  lastNumber = max(nums, key = nums.get)
elif lastNumber in nums:
  temp = lastNumber
lastNumber = i - nums[lastNumber]
nums[temp] = i
else:
  nums[lastNumber] = i
lastNumber = 0
i += 1
endTime = time.time()
print("The 2020th number is " + str(lastNumber) + ".")
print("This ran in " + str(endTime - startTime) + " seconds, or " + str((endTime - startTime) / 2020) + " per loop. Based on this, it will take " + str(((endTime - startTime) / 2020) * 30000000) + " seconds to run 30000000 loops.")






for (this_turn in first_turn:final_turn) {
  
  prev_mention <- counter %>% filter(last_mention_turn == this_turn - 1)

  if (nrow(prev_mention) == 0) {
    output <- 0
  } else {
    if (is.na(prev_mention$last_but_one_mention_turn)) {
      ## That was the first mention
      output <- 0
    } else {
      output <- prev_mention$last_mention_turn - prev_mention$last_but_one_mention_turn
    }
  }

  ## add/update
  output_mentions <- counter %>% filter(number == output)
  if (nrow(output_mentions) > 0) {
    #output_times <- output_mentions$times + 1
    output_last_but_one <- output_mentions$last_mention_turn
    output_last <- this_turn
  } else {
    output_last_but_one <- NA
    output_last <- this_turn
  }
  
  counter <- bind_rows(
    counter %>% filter(number != output),
    tibble(number = output, 
           last_mention_turn = output_last,
           last_but_one_mention_turn = output_last_but_one)
  )
  
  #message(output)
  #outputs <- c(outputs, output)
  if (this_turn %% 500 == 0) message("Turn ", this_turn, "...")
}

#toc()
message("2020th number: ", output)


## Part 2

# Given 0,3,6, the 30000000th number spoken is 175594.
# Given 1,3,2, the 30000000th number spoken is 2578.
# Given 2,1,3, the 30000000th number spoken is 3544142.
# Given 1,2,3, the 30000000th number spoken is 261214.
# Given 2,3,1, the 30000000th number spoken is 6895259.
# Given 3,2,1, the 30000000th number spoken is 18.
# Given 3,1,2, the 30000000th number spoken is 362.

# 38 secs for 10000
## hours for 30m: 31.6 ... :-(
# (30000000 / 10000) * 38 / 3600



