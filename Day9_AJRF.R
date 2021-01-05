library(tidyverse)

# test <- c(35,
# 20,
# 15,
# 25,
# 47,
# 40,
# 62,
# 55,
# 65,
# 95,
# 102,
# 117,
# 150,
# 182,
# 127,
# 219,
# 299,
# 277,
# 309,
# 576)
# 
# dat <- test

dat <- read_lines("Day09/Day9Data.txt")
dat <- as.double(dat)

preamble_length <- 25
start_at <- 1

done <- FALSE
while (!done) {

  end_at <- start_at + preamble_length - 1
  preamble <- dat[start_at:end_at]
  valid_sums <- expand_grid(a = preamble, b = preamble) %>% 
    mutate(total = a + b)
  if (!dat[end_at + 1] %in% valid_sums$total) {
    done <- TRUE
    invalid_number <- dat[end_at + 1]
    message(invalid_number, " is not valid")
  }
  start_at <- start_at + 1
}

## Part 2: find the range of values within dat which sums to this invalid number

range_length <- 2  ## start with length 2 and build up

done <- FALSE
while (!done) {
  
  for (start_pos in 1:(length(dat) - range_length + 1)) {
    rng <- dat[start_pos:(start_pos + range_length - 1)]
    
    if (sum(rng) == invalid_number) {
      done <- TRUE
      message("Values ", str_flatten(rng, collapse = ", "), " sum to ", invalid_number)
      encryption_weakness <- min(rng) + max(rng)
      message("Encryption weakness = ", encryption_weakness)
      break
    }
  }
  
  range_length <- range_length + 1
}
