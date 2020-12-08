## Day 8
library(tidyverse)

dat <- read_delim("Day08/Day8Data.txt", delim = " ", col_names = c("op", "arg"), col_types = "ci")
dat <- dat %>% mutate(exec_order = NA_integer_)

## Run through in step order, as soon as repeat one we stop
is_infinite_loop <- FALSE
step_counter <- 1L
current_row <- 1L
accumulator <- 0L

while (!is_infinite_loop) {
  
  row <- dat %>% slice(current_row)
  op <- row %>% pull(op)
  arg <- row %>% pull(arg)
  
  if (op == "nop") {
    current_row <- current_row + 1
  } else if (op == "acc") {
    accumulator <- accumulator + arg
    current_row <- current_row + 1
  } else if (op == "jmp") { ## jump
    current_row <- current_row + arg
  } else {
    stop("Unrecognised operator")
  }
  

  ## Check new row: have we been here before?
  if (is.na(dat[current_row, "exec_order"])) {
    dat[current_row, "exec_order"] <- step_counter
  } else {
    ## we've done this before:  reached the infinite loop, so stop
    message("Hooray!  The accumulator is ", accumulator)
    is_infinite_loop <- TRUE
  }
  
  step_counter <- step_counter + 1
}



## Part 2

# test <- tribble(~op, ~arg,
#                 "nop", +0,
#                 "acc", +1,
#                 "jmp", +4,
#                 "acc", +3,
#                 "jmp", -3,
#                 "acc", -99,
#                 "acc", +1,
#                 "jmp", -4,
#                 "acc", +6)
# 
# ## Add a final row to finish at
# test <- bind_rows(test, tibble(op = "nop", arg = 0))
# test <- test %>% mutate(exec_order = NA_integer_)
# 
# dat <- test

## Add a dummy end row, and take a copy so we can reset each time
dat_orig <- read_delim("Day08/Day8Data.txt", delim = " ", col_names = c("op", "arg"), col_types = "ci")
dat_orig <- dat_orig %>% mutate(exec_order = NA_integer_)

dat_orig <- bind_rows(dat_orig, tibble(op = "nop", arg = 0, exec_order = NA_integer_))

nop_jmp_rows <- which(dat_orig$op %in% c("nop", "jmp"))


for (nop_jmp_row in nop_jmp_rows) {

  message("Amending nop/jmp in row ", nop_jmp_row)

  ## Reset the table
  dat <- dat_orig
  
  ## Update the op and run the test
  dat[nop_jmp_row, "op"] <- if_else(dat[nop_jmp_row, "op"] == "nop", "jmp", "nop")
  
  ## Run the infinite loop test

  is_infinite_loop <- FALSE
  step_counter <- 1L
  current_row <- 1L
  accumulator <- 0L
  
  while (!is_infinite_loop) {
    
    row <- dat %>% slice(current_row)
    op <- row %>% pull(op)
    arg <- row %>% pull(arg)
    
    if (op == "nop") {
      current_row <- current_row + 1
    } else if (op == "acc") {
      accumulator <- accumulator + arg
      current_row <- current_row + 1
    } else if (op == "jmp") { ## jump
      current_row <- current_row + arg
    } else {
      stop("Unrecognised operator")
    }
    
    ## Have we reached the end and succeeded?
    if (current_row == nrow(dat)) {
      message("Hooray!  The accumulator is ", accumulator)
      break
    }
    
    ## Check new row: have we been here before?
    if (is.na(dat[current_row, "exec_order"])) {
      dat[current_row, "exec_order"] <- step_counter
    } else {
      ## we've done this before:  reached the infinite loop, so stop
      is_infinite_loop <- TRUE
    }
    
    step_counter <- step_counter + 1
  }

  if (!is_infinite_loop) {
    ## That was the right change to avoid the infinite loop: stop now
    message("Hurrah!")
    break
  } 
}

