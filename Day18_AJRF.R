library(tidyverse)

dat <- read_csv("Day18/Day18Data.txt", col_names = "formula", col_types = "c")

## Test data
test_dat <- tribble(~formula,
                     "1 + (2 * 3) + (4 * (5 + 6))",
                     "2 * 3 + (4 * 5)",
                     "5 + (8 * 3 + 9 + 3 * 4 * 3)",
                     "5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))",
                     "((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"
                     )

# calculate a chunk without parentheses
calculate_chunk_result <- function(formula_string) {
  
  bits <- (formula_string %>% str_split(" "))[[1]]

  ## Calculate the maths in the parens
  cume <- bits[1] %>% as.numeric()
  for (i in seq(2, length(bits), 2)) {
    operator <- bits[i]
    next_value <- bits[i + 1] %>% as.numeric()
    cume <- Reduce(operator, c(cume, next_value))
  }
  cume
}

calculate_result <- function(formula) {
  
  keep_going <- TRUE
  while (keep_going) {
    ## deal with inner parentheses first
    open_paren <- str_locate_all(formula, "\\(")[[1]][, "start"] %>% tail(1)
    if (length(open_paren) == 0) {
      ## no more parentheses, so do the final calc and finish up
      result <- formula %>% calculate_chunk_result()
      keep_going <- FALSE
    } else {
      close_paren <- str_locate(formula %>% str_sub(open_paren + 1), "\\)")[, "start"]
      close_paren <- open_paren + close_paren
      
      inner <- str_sub(formula, open_paren + 1, close_paren - 1)
      inner_result <- inner %>% calculate_chunk_result()
      
      ## ... and replace in the formula
      str_sub(formula, open_paren, close_paren) <- inner_result
      #formula
    }
  }
  result

}


## Test outputs
test_dat <- 
  test_dat %>% 
  mutate(result = map_dbl(formula, calculate_result),
         actual = c(51, 26, 437, 12240, 13632),
         check = result == actual) 
  
if (test_dat %>% filter(!check) %>% nrow() == 0) {
  message("All fine") ## No rows: all fine
} else {
  message("Test failing")
  test_dat %>% filter(!check)
}

## The real thing
dat <- 
  dat %>% 
  mutate(result = map_dbl(formula, calculate_result)) 

dat %>% summarise(summed_results = sum(result))


# Part 2: add before multiplying ------------------------------------------

## Change the chunk function
calculate_chunk_result <- function(formula_string) {
  
  bits <- (formula_string %>% str_split(" "))[[1]]
  
  ## Calculate the maths in the parens
  ## Do additions before multiplications
  values <- bits[seq(1, length(bits), 2)] %>% as.numeric()
  operators <- bits[seq(2, length(bits), 2)]
  
  additions <- which(operators == "+")
  while (length(additions) > 0) {
    addition_idx <- additions[1]
    sum_val <- values[addition_idx] + values[addition_idx + 1]
    
    ## replace the values with the result and remove that +
    values[addition_idx + 1] <- sum_val
    values <- values[-addition_idx]
    operators <- operators[-addition_idx]
    
    additions <- which(operators == "+")
  }
  
  cume <- values[1]
  for (i in seq_along(operators)) {
    cume <- cume * values[i + 1]
  }

  cume
}

test_dat <- 
  test_dat %>% 
  mutate(result = map_dbl(formula, calculate_result),
         actual = c(51, 46, 1445, 669060, 23340),
         check = result == actual) 

if (test_dat %>% filter(!check) %>% nrow() == 0) {
  message("All fine") ## No rows: all fine
} else {
  message("Test failing")
  test_dat %>% filter(!check)
}

## The real thing
dat <- read_csv("Day18/Day18Data.txt", col_names = "formula", col_types = "c")

dat <- 
  dat %>% 
  mutate(result = map_dbl(formula, calculate_result)) 

dat %>% summarise(summed_results = sum(result))
