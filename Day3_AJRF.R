library(tidyverse)

## Part 1

## Read in the data, and split into a matrix
tree_pattern <- read_lines("Day3Data.txt") 
row_count <- length(tree_pattern)
col_count <- str_length(tree_pattern[[1]])

tree_pattern <- tree_pattern %>% 
  str_split_fixed("", col_count)

## Could do this in a while loop, but I'm going to try to work out all the 
## coordinates up front...

## Create the coordinates we want:  starting at (1, 1) and adding 3 cols and 1 row each time.
row_coords <- seq_len(row_count)  # 1, 2, 3...
col_coords <- seq(1, row_count * 3, 3) # 1, 4, 7

## Our coords go out to 967, but the tree pattern only has 31 cols: after that 
## the pattern repeats.  We can either glue lots of copies of the pattern 
## together column-wise, or just find the remainder after dividing by ncol
## i.e. if there are 31 columns, col 32 is the same as col 1, as is col 63, col 94 etc.
## (the only exception is we need to handle multiples of ncol as the remainder is 0 
## and we want them to come out as 31)
effective_col_coords <- col_coords %% col_count
effective_col_coords[effective_col_coords == 0] <- col_count

## Now look at each point and find if there's a tree there (#)
is_tree <- map2_lgl(row_coords, effective_col_coords, ~tree_pattern[.x, .y] == "#")

## count the trees
sum(is_tree)


## Part 2: multiple different slopes
## Make a function from the above, and pass in different slopes

# Right 1, down 1
slopes <- list(c(1, 1),
               c(3, 1),
               c(5, 1),
               c(7, 1),
               c(1, 2))
num_trees <- 
  slopes %>% 
  map_int(function(slope) {
    
    right_steps <- slope[[1]]
    down_steps <- slope[[2]]
    #message("Slope: ", right_steps, " right, ", down_steps, " down")
    
    ## Create the coordinates we want:  starting at (1, 1) and adding n cols and m rows each time.
    row_coords <- seq(1, row_count, down_steps)  ## rows are finite: stop after 323 irrespective of the step size
    ## same number of col coords as rows
    col_coords <- seq(1, length(row_coords) * right_steps, right_steps)
    ## And adjust for the repeating columns
    effective_col_coords <- col_coords %% col_count
    effective_col_coords[effective_col_coords == 0] <- col_count
    
    ## Now look at each point and find if there's a tree there (#)
    is_tree <- map2_lgl(row_coords, effective_col_coords, ~tree_pattern[.x, .y] == "#")
    
    ## count the trees
    sum(is_tree)
    
  })

print(num_trees)

## and return the product of these values
prod(num_trees)
