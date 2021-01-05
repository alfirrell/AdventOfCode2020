library(tidyverse)

occupied_sym <- "#"

## find seats around and change state based on rules
update_state <- function(seats) {
  
  updated_seats <- 
    seats %>% 
    left_join(seats %>% select(state_1 = state, row, col), by = c("row_before" = "row", "col_before" = "col")) %>%  
    left_join(seats %>% select(state_2 = state, row, col), by = c("row_before" = "row", "col" = "col")) %>% 
    left_join(seats %>% select(state_3 = state, row, col), by = c("row_before" = "row", "col_after" = "col")) %>% 
    left_join(seats %>% select(state_4 = state, row, col), by = c("row" = "row", "col_before" = "col")) %>% 
    left_join(seats %>% select(state_5 = state, row, col), by = c("row" = "row", "col_after" = "col")) %>% 
    left_join(seats %>% select(state_6 = state, row, col), by = c("row_after" = "row", "col_before" = "col")) %>% 
    left_join(seats %>% select(state_7 = state, row, col), by = c("row_after" = "row", "col" = "col")) %>% 
    left_join(seats %>% select(state_8 = state, row, col), by = c("row_after" = "row", "col_after" = "col")) %>% 
    ## we could pivot_longer and sum-if here, but for now it's as easy to just check all 8 cols.
    mutate(n_surrounding_occupied = 
             if_else(replace_na(state_1, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_2, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_3, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_4, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_5, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_6, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_7, "") == occupied_sym, 1, 0) + 
             if_else(replace_na(state_8, "") == occupied_sym, 1, 0),
           state = case_when(state == "L" & n_surrounding_occupied == 0 ~ "#",
                             state == "#" & n_surrounding_occupied >= 4 ~ "L",
                             TRUE ~ state)
    ) %>% 
    select(-starts_with("state_"))
  
  updated_seats
}

lines <- read_csv("Day11/Day11Data.txt", col_names = "seats", col_types = "c")

line_len <- lines$seats[[1]] %>% str_length()

grid <- str_split_fixed(lines$seats, "", line_len)
colnames(grid) <- as.character(1:line_len)

seats <- 
  grid %>% 
  as_tibble() %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(-row, names_to = "col", values_to = "state") %>% 
  mutate(col = as.integer(col),
         row_before = row - 1,
         row_after = row + 1,
         col_before = col - 1,
         col_after = col + 1) %>% 
  filter(state != ".") ## don't need the spaces with no seat, as we have row & col 

done <- FALSE
while (!done) {
  
  new_seats <- 
    seats %>% 
    update_state()

  n_changes <- 
    seats %>% 
    inner_join(new_seats, by = c("row", "col"), suffix = c("_old", "_new")) %>% 
    filter(state_old != state_new) %>% 
    nrow()

  seats <- new_seats
  
  message(n_changes, " changes")
  if (n_changes == 0) {
    done <- TRUE
  }
}

## count occupied seats
n_occupied <- seats %>% filter(state == "#") %>% nrow()
message(n_occupied, " occupied seats")


## Part 2 - changing which seats are visible, changing rules a little
# For each seat, find the 8 (or fewer) surrounding seats (excluding floor)

# Functions ----
update_state2 <- function(seats) {
  
  ## Update the state of surrounding seats
  surrounds <- 
    surrounds %>% 
    select(-state) %>% 
    inner_join(seats, by = c("s_row" = "row", "s_col" = "col"))
  
  ## Join surrounding seats to each central seat, and check the state
  updated_seats <- 
    seats %>% 
    inner_join(surrounds %>% select(row, col, surround_state = state), 
               by = c("row" = "row", "col" = "col")) %>% 
    mutate(is_occupied = (replace_na(surround_state, "") == occupied_sym)) %>% 
    group_by(row, col, state) %>% ## keep the current state for the next step
    summarise(n_surrounding_occupied = sum(is_occupied)) %>% 
    ungroup() %>% 
    mutate(state = case_when(state == "L" & n_surrounding_occupied == 0 ~ "#",
                             state == "#" & n_surrounding_occupied >= 5 ~ "L",  ## now 5 not 4
                             TRUE ~ state)) %>% 
    select(row, col, state)
  
  updated_seats
}

#lines <- read_csv("Day11/TestData.txt", col_names = "seats", col_types = "c")
lines <- read_csv("Day11/Day11Data.txt", col_names = "seats", col_types = "c")

line_len <- lines$seats[[1]] %>% str_length()

grid <- str_split_fixed(lines$seats, "", line_len)
colnames(grid) <- as.character(1:line_len)

seats <- 
  grid %>% 
  as_tibble() %>% 
  mutate(row = row_number()) %>% 
  pivot_longer(-row, names_to = "col", values_to = "state") %>% 
  mutate(col = as.integer(col))

surrounds <- 
  bind_rows(
    seats %>% mutate(direction = "nw"),
    seats %>% mutate(direction = "n"),
    seats %>% mutate(direction = "ne"),
    seats %>% mutate(direction = "w"),
    seats %>% mutate(direction = "e"),
    seats %>% mutate(direction = "sw"),
    seats %>% mutate(direction = "s"),
    seats %>% mutate(direction = "se")
  ) %>% 
  mutate(s_row = row, 
         s_col = col,
         done = F)

loop_done = F
while (!loop_done) {
  already_done <- surrounds %>% filter(done)
  
  new_surrounds <- 
    surrounds %>% 
    filter(!done) %>% 
    mutate(s_row = case_when(str_detect(direction, "n") ~ s_row - 1L,
                             str_detect(direction, "s") ~ s_row + 1L,
                             TRUE ~ s_row),
           s_col = case_when(str_detect(direction, "w") ~ s_col - 1L,
                             str_detect(direction, "e") ~ s_col + 1L,
                             TRUE ~ s_col)) %>% 
    left_join(seats %>% select(s_row = row, s_col = col, s_state = state), 
              by = c("s_row", "s_col")) %>% 
    mutate(done = replace_na(s_state, "") != ".") %>% 
    select(-s_state)
  
  loop_done = (new_surrounds %>% filter(!done) %>% nrow() == 0)
  
  surrounds <- bind_rows(already_done, new_surrounds) 

}

## ... and tidy up: keep only the existing surrounding seats, remove any NA's
surrounds <- 
  surrounds %>% 
  select(-done) %>% 
  semi_join(seats %>% select(s_row = row, s_col = col), 
            by = c("s_row", "s_col"))


done <- FALSE
while (!done) {

  new_seats <- 
    seats %>% 
    update_state2()

  n_changes <- 
    seats %>% 
    inner_join(new_seats, by = c("row", "col"), suffix = c("_old", "_new")) %>% 
    filter(state_old != state_new) %>% 
    nrow()
  
  seats <- new_seats
  
  message(n_changes, " changes")
  if (n_changes == 0) {
    done <- TRUE
  }
}

## count occupied seats
n_occupied <- seats %>% filter(state == "#") %>% nrow()
message(n_occupied, " occupied seats")
