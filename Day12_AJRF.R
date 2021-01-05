library(tidyverse)

# dat <- tibble(inst = c("F10",
#                        "N3",
#                        "F7",
#                        "R90",
#                        "F11"))
dat <- read_csv("Day12/Day12Data.txt", col_names = "inst", col_types = "c")

dat <- dat %>% 
  mutate(command = str_sub(inst, 1, 1),
         value = as.integer(str_sub(inst, 2, 100)))

## check the left and right values: all right-angles?
dat %>% 
  filter(command %in% c("L", "R")) %>% 
  distinct(value)
## Yes, hurrah!

facing_start <- 90

dat <- dat %>% 
  mutate(north = case_when(command == "N" ~ value,
                           command == "S" ~ -value,
                           TRUE ~ 0L),
         east = case_when(command == "E" ~ value,
                           command == "W" ~ -value,
                           TRUE ~ 0L),
         facing_change = case_when(command == "R" ~ value,
                                   command == "L" ~ -value,
                                   TRUE ~ 0L),
         facing_cume = cumsum(facing_change) + facing_start,
         facing_cume = facing_cume %% 360,
         forward_north = if_else(command == "F",
                                 case_when(facing_cume == 0 ~ value,
                                           facing_cume == 180 ~ -value,
                                           TRUE ~ 0L),
                                 0L),
         forward_east = if_else(command == "F",
                                 case_when(facing_cume == 90 ~ value,
                                           facing_cume == 270 ~ -value,
                                           TRUE ~ 0L),
                                 0L),
         north_cume = cumsum(north + forward_north),
         east_cume = cumsum(east + forward_east))

dat %>% 
  summarise(north = last(north_cume),
            east = last(east_cume),
            manhattan = abs(north) + abs(east))


## Part 2
# dat <- tibble(inst = c("F10",
#                        "N3",
#                        "F7",
#                        "R90",
#                        "F11"))
dat <- read_csv("Day12/Day12Data.txt", col_names = "inst", col_types = "c")

dat <- dat %>% 
  mutate(command = str_sub(inst, 1, 1),
         value = as.numeric(str_sub(inst, 2, 100)))

# waypoint_north_start <- 1
# waypoint_east_start <- 10

# dat <- bind_rows(tibble(waypoint_north_change = 1, waypoint_east_change = 10,
#                         waypoint_north = 1, waypoint_east = 10),
#                  dat)

wp <- list(north = 1, east = 10)
ship <- list(north = 0, east = 0)

## Loop is a bit slow, but I was having problems with getting 
## the lag() working right in dplyr
for (i in 1:nrow(dat)) {
  command <- dat %>% slice(i) %>% pull(command)
  value <- dat %>% slice(i) %>% pull(value)
  
  new_north <-
    case_when(command == "N" ~ wp$north + value,
              command == "S" ~ wp$north - value,
              command == "R" ~ 
                case_when(value == 90 ~ -wp$east,
                          value == 180 ~ -wp$north,
                          value == 270 ~ wp$east,
                          TRUE ~ wp$north),
              command == "L" ~ 
                case_when(value == 90 ~ wp$east,
                          value == 180 ~ -wp$north,
                          value == 270 ~ -wp$east,
                          TRUE ~ wp$north),
              TRUE ~ wp$north)
  
  new_east <-
    case_when(command == "E" ~ wp$east + value,
              command == "W" ~ wp$east - value,
              command == "R" ~ 
                case_when(value == 90 ~ wp$north,
                          value == 180 ~ -wp$east,
                          value == 270 ~ -wp$north,
                          TRUE ~ wp$east),
              command == "L" ~ 
                case_when(value == 90 ~ -wp$north,
                          value == 180 ~ -wp$east,
                          value == 270 ~ wp$north,
                          TRUE ~ wp$east),
              TRUE ~ wp$east)
  
  ## Only update now so the east calc doesn't get messed up by changing the north beforehand
  wp$north <- new_north
  wp$east <- new_east
  
  ship$north <- ship$north + if_else(command == "F", value * wp$north, 0)
  ship$east <- ship$east + if_else(command == "F", value * wp$east, 0)

  dat[i, "north"] <- ship$north
  dat[i, "east"] <- ship$east
}

dat %>% 
  summarise(north = last(north),
            east = last(east),
            manhattan = abs(north) + abs(east))
