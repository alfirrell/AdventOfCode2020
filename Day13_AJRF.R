library(tidyverse)

#load the data
lines <- read_lines("Day13/Day13Data.txt")

## First line is my arrival time
arrival_time <- lines[[1]] %>% as.integer()
buses_raw <- (lines[[2]] %>% str_split(","))[[1]]

## Second line is the list of buses
buses <- 
  buses_raw %>% 
  tibble(bus = .) %>% 
  filter(bus != "x") %>% 
  mutate(bus = as.numeric(bus),
         ## find the multiple of <bus> after <arrival time>
         ## Integer divide will find the one just before, so add 1 and multiple back out
         next_bus = ((arrival_time %/% bus) + 1) * bus,
         waiting_time = next_bus - arrival_time)

# First bus arriving after me = the one with the lowest waiting time.
buses %>% 
  top_n(-1, waiting_time) %>% 
  mutate(id_times_waiting_time = bus * waiting_time)


## Part 2

## TEST DATA
#buses_raw <- c("7","13","x","x","59","x","31","19")
buses_raw <- (lines[[2]] %>% str_split(","))[[1]]

buses2 <- 
  buses_raw %>% 
  tibble(bus = .) %>% 
  mutate(after_mins = row_number() - 1) %>% 
  filter(bus != "x") %>% 
  mutate(bus = as.numeric(bus))

## We want a time where
## t is an exact multiple of bus 1
## t + m is an exact multiple of bus 2, where m is number of minutes after (distance of bus 2 from the first position in the vector)
## t + n is an exact vector of bus 3 etc...
# time %% bus1 == 0
# (time + after2) %% bus2 == 0
# (time + after3) %% bus3 == 0
# 
# ## Do it by hand to get the feel, then generalise
# bus1 <- buses2 %>% slice(1) %>% pull(bus)
# bus2 <- buses2 %>% slice(2) %>% pull(bus)
# after2 <- buses2 %>% slice(2) %>% pull(after_mins)
# bus3 <- buses2 %>% slice(3) %>% pull(bus)
# after3 <- buses2 %>% slice(3) %>% pull(after_mins)
# 
# ## Start at 0, and add bus1 until we reach a number where (time + after2) %% bus2 == 0
# time <- 0
# while ((time + after2) %% bus2 != 0) {
#   time <- time + bus1
# }
# 
# ## This will be true of time, and [time + any multiple of bus1*bus2]
# ## So the next increment is not bus1 but bus1*bus2
# multiple <- bus1 * bus2
# 
# ## TEST
# #(time + multiple) %% bus2
# #((time + multiple) - after_mins) %% bus1
# 
# ## Continue with bus3
# while ((time + after3) %% bus3 != 0) {
#   time <- time + multiple  ## add bus1*bus2 to get the next valid time
# }
# #TEST
# time %% bus1 == 0
# (time + after2) %% bus2 == 0
# (time + after3) %% bus3 == 0
# 
# ## New multiple becomes bus1 * bus2 * bus3

## Generalising:
time <- 0
multiple <- 1

for (i in 1:(nrow(buses2) - 1)) {
  bus1 <- buses2 %>% slice(i) %>% pull(bus)
  bus2 <- buses2 %>% slice(i + 1) %>% pull(bus)
  after2 <- buses2 %>% slice(i + 1) %>% pull(after_mins)
  
  multiple <- multiple * bus1
  
  while ((time + after2) %% bus2 != 0) {
    time <- time + multiple
  }
}

## TEST the first few
time %% (buses2 %>% slice(1) %>% pull(bus)) == 0
(time + (buses2 %>% slice(4) %>% pull(after_mins))) %% (buses2 %>% slice(4) %>% pull(bus)) == 0
(time + (buses2 %>% slice(5) %>% pull(after_mins))) %% (buses2 %>% slice(5) %>% pull(bus)) == 0

message("Time of the first bus is ", time)

