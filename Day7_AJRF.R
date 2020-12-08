library(tidyverse)
options(scipen = 999)

rules <- read_lines("Day7Data.txt")
rules <- rules %>% tibble(rules = .)

rules_links <- 
  rules %>% 
  mutate(rules = str_remove_all(rules, "bag[s]?"),
         rules = str_squish(rules)) %>% 
  separate(rules, c("outer_colour", "contents"), " contain ") %>% 
  separate_rows(contents, sep = ",") %>% 
  mutate(contents = str_trim(str_remove(contents, "\\."))) %>% 
  separate(contents, c("number", "inner_colour"), " ", extra = "merge") %>% 
  mutate(number = ifelse(number == "no", 0, number) %>% as.integer(),
         inner_colour = ifelse(inner_colour == "other", NA, inner_colour))


## Find those containing shiny gold
gold_containers <- c()

inner <- "shiny gold"

while (length(inner) > 0) {
  outer <- 
    rules_links %>% 
    inner_join(tibble(inner_colour = inner), by = "inner_colour") %>% 
    pull(outer_colour)
  
  gold_containers <- c(gold_containers, outer)
  inner <- outer
}

gold_containers <- unique(gold_containers)
length(gold_containers)

## Part 2:  How many bags inside one shiny gold bag
nodes <-
  rules_links %>% 
  distinct(outer_colour) %>% 
  rename(colour = outer_colour) 

edges <- rules_links %>% 
  rename(from = outer_colour, to = inner_colour, times = number)

edges_replicated <- 
  pmap_dfr(edges, function(from, to, times) {
    tibble(from = rep(from, times), to = rep(to, times))
  })

## Start at...
start <- "shiny gold"
gold_children <- edges_replicated %>% 
  filter(from == start) %>% 
  rename(to_1 = to)

counter <- 1
totaliser <- nrow(gold_children)

while (!finished) {
  old_to_col <- paste0("to_", counter)
  new_to_col <- sym(paste0("to_", counter + 1))
  
  gold_children <- gold_children %>% 
    ## inner join to exclude prev leaf nodes
    inner_join(edges_replicated, by = set_names("from", old_to_col)) %>% 
    rename({{new_to_col}} := to)
  
  if (nrow(gold_children) == 0) {
    break
  }
  ## else...
  totaliser <- totaliser + nrow(gold_children)
  counter <- counter + 1
}

totaliser

