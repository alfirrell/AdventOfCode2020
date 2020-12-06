## Day6
library(tidyverse)

txt <- read_lines("Day6Data.txt")
txt <- txt %>% tibble(txt = .)

## split by space
grp_count <- 
  txt %>% 
  mutate(grp = cumsum(txt == "")) %>% 
  group_by(grp) %>% 
  summarise(txt = str_flatten(txt)) %>% 
  mutate(letter_count = map_int(txt, function(x) {
    str_split(x, "")[[1]] %>% n_distinct()
  }))

grp_count$letter_count %>% sum()


## Part2:  Questions *everyone* in the group answered
grp_count2 <- 
  txt %>%
  mutate(grp = cumsum(txt == ""),
         txt_split = str_split(txt, "")) %>% 
  filter(txt != "") %>% 
  group_by(grp) %>% 
  summarise(intersect_count = length(reduce(txt_split, intersect)))

grp_count2$intersect_count %>% sum()

 