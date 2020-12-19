library(tidyverse)

dat <- read_lines("Day19Data.txt")
# dat <- c(  "0: 4 1 5",
#            "1: 2 3 | 3 2",
#            "2: 4 4 | 5 5",
#            "3: 4 5 | 5 4",
#            '4: "a"',
#            '5: "b"',
#            "",
#            "ababbb",
#            "bababa",
#            "abbbab",
#            "aaabbb",
#            "aaaabbb")

rules <- dat[1:(which(dat == "")-1)]
messages <- dat[(which(dat == "")+1):length(dat)]

rules_parsed <- 
  rules %>% 
  tibble(rule = .) %>% 
  separate(rule, c("rule_num", "rule"), ": ") %>% 
  mutate(rule_num = as.integer(rule_num)) %>% 
  arrange(rule_num)

rules_long <- 
  rules_parsed %>% 
  separate_rows(rule, sep = " ") %>% 
  group_by(rule_num) %>% 
  mutate(rule_item_num = row_number()) %>% 
  ungroup()

no_pipes_rules_long <- 
  rules_long %>% 
  filter(rule != "|")

letters <- 
  rules_long %>% 
  filter(str_detect(rule, '"')) %>% 
  mutate(rule_letter = str_remove_all(rule, '"')) %>% 
  select(rule_num, rule_letter)

no_pipes_rules_long <- 
  no_pipes_rules_long %>% 
  filter(!str_detect(rule, '"')) %>% 
  mutate(rule = as.integer(rule))

while (rules_parsed %>% anti_join(letters, by = "rule_num") %>% nrow() > 0) {
    
  updated_rules_long <- 
    no_pipes_rules_long %>% 
    left_join(letters, by = c("rule" = "rule_num"))
  
  done_rules <- 
    updated_rules_long %>% 
    group_by(rule_num) %>% 
    summarise(is_done = all(!is.na(rule_letter)), .groups = "drop") %>% 
    filter(is_done)
  
  updated_letters <- 
    rules_long %>% 
    semi_join(done_rules, by = "rule_num") %>% 
    left_join(updated_rules_long %>% select(-rule), by = c("rule_num", "rule_item_num")) %>% 
    mutate(rule_letter = replace_na(rule_letter, "|")) %>% 
    group_by(rule_num) %>% 
    summarise(rule_letter = str_flatten(rule_letter), .groups = "drop") %>% 
    mutate(rule_letter = if_else(str_detect(rule_letter, "\\|"), str_c("(", rule_letter, ")"), rule_letter))
  
  letters <- bind_rows(letters, updated_letters) %>% distinct()
}

rule_zero <- letters %>% 
  filter(rule_num == 0) %>% 
  pull(rule_letter) %>% 
  str_c("^", ., "$")

num_matches <- str_detect(messages, rule_zero) %>% sum()
message(num_matches, " matches")


