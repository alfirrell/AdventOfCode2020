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

## Part 2

## Rules 8 and 11 now have loops
#8: 42 | 42 8
#11: 42 31 | 42 11 31

loop_rules <- c(8, 11)

dat <- read_lines("Day19Data.txt")
# Test data
#dat <- read_lines("Day19TestData.txt")

rules <- dat[1:(which(dat == "")-1)]
messages <- dat[(which(dat == "")+1):length(dat)]

rules_parsed <- 
  rules %>% 
  tibble(rule = .) %>% 
  separate(rule, c("rule_num", "rule"), ": ") %>% 
  mutate(rule_num = as.integer(rule_num)) %>% 
  arrange(rule_num)

## Manually update rules 8 and 11
rules_parsed[9, "rule"] <- "42 | 42 8"
rules_parsed[12, "rule"] <- "42 31 | 42 11 31"

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

## 8 & 11: check dependencies
dep_rules <- 
  no_pipes_rules_long %>% 
  filter(rule %in% loop_rules,
         rule_num != rule) %>% 
  distinct(rule_num)

message("Rule(s) ", str_flatten(dep_rules, collapse = ", "), " depends on rules ", str_flatten(loop_rules, collapse = ", "))
## Only Rule 0 depends on 8 & 11

## Rerun the matching, but make 8 and 11 a repeating group

while (rules_parsed %>% anti_join(letters, by = "rule_num") %>% nrow() > 0) {
  
  updated_rules_long <- 
    no_pipes_rules_long %>% 
    left_join(letters, by = c("rule" = "rule_num"))
  
  done_rules <- 
    updated_rules_long %>% 
    group_by(rule_num) %>% 
    summarise(is_done = all(!is.na(rule_letter) | rule == rule_num), ## the loop will stay NA for now
              .groups = "drop") %>% 
    filter(is_done) %>% 
    anti_join(letters, by = "rule_num")  ## don't repeat already done ones
  
  updated_letters <- 
    rules_long %>% 
    semi_join(done_rules, by = "rule_num") %>% 
    left_join(updated_rules_long %>% select(-rule), by = c("rule_num", "rule_item_num")) %>% 
    mutate(rule_letter = if_else(rule == "|", "|", rule_letter),
           ## If it's a loop rule, insert a dummy x8x or x11x
           rule_letter = if_else(is.na(rule_letter) & rule_num %in% loop_rules, str_c("x", rule_num, "x"), rule_letter)) %>% 
    group_by(rule_num) %>% 
    summarise(rule_letter = str_flatten(rule_letter), .groups = "drop") %>% 
    mutate(rule_letter = if_else(str_detect(rule_letter, "\\|"), str_c("(", rule_letter, ")"), rule_letter))
  
  looped <- updated_letters %>% 
    filter(rule_num %in% loop_rules) %>% 
    rowwise() %>% 
    mutate(wildcard = str_c("x", rule_num, "x"),
           without_wildcard = str_remove(rule_letter, wildcard),
           loop1 = str_replace_all(rule_letter, wildcard, without_wildcard),
           loop2 = 
             str_replace_all(
               str_replace_all(rule_letter, wildcard, rule_letter), 
               wildcard, without_wildcard),
           loop3 = 
             str_replace_all(
               str_replace_all(
                 str_replace_all(rule_letter, wildcard, rule_letter),
                 wildcard, rule_letter), 
               wildcard, without_wildcard),
           loop4 = 
             str_replace_all(
               str_replace_all(
                 str_replace_all(
                   str_replace_all(rule_letter, wildcard, rule_letter),
                   wildcard, rule_letter),
                 wildcard, rule_letter), 
               wildcard, without_wildcard),
           # loop5 = 
           #   str_replace_all(
           #     str_replace_all(
           #       str_replace_all(
           #         str_replace_all(
           #           str_replace_all(rule_letter, wildcard, rule_letter),
           #           wildcard, rule_letter),
           #         wildcard, rule_letter),
           #       wildcard, rule_letter),
           #     wildcard, without_wildcard),
           loop_options = paste0("(", paste0(c(without_wildcard, loop1, loop2, loop3, loop4), collapse = "|"), ")")
    ) %>% 
    ungroup() %>% 
    select(rule_num, loop_options)
  
  updated_letters <- 
    updated_letters %>% 
    left_join(looped, by = "rule_num") %>% 
    mutate(rule_letter = if_else(!is.na(loop_options), loop_options, rule_letter))
  
  letters <- bind_rows(letters, updated_letters) #%>% distinct()
}


## Apply rule zero to the messages and count the matches
rule_zero <- letters %>% 
  filter(rule_num == 0) %>% 
  pull(rule_letter) %>% 
  str_c("^", ., "$")

num_matches <- str_detect(messages, rule_zero) %>% sum()
message(num_matches, " matches")
