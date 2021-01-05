library(tidyverse)

ticket_values <- function(line) {
  str_split(line, ",")[[1]] %>% as.integer()
}

lines <- read_lines("Day16/Day16Data.txt")
## find the blanks
blanks <- which(lines == "")

## First tranche is fields
fields <- lines[1:(blanks[1]-1)]

## Next two tranches are tickets
my_ticket <- lines[blanks[2]-1] #%>% ticket_values()
other_tickets <- lines[(blanks[2]+2):length(lines)]

tickets <- tibble(who = c("me", rep("others", length(other_tickets))),
                  value = c(my_ticket, other_tickets)
                  ) %>% 
  mutate(ticket_num = row_number())

ticket_values <- tickets %>% 
  separate_rows(value, sep = ",", convert = T) %>% 
  group_by(ticket_num) %>% 
  mutate(field_num = row_number()) %>% 
  ungroup()

## Parse the valid field ranges
field_ranges <- 
  fields %>% 
  tibble(fields = .) %>% 
  separate(fields, c("field", "range"), ": ", ) %>% 
  separate_rows(range, sep = " or ") %>% 
  separate(range, c("from", "to"), "-", convert = T)

valid_values <- 
  field_ranges %>%
  mutate(values = map2(from, to, ~.x:.y)) %>%
  pull(values) %>% 
  reduce(union)

invalid_values <- 
  setdiff(ticket_values$value %>% unique(),
          valid_values) %>% 
  tibble(value = .)

## Find the invalid values and sum to give the "error rate"
ticket_values %>% 
  inner_join(invalid_values, by = "value") %>% 
  summarise(error_rate = sum(value))

## Part 2: which field is which?

# Remove the invalid tickets:
valid_ticket_values <- 
  ticket_values %>% 
  anti_join(ticket_values %>% 
              inner_join(invalid_values, by = "value") %>% 
              distinct(ticket_num), by = "ticket_num")

## Now find the valid ranges for each field...
valid_values_by_field <- 
  field_ranges %>%
  mutate(value = map2(from, to, ~.x:.y)) %>% 
  unnest(value)

## And therefore the invalid ones by field id
invalid_field_match <- 
  valid_ticket_values %>% 
  left_join(valid_values_by_field %>% select(field, value), by = "value") %>% 
  pivot_wider(id_cols = c(who, ticket_num, field_num), names_from = field, values_from = value) %>% 
  pivot_longer(cols = -c(who, ticket_num, field_num), names_to = "field", values_to = "value", values_drop_na = F) %>% 
  filter(is.na(value)) %>% 
  distinct(field, field_num)
  
# If there's a field which could be anything we'll be missing it, so add it back in
possibles <- 
  invalid_field_match %>% 
  mutate(dummy = T) %>% 
  pivot_wider(field_num, names_from = "field", values_from = "dummy") 

## Add in the missing field and field num (the one which can be anything)
missing_field <- setdiff(field_ranges$field %>% unique(), 
                         invalid_field_match$field)
possibles[, missing_field] <- NA

missing_field_num <- setdiff(valid_ticket_values$field_num %>% unique(),
                             invalid_field_match$field_num %>% unique())
possibles <- bind_rows(possibles, tibble(field_num = missing_field_num))
                            
## Count the possibilities: one id will only have one possible field, one will have two, and so on... 
possibles <- possibles %>% 
  pivot_longer(-field_num, names_to = "field", values_drop_na = F) %>% 
  filter(is.na(value)) %>% 
  group_by(field_num) %>% 
  mutate(poss_count = n(),
         prev_poss_count = poss_count - 1)

## Anti-join each one to its predecessor to leave the one unallocated one for each id
actuals <- possibles %>% 
  anti_join(possibles, by = c("field", "prev_poss_count" = "poss_count")) %>% 
  select(field_num, field)

## And find the product of the values on my ticket for the 'departure xxx' fields
ticket_values %>% 
  filter(who == "me") %>% 
  inner_join(actuals, by = "field_num") %>% 
  filter(str_starts(field, "departure ")) %>% 
  summarise(product = prod(value))
