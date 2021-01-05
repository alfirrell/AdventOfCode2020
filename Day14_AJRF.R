library(tidyverse)
library(R.utils)

# Functions -------------
bit_vec_to_num <- function(bit_vec) {
  
  bit_vec <- as.integer(bit_vec)
  powers <- 2^((length(bit_vec):1) - 1)
  sum(bit_vec * powers)
}

convert_masked_int <- function(int_value, mask) {
  
  split_mask <- str_split(mask, "")[[1]]
  is_x <- split_mask == "X"
  
  value_bits <- as.integer(rev(intToBits(int_value))) ## reverse so ...16-8-4-2-1
  
  ## If the mask is bigger than 32 bits, prepend some zeros
  size_diff <- length(split_mask) - length(value_bits)
  if (size_diff > 0) {
    value_bits <- c(rep(0, size_diff), value_bits)
  }
  
  ## Replace the bits with the mask
  new_value <- split_mask
  new_value[is_x] <- value_bits[is_x]
  
  bit_vec_to_num(new_value)
  
}

# Main code --------------

dat <- read_delim("Day14/Day14Data.txt", delim = "=", col_names = c("type", "value"), col_types = "cc")

# Test data
# dat <- tribble(~type, ~value,
#                "mask", "XXXXXXXXXXXXXXXXXXXXXXXXXXXXX1XXXX0X",
#                "mem[8]", "11",
#                "mem[7]","101",
#                "mem[8]","0")

dat <- 
  dat %>% 
  mutate(type = str_trim(type),
         value = str_trim(value),
         slot = map_chr(type, ~str_match(., "mem\\[(\\d*)\\]")[,2]),
         slot = slot %>% as.integer(),
         new_type = if_else(is.na(slot), "mask", "mem"),
         value_num = if_else(new_type == "mem", value, NA_character_) %>% as.numeric(),
         bitmask = if_else(new_type == "mask", value, NA_character_),
         order_num = row_number(),
         group_id = cumsum(new_type == "mask")) %>% 
  group_by(group_id) %>% 
  mutate(bitmask = max(bitmask, na.rm=T)) %>% 
  ungroup() %>% 
  filter(new_type == "mem")

## check: is 32bit enough?
if (max(dat$value_num, na.rm=T) < (2^32 - 1)) {
  message("32-bit conversion is OK")
} else {
  stop("32-bit is not big enough")
}

## Run the calcs
dat_masked <- 
  dat %>% 
  rowwise() %>% 
  mutate(masked_value = convert_masked_int(value_num, bitmask)) %>% 
  ungroup()

## assign to slots.  Just keep the last overwriting of each slot
## and sum up the values
dat_masked %>% 
  group_by(slot) %>% 
  filter(order_num == max(order_num)) %>% 
  ungroup() %>% 
  summarise(sum(masked_value))

## Part 2

convert_masked_location <- function(location, mask) {
  
  split_mask <- str_split(mask, "")[[1]]
  is_x_index <- which(split_mask == "X")
  is_1 <- split_mask == "1"
  
  location_bits <- as.integer(rev(intToBits(location))) ## reverse so ...16-8-4-2-1

  ## If the mask is bigger than 32 bits, prepend some zeros
  size_diff <- length(split_mask) - length(location_bits)
  if (size_diff > 0) {
    location_bits <- c(rep(0, size_diff), location_bits)
  }
  
  ## Replace the bits with the mask
  location_bits[is_1] <- 1
  
  find_combos <- function(value, x_loc) {
    nv_0 <- nv_1 <- value
    nv_0[x_loc] <- 0
    nv_1[x_loc] <- 1
    list(nv_0, nv_1)
  }
  
  ## Set X to zero, then add the combinations
  location_bits_zerod <- location_bits
  location_bits_zerod[is_x_index] <- 0
  
  x_powers <- 2^(36 - is_x_index)
  x_grid <- expand.grid(map(x_powers, ~c(0, .)))
  to_add <- sort(rowSums(x_grid))
  
  base_num <- bit_vec_to_num(location_bits_zerod)
  base_num + to_add
  
}

# Test
# convert_masked_location(42, "000000000000000000000000000000X1001X")
# convert_masked_location(26, "00000000000000000000000000000000X0XX")

## Expand out the multiple new slots from the old
dat_masked2 <- 
  dat %>% 
  mutate(new_slot = map2(slot, bitmask, convert_masked_location)) %>% 
  unnest(new_slot)

## And again calc the sum of the final values
dat_masked2 %>% 
  group_by(new_slot) %>% 
  filter(order_num == max(order_num)) %>% 
  ungroup() %>% 
  summarise(sum(value_num))


