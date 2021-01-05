library(tidyverse)

dat <- read_csv("Day17/Day17Data.txt", col_names = "cubes", col_types = "c")
## Test data
# dat <- tribble(~cubes,
#                ".#.",
#                "..#",
#                "###")

initial_state <- 
  dat %>% 
  mutate(cube = map(cubes, ~str_split(., "")[[1]]),
         x = row_number()) %>% 
  unnest(cube) %>% 
  group_by(x) %>% 
  mutate(y = row_number(),
         z = 0) %>% 
  ungroup() %>% 
  select(-cubes)

## Find the 26 relative offsets
surrounds_add <- expand_grid(x_off = c(-1, 0, 1), y_off = c(-1, 0, 1), z_off = c(-1, 0, 1)) %>% 
  filter(!(x_off == 0 & y_off == 0 & z_off == 0))

add_offsets <- function(x_off, y_off, z_off, state) {
  state %>% 
    mutate(x_rel = x + x_off,
           y_rel = y + y_off,
           z_rel = z + z_off
    )
}

current_state <- initial_state

for (cycle in 1:6) {
  ## Find the cubes surrounding each of our current ones...
  new_state <- 
    pmap_dfr(surrounds_add, add_offsets, state = current_state)
  
  ## Add on the surrounding cubes as new ones
  extras <- 
    expand_grid(x = min(new_state$x_rel):max(new_state$x_rel),
                y = min(new_state$y_rel):max(new_state$y_rel),
                z = min(new_state$z_rel):max(new_state$z_rel)) %>% 
    mutate(cube = ".") %>% 
    anti_join(current_state, by = c("x", "y", "z"))
  
  extras <- extras %>% 
    pmap_dfr(surrounds_add, add_offsets, state = .)
  
  ## Join on the current state, where known
  new_state <- 
    new_state %>% 
    bind_rows(extras) %>% 
    left_join(current_state %>% rename(cube_rel = cube), 
              by = c("x_rel" = "x",
                     "y_rel" = "y",
                     "z_rel" = "z")) %>% 
    mutate(cube_rel = replace_na(cube_rel, ".")) %>% 
    group_by(x, y, z, cube) %>% 
    summarise(active_neighbour_count = sum(cube_rel == "#")) %>% 
    ungroup() %>% 
    ## Update the cube state
    mutate(cube = if_else(cube == "#",
                          ## active
                          if_else(active_neighbour_count %in% c(2, 3), "#", "."),
                          ## inactive
                          if_else(active_neighbour_count == 3, "#", ".")
    )) 
  
  ## Restrict to the area with any #'s in
  active_cubes <- new_state %>% filter(cube == "#")
  
  new_state <-
    new_state %>%
    filter(between(x, min(active_cubes$x), max(active_cubes$x)),
           between(y, min(active_cubes$y), max(active_cubes$y)),
           between(z, min(active_cubes$z), max(active_cubes$z))
    )

  ## Have a look
  # for (z_val in min(active_cubes$z):max(active_cubes$z)) {
  #   message("z = ", z_val)
  #   new_state %>%
  #     filter(z == z_val) %>%
  #     pivot_wider(x, names_from = y, values_from = cube) %>%
  #     print()
  # }

  ## Reset current_state to the updated values
  current_state <- 
    new_state %>% 
    select(cube, x, y, z)

}

## How many active cubes?
n_active <- 
  current_state %>% 
  filter(cube == "#") %>% 
  nrow()

message(n_active, " active cubes")



# Part 2:  4D -------------------------------------------------------------

# Exactly the same, but with a "w" dimension.

dat <- read_csv("Day17/Day17Data.txt", col_names = "cubes", col_types = "c")
# Test data
# dat <- tribble(~cubes,
#                ".#.",
#                "..#",
#                "###")

initial_state <- 
  dat %>% 
  mutate(cube = map(cubes, ~str_split(., "")[[1]]),
         x = row_number()) %>% 
  unnest(cube) %>% 
  group_by(x) %>% 
  mutate(y = row_number(),
         z = 0,
         w = 0) %>% 
  ungroup() %>% 
  select(-cubes)

## Find the 80 relative offsets
surrounds_add <- expand_grid(x_off = c(-1, 0, 1), 
                             y_off = c(-1, 0, 1), 
                             z_off = c(-1, 0, 1),
                             w_off = c(-1, 0, 1)) %>% 
  filter(!(x_off == 0 & y_off == 0 & z_off == 0 & w_off == 0))

add_offsets <- function(x_off, y_off, z_off, w_off, state) {
  state %>% 
    mutate(x_rel = x + x_off,
           y_rel = y + y_off,
           z_rel = z + z_off,
           w_rel = w + w_off
    )
}

current_state <- initial_state

for (cycle in 1:6) {
  ## Find the cubes surrounding each of our current ones...
  new_state <- 
    pmap_dfr(surrounds_add, add_offsets, state = current_state)
  
  ## Add on the surrounding cubes as new ones
  extras <- 
    expand_grid(x = min(new_state$x_rel):max(new_state$x_rel),
                y = min(new_state$y_rel):max(new_state$y_rel),
                z = min(new_state$z_rel):max(new_state$z_rel),
                w = min(new_state$w_rel):max(new_state$w_rel)
                ) %>% 
    mutate(cube = ".") %>% 
    anti_join(current_state, by = c("x", "y", "z", "w"))
  
  extras <- extras %>% 
    pmap_dfr(surrounds_add, add_offsets, state = .)
  
  ## Join on the current state, where known
  new_state <- 
    new_state %>% 
    bind_rows(extras) %>% 
    left_join(current_state %>% rename(cube_rel = cube), 
              by = c("x_rel" = "x",
                     "y_rel" = "y",
                     "z_rel" = "z",
                     "w_rel" = "w")) %>% 
    mutate(cube_rel = replace_na(cube_rel, ".")) %>% 
    group_by(x, y, z, w, cube) %>% 
    summarise(active_neighbour_count = sum(cube_rel == "#")) %>% 
    ungroup() %>% 
    ## Update the cube state
    mutate(cube = if_else(cube == "#",
                          ## active
                          if_else(active_neighbour_count %in% c(2, 3), "#", "."),
                          ## inactive
                          if_else(active_neighbour_count == 3, "#", ".")
    )) 
  
  ## Restrict to the area with any #'s in
  active_cubes <- new_state %>% filter(cube == "#")
  
  new_state <-
    new_state %>%
    filter(between(x, min(active_cubes$x), max(active_cubes$x)),
           between(y, min(active_cubes$y), max(active_cubes$y)),
           between(z, min(active_cubes$z), max(active_cubes$z)),
           between(w, min(active_cubes$w), max(active_cubes$w))
    )
  
  ## Have a look
  # for (w_val in min(active_cubes$w):max(active_cubes$w)) {
  #   for (z_val in min(active_cubes$z):max(active_cubes$z)) {
  #     message("w = ", w_val, ", z = ", z_val)
  #     new_state %>%
  #       filter(z == z_val,
  #              w == w_val) %>%
  #       pivot_wider(x, names_from = y, values_from = cube) %>%
  #       print()
  #   }
  # }
  
  ## Reset current_state to the updated values
  current_state <- 
    new_state %>% 
    select(cube, x, y, z, w)
  
}

## How many active cubes?
n_active <- 
  current_state %>% 
  filter(cube == "#") %>% 
  nrow()

message(n_active, " active cubes")
