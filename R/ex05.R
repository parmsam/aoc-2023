# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(5, 2023) 

d1_test <- tibble(
  x = readr::read_lines(
"seeds: 79 14 55 13

seed-to-soil map:
50 98 2
52 50 48

soil-to-fertilizer map:
0 15 37
37 52 2
39 0 15

fertilizer-to-water map:
49 53 8
0 11 42
42 0 7
57 7 4

water-to-light map:
88 18 7
18 25 70

light-to-temperature map:
45 77 23
81 45 19
68 64 13

temperature-to-humidity map:
0 69 1
1 0 69

humidity-to-location map:
60 56 37
56 93 4"
))
  
split_nums <- function(df, x){
  df %>% 
    mutate({{x}} := str_split({{x}}, "\\s+")) %>%
    mutate({{x}} := map({{x}}, \(a) a[!is.na(a)])) %>%
    mutate({{x}} := map({{x}}, \(a) as.numeric(a)))
}

# grab initial seeds
seeds <- d1 %>% filter(str_detect(x, "seeds:")) %>%
  mutate(x = str_remove(x, "seeds: ")) %>%
  split_nums(x)

# grab other maps
maps <- d1 %>%
  filter(!str_detect(x, "seeds:")) %>%
  filter(x != "") %>%
  mutate(new_section = str_detect(x, ":")) %>%
  mutate(section_name = ifelse(new_section, str_remove(x,":"), NA)) %>%
  fill(section_name) %>%
  filter(!new_section) %>%
  group_by(section_name) %>%
  split_nums(x) %>%
  mutate(start = map(x, pluck(2)),
         stop = map(x, \(x) pluck(x,2) + pluck(x,3))) %>%
  mutate(diff = map(x, \(x)(x[1] - x[2]))) %>%
  mutate(across(start:diff, as.numeric))

nested_maps <- maps %>%
  select(-x,-new_section) %>%
  group_by(section_name) %>%
  nest() %>% 
  ungroup()

# Part 1 ------------------------------------------------------------------
get_locations_vec <- function(nested_maps = nested_maps, seeds){
  seeds_vec <- seeds
  locations_vec <- c()
  for (x in seeds_vec){
    storage_unit <- c()
    for (j in 1:nrow(nested_maps)){
      find_cond <- nested_maps %>% 
        filter(row_number() == j) %>% 
        pull(data) %>% 
        .[[1]] %>%
        filter(start <= x & stop > x) 
      if(nrow(find_cond) != 0){
        x <- x + find_cond$diff
      } else {
        x <- x
      }
    }
    locations_vec <- c(locations_vec, x)
  }
  locations_vec
}
get_locations_vec(nested_maps, seeds$x[[1]]) %>% min()

# Part 2 ------------------------------------------------------------------
# split every two numbers in seeds$x[[1]] into a list
seeds_split <- seeds$x[[1]] %>% 
  split(ceiling(seq_along(.) / 2)) 
# convert into dataframe with two cols
split_df <- tibble(
  start = map_dbl(seeds_split, \(x) x[[1]]),
  diff = map_dbl(seeds_split, \(x) x[[2]]),
  stop = start + diff
)

library(furrr)
plan(multisession, workers = 2)
vals <- future_map(seeds_split, \(x) {
  print(glue::glue("running on {x[[1]]}"))
  seed_vec <- x[[1]]:(x[[1]]+x[[2]])
  locs <- get_locations_vec(nested_maps, seeds = seed_vec)
  min(locs)
})

min(unlist(vals))
