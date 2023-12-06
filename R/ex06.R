# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(6, 2023)

d1_test <- tibble(
  x = readr::read_lines(
    "Time:      7  15   30
Distance:  9  40  200"
  ))

d1_setup <- d1 %>% 
  separate(x, into = c("x", "y"), sep = ": ") %>%
  mutate(y = str_split(str_trim(y), "\\s+"))

# Part 1 ------------------------------------------------------------------
td1 <- d1_setup %>%
  mutate(y = map(y, as.numeric)) %>% 
  pivot_longer(!x, names_to = "col1", values_to = "col2") %>% 
  pivot_wider(names_from = "x", values_from = "col2") %>%
  unnest(c(Time, Distance)) %>%
  select(-col1)

calc_ways <- function(t,d){
  x <- 1:(t-1)
  tf <- x * (t-x) > d
  sum(tf)
}

td1 %>%
  mutate(Count = map2_int(Time, Distance, calc_ways)) %>%
  # cumulative multiplication
  summarize(Count = prod(Count))

# Part 2 ------------------------------------------------------------------
td2 <- d1_setup %>%
  mutate(y = map_chr(y, paste, collapse="")) %>%
  mutate(y = as.numeric(y)) %>%
  pivot_longer(!x, names_to = "col1", values_to = "col2") %>% 
  pivot_wider(names_from = "x", values_from = "col2") %>%
  unnest(c(Time, Distance)) %>%
  select(-col1)

td2 %>%
  mutate(Count = map2_int(Time, Distance, calc_ways))
  
