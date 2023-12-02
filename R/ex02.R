# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(2, 2023)

# Part 1 ------------------------------------------------------------------
grab_max <- function(x) {
    map_int(x, \(.x)max(as.numeric(str_extract(.x, "[0-9]+"))))
}

d1 %>%
  separate_wider_regex(
    x, patterns = c(
      "Game ", Game = "[0-9]+", ":"
  ), too_few = "align_start", cols_remove = F) %>%
  mutate(reds = str_extract_all(x, "[0-9]+ red")) %>%
  mutate(greens = str_extract_all(x, "[0-9]+ green")) %>%
  mutate(blues = str_extract_all(x, "[0-9]+ blue")) %>% 
  mutate(across(c(reds, greens, blues), grab_max)) %>%
  filter(reds <= 12, greens <= 13, blues <= 14) %>%
  summarize(tot = sum(as.numeric(Game)))

# Part 2 ------------------------------------------------------------------
d1 %>%
  separate_wider_regex(
    x, patterns = c(
      "Game ", Game = "[0-9]+", ":"
    ), too_few = "align_start", cols_remove = F) %>%
  mutate(reds = str_extract_all(x, "[0-9]+ red")) %>%
  mutate(greens = str_extract_all(x, "[0-9]+ green")) %>%
  mutate(blues = str_extract_all(x, "[0-9]+ blue")) %>% 
  mutate(across(c(reds, greens, blues), grab_max)) %>%
  mutate(power = reds * greens * blues) %>%
  summarize(tot = sum(as.numeric(power)))
