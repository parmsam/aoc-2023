library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

d1 <- adventdrob::advent_input(1, 2023)

# Part 1 ----
d1 %>%
  # remove all non-numeric characters
  mutate(x = str_remove_all(x, "[^0-9]")) %>%
  # select first and last digit of each number
  mutate(y = as.numeric(str_c(str_sub(x, 1, 1), str_sub(x, -1, -1)))) %>%
  # add up all the numbers
  summarize(tot = sum(y))

# Part 2 ----
# tibble with number and alphabetic name
conv_tbl <- tribble(
  ~num, ~name,
  1, "one",
  2, "two",
  3, "three",
  4, "four",
  5, "five",
  6, "six",
  7, "seven",
  8, "eight",
  9, "nine"
) %>%
  mutate(num = as.character(num))

conv_vals <- function(x, conv_tbl) {
  track_names <- conv_tbl %>% 
    mutate(pos = str_locate_all(x, name)) %>% 
    unnest_longer(pos) %>% 
    mutate(pos = pos[,1])
  
  track_nums <- conv_tbl %>% 
    mutate(pos = str_locate_all(x, num)) %>% 
    unnest_longer(pos) %>%
    mutate(pos = pos[,1])
  
  x <- track_names %>%
    add_row(track_nums) %>%
    arrange(pos) %>%
    pull(num) %>%
    paste0(collapse = "")
  
  return(x)
}
# conv_vals(x= "eightwothree531eighthree3", conv_tbl = conv_tbl)
# conv_vals(x= "eighthree", conv_tbl = conv_tbl)
# conv_vals(x = "2xjzgsjzfhzhm1", conv_tbl = conv_tbl)

input1 %>%
  mutate(x = map_chr(x, conv_vals, conv_tbl = conv_tbl)) %>%
  # select first and last digit of each number
  mutate(y = as.numeric(str_c(str_sub(x, 1, 1), str_sub(x, -1, -1)))) %>%
  # add up all the numbers
  summarize(tot = sum(y))
