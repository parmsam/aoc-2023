# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(9, 2023)

d1_test <- tibble(x = readr::read_lines(
  "0 3 6 9 12 15
1 3 6 10 15 21
10 13 16 21 30 45"
))

d1 <- d1 %>% 
  mutate(x = str_split(x, " ")) %>%
  mutate(x = map(x, as.numeric)) 

x <- d1$x[[3]]

# Part 1 ------------------------------------------------------------------
get_history <- function(x){
  a <- x[1:length(x)-1]
  b <- c(x[-1], 0)[1:length(x)-1]
  b - a
}
get_diff <- function(x){
  last <- x
  x <- list(x)
  while(!all(last == 0)){
    last <- get_history(last)
    # append vector to list
    x <- c(x, list(last))
  }
  return(x) 
}
calc_new <- function(x){
  x <- get_diff(x)
  x <- rev(x)
  # get last element of first item in list
  d <- 0
  last <- 0
  for(i in x){
    last <- i[length(i)]
    d <- d + last
  }
  return(d)
}
calc_new(x)

d1 %>% 
  mutate(x = map_int(x, calc_new)) %>%
  summarize(sum(x))

# Part 2 ------------------------------------------------------------------
# get_history2 <- function(x){
#   a <- x[1:length(x)-1]
#   b <- c(x[-1], 0)[1:length(x)-1]
#   b - a
# }
# get_history2(x)
# get_diff2 <- function(x){
#   last <- x
#   x <- list(x)
#   while(!all(last == 0)){
#     last <- get_history2(last)
#     # append vector to list
#     x <- c(x, list(last))
#   }
#   return(x) 
# }
# get_diff2(x)
calc_new2 <- function(x){
  x <- get_diff(x)
  x <- rev(x)
  # get last element of first item in list
  d <- 0
  for(i in x){
    first <- i[1]
    d <- first - d
  }
  return(d)
}
calc_new2(x)

d1 %>% 
  mutate(x = map_int(x, calc_new2)) %>%
  summarize(sum(x))
