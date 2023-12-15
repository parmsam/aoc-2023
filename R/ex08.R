# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(8, 2023)

d1_test1 <- tibble(x = readr::read_lines(
  "LLR

AAA = (BBB, BBB)
BBB = (AAA, ZZZ)
ZZZ = (ZZZ, ZZZ)"
))
d1_test2 <- tibble(x = readr::read_lines(
  "LR

11A = (11B, XXX)
11B = (XXX, 11Z)
11Z = (11B, XXX)
22A = (22B, XXX)
22B = (22C, 22C)
22C = (22Z, 22Z)
22Z = (22B, 22B)
XXX = (XXX, XXX)"))
instructions <- d1$x[[1]] %>%
  str_split("") %>% 
  .[[1]]
paths <- d1 %>% 
  filter(row_number() > 2) %>%
  tidyr::separate_wider_regex(
    x, 
    c(
      node = "\\w{3}", " = \\(", 
      left = "\\w{3}", ", ", 
      right = "\\w{3}", "\\)"
    ), 
    too_few = "align_start")

# Part 1 ------------------------------------------------------------------
pos <- "AAA"
ct <- 0
while( pos != "ZZZ"){
  for(i in instructions){
    print(i)
    ct <- ct + 1
    pos_df <- paths %>% filter(node == pos)
    if(i == "L"){
        pos <- pos_df %>% pull(left) 
    }else{
        pos <- pos_df %>% pull(right)
    }
    if(pos == "ZZZ"){
      break
    }
  }
}
print(ct)
# Part 2 ------------------------------------------------------------------
starting_nodes <- paths %>% 
  filter(str_ends(node, "A")) %>%
  pull(node)

starting_nodes
ct_set <- c()
for(pos in starting_nodes){
  ct <- 0
  print(pos)
  while( !all(str_ends(pos, "Z"))){
    for(i in instructions){
      ct <- ct + 1
      pos_df <- paths %>% filter(node %in% pos)
      if(i == "L"){
        pos <- pos_df %>% pull(left) 
      }else{
        pos <- pos_df %>% pull(right)
      }
      if(all(str_ends(pos, "Z"))){
        break
      }
    }
  }
  ct_set <- c(ct_set, ct)
}

# get least common multiple
ct_set %>% 
  purrr::reduce(numbers::LCM) -> x

formatC(x, format = "f", digits = 0)
