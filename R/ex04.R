# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(4, 2023)

d1_test <- tibble(
  x=readr::read_lines(
"Card 1: 41 48 83 86 17 | 83 86  6 31 17  9 48 53
Card 2: 13 32 20 16 61 | 61 30 68 82 17 32 24 19
Card 3:  1 21 53 59 44 | 69 82 63 72 16 21 14  1
Card 4: 41 92 73 84 69 | 59 84 76 51 58  5 54 83
Card 5: 87 83 26 28 32 | 88 30 70 12 93 22 82 36
Card 6: 31 18 13 56 72 | 74 77 10 23 35 67 36 11"
))

d1_load <- d1 %>% 
  separate(x, into = c("card", "ref", "hand"), sep = "[:|]") %>%
  mutate(card = str_remove_all(card, "Card ")) %>%
  mutate(ref = str_split(ref, " +")) %>%
  mutate(hand = str_split(hand, " +")) %>%
  mutate(across(is.list, \(x)map(x, as.numeric))) %>%
  mutate(ref = map(ref, \(x)x[!is.na(x)])) %>%
  mutate(hand = map(hand, \(x)x[!is.na(x)]))


# Part 1 ------------------------------------------------------------------
d1_load %>%
  mutate(matches = map2_int(ref, hand, \(x, y)sum(x %in% y))) %>%
  mutate(points = ifelse(matches > 0, 2^(matches-1), 0)) %>%
  summarize(tot = sum(points))

# Part 2 ------------------------------------------------------------------
# calculate card copy count
match_df <- d1_load %>%
  mutate(matches = map2_int(ref, hand, \(x, y)sum(x %in% y))) %>%
  mutate(card = as.numeric(card)) %>%
  select(card, matches) %>%
  filter(matches != 0) %>%
  mutate(returns = map2(card, matches, \(c,m)(c+1):(c+m)))

card_wins <- function(card, match_df) {
  return_cards <- match_df %>%
    filter(card == {{card}}) %>%
    pull(returns)
  if (length(return_cards) != 0) {
    return(return_cards %>% .[[1]])
  }
}

# setup card count vector initialized to 1 per card
card_hand <- vector(length = length(d1_load$card)) + 1

loop_increase <- function(card_hand) {
  for(i in 1:length(card_hand)){
    positions <- card_wins(i, match_df)
    # add one to each index in pos according to number of cards won
    card_hand[positions] <- card_hand[positions] + (1 * card_hand[i])
  }
  return(
    card_hand
  )
}

card_hand <- loop_increase(card_hand)
sum(card_hand)
