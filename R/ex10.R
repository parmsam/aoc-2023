# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(10, 2023)

d1_test <- tibble(x = readr::read_lines(
  "..F7.
.FJ|.
SJ.L7
|F--J
LJ..."
))

grid <- adventdrob::grid_tidy(d1, x) %>%
  select(value, everything())

# Part 1 ------------------------------------------------------------------
start_pos <-  grid %>%
  filter(value == "S")

#check surrounding cells
check_surrounding <- function(x, y, grid) {
  g1 <- grid %>%
    filter(row %in% c(x - 1, x, x + 1),
           col %in% c(y - 1, y, y + 1)) %>%
    # ignore diagonal cells
    filter(!(row == x - 1 & col == y - 1),
           !(row == x + 1 & col == y - 1),
           !(row == x - 1 & col == y + 1),
           !(row == x + 1 & col == y + 1)) %>%
    filter(value != ".")
  # add label column with top buttom left right
  g1 %>% 
    mutate(label = case_when(
      row == x - 1 & col == y ~ "top",
      row == x + 1 & col == y ~ "bottom",
      row == x & col == y - 1 ~ "left",
      row == x & col == y + 1 ~ "right"
    )) %>%
    filter(!is.na(label))
}
check_surrounding(3,1, grid)

check_connection <- function(value, previous){
  # right can have J, 7 or -
  if (value %in% c("J","7","-") & previous == "right") {
    return(TRUE)
  }
  # left can have F, J, or -
  if (value %in% c("F","L","-") & previous == "left") {
    return(TRUE)
  }
  # top can have |, F or 7
  if (value %in% c("|","F","7") & previous == "top") {
    return(TRUE)
  }
  # bottom can have |, J or L
  if (value %in% c("|","J","L") & previous == "bottom") {
    return(TRUE)
  }
}
check_connection("J", "right")
# apply check_connection on all rows
grab_valid_connections <- function(row, col, g=grid, connection_ct = 0){
  check_surrounding(row,col, g) %>%
    mutate(connected = map2(value, label, check_connection)) %>%
    unnest(c(connected)) %>%
    filter(connected == TRUE)
}
grab_valid_connections(start_pos$row, start_pos$col, grid)
# loop to apply grab_valid_connections on all possible connections
current_pos <- start_pos
history_tbl <- start_pos
ct <- 0
while(TRUE){
  print(ct)
  ct <- ct + 1
  current_pos <- current_pos %>% 
    mutate(y = row_number()) %>% 
    split(.$y) %>% 
    map(\(df) grab_valid_connections(df$row,df$col, grid)) %>% 
    list_rbind() %>% 
    ungroup()

  # filter out values that have are already in history_tbl
  current_pos <- current_pos %>% 
    filter(!paste(row,col) %in% paste(history_tbl$row, history_tbl$col))
  
  history_tbl <- current_pos %>% 
    select(value, row, col) %>%
    unique() %>%
    bind_rows(history_tbl,.) %>%
    unique()

  if("S" %in% current_pos$value & ct > 0 | nrow(current_pos) == 0){
    print("cond hit")
    print(glue::glue("final count {ct-1}"))
    break
  }
}

# Part 2 ------------------------------------------------------------------

