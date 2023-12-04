# Load packages -----------------------------------------------------------
library(adventdrob)
library(dplyr)
library(stringr)
library(purrr)
library(tidyr)

# Load data ---------------------------------------------------------------
d1 <- adventdrob::advent_input(3, 2023)
d1_test <- tibble(x=readr::read_lines(
"467..114..
...*......
..35..633.
......#...
617*......
.....+.58.
..592.....
......755.
...$.*....
.664.598.."
))
mat_orig <- adventdrob::grid_matrix(d1, x)

# Part 1 ------------------------------------------------------------------
# define function to check for valid neighbors
check_neighbors <- function(mat, i, j) {
    nrow_mat <- nrow(mat)
    ncol_mat <- ncol(mat)
    # Define the indices of the neighbors
    row_indices <- c(i - 1, i + 1, i     , i    , i - 1, i - 1, i + 1, i + 1)
    col_indices <- c(j    , j    , j - 1 , j + 1, j - 1, j + 1, j - 1, j + 1)
    # Check if the indices are within the matrix bounds
    valid_row_indices <- row_indices >= 1 & row_indices <= nrow_mat
    valid_col_indices <- col_indices >= 1 & col_indices <= ncol_mat
    # Get the valid neighbors
    neighbors <- mat[row_indices[valid_row_indices], col_indices[valid_col_indices]]
    # Remove the non-symbol values
    neighbors <- neighbors[!neighbors %in% 0:9 & neighbors != "."]
    # print(neighbors)
    if (length(neighbors) > 0 & mat[i,j] %in% 0:9) {
        return(1)
    } else {
        return(0)
    }
}
# run check_neighbor function on matrix and create new matrix
mat <- mat_orig
for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
        mat[i,j] <- check_neighbors(mat_orig, i, j)
    }
}
# Identify matrix indices that have a value between 0 and 9
indices_num <- which(mat_orig >= 0 & mat_orig <= 9, arr.ind = TRUE) %>%
    as_tibble() %>%
    arrange(row, col)
# Identify valid indices
indices_valid <- which(mat == 1, arr.ind = TRUE) %>%
    as_tibble() %>%
    arrange(row, col) %>%
    mutate(valid = 1)
# Merge two index dataframes
indices <- left_join(indices_num, indices_valid, by = c("row", "col")) %>%
    mutate(valid = ifelse(is.na(valid), 0, valid)) %>%
    mutate(num = map2(row, col, \(row,col) mat_orig[row, col]))
# grab whole numbers that match criteria
indices2 <- indices %>%
    mutate(valid = ifelse(
        lag(valid) == 1 & lag(col) == col - 1 | 
            lead(valid) == 1 & lead(col) == col + 1, 1, valid)) %>%
    mutate(valid = ifelse(
        lag(valid) == 1 & lag(col) == col - 1 |
            lead(valid) == 1 & lead(col) == col + 1, 1, valid)) %>%
    filter(valid == 1) %>%
    mutate(is_new_sequence = col != lag(col, default = first(col)) + 1,
           pos = cumsum(is_new_sequence)) %>% 
    group_by(row, pos) %>%
    mutate(num = paste(num, collapse = "")) %>%
    select(row, pos, num) %>% 
    unique()
# sum whole numbers together
indices2 %>%
    ungroup() %>%
    summarize(tot = sum(num %>% as.numeric()))

# Part 2 ------------------------------------------------------------------
check_neighbors2 <- function(mat, i, j) {
    nrow_mat <- nrow(mat)
    ncol_mat <- ncol(mat)
    # Define the indices of the neighbors
    row_indices <- c(i - 1, i + 1, i     , i    , i - 1, i - 1, i + 1, i + 1)
    col_indices <- c(j    , j    , j - 1 , j + 1, j - 1, j + 1, j - 1, j + 1)
    # Check if the indices are within the matrix bounds
    valid_row_indices <- row_indices >= 1 & row_indices <= nrow_mat
    valid_col_indices <- col_indices >= 1 & col_indices <= ncol_mat
    # Get the valid neighbors
    neighbors <- mat[row_indices[valid_row_indices], col_indices[valid_col_indices]]
    # Remove the non * symbol values
    neighbors <- neighbors[neighbors == "*"]
    # print(neighbors)
    if (length(neighbors) > 0 & mat[i,j] %in% c(0:9,"*") ) {
        return(1)
    } else {
        return(0)
    }
}
# run check_neighbor function on matrix and create new matrix
mat <- mat_orig
for (i in 1:nrow(mat)) {
    for (j in 1:ncol(mat)) {
        mat[i,j] <- check_neighbors2(mat_orig, i, j)
    }
}
# Identify matrix indices that have a value between 0 and 9 or *
indices_num <- which((mat_orig >= 0 & mat_orig <= 9) | mat_orig == "*", arr.ind = TRUE) %>%
    as_tibble() %>%
    arrange(row, col)
# Identify valid indices
indices_valid <- which(mat == 1, arr.ind = TRUE) %>%
    as_tibble() %>%
    arrange(row, col) %>%
    mutate(valid = 1)
# Merge two index dataframes
indices <- left_join(indices_num, indices_valid, by = c("row", "col")) %>%
    arrange(row, col) %>%
    mutate(valid = ifelse(is.na(valid), 0, valid)) %>%
    mutate(num = map2(row, col, \(row,col) mat_orig[row, col]))
# Grab whole numbers that match criteria
indices_base <- indices %>%
    mutate(valid = ifelse(
        lag(valid) == 1 & lag(col) == col - 1 | 
            lead(valid) == 1 & lead(col) == col + 1, 1, valid)) %>%
    mutate(valid = ifelse(
        lag(valid) == 1 & lag(col) == col - 1 |
            lead(valid) == 1 & lead(col) == col + 1, 1, valid)) %>%
    filter(valid == 1)

indices2 <- indices_base %>%
    filter(num != "*") %>%
    mutate(is_new_sequence = col != lag(col, default = first(col)) + 1) %>%
    mutate(pos = cumsum(is_new_sequence)) %>%
    mutate(pos = ifelse(num == "*", 0, pos)) 

indices3 <-  indices_base %>%
    filter(num == "*") %>%
    # list numbers surrounding row with num as *
    mutate(
        surr_nums = map2(row, col, 
             \(r,c) {
                 indices2 %>%
                     filter(row %in% c(r - 1, r, r + 1),
                                col %in% c(c - 1, c, c + 1)) %>%
                     filter(num != "*") %>%
                     pull(pos) %>%
                     unique()
                 })
    ) %>%
    unnest(surr_nums)
# get the reference numbers
ref_nums <- indices2 %>% 
    filter(num != "*") %>%
    group_by(row, pos) %>%
    mutate(num = paste(num, collapse = "")) %>%
    ungroup() %>%
    select(pos, ref_num = num) %>%
    unique()
# multiple numbers together
answer <- indices3 %>% 
    rename(pos = surr_nums) %>%
    left_join(ref_nums) %>%
    group_by(row, col) %>%
    mutate(rn = row_number()) %>%
    mutate(cum_prod = cumprod(ref_num)) %>%
    filter(n() == 2, rn == 2)
# sum the products
answer %>%
    summarise(across(everything(), last)) %>%
    ungroup()  %>%
    filter(rn < 3) %>%
    summarize(tot = sum(cum_prod))
