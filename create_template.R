#https://nrennie.rbind.io/blog/script-templates-r/
use_aoc_template <- function(exercise, year, date_chr = lubridate::today()) {
  ex <- stringr::str_pad(exercise, width = 2, side = "left", pad = "0")
  filename <- glue::glue("ex{ex}")
  # check date in correct format
  if (is.na(as.Date(date_chr, format = "%Y-%m-%d"))) {
    stop("'date_chr' in incorrect format. Should be yyyy-mm-dd.")
  }
  # make folder
  new_folder <- file.path("R")
  if (!file.exists(new_folder)) {
    dir.create(new_folder, recursive = TRUE)
    message("Created R folder")
  }
  # make new file
  new_file <- file.path(new_folder, paste0(filename, ".R"))
  if (!file.exists(new_file)) {
    file.create(new_file)
    message("Created '.R' file")
    # copy lines to .R file
    r_txt <- readLines(file.path("templates/r-template.R"))
    # replace placeholder text with variables
    r_txt <- gsub(
      pattern = "year",
      replacement = paste0(year),
      x = r_txt
    )
    r_txt <- gsub(
      pattern = "exercise",
      replacement = paste0(exercise),
      x = r_txt
    )
    # write to new file
    writeLines(r_txt, con = new_file)
    message("'.R' contents copied")
  }
  message("Template successfully copied!")
  rstudioapi::navigateToFile(new_file)
}
