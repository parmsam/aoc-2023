devtools::install_github("dgrtwo/adventdrob")
# https://github.com/dgrtwo/adventdrob
## You'll then have to set ADVENT_SESSION in your #.Renviron to your Advent of Code cookie. Example of how to get that in Chrome:
usethis::edit_r_environ()
## Visit adventofcode.com, and log in
## Right click + Inspect to view Developer Tools
## Select the Network tab
## Refresh the page, and select the "adventofcode.com" request
## Under Request Headers, there should be a cookie including session=<cookie here>. Copy that without the session=.
usethis::use_r("ex01")
source("create_template.R")
use_aoc_template(2, 2023)
use_aoc_template(3, 2023)
use_aoc_template(4, 2023)
use_aoc_template(5, 2023)
use_aoc_template(6, 2023)
use_aoc_template(7, 2023)
use_aoc_template(8, 2023)
use_aoc_template(9, 2023)
use_aoc_template(10, 2023)
