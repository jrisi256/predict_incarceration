library(here)
library(dplyr)
library(tidyr)
library(readxl)

bfs_raw <- read_excel(here("1_get_data", "output", "bfs_county.xlsx"), skip = 2)

bfs_clean <-
    bfs_raw |>
    rename(state = state_fips, county = county_fips, full_fips = `County Code`) |>
    pivot_longer(
        cols = matches("^BA"),
        names_to = c("dataset", "year"),
        values_to = "nr_new_businesses_formed",
        names_pattern = "(BA)([0-9]{4})"
    ) |>
    select(-State, -County, -dataset)

write_csv(bfs_clean, here("2_clean_data", "output", "bfs_county_clean.csv"))
