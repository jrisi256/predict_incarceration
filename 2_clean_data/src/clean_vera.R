library(here)
library(readr)
library(dplyr)
library(stringr)

################################################################################
# Read in raw data.
vera_data <- read_csv(here("1_get_data", "output", "vera_county.csv.gz"))

################################################################################
# Clean and save overall prison admissions.
vera_data_clean <-
    vera_data |>
    select(
        year, county_fips, state_fips, total_pop_15to64, total_prison_admits,
        total_prison_admits_rate
    ) |>
    # Prison data starts being reliably reported in 1983 and stops in 2019.
    filter(year >= 1983, year <= 2019) |>
    mutate(
        total_prison_adm_rate15to64 = total_prison_admits / total_pop_15to64 * 100000,
        county = str_sub(county_fips, 3, 5)
    ) |>
    select(-total_prison_admits_rate) |>
    rename(full_fips = county_fips, state = state_fips)

write_csv(
    vera_data_clean,
    here("2_clean_data", "output", "vera_county_clean.csv")
)
