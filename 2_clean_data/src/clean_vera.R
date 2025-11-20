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
        year, quarter, fips, total_pop, total_pop_15to64, total_prison_adm,
        total_prison_adm_rate
    ) |>
    # Prison data starts being reliably reported in 1983 and stops in 2019.
    filter(year >= 1983, year <= 2019) |>
    # There are some cases where a county has more people ages 15 to 64 than
    # there are people in the total population. Poor data quality, drop.
    filter(total_pop_15to64 <= total_pop) |>
    # Reduce data down to the yearly level (drop redundant quarterly rows).
    distinct(pick(-matches("quarter"))) |>
    group_by(fips, year) |>
    # Keep only rows which correspond to only 1 county-year combination OR
    # if there is more than one, keep only the non-empty rows.
    filter(n() == 1 | !is.na(total_prison_adm)) |>
    # Any remaining groups which have more than one non-empty entry are
    # suspicious and should be dropped.
    filter(n() == 1) |>
    ungroup() |>
    mutate(
        total_prison_adm_rate15to64 = total_prison_adm / total_pop_15to64 * 100000,
        total_prison_adm_rateAll = total_prison_adm / total_pop * 100000,
        state = str_sub(fips, 1, 2),
        county = str_sub(fips, 3, 5)
    ) |>
    # Drop entries where manually calculated rates do not match reported rates.
    filter(
        round(total_prison_adm_rate15to64, 2) == total_prison_adm_rate |
        (is.na(total_prison_adm_rate15to64) & is.na(total_prison_adm_rate))
    ) |>
    select(-total_prison_adm_rate) |>
    rename(full_fips = fips)

write_csv(
    vera_data_clean,
    here("2_clean_data", "output", "vera_county_clean.csv")
)

################################################################################
# Clean and save race-specific prison admissions.
vera_race_data_clean <-
    vera_data |>
    select(
        year, quarter, fips, total_pop_15to64, latinx_pop_15to64,
        white_pop_15to64, black_pop_15to64, latinx_prison_adm, white_prison_adm,
        black_prison_adm, latinx_prison_adm_rate, white_prison_adm_rate,
        black_prison_adm_rate
    ) |>
    # Prison race data starts being reliably reported in 1990 and stops in 2019.
    filter(year >= 1990, year <= 2019) |>
    # There are some cases where the summed total of people ages 15 to 64 by
    # race is greater than the total number of people ages 15 to 64. Drop them.
    mutate(sum = white_pop_15to64 + black_pop_15to64 + latinx_pop_15to64) |>
    filter(sum <= total_pop_15to64) |>
    # Reduce data down to the yearly level (drop redundant quarterly rows).
    distinct(pick(-matches("quarter"))) |>
    group_by(fips, year) |>
    # Keep only rows which correspond to only 1 county-year combination OR
    # if there is more than one, keep only the non-empty rows.
    filter(
        n() == 1 |
            !is.na(white_prison_adm) |
            !is.na(black_prison_adm) |
            !is.na(latinx_prison_adm)
    ) |>
    # Any remaining groups which have more than one non-empty entry are
    # suspicious and should be dropped.
    filter(n() == 1) |>
    ungroup() |>
    mutate(
        white_prison_adm_rate15to64 = white_prison_adm / white_pop_15to64 * 100000,
        black_prison_adm_rate15to64 = black_prison_adm / black_pop_15to64 * 100000,
        hisp_prison_adm_rate15to64 = latinx_prison_adm / latinx_pop_15to64 * 100000,
        state = str_sub(fips, 1, 2),
        county = str_sub(fips, 3, 5)
    ) |>
    # Drop entries where manually calculated rates do not match reported rates.
    filter(
        (
            round(white_prison_adm_rate15to64, 2) == white_prison_adm_rate |
            (is.na(white_prison_adm_rate15to64) & is.na(white_prison_adm_rate))
        ) &
        (
            round(black_prison_adm_rate15to64, 2) == black_prison_adm_rate |
            (is.na(black_prison_adm_rate15to64) & is.na(black_prison_adm_rate))
        ) &
        (
            round(hisp_prison_adm_rate15to64, 2) == latinx_prison_adm_rate |
            (is.na(hisp_prison_adm_rate15to64) & is.na(latinx_prison_adm_rate))
        )
        
    ) |>
    select(-sum, -total_pop_15to64, -matches("prison_adm_rate$")) |>
    rename(full_fips = fips)

write_csv(
    vera_race_data_clean,
    here("2_clean_data", "output", "vera_county_race_clean.csv")
)
