library(here)
library(readr)
library(dplyr)
library(purrr)
library(stringr)

################################################################################
# List all CDC time series data.
cdc_filepaths_deaths <-
    list.files(here("1_get_data", "output"), pattern = "cdc_d", full.names = T)

cdc_filepaths_homicides <-
    list.files(here("1_get_data", "output"), pattern = "cdc_h", full.names = T)

cdc_filepaths_nonH <-
    list.files(here("1_get_data", "output"), pattern = "cdc_n", full.names = T)

# Read in the time series tables of interest.
read_cdc_data <- function(filepath) {
    read_csv(filepath) |>
        select(-County, -`Year Code`, -Notes, -`Crude Rate`) |>
        rename(year = Year, full_fips = "County Code") |>
        filter(!is.na(full_fips), Population != "Not Applicable", !is.na(year)) |>
        mutate(across(everything(), as.character))
}

# If deaths are suppressed or missing, they cannot be used to recover homicides.
cdc_deaths <-
    map(cdc_filepaths_deaths, read_cdc_data) |>
    bind_rows() |>
    mutate(
        Deaths =
            if_else(
                Deaths %in% c("Suppressed", "Missing"), NA_character_, Deaths
            ),
        Deaths = as.numeric(Deaths)
    )

# If non-homicides are suppressed or missing, they cannot be used to recover homicides.
cdc_nonHomicides <-
    map(cdc_filepaths_nonH, read_cdc_data) |>
    bind_rows() |>
    mutate(
        Deaths =
            if_else(
                Deaths %in% c("Suppressed", "Missing"), NA_character_, Deaths
            ),
        Deaths = as.numeric(Deaths)
    ) |>
    rename(nonHomicides = Deaths) |>
    select(-Population)

# Create 3 different homicide variables.
# 1) Suppressed and missing are considered truly missing.
# 2) Suppressed values count as 9 and missing is truly missing.
# 3) Suppressed values count as 1 and missing is truly missing.
cdc_homicides <-
    map(cdc_filepaths_homicides, read_cdc_data) |>
    bind_rows() |>
    mutate(
        homicides = if_else(Deaths %in% c("Suppressed", "Missing"), NA_character_, Deaths),
        homicides_max =
            case_when(
                Deaths == "Missing" ~ NA,
                Deaths == "Suppressed" ~ "9",
                !(Deaths %in% c("Missing", "Suppressed")) ~ Deaths
            ),
        homicides_min =
            case_when(
                Deaths == "Missing" ~ NA,
                Deaths == "Suppressed" ~ "1",
                !(Deaths %in% c("Missing", "Suppressed")) ~ Deaths
            ),
        across(matches("homicides"), as.numeric)
    ) |>
    select(-Population, -Deaths)

################################################################################
# Finalize homicide data.
cdc_homicides_clean <-
    reduce(
        list(cdc_deaths, cdc_nonHomicides, cdc_homicides),
        function(df1, df2) {full_join(df1, df2, by = c("year", "full_fips"))}
    ) |>
    # Recover homicide values and use them when official homicide values are missing.
    mutate(
        homicides_self = Deaths - nonHomicides,
        homicides_final = if_else(is.na(homicides), homicides_self, homicides),
        state = str_sub(full_fips, 1, 2),
        county = str_sub(full_fips, 3, 5)
    ) |>
    select(-nonHomicides, -Deaths, -homicides, -homicides_self) |>
    arrange(full_fips, year) |>
    group_by(full_fips) |>
    mutate(
        across(
            matches("homicides"),
            list(
                "lag1" = function(col) {lag(col, 1)},
                "lag2" = function(col) {lag(col, 2)},
                "lag3" = function(col) {lag(col, 3)},
                "lag4" = function(col) {lag(col, 4)}
            )
        ),
        year = as.numeric(year)
    ) |>
    filter(year >= 1983) |>
    rowwise() |>
    mutate(
        homicides_final_3yr_avg  =
            mean(c(homicides_final, homicides_final_lag1, homicides_final_lag2)),
        homicides_max_3yr_avg  =
            mean(c(homicides_max, homicides_max_lag1, homicides_max_lag2)),
        homicides_min_3yr_avg  =
            mean(c(homicides_min, homicides_min_lag1, homicides_min_lag2)),
        homicides_final_5yr_avg  =
            mean(
                c(
                    homicides_final, homicides_final_lag1, homicides_final_lag2,
                    homicides_final_lag3, homicides_final_lag4
                )
            ),
        homicides_max_5yr_avg  =
            mean(
                c(
                    homicides_max, homicides_max_lag1, homicides_max_lag2,
                    homicides_max_lag3, homicides_max_lag4
                )
            ),
        homicides_min_5yr_avg  =
            mean(
                c(
                    homicides_min, homicides_min_lag1, homicides_min_lag2,
                    homicides_min_lag3, homicides_min_lag4
                )
            ),
    ) |>
    ungroup() |>
    select(-matches("lag"))

################################################################################
# Save results.
write_csv(
    cdc_homicides_clean,
    here("2_clean_data", "output", "cdc_county_homicides_clean.csv")
)
