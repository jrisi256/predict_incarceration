library(here)
library(dplyr)
library(haven)
library(readr)
library(stringr)
read_dir <- here("2_clean_data", "input")
write_dir <- here("2_clean_data", "output")

################################################################################
# Read in data.
################################################################################
state_ideology_raw <-
    read_dta(
        file.path(
            read_dir,
            "shor mccarty 1993-2020 state aggregate data April 2023 release.dta"
        )
    )

################################################################################
# Clean and save data.
################################################################################
state_ideology_clean <-
    state_ideology_raw |>
    filter(year >= 2010, year <= 2019) |>
    mutate(
        state =
            case_when(
                str_length(fips) == 1 ~ paste0(0, fips),
                str_length(fips) == 2 ~ as.character(fips)
            )
    ) |>
    select(-matches("error|rity"), -st, -alpha, -icpsr, -fips)

write_csv(
    state_ideology_clean,
    file.path(write_dir, "shor_stateIdeology_clean.csv")
)
