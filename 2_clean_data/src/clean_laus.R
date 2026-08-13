library(here)
library(dplyr)
library(purrr)
library(readr)
library(readxl)
read_dir <- here("1_get_data", "input")
write_dir <- here("2_clean_data", "output")

################################################################################
# Clean yearly unemployment data.
################################################################################
laus_files <- list.files(read_dir, pattern = "bls_county", full.names = T)

laus_df <-
    map(
        laus_files,
        function(x) {read_excel(x, skip = 1) |> slice((1:(n() - 3)))}
    ) |>
    bind_rows() |>
    rename(state = `State FIPS Code`, county = `County FIPS Code`, year = Year) |>
    mutate(
        ur_prcnt_est_16andOlder = Unemployed / `Labor Force`,
        full_fips = paste0(state, county)
    ) |>
    select(full_fips, state, county, year, ur_prcnt_est_16andOlder)

write_csv(laus_df, file.path(write_dir, "laus_county_clean.csv"))
