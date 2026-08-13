library(here)
library(readr)
library(dplyr)
library(tidyr)
library(stringr)

################################################################################
# Read in data.
data_all <- read_csv(here('3_merge_data', 'output', 'merged_data.csv.gz'))
data_no_missing <- data_all  |> filter(!is.na(total_prison_adm_rate15to64))

################################################################################
# Make data long so it is easier to work with for missing analysis.
data_long <-
    data_no_missing |>
    pivot_longer(
        cols = -matches("^year$|full_fips|^state$|^county$"),
        names_to = "variable",
        values_to = "value"
    ) |>
    arrange(year, variable, full_fips)

################################################################################
# Drop any columns which are completely missing in a given year.
data_variable_year_missing <-
    data_long |>
    group_by(variable, year) |>
    summarise(all_values_missing_this_year = all(is.na(value) | value == 0)) |>
    ungroup() |> 
    arrange(-all_values_missing_this_year, variable, year)

data_long_drop_missing_vars <-
    data_long |>
    filter(!str_detect(variable, "doNotLiveWithParent"))

################################################################################
# Drop columns which are missing a lot of values in a given year OR
# Drop counties which are consistently missing values across variables.
data_variable_missing <-
    data_long_drop_missing_vars |>
    mutate(is_missing = if_else(is.na(value) | is.nan(value) | is.infinite(value), 1, 0)) |>
    group_by(variable, year) |>
    summarise(number_missing_per_year = sum(is_missing)) |>
    ungroup() |>
    filter(number_missing_per_year > 0) |>
    arrange(variable, year)
    
# Create clean data with no missing values (backwards imputation process).
# Start here. And then go back to where the data is first made long. Run 
# everything again. If done correctly, data_variable_missing, should have no
# rows.
data_no_missing <-
    data_all |>
    filter(
        !is.na(total_prison_adm_rate15to64),
        !is.na(medianHhIncomeBlackToWhite_prcnt_est),
        !is.na(medianHhIncomeHispToWhite_prcnt_est),
        !is.na(s_chamber_sd),
        !is.na(nr_socialSupportServices_max_per10k),
        !is.na(blackToWhiteUnmarriedBirthRate_diff_est),
        !is.na(hispToWhiteUnmarriedBirthRate_diff_est),
        !is.na(blackToWhiteChildPoverty_diff_est),
        !is.na(yearMoved_median_est),
        !is.na(h_chamber_sd),
        !is.na(delta_index_black),
        !is.na(hhNoPhone_prcnt_est),
        !is.na(hispToWhiteChildPoverty_diff_est),
        !is.na(urBlackToWhite_diff_est_16andOlder),
        !is.na(urHispToWhite_diff_est_16andOlder)
    ) |>
    select(-doNotLiveWithParent_prcnt_est_ages17AndYounger)

write_csv(
    data_no_missing,
    here("4_clean_missing_data", "output", "data_no_missing.csv")
)

# Save table with missing variables except for outcome variable.
data_missing_predictors <-
    data_all |>
    filter(!is.na(total_prison_adm_rate15to64)) |>
    anti_join(data_no_missing, by = c("state", "county", "full_fips", "year"))

write_csv(
    data_missing_predictors,
    here("4_clean_missing_data", "output", "data_missing_predictors.csv")
)

# Save table with missing outcome variable.
# 51515, 51019, 02195, 02105, and 02198 -> complex boundary changes.
data_missing_outcome <-
    data_all |>
    filter(
        is.na(total_prison_adm_rate15to64),
        !(full_fips %in% c("51515", "51019", "02195", "02105", "02198"))
    )

write_csv(
    data_missing_outcome,
    here("4_clean_missing_data", "output", "data_missing_outcome.csv")
)

# Save table with counties that must be dropped due to boundary changes.
data_boundary_issues <-
    data_all |>
    filter((full_fips %in% c("51515", "51019", "02195", "02105", "02198")))

write_csv(
    data_boundary_issues,
    here("4_clean_missing_data", "output", "data_boundary_issues.csv")
)
