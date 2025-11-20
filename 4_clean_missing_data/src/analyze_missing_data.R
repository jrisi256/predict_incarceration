library(sf)
library(here)
library(readr)
library(dplyr)
library(tidyr)
library(ipumsr)
library(stringr)
library(ggplot2)

################################################################################
# Read in data.
data_all <- read_csv(here('3_merge_data', 'output', 'merged_data.csv.gz'))
data_race <- read_csv(here("3_merge_data", "output", "merged_data_race.csv.gz"))

data <-
    full_join(
        data_all,
        select(data_race, year, full_fips, state, county, matches("prison_adm_rate")),
        by = c("year", "full_fips", "state", "county")
    )

data_no_missing <- data

county_shapefiles_2021 <-
    read_ipums_sf(
        here("1_get_data", "output", "ipums_shapefileCountyState2021.zip"),
        file_select = "nhgis0102_shape/nhgis0102_shapefile_tl2021_us_county_2021.zip"
    )

state_shapefiles_2021 <-
    read_ipums_sf(
        here("1_get_data", "output", "ipums_shapefileCountyState2021.zip"),
        file_select = "nhgis0102_shape/nhgis0102_shapefile_tl2021_us_state_2021.zip"
    ) |>
    filter(!(STATEFP %in% c("02", 15, 72)))

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
    filter(!str_detect(variable, "[iI]nsurance|doNotLiveWithParent"))

################################################################################
# Drop columns which are missing a lot of values in a given year OR
# Drop counties which are consistently missing values across variables.
data_variable_missing <-
    data_long_drop_missing_vars |>
    mutate(is_missing = if_else(is.na(value), 1, 0)) |>
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
    data |>
    filter(
        !is.na(nr_socialAssociations_max_per10k),
        !is.na(nr_socialAssociations_min_per10k),
        !is.na(nrBirthsPer10000Unmarried_rate_est_ages15to50_allRaces_f),
        !is.na(nr_homicides_min_3yr_avg_per100k),
        !is.na(yearMoved_median_est),
        !is.na(hhNoPhone_prcnt_est),
        !is.na(hhShareOfIncome_1stQuintile_est),
        !is.na(homeValue_25thPtile_est),
        !is.na(grossMortgagePrcntIncome_median_est)
    ) |>
    # For now, drop columns with a high amount of missing values.
    select(
        -matches(
            paste0(
                "hhIncome_median.*(b|w|h)|medianHhIncome(Black|Hisp)|",
                "nr_socialSupportServices|[iI]nsurance|doNotLiveWithParent"
            )
        )
    )

# Prison admissions rate for all.
data_no_missing_all <-
    data_no_missing |>
    filter(!is.na(total_prison_adm_rate15to64)) |>
    select(-matches("(white|black|hisp)_prison"))

write_csv(
    data_no_missing_all,
    here("4_clean_missing_data", "output", "data_no_missing.csv")
)

# Compare prison admission rates for blacks vs. whites.
data_no_missing_black <-
    data_no_missing |>
    filter(
        !is.na(white_prison_adm_rate15to64), !is.na(black_prison_adm_rate15to64)
    ) |>
    mutate(
        black_to_white_prison_adm_rate =
            black_prison_adm_rate15to64 / white_prison_adm_rate15to64
    ) |>
    filter(
        !is.infinite(black_to_white_prison_adm_rate),
        !is.na(black_to_white_prison_adm_rate)
    ) |>
    select(-matches("total_prison|hisp_prison"))

write_csv(
    data_no_missing_black,
    here("4_clean_missing_data", "output", "data_no_missing_black.csv")
)

# Compare prison admission rates for Hispanics vs. whites.
data_no_missing_hisp <-
    data_no_missing |>
    filter(
        !is.na(white_prison_adm_rate15to64), !is.na(hisp_prison_adm_rate15to64)
    ) |>
    mutate(
        hisp_to_white_prison_adm_rate =
            hisp_prison_adm_rate15to64 / white_prison_adm_rate15to64,
    ) |>
    filter(
        !is.infinite(hisp_to_white_prison_adm_rate),
        !is.na(hisp_to_white_prison_adm_rate)
    ) |>
    select(-matches("total_prison|black_prison"))

write_csv(
    data_no_missing_hisp,
    here("4_clean_missing_data", "output", "data_no_missing_hisp.csv")
)

################################################################################
# Map county coverage for all.
counties_in_sample <-
    county_shapefiles_2021 |>
    filter(!(STATEFP %in% c("02", 15, 72))) |>
    mutate(in_sample = GEOID %in% data_no_missing_all$full_fips)

coverage_all <-
    ggplot(counties_in_sample) +
    geom_sf(aes(fill = in_sample), linewidth = 0.1) +
    geom_sf(data = state_shapefiles_2021, fill = NA, linewidth = 0.25) +
    labs(
        fill = "County in sample",
        title = "County coverage of prison admission rates: 2010 - 2019"
    ) +
    theme_void()

ggsave(
    here("4_clean_missing_data", "graphs", "county_coverage_all.png"),
    coverage_all,
    bg = "white",
    height = 7,
    width = 7
)

# Map county coverage for black-to-white disparity.
counties_in_sample_black <-
    county_shapefiles_2021 |>
    filter(!(STATEFP %in% c("02", 15, 72))) |>
    mutate(in_sample = GEOID %in% data_no_missing_black$full_fips)

coverage_black <-
    ggplot(counties_in_sample_black) +
    geom_sf(aes(fill = in_sample), linewidth = 0.1) +
    geom_sf(data = state_shapefiles_2021, fill = NA, linewidth = 0.25) +
    labs(
        fill = "County in sample",
        title = "County coverage of black-to-white disparity in prison admission rates: 2010 - 2019"
    ) +
    theme_void()

ggsave(
    here("4_clean_missing_data", "graphs", "county_coverage_black.png"),
    coverage_black,
    bg = "white",
    height = 7,
    width = 7
)

# Map county coverage for all.
counties_in_sample_hisp <-
    county_shapefiles_2021 |>
    filter(!(STATEFP %in% c("02", 15, 72))) |>
    mutate(in_sample = GEOID %in% data_no_missing_hisp$full_fips)

coverage_hisp <-
    ggplot(counties_in_sample_hisp) +
    geom_sf(aes(fill = in_sample), linewidth = 0.1) +
    geom_sf(data = state_shapefiles_2021, fill = NA, linewidth = 0.25) +
    labs(
        fill = "County in sample",
        title = "County coverage of hispanic-to-white disparity in prison admission rates: 2010 - 2019"
    ) +
    theme_void()

ggsave(
    here("4_clean_missing_data", "graphs", "county_coverage_hisp.png"),
    coverage_hisp,
    bg = "white",
    height = 7,
    width = 7
)
