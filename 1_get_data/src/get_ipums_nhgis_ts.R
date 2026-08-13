library(here)
library(purrr)
library(dplyr)
library(ipumsr)

download_dir <- here("1_get_data", "output")

# Define the spec for the extract request and the request itself.
# Submit the extract request, wait for the download, and then download the data.
download_ipums_timeseries <- function(table_arg, geog_arg, years_arg, dir) {
    spec <- tst_spec(table_arg, geog_levels = geog_arg, years = years_arg)
    request <- define_extract_nhgis(time_series_tables = spec)
    submit <- submit_extract(request)
    wait <- wait_for_extract(submit)
    download <- download_extract(wait, download_dir = dir)
    return(download)
}

################################################################################
# See all time series data sets available from IPUMS.
time_series_datasets <- get_metadata_catalog("nhgis", "time_series_tables")

# Get information on time series tables of interest.
meta_sex_by_age <- get_metadata("nhgis", time_series_table = "B58")
meta_race_by_ethnicity <- get_metadata("nhgis", time_series_table = "AE7")
meta_family_paoc <- get_metadata("nhgis", time_series_table = "AG4")
meta_marriage <- get_metadata("nhgis", time_series_table = "BL1")
meta_edu <- get_metadata("nhgis", time_series_table = "BW7")
meta_labor <- get_metadata("nhgis", time_series_table = "B84")
meta_labor_sex <- get_metadata("nhgis", time_series_table = "BS4")
meta_hh_income_1980 <- get_metadata("nhgis", time_series_table = "B70")
meta_hh_income <- get_metadata("nhgis", time_series_table = "B71")
meta_median_hh_income <- get_metadata("nhgis", time_series_table = "B79")
meta_poverty <- get_metadata("nhgis", time_series_table = "CL7")
meta_occupancy <- get_metadata("nhgis", time_series_table = "A43")
meta_owner_renter <- get_metadata("nhgis", time_series_table = "A40")
meta_owner_renter_race <- get_metadata("nhgis", time_series_table = "D03")
meta_poverty_children <- get_metadata("nhgis", time_series_table = "BV3")

# For time series where we want a lot of years, filter to the year we want.
years_family_paoc <- meta_family_paoc$years |> filter(name != "1970") |> pull(description)
years_marriage <- meta_marriage$years |> filter(name != "1970") |> pull(description)
years_edu <- meta_edu$years |> filter(name != "1970") |> pull(description)
years_labor <- meta_labor$years |> filter(name != "1970") |> pull(description)
years_labor_sex <- meta_labor_sex$years |> filter(name != "1970") |> pull(description)
years_hh_income_1980 <- "1980"
years_hh_income <- meta_hh_income$years$description
years_median_hh_income <- meta_median_hh_income$years$description
years_poverty <- meta_poverty$years$description
years_occupancy <- meta_occupancy$years |> filter(name != "1970") |> pull(description)
years_owner_renter <- meta_owner_renter$years$description
years_owner_renter_race <- meta_owner_renter_race$years$description

################################################################################
# Download time series tables of interest.
args_table <-
    list(
        "B58", "AE7", "AG4", "BL1", "BW7", "B85", "B84", "B70", "B71", "B79",
        "CL7", "A43", "A40", "D03", "BS4", "BV3"
    )
args_geog <- as.list(rep("county", length(args_table)))
args_year <-
    list(
        "1980", c("1980", "1990"), years_family_paoc, years_marriage, years_edu,
        "1990", years_labor, years_hh_income_1980, years_hh_income,
        years_median_hh_income, years_poverty, years_occupancy,
        years_owner_renter, years_owner_renter_race, years_labor_sex, "1980"
    )
names <-
    list(
        "sex_by_age", "race_and_ethnicity", "family_paoc", "marriage", "edu",
        "edu_1990", "labor", "hh_income_1980", "hh_income", "median_hh_income",
        "poverty", "occupancy", "owner_renter", "owner_renter_race", "labor_sex",
        "pover"
    ) %>%
    paste0("ipumsTS_", .) %>%
    file.path(download_dir, .) %>%
    paste0(., ".csv.zip") |>
    as.list()

ipums_filepaths <-
    pmap(
        list(args_table, args_geog, args_year),
        download_ipums_timeseries,
        dir = download_dir
    )

################################################################################
# Rename files for easy access later.
pwalk(list(ipums_filepaths, names), function(from, to) {file.rename(from, to)})
