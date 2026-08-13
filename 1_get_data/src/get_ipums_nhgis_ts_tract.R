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
meta_race_by_ethnicity <- get_metadata("nhgis", time_series_table = "B10")
meta_edu <- get_metadata("nhgis", time_series_table = "BW7")
meta_hh_income <- get_metadata("nhgis", time_series_table = "B71")

# For time series where we want a lot of years, filter to the year we want.
years_race_ethnicity <-
    meta_race_by_ethnicity$year |>
    filter(
        !(
            description %in%
                c(
                    "2000", "2020", "2006-2010", "2016-2020", "2017-2021",
                    "2018-2022", "2019-2023", "2020-2024"
                )
        )
    ) |>
    pull(description)

years_edu <-
    meta_edu$years |>
    filter(
        !(
            description %in%
                c(
                    "1970", "1980", "2000", "2016-2020", "2017-2021",
                    "2018-2022", "2019-2023", "2020-2024"
                )
        )
    ) |>
    pull(description)

years_hh_income <-
    meta_hh_income$years |>
    filter(
        !(
            description %in%
                c(
                    "1990", "2000", "2016-2020", "2017-2021", "2018-2022",
                    "2019-2023", "2020-2024"
                )
        )
    ) |>
    pull(description)

################################################################################
# Download time series tables of interest.
args_table <- list("B10", "BW7", "B71")
args_geog <- as.list(rep("tract", length(args_table)))
args_year <- list(years_race_ethnicity, years_edu, years_hh_income)
names <-
    list("race_and_ethnicity", "edu", "hh_income") %>%
    paste0("ipumsTS_tract_", .) %>%
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
