library(sf)
library(here)
library(dplyr)
library(ipumsr)
library(tigris)

# See available shape files.
shapefile_datasets <- get_metadata_catalog("nhgis", "shapefiles")

# Download shape files.
request <-
    define_extract_agg(
        "nhgis",
        shapefiles =
            c(
                "us_tract_2010_tl2010",
                "us_tract_2011_tl2011",
                "us_tract_2019_tl2019",
                "us_county_2019_tl2019",
                "us_state_2019_tl2019"
            )
    )

submit <- submit_extract(request)
wait <- wait_for_extract(submit)
download <- download_extract(wait, download_dir = here("1_get_data", "output"))
file.rename(
    download,
    here("1_get_data", "input", "ipums_shapefileTractCountyState.zip")
)

################################################################################
# Clean tract shape files.
################################################################################
# Read in census tract shape files.
tract_shapefiles_2019 <-
    read_ipums_sf(
        here("1_get_data", "input", "ipums_shapefileTractCountyState.zip"),
        file_select = "nhgis0108_shape/nhgis0108_shapefile_tl2019_us_tract_2019.zip"
    ) |>
    mutate(year = 2019)

tract_shapefiles_2011 <-
    read_ipums_sf(
        here("1_get_data", "input", "ipums_shapefileTractCountyState.zip"),
        file_select = "nhgis0108_shape/nhgis0108_shapefile_tl2011_us_tract_2011.zip"
    ) |>
    mutate(year = 2011)

tract_shapefiles_2010 <-
    read_ipums_sf(
        here("1_get_data", "input", "ipums_shapefileTractCountyState.zip"),
        file_select = "nhgis0108_shape/nhgis0108_shapefile_tl2010_us_tract_2010.zip"
    ) |>
    mutate(year = 2010)

tract_shapefiles <-
    tract_shapefiles_2019 |>
    select(STATEFP, COUNTYFP, TRACTCE, GEOID, ALAND, AWATER, year) |>
    bind_rows(
        tract_shapefiles_2011 |>
            filter(GEOID %in% c("06037800204", "06037930401")) |>
            select(STATEFP, COUNTYFP, TRACTCE, GEOID, ALAND, AWATER, year),
        tract_shapefiles_2010 |>
            filter(GEOID10 == "36065023000") |>
            select(STATEFP10, COUNTYFP10, TRACTCE10, GEOID10, ALAND10, AWATER10, year) |>
            rename(
                STATEFP = STATEFP10, COUNTYFP = COUNTYFP10, TRACTCE = TRACTCE10,
                GEOID = GEOID10, ALAND = ALAND10, AWATER = AWATER10
            )
    ) |>
    mutate(
        GEOID =
            case_when(
                GEOID == "06037800204" & year == 2011 ~ "06037800204_10-11",
                GEOID == "36065023000" & year == 2010 ~ "36065023000_10",
                T ~ GEOID
            )
    ) |>
    rename(
        state = STATEFP, county = COUNTYFP, tract = TRACTCE, tract_fips = GEOID
    ) |>
    select(-year)

# Read in census tracts that are only water as IPUMS does not provide them.
suffolk_ma <- tracts("MA", "Suffolk", cb = T, year = 2019)
skagit_wa <- tracts("WA", "Skagit", cb = T, year = 2019)
tigris_shapefiles <-
    bind_rows(
        suffolk_ma |> filter(GEOID == "25025990101") |> select(STATEFP, COUNTYFP, TRACTCE, GEOID, ALAND, AWATER),
        skagit_wa |> filter(GEOID == "53057990100") |> select(STATEFP, COUNTYFP, TRACTCE, GEOID, ALAND, AWATER)
    ) |>
    rename(state = STATEFP, county = COUNTYFP, tract = TRACTCE, tract_fips = GEOID) |>
    select(state, county, tract, tract_fips, ALAND, AWATER) |>
    st_transform(st_crs(tract_shapefiles))

# Save final tract data.
tract_shapefiles_final <-
    bind_rows(tract_shapefiles, tigris_shapefiles) |>
    mutate(
        area_meters =
            if_else(
                tract_fips %in% c("25025990101", "53057990100"), AWATER, ALAND
            )
    ) |>
    select(-AWATER, -ALAND)

saveRDS(
    tract_shapefiles_final,
    here("1_get_data", "output", "tract_shapefiles.rds")
)

################################################################################
# Clean county shape files.
################################################################################
county_shapefiles <-
    read_ipums_sf(
        here("1_get_data", "input", "ipums_shapefileTractCountyState.zip"),
        file_select = "nhgis0108_shape/nhgis0108_shapefile_tl2019_us_county_2019.zip"
    ) |>
    mutate(
        full_fips = paste0(STATEFP, COUNTYFP), aland_miles = ALAND / 2589988.11
    ) |>
    rename(state = STATEFP, county = COUNTYFP) |>
    select(state, county, full_fips, aland_miles)

saveRDS(
    county_shapefiles,
    here("1_get_data", "output", "county_shapefiles.rds")
)

################################################################################
# Clean state shape files.
################################################################################
state_shapefiles <-
    read_ipums_sf(
        here("1_get_data", "input", "ipums_shapefileTractCountyState.zip"),
        file_select = "nhgis0108_shape/nhgis0108_shapefile_tl2019_us_state_2019.zip"
    ) |>
    rename(state = STATEFP) |>
    select(state)

saveRDS(
    state_shapefiles,
    here("1_get_data", "output", "state_shapefiles.rds")
)
