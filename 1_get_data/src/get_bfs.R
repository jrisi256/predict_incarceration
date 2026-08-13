library(here)

download.file(
    "https://www.census.gov/econ/bfs/xlsx/bfs_county_apps_annual.xlsx",
    here("1_get_data", "output", "bfs_county.xlsx")
)