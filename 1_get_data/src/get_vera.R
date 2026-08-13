library(here)
library(R.utils)

download.file(
    "https://raw.githubusercontent.com/vera-institute/incarceration-trends/refs/heads/main/incarceration_trends_county.csv",
    here("1_get_data", "output", "vera_county.csv")
)

gzip(file.path("1_get_data", "output", "vera_county.csv"), remove = T, overwrite = T)
