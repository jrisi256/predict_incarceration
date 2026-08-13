library(here)
library(httr)
library(purrr)
dir <- here("1_get_data", "input")

################################################################################
# Download yearly unemployment data.
################################################################################
download_laus_year <- function(year_last_two) {
    GET(
        paste0("https://www.bls.gov/lau/laucnty", year_last_two, ".xlsx"),
        user_agent(
            paste0(
                "Mozilla/5.0 (Windows NT 10.0; Win64; x64)",
                "AppleWebKit/537.36 (KHTML, like Gecko)",
                "Chrome/120.0.0.0 Safari/537.36"
            )
        ),
        add_headers("Accept-Language" = "en-US,en;q=0.9"),
        write_disk(
            file.path(dir, paste0("bls_county_", year_last_two, ".xlsx")),
            overwrite = T
        )
    )
}

walk(10:19, download_laus_year)
