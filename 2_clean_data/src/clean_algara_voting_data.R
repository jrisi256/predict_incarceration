library(here)
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
read_dir <- here("2_clean_data", "input")
write_dir <- here("2_clean_data", "output")

################################################################################
# Read in data.
################################################################################
load(file.path(read_dir, "dataverse_shareable_presidential_county_returns_1868_2020.Rdata"))
load(file.path(read_dir, "dataverse_shareable_us_senate_county_returns_1908_2020.Rdata"))
load(file.path(read_dir, "dataverse_shareable_gubernatorial_county_returns_1865_2020.Rdata"))

################################################################################
# Clean county data.
################################################################################
# 46113 was renamed to 46102 in 2015.
clean_data <- function(df, election_cat) {
    df |>
        filter(!is.na(fips), !is.na(republican_raw_votes)) |>
        mutate(
            r_vote_share = republican_raw_votes / raw_county_vote_totals,
            state = str_sub(fips, 1, 2),
            county = str_sub(fips, 3, 5),
            election_type = election_cat
        ) |>
        rename(full_fips = fips, year = election_year) |>
        summarise(
            r_vote_share = mean(r_vote_share),
            .by = c(full_fips, year, state, county, election_type)
        ) |>
        mutate(
            full_fips = if_else(full_fips == "46113", "46102", full_fips),
            county = if_else(state == "46" & county == "113", "102", county)
        ) |>
        complete(full_fips, year = full_seq(year, 1)) |>
        fill(everything(), .by = full_fips, .direction = "down") |>
        filter(year >= 2010, year <= 2019, !is.na(full_fips), !is.na(r_vote_share))
}

elections_df <-
    pmap(
        list(
            list(gov_elections_release, pres_elections_release, senate_elections_release),
            list("governor", "president", "senate")
        ),
        clean_data
    ) |>
    bind_rows() |>
    pivot_wider(
        id_cols = c("full_fips", "year", "state","county"),
        names_from = "election_type",
        values_from = "r_vote_share"
    )

################################################################################
# Clean state data.
################################################################################
clean_state_data <- function(df, election_cat) {
    df |>
        filter(!is.na(fips), !is.na(republican_raw_votes)) |>
        mutate(state = str_sub(fips, 1, 2), election_type = election_cat) |>
        rename(year = election_year) |>
        distinct(fips, year, .keep_all = T) |>
        summarise(
            republican_raw_votes = sum(republican_raw_votes),
            raw_county_vote_totals = sum(raw_county_vote_totals),
            .by = c(state, year, election_type)
        ) |>
        mutate(r_vote_share = republican_raw_votes / raw_county_vote_totals) |>
        complete(state, year = full_seq(year, 1)) |>
        fill(everything(), .by = state, .direction = "down") |>
        filter(year >= 2010, year <= 2019, !is.na(r_vote_share))
}

elections_state_df <-
    pmap(
        list(
            list(gov_elections_release, pres_elections_release, senate_elections_release),
            list("governor_state", "president_state", "senate_state")
        ),
        clean_state_data
    ) |>
    bind_rows() |>
    pivot_wider(
        id_cols = c("year", "state"),
        names_from = "election_type",
        values_from = "r_vote_share"
    )

################################################################################
# Create final data.
################################################################################
elections_final_df <-
    full_join(elections_df, elections_state_df, by = c("state", "year")) |>
    mutate(
        lq_governor = governor / governor_state,
        lq_president = president / president_state,
        lq_senate = if_else(senate == 0 & senate_state == 0, 1, senate / senate_state)
    )

write_csv(
    elections_final_df,
    file.path(write_dir, "algara_voting_county_clean.csv")
)
