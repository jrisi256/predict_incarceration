library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)

read_dir <- here("1_get_data", "output")
save_dir <- here("2_clean_data", "output")

################################################################################
# Function for standardizing the age groups for the PEP from 2010 to 2024.
standardize_age_2010_2024 <- function(df) {
    df |>
        mutate(
            age =
                case_when(
                    AGEGRP == 0 ~ "All ages",
                    AGEGRP == 1 ~ "0-4 years",
                    AGEGRP == 2 ~ "5-9 years",
                    AGEGRP == 3 ~ "10-14 years",
                    AGEGRP == 4 ~ "15-19 years",
                    AGEGRP == 5 ~ "20-24 years",
                    AGEGRP == 6 ~ "25-29 years",
                    AGEGRP == 7 ~ "30-34 years",
                    AGEGRP == 8 ~ "35-39 years",
                    AGEGRP == 9 ~ "40-44 years",
                    AGEGRP == 10 ~ "45-49 years",
                    AGEGRP == 11 ~ "50-54 years",
                    AGEGRP == 12 ~ "55-59 years",
                    AGEGRP == 13 ~ "60-64 years",
                    AGEGRP == 14 ~ "65-69 years",
                    AGEGRP == 15 ~ "70-74 years",
                    AGEGRP == 16 ~ "75-79 years",
                    AGEGRP == 17 ~ "80-84 years",
                    AGEGRP == 18 ~ "85 years and over"
                )
        )
}

################################################################################
# Function for creating a standardized and clean PEP data frame.
create_clean_pep_df <- function(df) {
    diversity_df <-
        df |>
        filter(age == "All ages") |>
        mutate(
            pop_nr_est_allAges_w = NHBA_MALE + NHWA_FEMALE,
            pop_nr_est_allAges_b = NHBA_MALE + NHBA_FEMALE,
            pop_nr_est_allAges_h = H_MALE + H_FEMALE,
            pop_nr_est_allAges_aian = NHIA_MALE + NHIA_FEMALE,
            pop_nr_est_allAges_a = NHAA_MALE + NHAA_FEMALE,
            pop_nr_est_allAges_nhpi = NHNA_MALE + NHNA_FEMALE,
            pop_nr_est_allAges_m = NHTOM_MALE + NHTOM_FEMALE
        ) |>
        pivot_longer(
            matches("pop"), names_to = "race_ethnicity", values_to = "pop"
        ) |>
        group_by(full_fips, county, state, year) |>
        mutate(
            prcnt = pop / sum(pop),
            information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)
        ) |>
        summarise(
            shannon_index = sum(prcnt * information),
            gini_simpson_index = 1 - sum(prcnt ^ 2)
        ) |>
        mutate(shannon_index_scaled = shannon_index / log(7, 2)) |>
        ungroup()

    df <-
        df |>
        filter(age %in% c("All ages", "15-19 years", "20-24 years")) |>
        select(
            state, county, full_fips, year, TOT_POP, TOT_MALE, TOT_FEMALE,
            NHWA_MALE, NHWA_FEMALE, NHBA_MALE, NHBA_FEMALE, H_MALE, H_FEMALE, age
        ) |>
        pivot_wider(names_from = age, values_from = matches("POP|MALE")) |>
        rename("pop_nr_est" = "TOT_POP_All ages") |>
        mutate(
            pop_nr_est_allAges_w = `NHWA_MALE_All ages` + `NHWA_FEMALE_All ages`,
            pop_nr_est_allAges_b = `NHBA_MALE_All ages` + `NHBA_FEMALE_All ages`,
            pop_nr_est_allAges_h = `H_MALE_All ages` + `H_FEMALE_All ages`,
            pop_prcnt_est_allAges_w = pop_nr_est_allAges_w / pop_nr_est,
            pop_prcnt_est_allAges_b = pop_nr_est_allAges_b / pop_nr_est,
            pop_prcnt_est_allAges_h = pop_nr_est_allAges_h / pop_nr_est,
            pop_nr_est_15to24_allRaces_m = `TOT_MALE_15-19 years` + `TOT_MALE_20-24 years`,
            pop_prcnt_est_15to24_allRaces_m = pop_nr_est_15to24_allRaces_m / pop_nr_est,
            pop_nr_est_15to24_b_m = `NHBA_MALE_15-19 years` + `NHBA_MALE_20-24 years`,
            pop_nr_est_15to24_h_m = `H_MALE_15-19 years` + `H_MALE_20-24 years`,
            pop_prcnt_est_15to24_b_m = pop_nr_est_15to24_b_m / pop_nr_est_15to24_allRaces_m,
            pop_prcnt_est_15to24_h_m = pop_nr_est_15to24_h_m / pop_nr_est_15to24_allRaces_m
        ) |>
        select(-matches("All ages|15-19 years|20-24 years")) |>
        full_join(diversity_df, by = c("full_fips", "state", "county", "year"))
}

################################################################################
# Read in and clean/restructure the 1990 - 1999 data.
pep_1990_1999_county_df <-
    read_csv(file.path(read_dir, "pep_1999-2000_county.csv.gz")) |>
    mutate(
        age_group =
            case_when(
                AGEGRP == "00" ~ "<1 year",
                AGEGRP == "01" ~ "1-4 years",
                AGEGRP == "02" ~ "5-9 years",
                AGEGRP == "03" ~ "10-14 years",
                AGEGRP == "04" ~ "15-19 years",
                AGEGRP == "05" ~ "20-24 years",
                AGEGRP == "06" ~ "25-29 years",
                AGEGRP == "07" ~ "30-34 years",
                AGEGRP == "08" ~ "35-39 years",
                AGEGRP == "09" ~ "40-44 years",
                AGEGRP == "10" ~ "45-49 years",
                AGEGRP == "11" ~ "50-54 years",
                AGEGRP == "12" ~ "55-59 years",
                AGEGRP == "13" ~ "60-64 years",
                AGEGRP == "14" ~ "65-69 years",
                AGEGRP == "15" ~ "70-74 years",
                AGEGRP == "16" ~ "75-79 years",
                AGEGRP == "17" ~ "80-84 years",
                AGEGRP == "18" ~ "85 years and over"
            ),
        race_ethnicity =
            case_when(
                (RACE_SEX == "01" | RACE_SEX == "02") & HISP == 1 ~ "white",
                (RACE_SEX == "03" | RACE_SEX == "04") & HISP == 1 ~ "black",
                (RACE_SEX == "05" | RACE_SEX == "06") & HISP == 1 ~ "ai_or_an",
                (RACE_SEX == "07" | RACE_SEX == "08") & HISP == 1 ~ "asian_or_pi",
                HISP == 2 ~ "hispanic"
            ),
        sex =
            case_when(
                RACE_SEX %in% c("01", "03", "05", "07") ~ "male",
                RACE_SEX %in% c("02", "04", "06", "08") ~ "female"
            ),
        year =
            if_else(
                str_length(YEAR) == 1,
                paste0(19, YEAR, 9), paste0(19, YEAR)
            )
    )

diversity <-
    pep_1990_1999_county_df |>
    count(
        full_fips, state, county, race_ethnicity, year, wt = POP, name = "pop"
    ) |>
    group_by(full_fips, state, county, year) |>
    mutate(
        prcnt = pop / sum(pop),
        information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)
    ) |>
    summarise(
        shannon_index = sum(prcnt * information),
        gini_simpson_index = 1 - sum(prcnt ^ 2)
    ) |>
    ungroup() |>
    mutate(shannon_index_scaled = shannon_index / log(5, 2)) |>
    # IPUMS has more complete race/ethnicity data for the 1990 Census.
    filter(year != 1990)

total_pop <-
    pep_1990_1999_county_df |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est")

white_pop <-
    pep_1990_1999_county_df |>
    filter(race_ethnicity == "white") |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est_allAges_w")

black_pop <-
    pep_1990_1999_county_df |>
    filter(race_ethnicity == "black") |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est_allAges_b")

hispanic_pop <-
    pep_1990_1999_county_df |>
    filter(race_ethnicity == "hispanic") |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est_allAges_h")

ages15to24_pop <-
    pep_1990_1999_county_df |>
    filter(age_group %in% c("15-19 years", "20-24 years"), sex == "male") |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est_15to24_allRaces_m")

ages15to24_b_pop <-
    pep_1990_1999_county_df |>
    filter(age_group %in% c("15-19 years", "20-24 years"), sex == "male", race_ethnicity == "black") |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est_15to24_b_m")

ages15to24_h_pop <-
    pep_1990_1999_county_df |>
    filter(age_group %in% c("15-19 years", "20-24 years"), sex == "male", race_ethnicity == "hispanic") |>
    count(full_fips, state, county, year, wt = POP, name = "pop_nr_est_15to24_h_m")

clean_1990_2000 <-
    reduce(
        list(
            total_pop, white_pop, black_pop, hispanic_pop, ages15to24_pop,
            ages15to24_b_pop, ages15to24_h_pop, diversity
        ),
        function(x, y) {full_join(x, y, by = c("full_fips", "state", "county", "year"))}
    ) |>
    mutate(
        year = as.numeric(year),
        pop_prcnt_est_allAges_w = pop_nr_est_allAges_w / pop_nr_est,
        pop_prcnt_est_allAges_b = pop_nr_est_allAges_b / pop_nr_est,
        pop_prcnt_est_allAges_h = pop_nr_est_allAges_h / pop_nr_est,
        pop_prcnt_est_15to24_allRaces_m = pop_nr_est_15to24_allRaces_m / pop_nr_est,
        pop_prcnt_est_15to24_b_m = pop_nr_est_15to24_b_m / pop_nr_est_15to24_allRaces_m,
        pop_prcnt_est_15to24_h_m = pop_nr_est_15to24_h_m / pop_nr_est_15to24_allRaces_m
    )

################################################################################
# Read in and clean/restructure the 2000 - 2009 data.
pep_2000_2009_county_df <-
    read_csv(file.path(read_dir, "pep_2000-2009_county.csv.gz"))

clean_2000_2009 <-
    pep_2000_2009_county_df |>
    mutate(
        year =
            case_when(
                YEAR == 1 ~  "4/1/2000 (Census)",
                YEAR == 2 ~ "7/1/2000 (PEP)",
                YEAR == 3 ~ "2001",
                YEAR == 4 ~ "2002",
                YEAR == 5 ~ "2003",
                YEAR == 6 ~ "2004",
                YEAR == 7 ~ "2005",
                YEAR == 8 ~ "2006",
                YEAR == 9 ~ "2007",
                YEAR == 10 ~ "2008",
                YEAR == 11 ~ "2009",
                YEAR == 12 ~ "4/1/2010 (Census)",
                YEAR == 13 ~ "7/1/2010 (Census)"
            ),
        age =
            case_when(
                AGEGRP == 0 ~ "<1 year",
                AGEGRP == 1 ~ "1-4 years",
                AGEGRP == 2 ~ "5-9 years",
                AGEGRP == 3 ~ "10-14 years",
                AGEGRP == 4 ~ "15-19 years",
                AGEGRP == 5 ~ "20-24 years",
                AGEGRP == 6 ~ "25-29 years",
                AGEGRP == 7 ~ "30-34 years",
                AGEGRP == 8 ~ "35-39 years",
                AGEGRP == 9 ~ "40-44 years",
                AGEGRP == 10 ~ "45-49 years",
                AGEGRP == 11 ~ "50-54 years",
                AGEGRP == 12 ~ "55-59 years",
                AGEGRP == 13 ~ "60-64 years",
                AGEGRP == 14 ~ "65-69 years",
                AGEGRP == 15 ~ "70-74 years",
                AGEGRP == 16 ~ "75-79 years",
                AGEGRP == 17 ~ "80-84 years",
                AGEGRP == 18 ~ "85 years and over",
                AGEGRP == 99 ~ "All ages"
            )
    ) |>
    filter(
        !(year %in% c("7/1/2000 (PEP)", "4/1/2010 (Census)", "7/1/2010 (Census)"))
    ) |>
    mutate(
        year = if_else(year == "4/1/2000 (Census)", "2000", year),
        year = as.numeric(year)
    ) |>
    create_clean_pep_df()

################################################################################
# Read in and clean/restructure the 2010 - 2019 data.
# Aggregate counties 02063 and 02066 into 02261 (split in 2019).
pep_2010_2019_county_df <-
    read_csv(file.path(read_dir, "pep_2010-2019_county.csv.gz"))

clean_2010_2019 <-
    pep_2010_2019_county_df |>
    mutate(
        year =
            case_when(
                YEAR == 1 ~  "4/1/2010 (Census)",
                YEAR == 2 ~ "4/1/2010 (PEP)",
                YEAR == 3 ~ "7/1/2010 (PEP)",
                YEAR == 4 ~ "2011",
                YEAR == 5 ~ "2012",
                YEAR == 6 ~ "2013",
                YEAR == 7 ~ "2014",
                YEAR == 8 ~ "2015",
                YEAR == 9 ~ "2016",
                YEAR == 10 ~ "2017",
                YEAR == 11 ~ "2018",
                YEAR == 12 ~ "2019",
                YEAR == 13 ~ "7/1/2020 (PEP)",
            )
    ) |>
    standardize_age_2010_2024() |>
    mutate(
        county =
            case_when(
                state == "02" & county == "063" ~ "261",
                state == "02" & county == "066" ~ "261",
                T ~ county
            ),
        full_fips =
            case_when(
                full_fips == "02063" ~ "02261",
                full_fips == "02066" ~ "02261",
                T ~ full_fips
            ),
        year =
            case_when(
                year == "4/1/2010 (PEP)" & full_fips == "02261" ~ "2010",
                year == "4/1/2010 (Census)" & full_fips == "02261" ~ "4/1/2010 (PEP)",
                T ~ year
            )
    ) |>
    filter(!(year %in% c("4/1/2010 (PEP)", "7/1/2010 (PEP)", "7/1/2020 (PEP)"))) |>
    mutate(
        year = if_else(year == "4/1/2010 (Census)", "2010", year),
        year = as.numeric(year)
    ) |>
    summarise(
        across(-matches("state|county|year|age|fips"), function(col) {sum(col)}),
        .by = c(state, county, full_fips, year, YEAR, AGEGRP, age)
    ) |>
    create_clean_pep_df()

# Calculate location quotient.
state_df <-
    pep_2010_2019_county_df |>
    mutate(
        year =
            case_when(
                YEAR == 1 ~  "4/1/2010 (Census)",
                YEAR == 2 ~ "4/1/2010 (PEP)",
                YEAR == 3 ~ "7/1/2010 (PEP)",
                YEAR == 4 ~ "2011",
                YEAR == 5 ~ "2012",
                YEAR == 6 ~ "2013",
                YEAR == 7 ~ "2014",
                YEAR == 8 ~ "2015",
                YEAR == 9 ~ "2016",
                YEAR == 10 ~ "2017",
                YEAR == 11 ~ "2018",
                YEAR == 12 ~ "2019",
                YEAR == 13 ~ "7/1/2020 (PEP)",
            )
    ) |>
    standardize_age_2010_2024() |>
    filter(
        !(year %in% c("4/1/2010 (PEP)", "7/1/2010 (PEP)", "7/1/2020 (PEP)"))
    ) |>
    mutate(
        year = if_else(year == "4/1/2010 (Census)", "2010", year),
        year = as.numeric(year)
    ) |>
    filter(age == "All ages") |>
    mutate(
        pop_nr_est_allAges_w = NHBA_MALE + NHWA_FEMALE,
        pop_nr_est_allAges_b = NHBA_MALE + NHBA_FEMALE,
        pop_nr_est_allAges_h = H_MALE + H_FEMALE,
        pop_nr_est_allAges_aian = NHIA_MALE + NHIA_FEMALE,
        pop_nr_est_allAges_a = NHAA_MALE + NHAA_FEMALE,
        pop_nr_est_allAges_nhpi = NHNA_MALE + NHNA_FEMALE,
        pop_nr_est_allAges_m = NHTOM_MALE + NHTOM_FEMALE
    ) |>
    select(matches("state|pop_"), year) |>
    pivot_longer(
        matches("pop"), names_to = "race_ethnicity", values_to = "pop"
    ) |>
    summarise(
        state_pop = sum(pop, na.rm = T),
        .by = c(state, year, race_ethnicity)
    ) |>
    mutate(prcnt = state_pop / sum(state_pop), .by = c(state, year)) |>
    pivot_wider(
        id_cols = c("state", "year"),
        values_from = prcnt,
        names_from = race_ethnicity
    ) |>
    rename_with(
        .fn = function(col) {paste0(str_replace(col, "_nr_", "_prcnt_"), "_state")},
        matches("_nr_")
    )

clean_2010_2019_lq <-
    full_join(clean_2010_2019, state_df, by = c("state", "year")) |>
    mutate(
        lq_black = pop_prcnt_est_allAges_b / pop_prcnt_est_allAges_b_state,
        lq_white = pop_prcnt_est_allAges_w / pop_prcnt_est_allAges_w_state,
        lq_hisp = pop_prcnt_est_allAges_h / pop_prcnt_est_allAges_h_state
    ) |>
    select(-matches("_state"))

################################################################################
# Read in and clean/restructure the 2020 - 2024 data.
pep_2020_2024_county_df <-
    read_csv(file.path(read_dir, "pep_2020-2024_county.csv.gz"))

clean_2020_2024 <-
    pep_2020_2024_county_df |>
    mutate(
        year =
            case_when(
                YEAR == 1 ~  "4/1/2020 (Census)",
                YEAR == 2 ~ "7/1/2020 (PEP)",
                YEAR == 3 ~ "2021",
                YEAR == 4 ~ "2022",
                YEAR == 5 ~ "2023",
                YEAR == 6 ~ "2024"
            )
    ) |>
    standardize_age_2010_2024() |>
    filter(year != "7/1/2020 (PEP)") |>
    mutate(
        year = if_else(year == "4/1/2020 (Census)", "2020", year),
        year = as.numeric(year)
    ) |>
    create_clean_pep_df()

################################################################################
# Join together all the decades and save the data.
clean_pep_all <-
    list(clean_1990_2000, clean_2000_2009, clean_2010_2019_lq, clean_2020_2024) |>
    bind_rows() |>
    select(matches("pop_nr_est$|prcnt|index|year|fips|state|county|lq")) |>
    select(-shannon_index)

write_csv(clean_pep_all, file.path(save_dir, "pep_county_clean.csv"))
