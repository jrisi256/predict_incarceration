library(here)
library(dplyr)
library(readr)
library(stringr)
library(ggplot2)
read_dir <- here("2_clean_data", "input")
write_dir <- here("2_clean_data", "output")
graph_dir <- here("2_clean_data", "graphs")

################################################################################
# Read in data.
################################################################################
ucr_agency <- readRDS(file.path(read_dir, "offenses_known_yearly_1960_2024.rds"))

################################################################################
# Initial cleaning of data.
################################################################################
ucr_county <-
    ucr_agency |>
    filter(year >= 2010, year <= 2019) |>
    filter(
        !is.na(fips_state_code),
        !(state %in% c("american samoa", "canal zone", "guam", "puerto rico", "virgin islands")),
        population_group != "possessions"
    ) |>
    mutate(
        pop_group =
            case_when(
                population_group %in%
                    c(
                        "city 1,000,000+",
                        "city 500,000 thru 999,999",
                        "city 250,000 thru 499,999"
                    ) ~ "cities 250,000 and over",
                population_group == "city 100,000 thru 249,999" ~ "cities 100,000 - 249,999",
                population_group == "city 50,000 thru 99,999" ~ "cities 50,000 - 99,999",
                population_group == "city 25,000 thru 49,999" ~ "cities 25,000 - 49,999",
                population_group == "city 10,000 thru 24,999" ~ "cities 10,000 - 24,999",
                population_group == "city 2,500 thru 9,999" ~ "cities 2,500 - 9,999",
                population_group == "city under 2,500" ~ "cities under 2,500",
                population_group %in%
                    c(
                        "non-msa county under 10,000", "non-msa county 100,000+",
                        "non-msa county 25,000 thru 99,999", "non-msa state police",
                        "non-msa county 10,000 thru 24,999"
                    ) ~ "non-MSA counties and state police",
                population_group %in%
                    c(
                        "msa-county 100,000+", "msa-county 25,000 thru 99,999",
                        "msa-county 10,000 thru 24,999", "msa state police",
                        "msa-county under 10,000"
                    ) ~ "MSA counties and state police"
            )
    ) |>
    select(
        ori, agency_name, year, number_of_months_reported, fips_state_code,
        fips_county_code, fips_state_county_code, agency_type, pop_group,
        matches("^population_[123]$"), matches("population_[123]_county"),
        actual_all_crimes, actual_index_property, actual_index_violent,
        total_cleared_all_crimes, total_cleared_index_violent,
        total_cleared_index_property
    )

ucr_pop_group <-
    ucr_county |>
    filter(number_of_months_reported == 12) |>
    summarise(
        across(
            matches("actual|cleared"),
            function(col) {mean(col)},
            .names = "{.col}_mean"
        ),
        .by = c(year, fips_state_code, pop_group),
    )

ucr_county_pop_group <-
    ucr_county |>
    left_join(ucr_pop_group, by = c("year", "fips_state_code", "pop_group")) |>
    mutate(across(matches("mean"), function(col) {if_else(is.na(col), 0, col)}))

################################################################################
# Impute missing values.
################################################################################
ucr_county_imputed <-
    ucr_county_pop_group |>
    mutate(
        across(
            matches("(property|violent|crimes)$"),
            function(col) {
                case_when(
                    number_of_months_reported == 12 ~ col,
                    number_of_months_reported >= 3 & number_of_months_reported <= 11 ~ col * (12 / number_of_months_reported),
                    number_of_months_reported >= 1 & number_of_months_reported <= 2 ~ get(paste0(cur_column(), "_mean")),
                    number_of_months_reported == 0 ~ 0
                )
            },
            .names = "{.col}_imputed"
        )
    ) |>
    summarise(
        across(
            matches("_imputed"),
            function(col) {sum(col)}
        ),
        .by = c(year, fips_state_code, fips_county_code, fips_state_county_code)
    )

################################################################################
# Compare to official values.
################################################################################
load(file.path(read_dir, "33523-0004-Data.rda"))
load(file.path(read_dir, "34582-0004-Data.rda"))
load(file.path(read_dir, "35019-0004-Data.rda"))
load(file.path(read_dir, "36117-0004-Data.rda"))
load(file.path(read_dir, "36399-0004-Data.rda"))
load(file.path(read_dir, "37059-0004-Data.rda"))
ucr_2010 <- da33523.0004 |> mutate(year = 2010) |> select(-STUDYNO, -EDITION, -PART)
ucr_2011 <- da34582.0004 |> mutate(year = 2011) |> select(-STUDYNO, -EDITION, -PART)
ucr_2012 <- da35019.0004 |> mutate(year = 2012) |> select(-STUDYNO, -EDITION, -PART)
ucr_2013 <- da36117.0004 |> mutate(year = 2013) |> select(-STUDYNO, -EDITION, -PART)
ucr_2014 <- da36399.0004 |> mutate(year = 2014) |> select(-STUDYNO, -EDITION, -PART)
ucr_2016 <- da37059.0004 |> mutate(year = 2016) |> select(-STUDYNO, -EDITION, -PART)

ucr_official <-
    bind_rows(ucr_2010, ucr_2011, ucr_2012, ucr_2013, ucr_2014, ucr_2016) |>
    mutate(
        fips_state_code =
            if_else(
                str_length(FIPS_ST) == 1,
                paste0("0", FIPS_ST),
                as.character(FIPS_ST)
            ),
        fips_county_code =
            case_when(
                str_length(FIPS_CTY) == 1 ~ paste0("00", FIPS_CTY),
                str_length(FIPS_CTY) == 2 ~ paste0("0", FIPS_CTY),
                str_length(FIPS_CTY) == 3 ~ as.character(FIPS_CTY)
            )
    ) |>
    select(matches("fips_(state|county)|year|VIOL|PROPERTY"))

match <-
    ucr_county_imputed |>
    left_join(
        ucr_official, by = c("fips_state_code", "fips_county_code", "year")
    ) |>
    relocate(
        actual_index_violent_imputed, VIOL, actual_index_property_imputed,
        PROPERTY
    ) |>
    mutate(
        prcnt_diff_violence =
            if_else(
                actual_index_violent_imputed == 0 & VIOL == 0,
                0,
                abs(actual_index_violent_imputed - VIOL) / ((actual_index_violent_imputed + VIOL) / 2) * 100
            ),
        prcnt_diff_property =
            if_else(
                actual_index_property_imputed == 0 & PROPERTY == 0,
                0,
                abs(actual_index_property_imputed - PROPERTY) / ((actual_index_property_imputed + PROPERTY) / 2) * 100
            )
    ) |>
    rename(
        state = fips_state_code,
        county = fips_county_code,
        full_fips = fips_state_county_code
    )

write_csv(match, file.path(write_dir, "ucr_county_clean.csv"))

################################################################################
# Graph comparison between imputed values and official values.
################################################################################
violence_diff_graph <-
    ggplot(match, aes(x = prcnt_diff_violence, y = after_stat(count / sum(count)))) +
    geom_histogram(bins = 40, color = "white") +
    theme_bw() +
    labs(
        x = "Percentage Difference in Violent Crime Rates",
        y = "Proportion",
        title = "Percentage Difference in Violent Crime Rates: Official vs. Manually Calculated"
    ) +
    scale_x_continuous(breaks = seq(0, 200, by = 10)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05))

ggsave(
    file.path(graph_dir, "violent_crime_diff_graph.png"),
    violence_diff_graph,
    width = 8,
    height = 4
)

violence_diff_corr <-
    ggplot(match, aes(x = actual_index_violent_imputed, y = VIOL)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw() +
    labs(
        x = "Manually Calculated Violent Crime Rates",
        y = "Officially Reported Violent Crime Rates"
    ) +
    annotate(
        "text",
        x = 5700,
        y = 55000,
        label =
            paste0(
                "Correlation Coefficient: ",
                round(
                    cor(
                        match$actual_index_violent_imputed,
                        match$VIOL,
                        use = "complete.obs"
                    ),
                    3
                )
            ),
        fontface = "bold"
    )

ggsave(
    file.path(graph_dir, "violent_crime_corr_graph.png"),
    violence_diff_corr,
    width = 12,
    height = 8
)

property_diff_graph <-
    ggplot(match, aes(x = prcnt_diff_property, y = after_stat(count / sum(count)))) +
    geom_histogram(bins = 40, color = "white") +
    theme_bw() +
    labs(
        x = "Percentage Difference in Property Crime Rates",
        y = "Proportion",
        title = "Percentage Difference in Property Crime Rates: Official vs. Manually Calculated"
    ) +
    scale_x_continuous(breaks = seq(0, 200, by = 10)) +
    scale_y_continuous(breaks = seq(0, 1, by = 0.05))

ggsave(
    file.path(graph_dir, "property_crime_diff_graph.png"),
    property_diff_graph,
    width = 8,
    height = 4
)

property_diff_corr <-
    ggplot(match, aes(x = actual_index_property_imputed, y = PROPERTY)) +
    geom_point() +
    geom_abline(intercept = 0, slope = 1) +
    theme_bw() +
    labs(
        x = "Manually Calculated Property Crime Rates",
        y = "Officially Reported Property Crime Rates"
    ) +
    annotate(
        "text",
        x = 18260,
        y = 220400,
        label =
            paste0(
                "Correlation Coefficient: ",
                round(
                    cor(
                        match$actual_index_property_imputed,
                        match$PROPERTY,
                        use = "complete.obs"
                    ),
                    3
                )
            ),
        fontface = "bold"
    )

ggsave(
    file.path(graph_dir, "property_crime_corr_graph.png"),
    property_diff_corr,
    width = 13,
    height = 9
)
