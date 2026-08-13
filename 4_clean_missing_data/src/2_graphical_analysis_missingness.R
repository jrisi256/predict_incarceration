library(sf)
library(here)
library(readr)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
source(here("functions.R"))

################################################################################
# Read in data.
################################################################################
read_dir <- here("4_clean_missing_data", "output")
county_shapefiles <- readRDS(here("1_get_data", "output", "county_shapefiles.rds"))
state_shapefiles <- readRDS(here("1_get_data", "output", "state_shapefiles.rds"))

data_missing_outcome <- read_csv(file.path(read_dir, "data_missing_outcome.csv"))
data_missing_predictors <- read_csv(file.path(read_dir, "data_missing_predictors.csv"))
data_no_missing <- read_csv(file.path(read_dir, "data_no_missing.csv"))
data_boundary_issues <- read_csv(file.path(read_dir, "data_boundary_issues.csv"))

data_all <-
    bind_rows(
        data_missing_outcome,
        data_missing_predictors,
        data_no_missing,
        data_boundary_issues |>
            filter(full_fips != "51515") |>
            mutate(across(-matches("state|county|fips|^year$"), function(col) {NA}))
    )

################################################################################
# Graph county coverage.
################################################################################
county_missing_outcome <-
    data_missing_outcome |>
    count(full_fips) |>
    filter(n == 10) |>
    mutate(sample = "Missing outcome") |>
    select(-n)

county_no_missing <-
    data_no_missing |>
    distinct(full_fips) |>
    mutate(sample = "Main analytical sample")

county_missing_predictor <-
    data_missing_predictors |>
    filter(
        !(full_fips %in% c(county_missing_outcome$full_fips, county_no_missing$full_fips))
    ) |>
    distinct(full_fips) |>
    mutate(sample = "Missing predictor")

county_boundary_issues <-
    data_boundary_issues |>
    filter(full_fips != "51515") |>
    distinct(full_fips) |>
    mutate(sample = "Boundary issues")

counties_in_sample <-
    county_shapefiles |>
    filter(!(state %in% c("02", 15, 72))) |>
    left_join(
        bind_rows(
            county_missing_outcome, county_no_missing, county_missing_predictor,
            county_boundary_issues
        ),
        by = "full_fips"
    )

states_in_sample <- state_shapefiles |> filter(!(state %in% c("02", 15, 72)))

prcnt_counties <-
    paste0(
        signif(
            nrow(county_no_missing) /
                (nrow(county_no_missing) +
                     nrow(county_missing_predictor) +
                     nrow(county_missing_outcome) +
                     nrow(county_boundary_issues)
                ),
            4
        ) * 100,
        "%"
    )

coverage_map <-
    ggplot(counties_in_sample) +
    geom_sf(aes(fill = sample), linewidth = 0.1) +
    geom_sf(data = states_in_sample, fill = NA, linewidth = 0.25) +
    labs(
        fill = "Sample",
        title = "Counties with at least one full year of coverage: 2010 - 2019",
        caption =
            paste0(
                "% of counties in study sample: ", prcnt_counties, "\n",
                "Blue counties are missing the outcome variable for all 10 years.\n",
                "Purple counties are missing at least one predictor for all 10 years but have at least 1 year for the outcome variable.\n",
                "Green counties have a complete set of variables for at least 1 year.\n",
                "Orange counties had to be dropped due to boundary changes.\n",
                "Alaska and Hawaii are dropped as they are missing the outcome variable"
            )
    ) +
    theme_void() +
    theme(plot.caption = element_text(size = 9))

ggsave(
    here("4_clean_missing_data", "graphs", "county_map.png"),
    coverage_map,
    bg = "white",
    height = 8,
    width = 10
)

################################################################################
# Graph county-year coverage.
################################################################################
county_year_coverage <-
    data_no_missing |>
    count(full_fips, name = "nr_years") |>
    count(nr_years) |>
    mutate(prop = n / sum(n))

prcnt_county_years <-
    paste0(signif(nrow(data_no_missing) / nrow(data_all), 4) * 100, "%")

graph_county_years <-
    county_year_coverage |>
    ggplot(aes(x = nr_years, y = prop)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    theme_bw() +
    labs(
        x = "Number of years reported",
        y = "Proportion of counties reporting \'X\' years",
        title = "Number of years reported for each county",
        caption =
            paste0("% of county-years in study sample: ", prcnt_county_years)
    ) +
    scale_x_continuous(breaks = seq(1, 10, 1)) +
    scale_y_continuous(breaks = seq(0, 0.4, 0.05))

ggsave(
    here("4_clean_missing_data", "graphs", "county_year_coverage.png"),
    graph_county_years,
    height = 8,
    width = 10
)
    
################################################################################
# Graph population coverage.
################################################################################
pop_data_total <-
    read_csv(here("2_clean_data", "output", "pep_county_clean.csv")) |>
    filter(year >= 2010, year <= 2019) |>
    summarise(total_pop = sum(pop_nr_est), .by = year)

pop_data_sample <-
    data_no_missing |>
    summarise(total_pop_sample = sum(pop_nr_est), .by = year)

pop_data_comparison <-
    full_join(pop_data_sample, pop_data_total, by = "year") |>
    mutate(prcnt_coverage = total_pop_sample / total_pop * 100)

overall_coverage <-
    pop_data_comparison |>
    summarise(
        total_pop_sample = sum(total_pop_sample), total_pop = sum(total_pop)
    ) |>
    mutate(prcnt_coverage = total_pop_sample / total_pop * 100)

yearly_pop_coverage <-
    ggplot(pop_data_comparison, aes(x = year, y = prcnt_coverage)) +
    geom_point() +
    geom_line(aes(group = 1)) +
    labs(
        x = "Year",
        y = "Percent Coverage",
        title = "% of Population Covered By Sample",
        caption = paste0("Overall coverage: ", round(overall_coverage$prcnt_coverage, 2), "%")
    ) +
    theme_bw() +
    scale_x_continuous(breaks = seq(2010, 2019, 1)) +
    scale_y_continuous(breaks = seq(70, 90, 5), limits = c(70, 90))

ggsave(
    here("4_clean_missing_data", "graphs", "yearly_coverage.png"),
    yearly_pop_coverage,
    height = 7,
    width = 7
)

################################################################################
# What variables are most missing?
################################################################################
missing <-
    data_all |>
    pivot_longer(
        cols = -matches("^state$|county|fips|^year$"),
        values_to = "value",
        names_to = "variable"
    ) |>
    group_by(variable) |>
    summarise(
        nr_missing = sum(is.na(value)), prcnt_missing = nr_missing / n()
    ) |>
    assign_var_category() |>
    std_var_names()

variable_missing_graph <-
    missing |>
    arrange(prcnt_missing) |>
    mutate(variable = factor(variable, variable)) |>
    filter(
        prcnt_missing >= 0.007,
        !(variable %in% c("% of children who do not live w/ parent", "Prison Admission Rate"))
    ) |>
    ggplot(aes(x = variable, y = prcnt_missing)) +
    geom_point() +
    theme_bw() +
    theme(axis.text.y = element_text(size = 8), strip.text = element_text(size = 9)) +
    labs(x = "Variable", y = "% Missing") +
    coord_flip() +
    facet_wrap(~var_category, scales = "free_y")

ggsave(
    here("4_clean_missing_data", "graphs", "variable_missing_graph.png"),
    variable_missing_graph,
    height = 12,
    width = 18
)

################################################################################
# Compare different samples' variable distributions.
################################################################################
generate_long <- function(df) {
    df |>
        pivot_longer(
            cols = -matches("^year$|state|county|fips"),
            values_to = "value",
            names_to = "variable"
        ) |>
        filter(!is.na(value))
}

generate_summary <- function(df) {
    df |>
        mutate(not_missing = !is.na(value) & !is.nan(value) & !is.infinite(value)) |>
        summarise(
            n = sum(not_missing),
            mean = mean(value),
            sd = sd(value),
            min = min(value),
            p10 = quantile(value, seq(0, 1, 0.1))[["10%"]],
            p25 = quantile(value, seq(0, 1, 0.25))[["25%"]],
            median = median(value),
            p75 = quantile(value, seq(0, 1, 0.25))[["75%"]],
            p90 = quantile(value, seq(0, 1, 0.1))[["90%"]],
            max = max(value),
            iqr = IQR(value),
            mad = mad(value),
            .by = c(variable)
        )
}

summary_no_missing <- generate_long(data_no_missing) |> generate_summary()
summary_missing_outcome <- generate_long(data_missing_outcome) |> generate_summary()
summary_missing_predictor <- generate_long(data_missing_predictors) |> generate_summary()

compare_no_outcome <-
    inner_join(
        summary_missing_outcome, summary_no_missing, by = "variable",
        suffix = c("_noOutcome", "_fullSample")
    ) |>
    mutate(std_diff_mean = (mean_noOutcome - mean_fullSample) / sqrt(((sd_noOutcome ^ 2 + sd_fullSample ^ 2) / 2))) |>
    arrange(std_diff_mean) |>
    assign_var_category() |>
    std_var_names() |>
    mutate(variable = factor(variable, variable))

compare_no_predictor <-
    inner_join(
        summary_missing_predictor, summary_no_missing, by = "variable",
        suffix = c("_noPredictor", "_fullSample")
    ) |>
    mutate(std_diff_mean = (mean_noPredictor - mean_fullSample) / sqrt(((sd_noPredictor ^ 2 + sd_fullSample ^ 2) / 2))) |>
    arrange(std_diff_mean) |>
    assign_var_category() |>
    std_var_names() |>
    mutate(variable = factor(variable, variable))

compare_no_outcome |>
    filter(std_diff_mean > 0.25 | std_diff_mean < -0.25) |>
    ggplot(aes(x = variable, y = std_diff_mean)) +
    geom_point() +
    theme_bw() +
    coord_flip() +
    geom_hline(yintercept = 0) +
    facet_wrap(~var_category, scales = "free_y")

compare_no_predictor |>
    filter(std_diff_mean > 0.25 | std_diff_mean < -0.25) |>
    ggplot(aes(x = variable, y = std_diff_mean)) +
    geom_point() +
    theme_bw() +
    coord_flip() +
    geom_hline(yintercept = 0) +
    facet_wrap(~var_category, scales = "free_y")
