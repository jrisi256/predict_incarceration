library(sf)
library(here)
library(tidyr)
library(dplyr)
library(readr)
library(broom)
library(ipumsr)
library(lavaan)
library(riclpmr)
library(ggplot2)
library(stringr)

################################################################################
# Read in data and prepare it for analysis.
data <- read_csv(here("4_clean_missing_data", "output", "data_no_missing.csv"))

data_wide <-
    data |>
    select(
        full_fips, year, total_prison_adm_rate15to64, poverty_prcnt_est_allAges,
        nr_homicides_max_per100k
    ) |>
    pivot_wider(
        id_cols = "full_fips",
        names_from = "year",
        values_from = matches("prison|poverty|homicides")
    ) |>
    filter(if_all(everything(), function(col) {!is.na(col)})) |>
    mutate(across(where(is.numeric), function(col) {scale(col)[,1]}))

county_shapefiles_2021 <-
    read_ipums_sf(
        here("1_get_data", "output", "ipums_shapefileCountyState2021.zip"),
        file_select = "nhgis0102_shape/nhgis0102_shapefile_tl2021_us_county_2021.zip"
    )

state_shapefiles_2021 <-
    read_ipums_sf(
        here("1_get_data", "output", "ipums_shapefileCountyState2021.zip"),
        file_select = "nhgis0102_shape/nhgis0102_shapefile_tl2021_us_state_2021.zip"
    ) |>
    filter(!(STATEFP %in% c("02", 15, 72)))

counties_in_sample <-
    county_shapefiles_2021 |>
    filter(!(STATEFP %in% c("02", 15, 72))) |>
    mutate(in_sample = GEOID %in% data_wide$full_fips)

coverage_all <-
    ggplot(counties_in_sample) +
    geom_sf(aes(fill = in_sample), linewidth = 0.1) +
    geom_sf(data = state_shapefiles_2021, fill = NA, linewidth = 0.25) +
    labs(
        fill = "County in sample",
        title = "County coverage: 2010 - 2019"
    ) +
    theme_void()

ggsave(
    here("4_clean_missing_data", "graphs", "county_coverage_wide.png"),
    coverage_all,
    bg = "white",
    height = 7,
    width = 7
)

a <- counties_in_sample |> filter(in_sample)
a2 <- data |> mutate(in_sample = full_fips %in% a$GEOID)
a3 <- a2 |> count(in_sample, wt = pop_nr_est)
a3 <- a3 |> mutate(prop = n / sum(n))

################################################################################
var_list <- list(
    x =
        c(
            "total_prison_adm_rate15to64_2010", "total_prison_adm_rate15to64_2011",
            "total_prison_adm_rate15to64_2012", "total_prison_adm_rate15to64_2013",
            "total_prison_adm_rate15to64_2014", "total_prison_adm_rate15to64_2015",
            "total_prison_adm_rate15to64_2016", "total_prison_adm_rate15to64_2017",
            "total_prison_adm_rate15to64_2018", "total_prison_adm_rate15to64_2019"
        ),
    y =
        c(
            "poverty_prcnt_est_allAges_2010", "poverty_prcnt_est_allAges_2011",
            "poverty_prcnt_est_allAges_2012", "poverty_prcnt_est_allAges_2013",
            "poverty_prcnt_est_allAges_2014", "poverty_prcnt_est_allAges_2015",
            "poverty_prcnt_est_allAges_2016", "poverty_prcnt_est_allAges_2017",
            "poverty_prcnt_est_allAges_2018", "poverty_prcnt_est_allAges_2019" 
        ),
    z =
        c(
            "nr_homicides_max_per100k_2010", "nr_homicides_max_per100k_2011",
            "nr_homicides_max_per100k_2012", "nr_homicides_max_per100k_2013",
            "nr_homicides_max_per100k_2014", "nr_homicides_max_per100k_2015",
            "nr_homicides_max_per100k_2016", "nr_homicides_max_per100k_2017",
            "nr_homicides_max_per100k_2018", "nr_homicides_max_per100k_2019"  
        )
)

model_text <- riclpm_text(var_list)
fit <- lavriclpm(riclpmModel = model_text, data = data_wide)
lavaan::summary(fit)

# A. Extract and tidy the model output
model_results <- parameterEstimates(fit, standardized = T, ci = T)

results_df <-
    model_results |>
    filter(op == "~") |>
    distinct(label, .keep_all = T) |>
    mutate(
        lhs_equation =
            case_when(
                lhs == "lat_x2" ~ " -> Prison admissions (Time = T)",
                lhs == "lat_y2" ~ " -> Poverty (Time = T)",
                lhs == "lat_z2" ~ " -> Homicides (Time = T)"
            ),
        rhs_equation =
            case_when(
                rhs == "lat_x1" ~ "Prison admissions (Time = T-1)",
                rhs == "lat_y1" ~ "Poverty (Time = T-1)",
                rhs == "lat_z1" ~ "Homicides (Time = T-1)"
            ),
        equation = paste0(rhs_equation, lhs_equation)
    ) |>
    mutate(
        equation = factor(
            equation,
            levels = c(
                "Prison admissions (Time = T-1) -> Prison admissions (Time = T)",
                "Poverty (Time = T-1) -> Prison admissions (Time = T)",
                "Homicides (Time = T-1) -> Prison admissions (Time = T)",
                "Poverty (Time = T-1) -> Poverty (Time = T)",
                "Prison admissions (Time = T-1) -> Poverty (Time = T)",
                "Homicides (Time = T-1) -> Poverty (Time = T)",
                "Homicides (Time = T-1) -> Homicides (Time = T)",
                "Poverty (Time = T-1) -> Homicides (Time = T)",
                "Prison admissions (Time = T-1) -> Homicides (Time = T)"
            )
        )
    )

ggplot(results_df, aes(x = equation, y = est, color = lhs)) +
    geom_point(size = 3) +
    # Add confidence interval error bars
    geom_errorbar(aes(ymin = ci.lower, ymax = ci.upper, , color = lhs), width = 0.2) +
    # Draw a dashed line at zero: effects crossing this line are not significant
    geom_hline(yintercept = 0, linetype = "dashed", color = "black", alpha = 0.5) +
    labs(
        title = "Within-County Cross-Lagged Effects from RI-CLPM",
        subtitle = "Standardized Estimates and 95% Confidence Intervals",
        x = "Directional Path",
        y = "Standardized Effects"
    ) +
    coord_flip() + # Flip coordinates for easier reading of labels
    theme_bw() +
    theme(
        legend.position = "none",
        text = element_text(size = 20)
    )
