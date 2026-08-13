library(here)
library(readr)
library(dplyr)
library(purrr)

read_dir <- here("1_get_data", "output")
save_dir <- here("2_clean_data", "output")

# Read in data.
saipe_county <- read_csv(file.path(read_dir, "saipe_county.csv"))

# Variable names and labels.
var_labels <-
    c(
        hhIncome_median_est = "SAEMHI_PT",
        hhIncome_median_moe = "SAEMHI_MOE",
        poverty_nr_est_0to17 = "SAEPOV0_17_PT",
        poverty_nr_moe_0to17 = "SAEPOV0_17_MOE",
        poverty_nr_est_allAges = "SAEPOVALL_PT",
        poverty_nr_moe_allAges = "SAEPOVALL_MOE",
        poverty_prcnt_est_0to17 = "SAEPOVRT0_17_PT",
        poverty_prcnt_moe_0to17 = "SAEPOVRT0_17_MOE",
        poverty_prcnt_est_allAges = "SAEPOVRTALL_PT",
        poverty_prcnt_moe_allAges = "SAEPOVRTALL_MOE",
        poverty_nr_denom_0to17Ages = "SAEPOVU_0_17",
        poverty_nr_denom_allAges = "SAEPOVU_ALL"
    )

# Rename columns and keep only pertinent columns.
saipe_county_clean <-
    saipe_county |>
    rename(all_of(var_labels)) |>
    select(matches("prcnt_est|median_est|fips|year|state|county"))

# Save cleaned data.
write_csv(saipe_county_clean, file.path(save_dir, "saipe_county_clean.csv"))
