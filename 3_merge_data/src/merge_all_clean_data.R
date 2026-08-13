library(sf)
library(here)
library(dplyr)
library(purrr)
library(readr)
library(R.utils)

################################################################################
# Directories.
read_dir <- here("2_clean_data", "output")

################################################################################
# Read in data.
# 02270 was renamed to 02158 in 2015.
# 46113 was renamed to 46102 in 2015.
acs <-
    read_csv(file.path(read_dir, "acs_county_clean.csv.gz")) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

algara_vote <-
    read_csv(file.path(read_dir, "algara_voting_county_clean.csv")) |>
    select(-matches("_state"))

bfs <-
    read_csv(file.path(read_dir, "bfs_county_clean.csv")) |>
    filter(
        full_fips != "02270" | !(year %in% 2016:2024),
        full_fips != "02158" | !(year %in% 2005:2015),
        full_fips != "46113" | !(year %in% 2016:2024),
        full_fips != "46102" | !(year %in% 2005:2015)
    ) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

cbp <-
    read_csv(file.path(read_dir, "cbp_county_clean.csv")) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

cdc_homicides <-
    read_csv(file.path(read_dir, "cdc_county_homicides_clean.csv")) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

ipumsTs <-
    read_csv(file.path(read_dir, "ipumsTS_county_clean.csv")) |>
    filter(
        full_fips != "02270" | !(year %in% 2015:2023),
        full_fips != "02158" | !(year %in% 1980:2014),
        full_fips != "46113" | !(year %in% 2015:2023),
        full_fips != "46102" | !(year %in% 1980:2014)
    ) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

laus <- read_csv(file.path(read_dir, "laus_county_clean.csv"))

pep <-
    read_csv(file.path(read_dir, "pep_county_clean.csv")) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

saipe <-
    read_csv(file.path(read_dir, "saipe_county_clean.csv")) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

shor_state_ideology <-
    read_csv(file.path(read_dir, "shor_stateIdeology_clean.csv")) |>
    select(-h_diffs, -s_diffs)

ucr <-
    read_csv(file.path(read_dir, "ucr_county_clean.csv")) |>
    mutate(
        full_fips =
            case_when(
                full_fips == "02270" ~ "02158",
                full_fips == "46113" ~ "46102",
                T ~ full_fips
            ),
        county =
            case_when(
                state == "02" & county == "270" ~"158",
                state == "46" & county == "113" ~ "102",
                T ~ county
            )
    )

vera <-
    read_csv(file.path(read_dir, "vera_county_clean.csv")) |>
    select(matches("year|full_fips|state|county|total_prison.*rate"))

county_shapefiles <- readRDS(here("1_get_data", "output", "county_shapefiles.rds"))
segregation <- read_csv(file.path(read_dir, "segregation_county_clean.csv"))

################################################################################
# Drop any columns which need to be merged across different data sources.
ipumsTs_no_merge <-
    ipumsTs |>
    select(
        -matches("pop_prcnt|(white|black|hispanic)_prcnt|shannon_index_scaled$|gini_simpson_index$|belowPoverty_prcnt|median|renter|single|pop_nr_est|ur_prcnt")
    )

acs_no_merge <-
    acs |>
    select(-matches("renters_(prcnt|diff)|^single.*est$"))

################################################################################
# Calculate population density.
pep <-
    pep |>
    full_join(
        county_shapefiles |> st_drop_geometry(),
        by = c("state", "county", "full_fips")
    ) |>
    mutate(pop_density = pop_nr_est / aland_miles) |>
    select(-aland_miles)

################################################################################
# Merge PEP and IPUMS Time Series data.
ipumsTs_race_age <-
    ipumsTs |>
    select(matches("year|fips|state|county|(white|black|hispanic|pop)_prcnt|index|pop_nr")) |>
    filter(year %in% c(1980, 1990)) |>
    rename_with(
        .fn = function(col) {paste0("ipumsTs_", col)},
        .cols = matches("(white|black|hispanic|pop)_prcnt|index|pop_nr")
    )
    
merge_ipumsTs_pep <-
    full_join(ipumsTs_race_age, pep, by = c("year", "full_fips", "state", "county")) |>
    mutate(
        shannon_index_scaled =
            if_else(
                year %in% c(1980, 1990),
                ipumsTs_shannon_index_scaled,
                shannon_index_scaled
            ),
        gini_simpson_index =
            if_else(
                year %in% c(1980, 1990),
                ipumsTs_gini_simpson_index,
                gini_simpson_index
            ),
        pop_prcnt_est_allAges_w =
            if_else(
                year %in% c(1980, 1990),
                ipumsTs_white_prcnt_est_allAges,
                pop_prcnt_est_allAges_w
            ),
        pop_prcnt_est_allAges_b =
            if_else(
                year %in% c(1980, 1990),
                ipumsTs_black_prcnt_est_allAges,
                pop_prcnt_est_allAges_b
            ),
        pop_prcnt_est_allAges_h =
            if_else(
                year %in% c(1980, 1990),
                ipumsTs_hispanic_prcnt_est_allAges,
                pop_prcnt_est_allAges_h
            ),
        pop_prcnt_est_15to24_allRaces_m =
            if_else(
                year == 1980,
                ipumsTs_pop_prcnt_est_15to24_allRaces_m,
                pop_prcnt_est_15to24_allRaces_m
            ),
        pop_nr_est = if_else(year == 1980, ipumsTs_pop_nr_est, pop_nr_est)
    ) |>
    select(-matches("shannon_index$|ipumsTs"))

################################################################################
# Merge LAUS and IPUMS time series data.
ipumsTs_ur <-
    ipumsTs |>
    select(matches("year|fips|state|county|ur_prcnt")) |>
    filter(year < 2010 | year > 2019)

merge_ipumsTs_laus <-
    full_join(ipumsTs_ur, laus, by = c("year", "full_fips", "state", "county")) |>
    mutate(
        ur_prcnt_est_16andOlder =
            if_else(
                year < 2010 | year > 2019,
                ur_prcnt_est_16andOlder.x,
                ur_prcnt_est_16andOlder.y
            )
    ) |>
    select(-matches("Older.(x|y)"))

################################################################################
# Merge SAIPE and IPUMS time series data.
ipumsTs_poverty <-
    ipumsTs |>
    select(matches("year|fips|state|county|belowPoverty_prcnt|median")) |>
    filter(year == 1980) |>
    rename_with(
        .fn = function(col) {paste0("ipumsTs_", col)}, .cols = matches("median")
    )

merge_ipumsTs_saipe <-
    full_join(ipumsTs_poverty, saipe, by = c("year", "full_fips", "state", "county")) |>
    mutate(
        poverty_prcnt_est_allAges =
            if_else(
                year == 1980,
                belowPoverty_prcnt_est_povertyUniverse,
                poverty_prcnt_est_allAges
            ),
        poverty_prcnt_est_0to17 =
            if_else(
                year == 1980,
                belowPoverty_prcnt_est_under18,
                poverty_prcnt_est_0to17
            ),
        hhIncome_median_est =
            if_else(
                year == 1980,
                ipumsTs_HhIncome_median_est,
                hhIncome_median_est
            )
    ) |>
    select(-matches("belowPoverty|ipumsTs"))

################################################################################
# Merge ACS and IPUMS time series data.
ipumsTs_renters_race <-
    ipumsTs |>
    select(matches('year|full_fips|state|county|renter')) |>
    filter(year %in% c(1980, 1990, 2010, 2020))

acs_renters_race <-
    acs |>
    select(matches("^year$|full_fips|^state$|^county$|renters_(prcnt|diff)")) |>
    filter(year %in% c(2009, 2011:2019, 2021:2023))

merge_ipumsTs_acs_renters_race <- bind_rows(ipumsTs_renters_race, acs_renters_race)

ipumsTs_paoc <-
    ipumsTs |>
    select(matches("year|full_fips|state|county|single")) |>
    filter(year %in% c(1980, 1990, 2000, 2010, 2020))

acs_paoc <-
    acs |>
    select(matches("^year$|full_fips|^state$|^county$|^single.*est$")) |>
    filter(year %in% c(2009, 2011:2019, 2021:2023))

merge_ipumsTs_acs_paoc <- bind_rows(ipumsTs_paoc, acs_paoc)

################################################################################
# Merge CBP and IPUMS time series/PEP population data.
merge_ipumsTs_cbp <-
    inner_join(
        cbp,
        select(merge_ipumsTs_pep, year, full_fips, state, county, pop_nr_est),
        by = c("year", "full_fips", "state", "county")
    ) |>
    mutate(
        nr_businesses_per10k = nr_businesses * 10000 / pop_nr_est,
        nr_socialAssociations_min_per10k = nr_social_associations_min * 10000 / pop_nr_est,
        nr_socialSupportServices_min_per10k = nr_social_support_services_min * 10000 / pop_nr_est,
        nr_allSocialBeautyBusinesses_min_per10k = nr_all_social_and_beauty_businesses_min * 10000 / pop_nr_est,
        nr_socialAssociations_max_per10k = nr_social_associations_max * 10000 / pop_nr_est,
        nr_socialSupportServices_max_per10k = nr_social_support_services_max * 10000 / pop_nr_est,
        nr_allSocialBeautyBusinesses_max_per10k = nr_all_social_and_beauty_businesses_max * 10000 / pop_nr_est
    ) |>
    select(matches("year|full_fips|state|county|_per10k"))

################################################################################
# Merge BFS and IPUMS time series/PEP population data.
merge_ipumsTs_bfs <-
    inner_join(
        bfs,
        select(merge_ipumsTs_pep, year, full_fips, state, county, pop_nr_est),
        by = c("year", "full_fips", "state", "county")
    ) |>
    mutate(
        nr_new_businesses_per10k = nr_new_businesses_formed * 10000 / pop_nr_est
    ) |>
    select(matches("year|full_fips|state|county|_per10k"))

################################################################################
# Merge UCR and IPUMS time series/PEP population data.
merge_ipumsTs_ucr <-
    inner_join(
        ucr,
        select(merge_ipumsTs_pep, year, full_fips, state, county, pop_nr_est),
        by = c("year", "full_fips", "state", "county")
    ) |>
    mutate(
        violent_crime_per100k = actual_index_violent_imputed * 100000 / pop_nr_est,
        property_crime_per100k = actual_index_property_imputed * 100000 / pop_nr_est,
        all_crimes_per100k = actual_all_crimes_imputed * 100000 / pop_nr_est
    ) |>
    select(matches("year|fips|state|county|100k"))

################################################################################
# Merge CDC homicide data and IPUMS time series/PEP population data.
# Interestingly, all the rows with missing population data also have missing
# homicide data. We can use the population data provided by CDC.
merge_ipumsTs_cdc_homicides <-
    inner_join(
        cdc_homicides,
        select(merge_ipumsTs_pep, year, full_fips, state, county, pop_nr_est),
        by = c("year", "full_fips", "state", "county")
    ) |>
    filter(Population != "Missing") |>
    mutate(
        Population = as.numeric(Population),
        nr_homicides_per100k = homicides_final * 100000 / Population,
        nr_homicides_max_per100k = homicides_max * 100000 / Population,
        nr_homicides_min_per100k = homicides_min * 100000 / Population,
        nr_homicides_3yr_avg_per100k = homicides_final_3yr_avg * 100000 / Population,
        nr_homicides_max_3yr_avg_per100k = homicides_max_3yr_avg * 100000 / Population,
        nr_homicides_min_3yr_avg_per100k = homicides_min_3yr_avg * 100000 / Population,
        nr_homicides_5yr_avg_per100k = homicides_final_5yr_avg * 100000 / Population,
        nr_homicides_max_5yr_avg_per100k = homicides_max_5yr_avg * 100000 / Population,
        nr_homicides_min_5yr_avg_per100k = homicides_min_5yr_avg * 100000 / Population
    ) |>
    select(matches("year|full_fips|state|county|_per100k"))

################################################################################
# Join together all data now with unique columns.
df_final <-
    list(
        acs_no_merge, ipumsTs_no_merge, merge_ipumsTs_acs_paoc,
        merge_ipumsTs_acs_renters_race, merge_ipumsTs_cbp, merge_ipumsTs_pep,
        merge_ipumsTs_saipe, vera, merge_ipumsTs_cdc_homicides, algara_vote,
        merge_ipumsTs_bfs, merge_ipumsTs_laus, merge_ipumsTs_ucr, segregation
    ) |>
    reduce(
        function(x, y) {
            full_join(x, y, by = c("year", "full_fips", "state", "county"))
        }
    ) |>
    full_join(shor_state_ideology, by = c("year", "state")) |>
    filter(year >= 2010 & year <= 2019) |>
    arrange(full_fips, year) |>
    # Drop columns which are collinear.
    select(
        -hhCostsLessThan10PrcntIncome_prcnt_est,
        -hhLessThan5PersonPerRoom_prcnt_est, -hhWith0Problem_prcnt_est,
        -hhAbove1000Value_prcnt_est, -hhShareOfIncome_5thQuintile_est,
        -hhOwns5orMoreVehicle_prcnt_est, -college_prcnt_est_25older,
        -hhIncomeabove150_prcnt_est,
        -ratioIncomeToPovertyAbove200_prcnt_est_povertyUniverse,
        -singleDad_prcnt_est, -nr_homicides_per100k,
        -nr_homicides_3yr_avg_per100k, -nr_homicides_5yr_avg_per100k
    )

################################################################################
# Drop unnecessary counties. Save results.
################################################################################
# State FIPS code 72 is Puerto Rico.
# 13041 and 13203 were added to 13121 in 1932.
# 46133 was annexed by surrounding counties in 1943.
# 46001 was annexed by 46041 in 1952.
# 51055 merged with 51650 in 1952.
# 51189 merged with another county to form 51700 in 1958.
# 51129 merged with 51785 to form 51550 in 1963.
# 51151 merged with another county to form 51810 in 1963.
# 32025 was merged with 32510 in 1969.
# 51123 was annexed by 51800 in 1972.
# 46131 was annexed by 46071 in 1979.
# 02140 was added to 02188 in 1986.
# 02010 was split into 02013 and 02016 in 1987.
# 02231 was split into 02232 and 02282 in 1992.
# 51780 was added to 51083 in June 1995.
# 12025 was renamed to 12086 in 1997.
# 30113 was annexed by 30031 and 30067 in 1997.
# 51560 was added to 51005 in 2001.
# 02232 was split into 02230 and 02105 in 2007.
# 02201 was split into 02130, 02275, and 02198 in 2008.
# 02280 was split into 02275 and 02195 in 2008.
# 51515 was annexed by 51019 in 2013.
# 02261 was split into 02063 and 02066 in 2019. New counties were used in 2020.
# 090110 - 09190 were created post-2019.
df_final_counties <-
    df_final |>
    filter(
        state != 72,
        !(
            full_fips %in%
                c(
                    "02010", "02140", "02201", "02231", "02232", "02280", "09110",
                    "09120", "09130", "09140", "09150", "09160", "09170", "09180",
                    "09190", "12025", "13041", "13203", "30113", "32025", "46001",
                    "46131", "46133", "51055", "51123", "51129", "51151", "51189",
                    "51560", "51780", "51785", "02063", "02066"
                )
        )
    )

write_csv(df_final_counties, here("3_merge_data", "output", "merged_data.csv"))
gzip(here("3_merge_data", "output", "merged_data.csv"), remove = T, overwrite = T)
