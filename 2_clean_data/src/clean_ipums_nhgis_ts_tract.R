library(sf)
library(sp)
library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ipumsr)
library(OasisR)
library(stringr)

# List all IPUMS time series data.
ipums_ts_filepaths <-
    list.files(
        here("1_get_data", "output"),
        pattern = "ipumsTS_tract",
        full.names = T
    )

ipums_ts_names <-
    list.files(here("1_get_data", "output"), pattern = "ipumsTS_tract") |>
    str_replace_all("ipumsTS_|.csv.zip", "")

# Read in the time series tables of interest.
ipums_data <-
    map(ipums_ts_filepaths, function(filepath) {read_ipums_agg(filepath)})

names(ipums_data) <- ipums_ts_names

# Read in census tract shape files.
tract_shapefiles <- readRDS(here("1_get_data", "output", "tract_shapefiles.rds"))

################################################################################
# Function for renaming columns in each of the IPUMS data sets.
rename_year <- function(col) {
    case_when(
        col %in% c("STATEFP", "COUNTYFP", "TRACTA") ~ "",
        str_detect(col, "2010") ~ "_2010",
        str_detect(col, "105") ~ "_2006-2010",
        str_detect(col, "115") ~ "_2007-2011",
        str_detect(col, "125") ~ "_2008-2012",
        str_detect(col, "135") ~ "_2009-2013",
        str_detect(col, "145") ~ "_2010-2014",
        str_detect(col, "155") ~ "_2011-2015",
        str_detect(col, "165") ~ "_2012-2016",
        str_detect(col, "175") ~ "_2013-2017",
        str_detect(col, "185") ~ "_2014-2018",
        str_detect(col, "195") ~ "_2015-2019",
        T ~ col
    )
}

rename_race_cols <- function(col) {
    col <- str_remove(col, "^B10")
    
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            col == "TRACTA" ~ "tract",
            str_detect(col, "[0-9]M") ~ col,
            str_detect(col, "^AA") ~ "whiteNh",
            str_detect(col, "^AB") ~ "blackNh",
            str_detect(col, "^AC") ~ "aianNh",
            str_detect(col, "^AD") ~ "asianNh",
            str_detect(col, "^AE") ~ "nhpiNh",
            str_detect(col, "^AF") ~ "otherNh",
            str_detect(col, "^AG") ~ "multiNh",
            str_detect(col, "^AH") ~ "whiteHisp",
            str_detect(col, "^AI") ~ "blackHisp",
            str_detect(col, "^AJ") ~ "aianHisp",
            str_detect(col, "^AK") ~ "asianHisp",
            str_detect(col, "^AL") ~ "nhpiHisp",
            str_detect(col, "^AM") ~ "otherHisp",
            str_detect(col, "^AN") ~ "multiHisp",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_edu_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            col == "TRACTA" ~ "tract",
            str_detect(col, "[0-9]M") ~ col,
            str_detect(col, "[0-9]AA") ~ "lessThan9th",
            str_detect(col, "[0-9]AB") ~ "lessThan12th",
            str_detect(col, "[0-9]AC") ~ "hs",
            str_detect(col, "[0-9]AD") ~ "someCollege",
            str_detect(col, "[0-9]AE") ~ "college",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_income_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            col == "TRACTA" ~ "tract",
            str_detect(col, "[0-9]M") ~ col,
            str_detect(col, "[0-9]AA") ~ "below10",
            str_detect(col, "[0-9]AB") ~ "from10to15",
            str_detect(col, "[0-9]AC") ~ "from15to20",
            str_detect(col, "[0-9]AD") ~ "from20to25",
            str_detect(col, "[0-9]AE") ~ "from25to30",
            str_detect(col, "[0-9]AF") ~ "from30to35",
            str_detect(col, "[0-9]AG") ~ "from35to40",
            str_detect(col, "[0-9]AH") ~ "from40to45",
            str_detect(col, "[0-9]AI") ~ "from45to50",
            str_detect(col, "[0-9]AJ") ~ "from50to60",
            str_detect(col, "[0-9]AK") ~ "from60to75",
            str_detect(col, "[0-9]AL") ~ "from75to100",
            str_detect(col, "[0-9]AM") ~ "from100to125",
            str_detect(col, "[0-9]AN") ~ "from125to150",
            str_detect(col, "[0-9]AO") ~ "above150",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

################################################################################
# Clean FIPS codes. Census tracts changed over time, but we can fix them.
fix_tract_fips <- function(df) {
    df |>
        filter(
            tract_fips != "02270000100" | !(year %in% c("2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "02158000100" | !(year %in% c("2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014")),
            tract_fips != "04019002701" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019002704" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "04019002903" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019002906" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "04019410501" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019004118" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "04019410502" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019004121" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "04019410503" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019004125" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "04019470400" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019005200" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "04019470500" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "04019005300" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "06037137000" | !(year %in% c("2006-2010", "2007-2011")),
            tract_fips != "06037930401" | !(year %in% c("2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053940101" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030101" | year != "2006-2010",
            tract_fips != "36053940102" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030102" | year != "2006-2010",
            tract_fips != "36053940103" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030103" | year != "2006-2010",
            tract_fips != "36053940200" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030200" | year != "2006-2010",
            tract_fips != "36053940300" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030300" | year != "2006-2010",
            tract_fips != "36053940401" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030401" | year != "2006-2010",
            tract_fips != "36053940403" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030403" | year != "2006-2010",
            tract_fips != "36053940600" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030600" | year != "2006-2010",
            tract_fips != "36053940700" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36053030402" | year != "2006-2010",
            tract_fips != "36065940000" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36065024800" | year != "2006-2010",
            tract_fips != "36065940100" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36065024700" | year != "2006-2010",
            tract_fips != "36065940200" | !(year %in% c("2007-2011", "2008-2012", "2009-2013", "2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "36065024900" | year != "2006-2010",
            tract_fips != "46113940500" | !(year %in% c("2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "46102940500" | !(year %in% c("2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014")),
            tract_fips != "46113940800" | !(year %in% c("2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "46102940800" | !(year %in% c("2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014")),
            tract_fips != "46113940900" | !(year %in% c("2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019")),
            tract_fips != "46102940900" | !(year %in% c("2006-2010", "2007-2011", "2008-2012", "2009-2013", "2010-2014")),
            tract_fips != "51019050100" | !(year %in% c("2006-2010", "2007-2011", "2008-2012", "2009-2013")),
            tract_fips != "51515050100" | !(year %in% c("2010-2014", "2011-2015", "2012-2016", "2013-2017", "2014-2018", "2015-2019"))
        ) |>
        mutate(
            county =
                case_when(
                    tract_fips == "02270000100" ~ "158",
                    tract_fips %in% c("46113940500", "46113940800", "46113940900") ~ "102",
                    tract_fips == "51515050100" ~ "019",
                    T ~ county
                ),
            tract =
                case_when(
                    tract_fips == "04019002701" ~ "002704",
                    tract_fips == "04019002903" ~ "002906",
                    tract_fips == "04019410501" ~ "004118",
                    tract_fips == "04019410502" ~ "004121",
                    tract_fips == "04019410503" ~ "004125",
                    tract_fips == "04019470400" ~ "005200",
                    tract_fips == "04019470500" ~ "005300",
                    tract_fips == "36053940101" ~ "030101",
                    tract_fips == "36053940102" ~ "030102",
                    tract_fips == "36053940103" ~ "030103",
                    tract_fips == "36053940200" ~ "030200",
                    tract_fips == "36053940300" ~ "030300",
                    tract_fips == "36053940401" ~ "030401",
                    tract_fips == "36053940403" ~ "030403",
                    tract_fips == "36053940600" ~ "030600",
                    tract_fips == "36053940700" ~ "030402",
                    tract_fips == "36065940000" ~ "024800",
                    tract_fips == "36065940100" ~ "024700",
                    tract_fips == "36065940200" ~ "024900",
                    T ~ tract
                ),
            tract_fips =
                case_when(
                    tract_fips == "02270000100" ~ "02158000100",
                    tract_fips == "04019002701" ~ "04019002704",
                    tract_fips == "04019002903" ~ "04019002906",
                    tract_fips == "04019410501" ~ "04019004118",
                    tract_fips == "04019410502" ~ "04019004121",
                    tract_fips == "04019410503" ~ "04019004125",
                    tract_fips == "04019470400" ~ "04019005200",
                    tract_fips == "04019470500" ~ "04019005300",
                    tract_fips == "06037800204" & year %in% c("2006-2010", "2007-2011") ~ "06037800204_10-11",
                    tract_fips == "36053940101" ~ "36053030101",
                    tract_fips == "36053940102" ~ "36053030102",
                    tract_fips == "36053940103" ~ "36053030103",
                    tract_fips == "36053940200" ~ "36053030200",
                    tract_fips == "36053940300" ~ "36053030300",
                    tract_fips == "36053940401" ~ "36053030401",
                    tract_fips == "36053940403" ~ "36053030403",
                    tract_fips == "36053940600" ~ "36053030600",
                    tract_fips == "36053940700" ~ "36053030402",
                    tract_fips == "36065940000" ~ "36065024800",
                    tract_fips == "36065940100" ~ "36065024700",
                    tract_fips == "36065940200" ~ "36065024900",
                    tract_fips == "36065023000" & year %in% c("2006-2010") ~ "36065023000_10",
                    tract_fips == "46113940500" ~ "46102940500",
                    tract_fips == "46113940800" ~ "46102940800",
                    tract_fips == "46113940900" ~ "46102940900",
                    tract_fips == "51515050100" ~ "51019050100",
                    T ~ tract_fips
                )
        )
}

################################################################################
# Function for pivoting the data from wide to long back to wide again.
pivot_ipums <- function(df, pivot_cols_str) {
    df |>
        mutate(tract_fips = paste0(state, county, tract)) |>
        pivot_longer(
            cols = matches(pivot_cols_str),
            names_to = "variable",
            values_to = "value"
        ) |>
        separate_wider_delim(variable, delim = "_", names = c("subject", "year")) |>
        pivot_wider(
            id_cols = matches("fips|^state$|^county$|year|^tract"),
            names_from = subject,
            values_from = value
        )
}

################################################################################
# Calculate dissimilarity index.
# Order of groups does not matter.
calc_dissimilarity_index <- function(df, g1, g2, col_name) {
    pop_totals <-
        df |>
        summarise(
            sum_g1 = sum(.data[[g1]]),
            sum_g2 = sum(.data[[g2]]),
            .by = c(state, county, year)
        )
    
    df |>
        full_join(pop_totals, by = c("state", "county", "year")) |>
        mutate(
            prop_g1 = .data[[g1]] / sum_g1,
            prop_g2 = .data[[g2]] / sum_g2,
            diff_prop = abs(prop_g1 - prop_g2)
        ) |>
        summarise(
            "{col_name}" := 0.5 * sum(diff_prop),
            .by = c("state", "county", "year")
        )
}

# Order of groups matters when you have more than 2 groups in unit of analysis,
# AND you care about the non-normalized index. Normalized index is symmetric.
calc_norm_exposure_index <- function(df, g1, g2, col_name) {
    pop_totals <-
        df |>
        summarise(
            sum_total = sum(total),
            sum_g1 = sum(.data[[g1]]),
            sum_g2 = sum(.data[[g2]]),
            .by = c(state, county, year)
        ) |>
        mutate(prop_g2_county = sum_g2 / sum_total)
    
    df |>
        full_join(pop_totals, by = c("state", "county", "year")) |>
        mutate(
            prop_g1 = .data[[g1]] / sum_g1,
            prop_g2 = .data[[g2]] / sum_total,
            product = prop_g1 * prop_g2
        ) |>
        summarise(
            exposure_index = sum(product),
            prop_g2_county = unique(prop_g2_county),
            .by = c(state, county, year)
        ) |>
        mutate("{col_name}" := (prop_g2_county - exposure_index) / prop_g2_county)
}

calc_multigroup_entropy <- function(df, target_cols, col_name) {
    # Calculate the entropy or diversity for the total county.
    diversity_county <-
        df |>
        select(matches("state|county|tract|year"), all_of(target_cols)) |>
        summarise(
            across(all_of(target_cols), function(col) {sum(col)}),
            .by = c("state", "county", "year")
        ) |>
        pivot_longer(
            cols = all_of(target_cols), names_to = "group", values_to = "n"
        ) |>
        mutate(prcnt = n / sum(n), .by = c("state", "county", "year")) |>
        mutate(information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)) |>
        summarise(
            diversity_county = sum(prcnt * information),
            county_pop = sum(n),
            .by = c("state", "county", "year")
        )
    
    # Calculate the entropy or diversity for each census tract.
    diversity_tract <-
        df |>
        select(matches("state|county|tract|year"), all_of(target_cols)) |>
        summarise(
            across(all_of(target_cols), function(col) {sum(col)}),
            .by = c("state", "county", "year", "tract", "tract_fips")
        ) |>
        pivot_longer(
            cols = all_of(target_cols), names_to = "group", values_to = "n"
        ) |>
        mutate(
            tract_pop = sum(n),
            prcnt = if_else(tract_pop == 0, 0, n / tract_pop), # check if tract has no population
            .by = c("state", "county", "year", "tract", "tract_fips")
        ) |>
        mutate(information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)) |>
        summarise(
            diversity_tract = sum(prcnt * information),
            tract_pop = sum(n),
            .by = c("state", "county", "year", "tract", "tract_fips")
        ) |>
        full_join(diversity_county, by = c("state", "county", "year")) |>
        # Calculate weighted-average deviation from county-level entropy.
        mutate(
            deviation_entropy_tract =
                (tract_pop * (diversity_county - diversity_tract)) / (diversity_county * county_pop)
        ) |>
        # Sum weighted-average deviations from county-level entropy.
        summarise(
            "{col_name}" := sum(deviation_entropy_tract),
            .by = c("state", "county", "year")
        )
}

################################################################################
# Clean/restructure IPUMS data (education).
ipums_education <-
    ipums_data$tract_edu |>
    rename_with(rename_edu_cols) |>
    pivot_ipums("9th|12th|hs_|college") |>
    mutate(
        lessThanHs = lessThan9th + lessThan12th,
        noCollegeDegree = lessThanHs + hs + someCollege,
        total = rowSums(pick(c(lessThanHs, hs, someCollege, college)))
    ) |>
    select(-lessThan9th, -lessThan12th) |>
    fix_tract_fips() |>
    # No Puerto Rico or tract-years w/ nobody living there.
    filter(state != "72", total != 0) |>
    # Drop counties w/ only 1 census tract. Segregation measures meaningless.
    filter(n() > 1, .by = c("state", "county", "year")) |>
    left_join(
        tract_shapefiles, by = c("state", "county", "tract", "tract_fips")
    )

dissimilarity_low_high_edu <- calc_dissimilarity_index(ipums_education, "noCollegeDegree", "college", "dissimilarity_index_edu")
entropy_edu <- calc_multigroup_entropy(ipums_education, c("hs", "lessThanHs", "someCollege", "college"), "multigroup_entropy_edu")
exposure_low_high_edu <- calc_norm_exposure_index(ipums_education, "noCollegeDegree", "college", "norm_exposure_index_edu")
delta_low_edu <- calc_dissimilarity_index(ipums_education, "noCollegeDegree", "area_meters", "delta_index_edu")

################################################################################
# Clean/restructure IPUMS data household income categories.
ipums_hh_income <-
    ipums_data$tract_hh_income |>
    rename_with(rename_income_cols) |>
    select(matches("^state$|^county$|^tract$|from|above|below")) |>
    pivot_ipums("from|above|below") |>
    mutate(
        below30 = below10 + from10to15 + from20to25 + from25to30,
        from30to60 = from30to35 + from35to40 + from40to45 + from45to50 + from50to60,
        from60to100 = from60to75 + from75to100,
        from100to150 = from100to125 + from125to150,
        belowMedianRough = below30 + from30to60,
        aboveMedianRough = from60to100 + from100to150 + above150,
        total = rowSums(pick(matches("below30|from30to60|from60to100|from100to150|above150")))
    ) |>
    select(
        state, county, tract, tract_fips, year, below30, from30to60,
        from60to100, from100to150, above150, total, matches("Median")
    ) |>
    fix_tract_fips() |>
    # No Puerto Rico or tract-years w/ nobody living there.
    filter(state != "72", total != 0) |>
    # Drop counties w/ only 1 census tract. Segregation measures meaningless.
    filter(n() > 1, .by = c("state", "county", "year")) |>
    left_join(
        tract_shapefiles, by = c("state", "county", "tract", "tract_fips")
    )

dissimilarity_low_high_income <-
    calc_dissimilarity_index(
        ipums_hh_income,
        "belowMedianRough",
        "aboveMedianRough",
        "dissimilarity_index_hhIncome"
    )

entropy_income <-
    calc_multigroup_entropy(
        ipums_hh_income,
        c("below30", "from30to60", "from60to100", "from100to150", "above150"),
        "multigroup_entropy_hhIncome"
    )

exposure_low_high_income <-
    calc_norm_exposure_index(
        ipums_hh_income,
        "belowMedianRough",
        "aboveMedianRough",
        "norm_exposure_index_hhIncome"
    )

delta_low_income <- calc_dissimilarity_index(ipums_hh_income, "belowMedianRough", "area_meters", "delta_index_income")

################################################################################
# Clean/restructure IPUMS data (race).
ipums_race <-
    ipums_data$tract_race_and_ethnicity |>
    rename_with(rename_race_cols) |>
    select(
        matches("^state$|^county$|^tract$|white|black|aian|asian|nhpi|other|multi")
    )|>
    pivot_ipums("white|black|aian|asian|nhpi|other|multi") |>
    mutate(
        hispanic = whiteHisp + blackHisp + aianHisp + asianHisp + nhpiHisp +
            otherHisp + multiHisp,
        total = rowSums(pick(matches("white|black|aian|asian|nhpi|other|multi|hispanic")))
    ) |>
    select(-matches("Hisp$")) |>
    fix_tract_fips() |>
    # No Puerto Rico or tract-years w/ nobody living there.
    filter(state != "72", total != 0) |>
    # Drop counties w/ only 1 census tract. Segregation measures meaningless.
    filter(n() > 1, .by = c("state", "county", "year")) |>
    left_join(
        tract_shapefiles, by = c("state", "county", "tract", "tract_fips")
    )

dissimilarity_white_black <-
    calc_dissimilarity_index(
        ipums_race, "whiteNh", "blackNh", "dissimilarity_index_WhiteBlack"
    )

dissimilarity_white_hisp <-
    calc_dissimilarity_index(
        ipums_race, "whiteNh", "hispanic", "dissimilarity_index_WhiteHisp"
    )

entropy_race <-
    calc_multigroup_entropy(
        ipums_race,
        c("whiteNh", "blackNh", "aianNh", "asianNh", "nhpiNh", "otherNh", "multiNh", "hispanic"),
        "multigroup_entropy_race"
    )

exposure_white_black <-
    calc_norm_exposure_index(
        ipums_race, "whiteNh", "blackNh", "norm_exposure_index_WhiteBlack"
    )

exposure_white_hisp <-
    calc_norm_exposure_index(
        ipums_race, "whiteNh", "hispanic", "norm_exposure_index_WhiteHisp"
    )

delta_index_white <- calc_dissimilarity_index(ipums_race, "whiteNh", "area_meters", "delta_index_white")
delta_index_black <- calc_dissimilarity_index(ipums_race, "blackNh", "area_meters", "delta_index_black")
delta_index_hisp <- calc_dissimilarity_index(ipums_race, "hispanic", "area_meters", "delta_index_hisp")

################################################################################
# Calculate Spatial Proximity Index (need OasisR).
################################################################################
calc_sp_proximity <- function(df, cols, beta, diagval) {
    matrix <- as.matrix(df[, cols] |> st_drop_geometry())
    SP(matrix, spatobj = df, beta = beta, diagval = diagval)
}

################################################################################
# Education.
education_split <-
    ipums_education |>
    group_by(state, county, year) |>
    nest()

education_split_b001 <-
    education_split |>
    mutate(
        data = map(data, function(df) {st_as_sf(df)}),
        spatial_proximity_edu_b001 =
            map(
                data,
                calc_sp_proximity,
                cols = c("lessThanHs", "hs", "someCollege", "college"),
                beta = 0.001,
                diagval = "a"
            ) |>
            unlist()
    )

education_split_b01 <-
    education_split |>
    mutate(
        data = map(data, function(df) {st_as_sf(df)}),
        spatial_proximity_edu_b01 =
            map(
                data,
                calc_sp_proximity,
                cols = c("lessThanHs", "hs", "someCollege", "college"),
                beta = 0.01,
                diagval = "a"
            ) |>
            unlist()
    )

################################################################################
# Income.
income_split <-
    ipums_hh_income |>
    group_by(state, county, year) |>
    nest()

income_split_b001 <-
    income_split |>
    mutate(
        data = map(data, function(df) {st_as_sf(df)}),
        spatial_proximity_income_b001 =
            map(
                data,
                calc_sp_proximity,
                cols = c("below30", "from30to60", "from60to100", "from100to150", "above150"),
                beta = 0.001,
                diagval = "a"
            ) |>
            unlist()
    )

income_split_b01 <-
    income_split |>
    mutate(
        data = map(data, function(df) {st_as_sf(df)}),
        spatial_proximity_income_b01 =
            map(
                data,
                calc_sp_proximity,
                cols = c("below30", "from30to60", "from60to100", "from100to150", "above150"),
                beta = 0.01,
                diagval = "a"
            ) |>
            unlist()
    )

################################################################################
# Race/ethnicity.
race_split <-
    ipums_race |>
    group_by(state, county, year) |>
    nest()

race_split_b001 <-
    race_split |>
    mutate(
        data = map(data, function(df) {st_as_sf(df)}),
        spatial_proximity_race_b001 =
            map(
                data,
                calc_sp_proximity,
                cols = c("whiteNh", "blackNh", "aianNh", "asianNh", "nhpiNh", "otherNh", "multiNh", "hispanic"),
                beta = 0.001,
                diagval = "a"
            ) |>
            unlist()
    )

race_split_b01 <-
    race_split |>
    mutate(
        data = map(data, function(df) {st_as_sf(df)}),
        spatial_proximity_race_b01 =
            map(
                data,
                calc_sp_proximity,
                cols = c("whiteNh", "blackNh", "aianNh", "asianNh", "nhpiNh", "otherNh", "multiNh", "hispanic"),
                beta = 0.01,
                diagval = "a"
            ) |>
            unlist()
    )

################################################################################
# Merge together all the segregation indices.
################################################################################
segregation_df <-
    list(
        delta_index_black, delta_index_hisp, delta_index_white, delta_low_edu,
        delta_low_income, dissimilarity_low_high_edu, dissimilarity_low_high_income,
        dissimilarity_white_hisp, dissimilarity_white_black, entropy_edu,
        entropy_income, entropy_race,
        education_split_b001 |> select(-data),
        education_split_b01 |> select(-data),
        income_split_b01 |> select(-data), income_split_b001 |> select(-data),
        race_split_b001 |> select(-data), race_split_b01 |> select(-data),
        exposure_low_high_edu |> select(-exposure_index, -prop_g2_county),
        exposure_low_high_income |> select(-exposure_index, -prop_g2_county),
        exposure_white_black |> select(-exposure_index, -prop_g2_county),
        exposure_white_hisp |> select(-exposure_index, -prop_g2_county)
    ) |>
    map(
        function(df) {
            df |>
                mutate(year = if_else(year == 2010, year, str_sub(year, 6, 10)))
        }
    ) |>
    reduce(
        function(x, y) {full_join(x, y, by = c("state", "county", "year"))}
    ) |>
    mutate(full_fips = paste0(state, county))

write_csv(
    segregation_df,
    here("2_clean_data", "output", "segregation_county_clean.csv")
)
