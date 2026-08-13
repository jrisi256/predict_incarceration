library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(readr)
library(ipumsr)
library(stringr)

# List all IPUMS time series data.
ipums_ts_filepaths <-
    list.files(
        here("1_get_data", "output"),
        pattern = "ipumsTS_(edu|family|hh|labor|marriage|median|occupancy|owner|poverty|race|sex)",
        full.names = T
    )

ipums_ts_names <-
    list.files(
        here("1_get_data", "output"),
        pattern = "ipumsTS_(edu|family|hh|labor|marriage|median|occupancy|owner|poverty|race|sex)"
    ) |>
    str_replace_all("ipumsTS_|.csv.zip", "")

# Read in the time series tables of interest.
ipums_data <-
    map(ipums_ts_filepaths, function(filepath) {read_ipums_agg(filepath)})
names(ipums_data) <- ipums_ts_names

################################################################################
# Function for renaming columns in each of the IPUMS data sets.
rename_year <- function(col) {
    case_when(
        col %in% c("STATEFP", "COUNTYFP") ~ "",
        str_detect(col, "1980") ~ "_1980",
        str_detect(col, "1990") ~ "_1990",
        str_detect(col, "2000") ~ "_2000",
        str_detect(col, "2010") ~ "_2010",
        str_detect(col, "2020") ~ "_2020",
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
        str_detect(col, "205") ~ "_2016-2020",
        str_detect(col, "215") ~ "_2017-2021",
        str_detect(col, "225") ~ "_2018-2022",
        str_detect(col, "235") ~ "_2019-2023",
        T ~ col
    )
}

rename_edu_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "lessThan9th",
            str_detect(col, "AB") ~ "lessThan12th",
            str_detect(col, "AC") ~ "hs",
            str_detect(col, "AD") ~ "someCollege",
            str_detect(col, "AE") ~ "college",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_paoc_cols <- function(col) {
    col <- str_remove(col, "^AG4")
    
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "AA") ~ "marriedChildren",
            str_detect(col, "AB") ~ "marriedChildrenUnder6",
            str_detect(col, "AC") ~ "marriedChildrenOver6",
            str_detect(col, "AD") ~ "marriedNoChildren",
            str_detect(col, "AE") ~ "malehhChildren",
            str_detect(col, "AF") ~ "malehhChildrenUnder6",
            str_detect(col, "AG") ~ "malehhChildrenOver6",
            str_detect(col, "AH") ~ "malehhNoChildren",
            str_detect(col, "AI") ~ "femalehhChildren",
            str_detect(col, "AJ") ~ "femalehhChildrenUnder6",
            str_detect(col, "AK") ~ "femalehhChildrenOver6",
            str_detect(col, "AL") ~ "femalehhNoChildren",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_labor_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "inLaborForce16andOlder",
            str_detect(col, "AB") ~ "armedForces16andOlder",
            str_detect(col, "AC") ~ "civilian16andOlder",
            str_detect(col, "AD") ~ "employed16andOlder",
            str_detect(col, "AE") ~ "unemployed16andOlder",
            str_detect(col, "AF") ~ "notInLaborForce16andOlder",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_marriage_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "maleNeverMarried15andOlder",
            str_detect(col, "AB") ~ "maleMarried15andOlder",
            str_detect(col, "AC") ~ "maleMarriedNotSeparated15andOlder",
            str_detect(col, "AD") ~ "maleMarriedSeparated15andOlder",
            str_detect(col, "AE") ~ "maleWidowed15andOlder",
            str_detect(col, "AF") ~ "maleDivorced15andOlder",
            str_detect(col, "AG") ~ "femaleNeverMarried15andOlder",
            str_detect(col, "AH") ~ "femaleMarried15andOlder",
            str_detect(col, "AI") ~ "femaleMarriedNotSeparated15andOlder",
            str_detect(col, "AJ") ~ "femaleMarriedSeparated15andOlder",
            str_detect(col, "AK") ~ "femaleWidowed15andOlder",
            str_detect(col, "AL") ~ "femaleDivorced15andOlder",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_medianHhIncome_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "medianHHIncome",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_occupancy_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "nrOccupiedHousingUnits",
            str_detect(col, "AB") ~ "nrVacantHousingUnits",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_owner_renter_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "nrPeopleInOwnedUnits",
            str_detect(col, "AB") ~ "nrPeopleInRentedUnits",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_poverty_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "ratioBelow75",
            str_detect(col, "AB") ~ "ratio75to99",
            str_detect(col, "AC") ~ "ratio100to124",
            str_detect(col, "AD") ~ "ratio125to149",
            str_detect(col, "AE") ~ "ratio150to174",
            str_detect(col, "AF") ~ "ratio175to199",
            str_detect(col, "AG") ~ "ratioAbove200",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_race_cols <- function(col) {
    col <- str_remove(col, "^AE7")
    
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "whiteNh",
            str_detect(col, "AB") ~ "blackNh",
            str_detect(col, "AC") ~ "aianNh",
            str_detect(col, "AD") ~ "asianAndPiNh",
            str_detect(col, "AE") ~ "otherNh",
            str_detect(col, "AG") ~ "whiteHisp",
            str_detect(col, "AH") ~ "blackHisp",
            str_detect(col, "AI") ~ "aianHisp",
            str_detect(col, "AJ") ~ "asianAndPiHisp",
            str_detect(col, "AK") ~ "otherHisp",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_sex_age_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "AA") ~ "maleUnder5",
            str_detect(col, "AB") ~ "male5to9",
            str_detect(col, "AC") ~ "male10to14",
            str_detect(col, "AD") ~ "male15to17",
            str_detect(col, "AE") ~ "male18to19",
            str_detect(col, "AF") ~ "male20",
            str_detect(col, "AG") ~ "male21",
            str_detect(col, "AH") ~ "male22to24",
            str_detect(col, "AI") ~ "male25to29",
            str_detect(col, "AJ") ~ "male30to34",
            str_detect(col, "AK") ~ "male35to44",
            str_detect(col, "AL") ~ "male45to54",
            str_detect(col, "AM") ~ "male55to59",
            str_detect(col, "AN") ~ "male60to61",
            str_detect(col, "AO") ~ "male62to64",
            str_detect(col, "AP") ~ "male65to74",
            str_detect(col, "AQ") ~ "male75to84",
            str_detect(col, "AR") ~ "maleAbove85",
            str_detect(col, "AS") ~ "femaleUnder5",
            str_detect(col, "AT[0-9]") ~ "female5to9",
            str_detect(col, "AU") ~ "female10to14",
            str_detect(col, "AV") ~ "female15to17",
            str_detect(col, "AW") ~ "female18to19",
            str_detect(col, "AX") ~ "female20",
            str_detect(col, "AY") ~ "female21",
            str_detect(col, "AZ") ~ "female22to24",
            str_detect(col, "BA") ~ "female25to29",
            str_detect(col, "BB") ~ "female30to34",
            str_detect(col, "BC") ~ "female35to44",
            str_detect(col, "BD") ~ "female45to54",
            str_detect(col, "BE") ~ "female55to59",
            str_detect(col, "BF") ~ "female60to61",
            str_detect(col, "BG") ~ "female62to64",
            str_detect(col, "BH") ~ "female65to74",
            str_detect(col, "BI") ~ "female75to84",
            str_detect(col, "BJ") ~ "femaleAbove85",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_owner_renter_race_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "AA") ~ "whiteNhHhOwner",
            str_detect(col, "AB") ~ "blackNhHhOwner",
            str_detect(col, "AC") ~ "aianNhHhOwner",
            str_detect(col, "AD") ~ "asianAndPiNhHhOwner",
            str_detect(col, "AE") ~ "otherNhHhOwner",
            str_detect(col, "AF") ~ "multiNhHhOwner",
            str_detect(col, "AG") ~ "whiteHispHhOwner",
            str_detect(col, "AH") ~ "blackHispHhOwner",
            str_detect(col, "AI") ~ "aianHispHhOwner",
            str_detect(col, "AJ") ~ "asianAndPiHispHhOwner",
            str_detect(col, "AK") ~ "otherHispHhOwner",
            str_detect(col, "AL") ~ "multiHispHhOwner",
            str_detect(col, "AM") ~ "whiteNhHhRenter",
            str_detect(col, "AN") ~ "blackNhHhRenter",
            str_detect(col, "AO") ~ "aianNhHhRenter",
            str_detect(col, "AP") ~ "asianAndPiNhHhRenter",
            str_detect(col, "AQ") ~ "otherNhHhRenter",
            str_detect(col, "AR") ~ "multiNhHhRenter",
            str_detect(col, "AS") ~ "whiteHispHhRenter",
            str_detect(col, "AT[0-9]") ~ "blackHispHhRenter",
            str_detect(col, "AU") ~ "aianHispHhRenter",
            str_detect(col, "AV") ~ "asianAndPiHispHhRenter",
            str_detect(col, "AW") ~ "otherHispHhRenter",
            str_detect(col, "AX") ~ "multiHispHhRenter",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_income_1980_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "below10",
            str_detect(col, "AB") ~ "from10to15",
            str_detect(col, "AC") ~ "from15to20",
            str_detect(col, "AD") ~ "from20to25",
            str_detect(col, "AE") ~ "from25to30",
            str_detect(col, "AF") ~ "from30to35",
            str_detect(col, "AG") ~ "from35to40",
            str_detect(col, "AH") ~ "from40to50",
            str_detect(col, "AI") ~ "from50to75",
            str_detect(col, "AJ") ~ "above75",
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

rename_labor_sex_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "M") ~ col,
            str_detect(col, "AA") ~ "maleInLaborForce16andOlder",
            str_detect(col, "AB") ~ "maleArmedForces16andOlder",
            str_detect(col, "AC") ~ "maleCivilian16andOlder",
            str_detect(col, "AD") ~ "maleEmployed16andOlder",
            str_detect(col, "AE") ~ "maleUnemployed16andOlder",
            str_detect(col, "AF") ~ "maleNotInLaborForce16andOlder",
            str_detect(col, "AG") ~ "femaleInLaborForce16andOlder",
            str_detect(col, "AH") ~ "femaleArmedForces16andOlder",
            str_detect(col, "AI") ~ "femaleCivilian16andOlder",
            str_detect(col, "AJ") ~ "femaleEmployed16andOlder",
            str_detect(col, "AK") ~ "femaleUnemployed16andOlder",
            str_detect(col, "AL") ~ "femaleNotInLaborForce16andOlder",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

rename_poverty_children_cols <- function(col) {
    subject <-
        case_when(
            col == "STATEFP" ~ "state",
            col == "COUNTYFP" ~ "county",
            str_detect(col, "AA") ~ "inPovertyUnder18",
            str_detect(col, "AB") ~ "inPoverty18to54",
            str_detect(col, "AC") ~ "inPoverty55to64",
            str_detect(col, "AD") ~ "inPoverty65to74",
            str_detect(col, "AE") ~ "inPovertyAbove75",
            str_detect(col, "AF") ~ "abovePovertyUnder18",
            str_detect(col, "AG") ~ "abovePoverty18to54",
            str_detect(col, "AH") ~ "abovePoverty55to64",
            str_detect(col, "AI") ~ "abovePoverty65to74",
            str_detect(col, "AJ") ~ "abovePovertyAbove75",
            T ~ col
        )
    
    year <- rename_year(col)
    return(paste0(subject, year))
}

################################################################################
# Function for pivoting the data from wide to long back to wide again.
pivot_ipums <- function(df, pivot_cols_str) {
    df |>
        mutate(full_fips = paste0(state, county)) |>
        pivot_longer(
            cols = matches(pivot_cols_str),
            names_to = "variable",
            values_to = "value"
        ) |>
        separate_wider_delim(variable, "_", names = c("subject", "year")) |>
        pivot_wider(
            id_cols = matches("fips|^state$|^county$|year"),
            names_from = subject,
            values_from = value
        )
}

################################################################################
# Clean/restructure IPUMS data (education).
education_1990 <-
    ipums_data$edu_1990 |>
    mutate(
        year = "1990",
        full_fips = paste0(STATEFP, COUNTYFP),
        total = B85AA1990 + B85AB1990 + B85AC1990 + B85AD1990 + B85AE1990 + B85AF1990 + B85AG1990,
        lessThanHs_nr_est_25older = B85AA1990 + B85AB1990,
        hs_nr_est_25older = B85AC1990,
        someCollege_nr_est_25older = B85AD1990 + B85AE1990,
        college_nr_est_25older = B85AF1990 + B85AG1990,
        lessThanHs_prcnt_est_25older = lessThanHs_nr_est_25older / total,
        hs_prcnt_est_25older = hs_nr_est_25older / total,
        someCollege_prcnt_est_25older = someCollege_nr_est_25older / total,
        college_prcnt_est_25older = college_nr_est_25older / total
    ) |>
    rename(state = STATEFP, county = COUNTYFP) |>
    select(matches("prcnt|^state$|^county$|year|_fips"), -STATE, -COUNTY) |>
    zap_ipums_attributes()

education <-
    ipums_data$edu |>
    rename_with(rename_edu_cols) |>
    pivot_ipums("9th|12th|hs_|college") |>
    mutate(
        total = lessThan9th + lessThan12th + hs + someCollege + college,
        lessThanHs_prcnt_est_25older = (lessThan9th + lessThan12th) / total,
        hs_prcnt_est_25older = hs / total,
        someCollege_prcnt_est_25older = someCollege / total,
        college_prcnt_est_25older = college / total
    ) |>
    select(matches("fips|year|state|county|prcnt_est")) |>
    bind_rows(education_1990)

diversity_education <-
    education |>
    pivot_longer(
        cols = matches("prcnt_est"), names_to = "variable", values_to = "prcnt"
    ) |>
    group_by(full_fips, county, state, year) |>
    mutate(n = n(), information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)) |>
    summarise(
        shannon_index_edu = sum(prcnt * information),
        gini_simpson_index_edu = 1 - sum(prcnt ^ 2)
    ) |>
    mutate(shannon_index_scaled_edu = shannon_index_edu / log(4, 2)) |>
    ungroup() |>
    select(-shannon_index_edu)

ipums_education <-
    education |>
    full_join(
        diversity_education, by = c("full_fips", "state", "county", "year")
    )

################################################################################
# Clean/restructure IPUMS data (presence of own children + family type).
ipums_family_paoc <-
    ipums_data$family_paoc |>
    rename_with(rename_paoc_cols) |>
    pivot_ipums("married|malehh|femalehh") |>
    mutate(singleParenthhChildren = malehhChildren + femalehhChildren) |>
    select(matches("state|county|fips|year|hhChildren$")) |>
    zap_ipums_attributes()

################################################################################
# Clean/restructure IPUMS data (employment).
ipums_labor <-
    ipums_data$labor |>
    rename_with(rename_labor_cols) |>
    pivot_ipums("16andOlder") |>
    mutate(
        civilian_noninst_pop = civilian16andOlder + notInLaborForce16andOlder,
        lfpr_prcnt_est_16andOlder = civilian16andOlder / civilian_noninst_pop,
        epr_prcnt_est_16andOlder = employed16andOlder / civilian_noninst_pop,
        ur_prcnt_est_16andOlder = unemployed16andOlder / civilian16andOlder
    ) |>
    select(matches("year|fips|state|county|prcnt"))

################################################################################
# Clean/restructure IPUMS data (marriage).
ipums_marriage <-
    ipums_data$marriage |>
    rename_with(rename_marriage_cols) |>
    pivot_ipums("15andOlder") |>
    mutate(
        total_pop_15andOlder = 
            maleNeverMarried15andOlder + maleMarried15andOlder +
            maleWidowed15andOlder + maleDivorced15andOlder +
            femaleNeverMarried15andOlder + femaleMarried15andOlder +
            femaleWidowed15andOlder + femaleDivorced15andOlder,
        marriageNotSeparated_prcnt_est_15andOlder =
            (maleMarriedNotSeparated15andOlder + femaleMarriedNotSeparated15andOlder) / total_pop_15andOlder,
        everMarried_prcnt_est_15andOlder =
            (
                maleMarried15andOlder + maleWidowed15andOlder + maleDivorced15andOlder +
                    femaleMarried15andOlder + femaleWidowed15andOlder + femaleDivorced15andOlder
            ) / total_pop_15andOlder
    ) |>
    select(matches("year|fips|state|county|prcnt"))

################################################################################
# Clean/restructure IPUMS data (median household income).
ipums_medianHhIncome <-
    ipums_data$median_hh_income |>
    rename_with(rename_medianHhIncome_cols) |>
    pivot_ipums("median") |>
    rename(HhIncome_median_est = medianHHIncome)

################################################################################
# Clean/restructure IPUMS data (occupancy).
ipums_occupancy <-
    ipums_data$occupancy |>
    rename_with(rename_occupancy_cols) |>
    pivot_ipums("Housing") |>
    mutate(vacantHousingUnits_prcnt_est = nrVacantHousingUnits / (nrVacantHousingUnits + nrOccupiedHousingUnits)) |>
    select(matches("year|fips|state|county|prcnt|nrOccupied")) |>
    # Data was collected in the 2010, 2020 Census and is preferred over the ACS.
    filter((!year %in% c("2006-2010", "2016-2020")))

################################################################################
# Clean/restructure IPUMS data (owner/renter).
ipums_owner_renter <-
    ipums_data$owner_renter |>
    rename_with(rename_owner_renter_cols) |>
    pivot_ipums("Units") |>
    mutate(liveInRental_prcnt_est_allAges = nrPeopleInRentedUnits / (nrPeopleInRentedUnits + nrPeopleInOwnedUnits)) |>
    select(matches("year|fips|state|county|prcnt")) |>
    # Data was collected in the 2010 Census and is preferred over the ACS.
    filter(year != "2006-2010")

################################################################################
# Clean/restructure IPUMS data (poverty).
ipums_poverty <-
    ipums_data$poverty |>
    rename_with(rename_poverty_cols) |>
    pivot_ipums("ratio") |>
    mutate(
        total =
            ratioBelow75 + ratio75to99 + ratio100to124 + ratio125to149 + ratio150to174 + ratio175to199 + ratioAbove200,
        ratioIncomeToPovertyBelow75_prcnt_est_povertyUniverse = ratioBelow75 / total,
        ratioIncomeToPoverty75to99_prcnt_est_povertyUniverse = ratio75to99 / total,
        belowPoverty_prcnt_est_povertyUniverse = (ratioBelow75 + ratio75to99) / total,
        ratioIncomeToPoverty100to124_prcnt_est_povertyUniverse = ratio100to124 / total,
        ratioIncomeToPoverty125to149_prcnt_est_povertyUniverse = ratio125to149 / total,
        ratioIncomeToPoverty150to174_prcnt_est_povertyUniverse = ratio150to174 / total,
        ratioIncomeToPoverty175to199_prcnt_est_povertyUniverse = ratio175to199 / total,
        ratioIncomeToPovertyAbove200_prcnt_est_povertyUniverse = ratioAbove200 / total,
    ) |>
    select(matches("year|fips|state|county|prcnt"))

################################################################################
# Clean/restructure IPUMS data (race).
race <- ipums_data$race_and_ethnicity |> rename_with(rename_race_cols)

diversity <-
    race |>
    mutate(full_fips = paste0(state, county)) |>
    pivot_longer(
        cols = matches("white|black|aian|asian|other"),
        names_to = "variable",
        values_to = "value"
    ) |>
    separate_wider_delim(variable, "_", names = c("subject", "year")) |>
    mutate(
        subject = if_else(str_detect(subject, "Hisp"), "hispanic", subject)
    ) |>
    count(
        full_fips, county, state, year, subject, wt = value, name = "pop"
    ) |>
    group_by(full_fips, county, state, year) |>
    mutate(
        n = n(),
        prcnt = pop / sum(pop),
        information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)
    ) |>
    summarise(
        shannon_index = sum(prcnt * information),
        gini_simpson_index = 1 - sum(prcnt ^ 2)
    ) |>
    mutate(shannon_index_scaled = shannon_index / log(6, 2)) |>
    ungroup() |>
    select(-shannon_index)
    
ipums_race <-
    race |>
    pivot_ipums("white|black|aian|asian|other") |>
    mutate(
        hispanic = whiteHisp + blackHisp + aianHisp + asianAndPiHisp + otherHisp,
        total = whiteNh + blackNh + aianNh + asianAndPiNh + otherNh + hispanic,
        white_prcnt_est_allAges = whiteNh / total,
        black_prcnt_est_allAges = blackNh / total,
        hispanic_prcnt_est_allAges = hispanic / total
    ) |>
    select(matches("year|fips|state|county|prcnt")) |>
    full_join(diversity, by = c("year", "full_fips", "state", "county"))

################################################################################
# Clean/restructure IPUMS data (sex + age).
ipums_sex_age <-
    ipums_data$sex_by_age |>
    rename_with(rename_sex_age_cols) |>
    pivot_ipums("male") |>
    mutate(
        pop_nr_est =
            maleUnder5 + male5to9 + male10to14 + male15to17 + male18to19 +
            male20 + male21 + male22to24 + male25to29 + male30to34 +
            male35to44 + male45to54 + male55to59 + male60to61 + male62to64 +
            male65to74 + male75to84 + maleAbove85 + femaleUnder5 + female5to9 +
            female10to14 + female15to17 + female18to19 + female20 + female21 +
            female22to24 + female25to29 + female30to34 + female35to44 +
            female45to54 + female55to59 + female60to61 + female62to64 +
            female65to74 + female75to84 + femaleAbove85,
        pop_prcnt_est_15to24_allRaces_m =
            (male15to17 + male18to19 + male20 + male21 + male22to24) / pop_nr_est
    ) |>
    select(matches("year|fips|state|county|prcnt|pop_nr_est"))

################################################################################
# Clean/restructure IPUMS data (own vs. rent by race/ethnicity).
ipums_owner_renter_race <-
    ipums_data$owner_renter_race |>
    rename_with(rename_owner_renter_race_cols) |>
    pivot_ipums("Owner|Renter") |>
    rowwise() |>
    mutate(
        hisp_total =
            sum(
                whiteHispHhOwner, blackHispHhOwner, aianHispHhOwner,
                asianAndPiHispHhOwner, otherHispHhOwner, multiHispHhOwner,
                whiteHispHhRenter, blackHispHhRenter, aianHispHhRenter,
                asianAndPiHispHhRenter, otherHispHhRenter, multiHispHhRenter,
                na.rm = T
            ),
        hisp_renter_total =
            sum(
                whiteHispHhRenter, blackHispHhRenter, aianHispHhRenter,
                asianAndPiHispHhRenter, otherHispHhRenter, multiHispHhRenter,
                na.rm = T
            ),
    ) |>
    ungroup() |>
    mutate(
        white_total = whiteNhHhOwner + whiteNhHhRenter,
        black_total = blackNhHhOwner + blackNhHhRenter,
        renters_prcnt_est_allAges_w = whiteNhHhRenter / white_total,
        renters_prcnt_est_allAges_b = blackNhHhRenter / black_total,
        renters_prcnt_est_allAges_h = hisp_renter_total / hisp_total,
        blackToWhiteRenters_diff_est = renters_prcnt_est_allAges_b - renters_prcnt_est_allAges_w,
        hispToWhiteRenters_diff_est = renters_prcnt_est_allAges_h - renters_prcnt_est_allAges_w
    ) |>
    select(matches("year|fips|state|county|diff"))

################################################################################
# Clean/restructure IPUMS data (income categories from 1980).
hh_income_1980 <-
    ipums_data$hh_income_1980 |>
    rename_with(rename_income_1980_cols) |>
    pivot_ipums("from|above|below") |>
    mutate(
        total =
            below10 + from10to15 + from15to20 + from20to25 + from25to30 +
            from30to35 + from35to40 + from40to50 + from50to75 + above75,
        across(
            matches("from|above|below"),
            function(col) {col / total},
            .names = "hhIncome{.col}_prcnt_est"
        )
    ) |>
    select(-total, -matches("^from|^above|^below"))

################################################################################
# Clean/restructure IPUMS data (income categories from 1990 and on).
hh_income <-
    ipums_data$hh_income |>
    rename_with(rename_income_cols) |>
    pivot_ipums("from|above|below") |>
    mutate(
        from40to50 = from40to45 + from45to50,
        from50to75 = from50to60 + from60to75,
        above75 = from75to100 + from100to125 + from125to150 + above150,
        below30 = below10 + from10to15 + from20to25 + from25to30,
        from30to60 = from30to35 + from35to40 + from40to45 + from45to50 + from50to60,
        from60to100 = from60to75 + from75to100,
        from100to150 = from100to125 + from125to150
    ) |>
    mutate(
        total =
            below10 + from10to15 + from15to20 + from20to25 + from25to30 +
            from30to35 + from35to40 + from40to50 + from50to75 + from75to100 +
            from100to125 + from125to150 + above150,
        across(
            matches("from|above|below"),
            function(col) {col / total},
            .names = "hhIncome{.col}_prcnt_est"
        )
    ) |>
    bind_rows(hh_income_1980) |>
    select(
        full_fips, state, county, year, hhIncomebelow30_prcnt_est,
        hhIncomefrom30to60_prcnt_est, hhIncomefrom60to100_prcnt_est,
        hhIncomefrom100to150_prcnt_est, hhIncomeabove150_prcnt_est
    )

diversity_hhIncome <-
    hh_income |>
    pivot_longer(
        cols = matches("prcnt_est"), names_to = "variable", values_to = "prcnt"
    ) |>
    group_by(full_fips, county, state, year) |>
    mutate(n = n(), information = if_else(prcnt > 0, log((1 / prcnt), 2), 0)) |>
    summarise(
        shannon_index_hhIncome = sum(prcnt * information),
        gini_simpson_index_hhIncome = 1 - sum(prcnt ^ 2)
    ) |>
    mutate(shannon_index_scaled_hhIncome = shannon_index_hhIncome / log(5, 2)) |>
    ungroup() |>
    select(-shannon_index_hhIncome)

ipums_hh_income <-
    hh_income |>
    full_join(
        diversity_hhIncome, by = c("full_fips", "state", "county", "year")
    )

################################################################################
# Clean/restructure IPUMS data (labor status + sex).
ipums_labor_sex <-
    ipums_data$labor_sex |>
    rename_with(rename_labor_sex_cols) |>
    pivot_ipums("male") |>
    mutate(
        male_civilian_noninst_pop =
            maleCivilian16andOlder + maleNotInLaborForce16andOlder,
        lfpr_prcnt_est_16andOlder_allRaces_m =
            maleCivilian16andOlder / male_civilian_noninst_pop,
        epr_prcnt_est_16andOlder_allRaces_m =
            maleEmployed16andOlder / male_civilian_noninst_pop,
        ur_prcnt_est_16andOlder_allRaces_m =
            maleUnemployed16andOlder / (maleEmployed16andOlder + maleUnemployed16andOlder),
        female_civilian_noninst_pop =
            femaleCivilian16andOlder + femaleNotInLaborForce16andOlder,
        lfpr_prcnt_est_16andOlder_allRaces_f =
            femaleCivilian16andOlder / female_civilian_noninst_pop,
        epr_prcnt_est_16andOlder_allRaces_f =
            femaleEmployed16andOlder / female_civilian_noninst_pop,
        ur_prcnt_est_16andOlder_allRaces_f =
            femaleUnemployed16andOlder / (femaleEmployed16andOlder + femaleUnemployed16andOlder),
    ) |>
    select(matches("year|fips|state|county|prcnt"))

################################################################################
# Clean/restructure IPUMS data (child poverty).
ipums_poverty_children <-
    ipums_data$poverty_children |>
    rename_with(rename_poverty_children_cols) |>
    pivot_ipums("Poverty") |>
    mutate(
        belowPoverty_prcnt_est_under18 =
            inPovertyUnder18 / (inPovertyUnder18 + abovePovertyUnder18)
    ) |>
    select(matches("year|fips|state|county|prcnt"))

################################################################################
# Combine all data frames and save results.
ipums_county_clean <-
    list(
        ipums_education, ipums_family_paoc, ipums_hh_income, ipums_labor,
        ipums_marriage, ipums_medianHhIncome, ipums_occupancy,
        ipums_owner_renter, ipums_owner_renter_race, ipums_poverty, ipums_race,
        ipums_sex_age, ipums_poverty_children
    ) |>
    reduce(
        function(x, y) {
            x <-
                x |>
                mutate(
                    year =
                        if_else(
                            str_length(year) == 4,
                            year,
                            str_extract(year, "[0-9]{4}$")
                        )
                )
            
            y <-
                y |>
                mutate(
                    year =
                        if_else(
                            str_length(year) == 4,
                            year,
                            str_extract(year, "[0-9]{4}$")
                        )
                )
            
            full_join(x, y, by = c("year", "full_fips", "state", "county"))
        }
    ) |>
    mutate(
        singleMom_prcnt_est = femalehhChildren / nrOccupiedHousingUnits,
        singleDad_prcnt_est = malehhChildren / nrOccupiedHousingUnits,
        singleParent_prcnt_est = singleParenthhChildren / nrOccupiedHousingUnits
    ) |>
    select(-nrOccupiedHousingUnits, -singleParenthhChildren, -matches("hhChildren"))

write_csv(
    ipums_county_clean,
    here("2_clean_data", "output", "ipumsTS_county_clean.csv")
)
