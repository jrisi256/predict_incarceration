library(here)
library(readr)
library(dplyr)
library(purrr)
library(R.utils)

read_dir <- here("1_get_data", "output")
save_dir <- here("2_clean_data", "output")

################################################################################
# Read in ACS data.
# County 51515 was annexed by county 51019 in 2013.
acs_df <-
    read_csv(file.path(read_dir, "acs_county.csv.gz")) |>
    mutate(
        full_fips = if_else(full_fips == "51515", "51019", full_fips),
        county = if_else(state == "51" & county == "515", "019", county)
    )

################################################################################
# Clean educational data.
df_edu <-
    acs_df |>
    mutate(
        lessThanHs_prcnt_est_25older_b = (maleBlackLessThanHS + femaleBlackLessThanHS) / totalBlack25AndOlder,
        hs_prcnt_est_25older_b = (maleBlackHS + femaleBlackHS) / totalBlack25AndOlder,
        someCollege_prcnt_est_25older_b = (maleBlackSomeCollege + femaleBlackSomeCollege) / totalBlack25AndOlder,
        college_prcnt_est_25older_b = (maleBlackCollege + femaleBlackCollege) / totalBlack25AndOlder,
        lessThanHs_prcnt_est_25older_h = (maleHispLessThanHS + femaleHispLessThanHS) / totalHisp25AndOlder,
        hs_prcnt_est_25older_h = (maleHispHS + femaleHispHS) / totalHisp25AndOlder,
        someCollege_prcnt_est_25older_h = (maleHispSomeCollege + femaleHispSomeCollege) / totalHisp25AndOlder,
        college_prcnt_est_25older_h = (maleHispCollege + femaleHispCollege) / totalHisp25AndOlder,
        lessThanHs_prcnt_est_25older_w = (maleWhiteLessThanHS + femaleWhiteLessThanHS) / totalWhite25AndOlder,
        hs_prcnt_est_25older_w = (maleWhiteHS + femaleWhiteHS) / totalWhite25AndOlder,
        someCollege_prcnt_est_25older_w = (maleWhiteSomeCollege + femaleWhiteSomeCollege) / totalWhite25AndOlder,
        college_prcnt_est_25older_w = (maleWhiteCollege + femaleWhiteCollege) / totalWhite25AndOlder,
        blackToWhiteCollege_diff_est = college_prcnt_est_25older_b - college_prcnt_est_25older_w,
        hispToWhiteCollege_diff_est = college_prcnt_est_25older_h - college_prcnt_est_25older_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Clean poverty data.
df_poverty <-
    acs_df |>
    mutate(
        inPoverty_prcnt_est_allAges_b = black_inPoverty / black_pop_povertyStatusCalculable,
        inPoverty_prcnt_est_allAges_w = white_inPoverty / white_pop_povertyStatusCalculable,
        inPoverty_prcnt_est_allAges_h = hisp_inPoverty / hisp_pop_povertyStatusCalculable,
        blackToWhitePoverty_diff_est = inPoverty_prcnt_est_allAges_b - inPoverty_prcnt_est_allAges_w,
        hispToWhitePoverty_diff_est = inPoverty_prcnt_est_allAges_h - inPoverty_prcnt_est_allAges_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Clean income, poverty, and welfare data.
df_income_black <-
    acs_df |>
    mutate(
        hhIncomeBelow10_prcnt_est_allAges_b = black_below10 / total_nr_black_hh,
        hhIncome10to15_prcnt_est_allAges_b = black_from10to15 / total_nr_black_hh,
        hhIncome15to20_prcnt_est_allAges_b = black_from15to20 / total_nr_black_hh,
        hhIncome20to25_prcnt_est_allAges_b = black_from20to25 / total_nr_black_hh,
        hhIncome25to30_prcnt_est_allAges_b = black_from25to30 / total_nr_black_hh,
        hhIncome30to35_prcnt_est_allAges_b = black_from30to35 / total_nr_black_hh,
        hhIncome35to40_prcnt_est_allAges_b = black_from35to40 / total_nr_black_hh,
        hhIncome40to50_prcnt_est_allAges_b = (black_from40to45 + black_from45to50) / total_nr_black_hh,
        hhIncome50to75_prcnt_est_allAges_b = (black_from50to60 + black_from60to75) / total_nr_black_hh,
        hhIncome75to100_prcnt_est_allAges_b = black_from75to100 / total_nr_black_hh,
        hhIncome100to125_prcnt_est_allAges_b = black_from100to125 / total_nr_black_hh,
        hhIncome125to150_prcnt_est_allAges_b = black_from125to150 / total_nr_black_hh,
        hhIncomeAbove150_prcnt_est_allAges_b = (black_from150to200 + black_above200) / total_nr_black_hh,
        hhIncomeAbove75_prcnt_est_allAges_b =
            (black_from75to100 + black_from100to125 + black_from125to150 + black_from150to200 + black_above200) / total_nr_black_hh
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est|diff_est"))

df_income_white <-
    acs_df |>
    mutate(
        hhIncomeBelow10_prcnt_est_allAges_w = white_below10 / total_nr_white_hh,
        hhIncome10to15_prcnt_est_allAges_w = white_from10to15 / total_nr_white_hh,
        hhIncome15to20_prcnt_est_allAges_w = white_from15to20 / total_nr_white_hh,
        hhIncome20to25_prcnt_est_allAges_w = white_from20to25 / total_nr_white_hh,
        hhIncome25to30_prcnt_est_allAges_w = white_from25to30 / total_nr_white_hh,
        hhIncome30to35_prcnt_est_allAges_w = white_from30to35 / total_nr_white_hh,
        hhIncome35to40_prcnt_est_allAges_w = white_from35to40 / total_nr_white_hh,
        hhIncome40to50_prcnt_est_allAges_w =(white_from40to45 + white_from45to50) / total_nr_white_hh,
        hhIncome50to75_prcnt_est_allAges_w = (white_from50to60 + white_from60to75) / total_nr_white_hh,
        hhIncome75to100_prcnt_est_allAges_w = white_from75to100 / total_nr_white_hh,
        hhIncome100to125_prcnt_est_allAges_w = white_from100to125 / total_nr_white_hh,
        hhIncome125to150_prcnt_est_allAges_w = white_from125to150 / total_nr_white_hh,
        hhIncomeAbove150_prcnt_est_allAges_w = (white_from150to200 + white_above200) / total_nr_white_hh,
        hhIncomeAbove75_prcnt_est_allAges_w =
            (white_from75to100 + white_from100to125 + white_from125to150 + white_from150to200 + white_above200) / total_nr_white_hh
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est|diff_est"))

df_income_hisp <-
    acs_df |>
    mutate(
        hhIncomeBelow10_prcnt_est_allAges_h = hisp_below10 / total_nr_hisp_hh,
        hhIncome10to15_prcnt_est_allAges_h = hisp_from10to15 / total_nr_hisp_hh,
        hhIncome15to20_prcnt_est_allAges_h = hisp_from15to20 / total_nr_hisp_hh,
        hhIncome20to25_prcnt_est_allAges_h = hisp_from20to25 / total_nr_hisp_hh,
        hhIncome25to30_prcnt_est_allAges_h = hisp_from25to30 / total_nr_hisp_hh,
        hhIncome30to35_prcnt_est_allAges_h = hisp_from30to35 / total_nr_hisp_hh,
        hhIncome35to40_prcnt_est_allAges_h = hisp_from35to40 / total_nr_hisp_hh,
        hhIncome40to50_prcnt_est_allAges_h = (hisp_from40to45 + hisp_from45to50) / total_nr_hisp_hh,
        hhIncome50to75_prcnt_est_allAges_h = (hisp_from50to60 + hisp_from60to75) / total_nr_hisp_hh,
        hhIncome75to100_prcnt_est_allAges_h = hisp_from75to100 / total_nr_hisp_hh,
        hhIncome100to125_prcnt_est_allAges_h = hisp_from100to125 / total_nr_hisp_hh,
        hhIncome125to150_prcnt_est_allAges_h = hisp_from125to150 / total_nr_hisp_hh,
        hhIncomeAbove150_prcnt_est_allAges_h = (hisp_from150to200 + hisp_above200) / total_nr_hisp_hh,
        hhIncomeAbove75_prcnt_est_allAges_h =
            (hisp_from75to100 + hisp_from100to125 + hisp_from125to150 + hisp_from150to200 + hisp_above200) / total_nr_hisp_hh
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est|diff_est"))

df_median_income <-
    acs_df |>
    mutate(
        hhIncome_median_est_allAges_b = if_else(black_medianHHIncome < 0, NA, black_medianHHIncome),
        hhIncome_median_est_allAges_w = if_else(white_medianHHIncome < 0, NA, white_medianHHIncome),
        hhIncome_median_est_allAges_h = if_else(hisp_medianHHIncome < 0, NA, hisp_medianHHIncome),
        medianHhIncomeBlackToWhite_prcnt_est = (hhIncome_median_est_allAges_w - hhIncome_median_est_allAges_b) / hhIncome_median_est_allAges_b,
        medianHhIncomeHispToWhite_prcnt_est = (hhIncome_median_est_allAges_w - hhIncome_median_est_allAges_h) / hhIncome_median_est_allAges_h
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|medianHhIncome.*prcnt"))

df_welfare <-
    acs_df |>
    mutate(hhReceiveCpaSnap_prcnt_est = total_nr_hh_publicAsst_Snap / total_nr_hh) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est"))

df_income_inequality <-
    acs_df |>
    mutate(
        hhShareOfIncome_1stQuintile_est = shareOfIncome_lowest_quintile,
        hhShareOfIncome_2ndQuintile_est = shareOfIncome_second_quintile,
        hhShareOfIncome_3rdQuintile_est = shareOfIncome_third_quintile,
        hhShareOfIncome_4thQuintile_est = shareOfIncome_fourth_quintile,
        hhShareOfIncome_5thQuintile_est = shareOfIncome_fifth_quintile,
        hhShareOfIncome_top5Prcnt_est = shareOfIncome_top5prcnt,
        giniCoefficient_index_est = gini_coefficient
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|index|hhShareOfIncome"))

df_snap <-
    acs_df |>
    mutate(
        hhSnap_prcnt_est_allAges_b = blackHH_Snap / total_nr_black_hh,
        hhSnap_prcnt_est_allAges_w = whiteHH_Snap / total_nr_white_hh,
        hhSnap_prcnt_est_allAges_h = hispHH_Snap / total_nr_hisp_hh,
        hhSnapBlackToWhite_diff_est = hhSnap_prcnt_est_allAges_b - hhSnap_prcnt_est_allAges_w,
        hhSnapHispToWhite_diff_est = hhSnap_prcnt_est_allAges_h - hhSnap_prcnt_est_allAges_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Clean labor data.
df_labor <-
    acs_df |>
    mutate(
        nr_employed_black =
            black_employed_male_16to64 + black_employed_male_65andOlder +
            black_employed_female_16to64 + black_employed_female_65andOlder,
        nr_unemployed_black =
            black_unemployed_male_16to64 + black_unemployed_male_65andOlder +
            black_unemployed_female_16to64 + black_unemployed_female_65andOlder,
        nr_civilianLaborForceBlack =
            black_unemployed_male_16to64 + black_unemployed_male_65andOlder +
            black_unemployed_female_16to64 + black_unemployed_female_65andOlder +
            black_employed_male_16to64 + black_employed_male_65andOlder +
            black_employed_female_16to64 + black_employed_female_65andOlder,
        nr_total_blackCivilian_16andOlder_pop =  black_pop_male_16andOlder + black_pop_female_16andOlder,
        ur_prcnt_est_16andOlder_b = nr_unemployed_black / nr_civilianLaborForceBlack,
        lfpr_prcnt_est_16andOlder_b = nr_civilianLaborForceBlack / nr_total_blackCivilian_16andOlder_pop,
        epr_prcnt_est_16andOlder_b = nr_employed_black/ nr_total_blackCivilian_16andOlder_pop,
        nr_employed_white =
            white_employed_male_16to64 + white_employed_male_65andOlder +
            white_employed_female_16to64 + white_employed_female_65andOlder,
        nr_unemployed_white =
            white_unemployed_male_16to64 + white_unemployed_male_65andOlder +
            white_unemployed_female_16to64 + white_unemployed_female_65andOlder,
        nr_civilianLaborForceWhite =
            white_unemployed_male_16to64 + white_unemployed_male_65andOlder +
            white_unemployed_female_16to64 + white_unemployed_female_65andOlder +
            white_employed_male_16to64 + white_employed_male_65andOlder +
            white_employed_female_16to64 + white_employed_female_65andOlder,
        nr_total_whiteCivilian_16andOlder_pop =  white_pop_male_16andOlder + white_pop_female_16andOlder,
        ur_prcnt_est_16andOlder_w = nr_unemployed_white / nr_civilianLaborForceWhite,
        lfpr_prcnt_est_16andOlder_w = nr_civilianLaborForceWhite / nr_total_whiteCivilian_16andOlder_pop,
        epr_prcnt_est_16andOlder_w = nr_employed_white/ nr_total_whiteCivilian_16andOlder_pop,
        nr_employed_hisp =
            hisp_employed_male_16to64 + hisp_employed_male_65andOlder +
            hisp_employed_female_16to64 + hisp_employed_female_65andOlder,
        nr_unemployed_hisp =
            hisp_unemployed_male_16to64 + hisp_unemployed_male_65andOlder +
            hisp_unemployed_female_16to64 + hisp_unemployed_female_65andOlder,
        nr_civilianLaborForceHisp =
            hisp_unemployed_male_16to64 + hisp_unemployed_male_65andOlder +
            hisp_unemployed_female_16to64 + hisp_unemployed_female_65andOlder +
            hisp_employed_male_16to64 + hisp_employed_male_65andOlder +
            hisp_employed_female_16to64 + hisp_employed_female_65andOlder,
        nr_total_hispCivilian_16andOlder_pop =  hisp_pop_male_16andOlder + hisp_pop_female_16andOlder,
        ur_prcnt_est_16andOlder_h = nr_unemployed_hisp / nr_civilianLaborForceHisp,
        lfpr_prcnt_est_16andOlder_h = nr_civilianLaborForceHisp / nr_total_hispCivilian_16andOlder_pop,
        epr_prcnt_est_16andOlder_h = nr_employed_hisp/ nr_total_hispCivilian_16andOlder_pop,
        lfprBlackToWhite_diff_est_16andOlder = lfpr_prcnt_est_16andOlder_b - lfpr_prcnt_est_16andOlder_w,
        urBlackToWhite_diff_est_16andOlder = ur_prcnt_est_16andOlder_b - ur_prcnt_est_16andOlder_w,
        lfprHispToWhite_diff_est_16andOlder = lfpr_prcnt_est_16andOlder_h - lfpr_prcnt_est_16andOlder_w,
        urHispToWhite_diff_est_16andOlder = ur_prcnt_est_16andOlder_h - ur_prcnt_est_16andOlder_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Housing problems, housing costs, and housing value data.
df_housing_costs <-
    acs_df |>
    mutate(
        hhCostsLessThan10PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_lessThan10 + mortgage_prcnt_of_income_lessThan10 + hhCosts_prcnt_of_income_lessThan10) / total_nr_hh,
        hhCostsBetween10and15PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_10to15 + mortgage_prcnt_of_income_10to15 + hhCosts_prcnt_of_income_10to15) / total_nr_hh,
        hhCostsBetween15and20PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_15to20 + mortgage_prcnt_of_income_15to20 + hhCosts_prcnt_of_income_15to20) / total_nr_hh,
        hhCostsBetween20and25PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_20to25 + mortgage_prcnt_of_income_20to25 + hhCosts_prcnt_of_income_20to25) / total_nr_hh,
        hhCostsBetween25and30PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_25to30 + mortgage_prcnt_of_income_25to30 + hhCosts_prcnt_of_income_25to30) / total_nr_hh,
        hhCostsBetween30and35PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_30to35 + mortgage_prcnt_of_income_30to35 + hhCosts_prcnt_of_income_30to35) / total_nr_hh,
        hhCostsBetween35and40PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_35to40 + mortgage_prcnt_of_income_35to40 + hhCosts_prcnt_of_income_35to40) / total_nr_hh,
        hhCostsBetween40and50PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_40to50 + mortgage_prcnt_of_income_40to50 + hhCosts_prcnt_of_income_40to50) / total_nr_hh,
        hhCostsAbove50PrcntIncome_prcnt_est =
            (rent_prcnt_of_income_above50 + mortgage_prcnt_of_income_above50 + hhCosts_prcnt_of_income_above50) / total_nr_hh,
        grossRentPrcntIncome_median_est = if_else(median_rent_prcnt_of_income < 0, NA, median_rent_prcnt_of_income),
        grossMortgagePrcntIncome_median_est = if_else(median_mortgage_prcnt_of_income < 0, NA, median_mortgage_prcnt_of_income),
        hhCostsPrcntIncome_median_est = if_else(median_hhCosts_prcnt_of_income < 0, NA, median_hhCosts_prcnt_of_income)
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est|median_est"))

df_vehicle <-
    acs_df |>
    mutate(
        hhOwnsNoVehicle_prcnt_est = (owner_no_vehicle + renter_no_vehicle) / total_nr_hh,
        hhOwns1Vehicle_prcnt_est = (owner_one_vehicle + renter_one_vehicle) / total_nr_hh,
        hhOwns2Vehicle_prcnt_est = (owner_two_vehicle + renter_no_vehicle) / total_nr_hh,
        hhOwns3Vehicle_prcnt_est = (owner_three_vehicle + renter_no_vehicle) / total_nr_hh,
        hhOwns4Vehicle_prcnt_est = (owner_four_vehicle + renter_no_vehicle) / total_nr_hh,
        hhOwns5orMoreVehicle_prcnt_est = (owner_fiveOrMore_vehicle + renter_fiveOrMore_vehicle) / total_nr_hh
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est"))

df_housing_problems <-
    acs_df |>
    mutate(
        hhNoPhone_prcnt_est = (total_nr_owner_occupied_units_no_telphone + total_nr_renter_occupied_units_no_telephone) / total_nr_hh,
        hhKitchenProblems_prcnt_est = lacks_kitchen / total_nr_hh,
        hhBathroomProblems_prcnt_est = lacks_plumbing / total_nr_hh,
        hhLessThan5PersonPerRoom_prcnt_est = (owner_nr_people_per_room_below5 + renter_nr_people_per_room_below5) / total_nr_hh,
        hhBetweeen51and100PersonPerRoom_prcnt_est = (owner_nr_people_per_room_51to100 + renter_nr_people_per_room_51to100) / total_nr_hh,
        hhBetweeen101and150PersonPerRoom_prcnt_est = (owner_nr_people_per_room_101to150 + renter_nr_people_per_room_101to150) / total_nr_hh,
        hhBetweeen151and200PersonPerRoom_prcnt_est = (owner_nr_people_per_room_151to2 + renter_nr_people_per_room_151to2) / total_nr_hh,
        hhAbove200PersonPerRoom_prcnt_est = (owner_nr_people_per_room_above2 + renter_nr_people_per_room_above2) / total_nr_hh,
        hhWith0Problem_prcnt_est = (owner_occupied_no_problems + renter_occupied_no_problems) / total_nr_hh,
        hhWith1Problem_prcnt_est = (owner_occupied_one_problem + renter_occupied_one_problem) / total_nr_hh,
        hhWith2Problem_prcnt_est = (owner_occupied_two_problems + renter_occupied_two_problems) / total_nr_hh,
        hhWith3Problem_prcnt_est = (owner_occupied_three_problems + renter_occupied_three_problems) / total_nr_hh,
        hhWith4Problem_prcnt_est = (owner_occupied_all_problems + renter_occupied_all_problems) / total_nr_hh
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est"))

df_housing_values <-
    acs_df |>
    mutate(
        hhLessThan50Value_prcnt_est =
            (
                house_value_owner_occupied_below10 + house_value_owner_occupied_10to15 +
                    house_value_owner_occupied_15to20 + house_value_owner_occupied_20to25 +
                    house_value_owner_occupied_25to30 + house_value_owner_occupied_30to35 +
                    house_value_owner_occupied_35to40 + house_value_owner_occupied_40to50
            ) /
            total_nr_owner_occupied_units,
        hhBetween50And100Value_prcnt_est =
            (
                house_value_owner_occupied_50to60 + house_value_owner_occupied_60to70 +
                    house_value_owner_occupied_70to80 + house_value_owner_occupied_80to90 +
                    house_value_owner_occupied_90to100
            ) /
            total_nr_owner_occupied_units,
        hhBetween100And150Value_prcnt_est =
            (house_value_owner_occupied_100to125 + house_value_owner_occupied_125to150) / total_nr_owner_occupied_units,
        hhBetween150And200Value_prcnt_est =
                (house_value_owner_occupied_150to175 + house_value_owner_occupied_175to200) / total_nr_owner_occupied_units,
        hhBetween200And300Value_prcnt_est =
                (house_value_owner_occupied_200to250 + house_value_owner_occupied_250to300) / total_nr_owner_occupied_units,
        hhBetween300And500Value_prcnt_est =
                (house_value_owner_occupied_300to400 + house_value_owner_occupied_400to500) / total_nr_owner_occupied_units,
        hhBetween500And1000Value_prcnt_est =
                (house_value_owner_occupied_500to750 + house_value_owner_occupied_750to1000) / total_nr_owner_occupied_units,
        hhAbove1000Value_nr_est =
            if_else(
                year < 2015,
                house_value_owner_occupied_above1000,
                house_value_owner_occupied_1000to1500 + house_value_owner_occupied_1500to2000 + house_value_owner_occupied_above2000
            ),
        hhAbove1000Value_prcnt_est = hhAbove1000Value_nr_est  / total_nr_owner_occupied_units,
        homeValue_25thPtile_est = if_else(house_value_owner_occupied_25thPtile >= 0, house_value_owner_occupied_25thPtile, NA),
        homeValue_median_est = if_else(house_value_owner_occupied_median >= 0, house_value_owner_occupied_median, NA),
        homeValue_75thPtile_est = if_else(house_value_owner_occupied_75thPtile >= 0, house_value_owner_occupied_75thPtile, NA)
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|(prcnt|median|ptile)_est"))

################################################################################
# Clean occupants per room by race data.
df_occupants_per_room_by_race <-
    acs_df |>
    mutate(
        hhOccupantsPerRoomAbove101_prcnt_est_allAges_b = nr_people_per_room_above101_black / total_nr_black_hh,
        hhOccupantsPerRoomAbove101_prcnt_est_allAges_w = nr_people_per_room_above101_white / total_nr_white_hh,
        hhOccupantsPerRoomAbove101_prcnt_est_allAges_h = nr_people_per_room_above101_hisp / total_nr_hisp_hh,
        ratioBlackWhiteOvercrowding_diff_est =
                hhOccupantsPerRoomAbove101_prcnt_est_allAges_b - hhOccupantsPerRoomAbove101_prcnt_est_allAges_w,
        ratioHispWhiteOvercrowding_diff_est =
                hhOccupantsPerRoomAbove101_prcnt_est_allAges_h - hhOccupantsPerRoomAbove101_prcnt_est_allAges_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Rental data by race.
df_renters_by_race <-
    acs_df |>
    mutate(
        renters_prcnt_est_allAges_b = nr_renter_black / total_nr_black_hh,
        renters_prcnt_est_allAges_w = nr_renter_white / total_nr_white_hh,
        renters_prcnt_est_allAges_h = nr_renter_hisp / total_nr_hisp_hh,
        blackToWhiteRenters_diff_est = renters_prcnt_est_allAges_b - renters_prcnt_est_allAges_w,
        hispToWhiteRenters_diff_est = renters_prcnt_est_allAges_h - renters_prcnt_est_allAges_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Presence of own children.
df_paoc <-
    acs_df |>
    mutate(
        singleMom_prcnt_est = femaleHHAlone_related_children / total_nr_hh,
        singleDad_prcnt_est = maleHHAlone_related_children / total_nr_hh,
        singleParent_prcnt_est = (femaleHHAlone_related_children + maleHHAlone_related_children) / total_nr_hh
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est"))

################################################################################
# Presence of own children by race.
df_paoc_race <-
    acs_df |>
    mutate(
        singleMom_prcnt_est_allAges_b =
            (femaleHHAlone_related_children_black_in_poverty + femaleHHAlone_related_children_black_above_poverty) / total_nr_black_hh,
        singleParent_prcnt_est_allAges_b = 
            (
                femaleHHAlone_related_children_black_in_poverty +
                    femaleHHAlone_related_children_black_above_poverty +
                    maleHHAlone_related_children_black_in_poverty +
                    maleHHAlone_related_children_black_above_poverty
            ) / total_nr_black_hh,
        singleMom_prcnt_est_allAges_w =
            (femaleHHAlone_related_children_white_in_poverty + femaleHHAlone_related_children_white_above_poverty) / total_nr_white_hh,
        singleParent_prcnt_est_allAges_w =
            (
                femaleHHAlone_related_children_white_in_poverty +
                    femaleHHAlone_related_children_white_above_poverty +
                    maleHHAlone_related_children_white_in_poverty +
                    maleHHAlone_related_children_white_above_poverty
            ) / total_nr_white_hh,
        singleMom_prcnt_est_allAges_h = 
                (femaleHHAlone_related_children_hisp_in_poverty + femaleHHAlone_related_children_hisp_above_poverty) / total_nr_hisp_hh,
        singleParent_prcnt_est_allAges_h =
            (
                femaleHHAlone_related_children_hisp_in_poverty +
                    femaleHHAlone_related_children_hisp_above_poverty +
                    maleHHAlone_related_children_hisp_in_poverty +
                    maleHHAlone_related_children_hisp_above_poverty
                ) / total_nr_hisp_hh,
        blackToWhiteSingleMom_diff_est = singleMom_prcnt_est_allAges_b - singleMom_prcnt_est_allAges_w,
        hispToWhiteSingleMom_diff_est = singleMom_prcnt_est_allAges_h - singleMom_prcnt_est_allAges_w,
        blackToWhiteSingleParent_diff_est = singleParent_prcnt_est_allAges_b - singleParent_prcnt_est_allAges_w,
        hispToWhiteSingleParent_diff_est = singleParent_prcnt_est_allAges_h - singleParent_prcnt_est_allAges_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Residential mobility.
df_mobility <-
    acs_df |>
    mutate(
        ###################################### All races.
        nr_moved_past_year = (total_nr_moved_same_county + total_nr_moved_same_state + total_nr_moved_same_country + total_nr_moved_diff_country),
        yearMoved_median_est = if_else(median_year_moved_into_unit >= 0, year - median_year_moved_into_unit, NA),
        movedPastYear_prcnt_est_1AndOlder = nr_moved_past_year / total_population,
        movedWithinCounty_prcnt_est_1AndOlder = total_nr_moved_same_county / nr_moved_past_year,
        movedWithinState_prcnt_est_1AndOlder = total_nr_moved_same_state / nr_moved_past_year,
        movedDiff_prcnt_est_1AndOlder = (total_nr_moved_same_country + total_nr_moved_diff_country) / nr_moved_past_year,
        ######################################### Black individuals.
        nr_moved_past_year_black =
            total_nr_moved_same_county_black + total_nr_moved_same_state_black +
            total_nr_moved_same_country_black + total_nr_moved_diff_country_black,
        movedPastYear_prcnt_est_1AndOlder_b = nr_moved_past_year_black / total_black_pop,
        movedWithinCounty_prcnt_est_1AndOlder_b = total_nr_moved_same_county_black / nr_moved_past_year_black,
        movedWithinState_prcnt_est_1AndOlder_b = total_nr_moved_same_state_black / nr_moved_past_year_black,
        movedDiff_prcnt_est_1AndOlder_b = (total_nr_moved_same_country_black + total_nr_moved_diff_country_black) / nr_moved_past_year_black,
        ######################################### White individuals.
        nr_moved_past_year_white =
            total_nr_moved_same_county_white + total_nr_moved_same_state_white +
            total_nr_moved_same_country_white + total_nr_moved_diff_country_white,
        movedPastYear_prcnt_est_1AndOlder_w = nr_moved_past_year_white / total_white_pop,
        movedWithinCounty_prcnt_est_1AndOlder_w = total_nr_moved_same_county_white / nr_moved_past_year_white,
        movedWithinState_prcnt_est_1AndOlder_w = total_nr_moved_same_state_white / nr_moved_past_year_white,
        movedDiff_prcnt_est_1AndOlder_w = (total_nr_moved_same_country_white + total_nr_moved_diff_country_white) / nr_moved_past_year_white,
        ######################################### Hispanic individuals.
        nr_moved_past_year_hisp =
            total_nr_moved_same_county_hisp + total_nr_moved_same_state_hisp +
            total_nr_moved_same_country_hisp + total_nr_moved_diff_country_hisp,
        movedPastYear_prcnt_est_1AndOlder_h = nr_moved_past_year_hisp / total_hisp_pop,
        movedWithinCounty_prcnt_est_1AndOlder_h = total_nr_moved_same_county_hisp / nr_moved_past_year_hisp,
        movedWithinState_prcnt_est_1AndOlder_h = total_nr_moved_same_state_hisp / nr_moved_past_year_hisp,
        movedDiff_prcnt_est_1AndOlder_h = (total_nr_moved_same_country_hisp + total_nr_moved_diff_country_hisp) / nr_moved_past_year_hisp,
        ######################################### Black to white risk differences.
        blackToWhiteMovers_diff_est = movedPastYear_prcnt_est_1AndOlder_b - movedPastYear_prcnt_est_1AndOlder_w,
        blackToWhiteMoversWithinCounty_diff_est = movedWithinCounty_prcnt_est_1AndOlder_b - movedWithinCounty_prcnt_est_1AndOlder_w,
        blackToWhiteMoversWithinState_diff_est = movedWithinState_prcnt_est_1AndOlder_b - movedWithinState_prcnt_est_1AndOlder_w,
        blackToWhiteMoversDiff_diff_est = movedDiff_prcnt_est_1AndOlder_b - movedDiff_prcnt_est_1AndOlder_w,
        ######################################### Hispanic to white risk differences.
        hispToWhiteMovers_diff_est = movedPastYear_prcnt_est_1AndOlder_h - movedPastYear_prcnt_est_1AndOlder_w,
        hispToWhiteMoversWithinCounty_diff_est = movedWithinCounty_prcnt_est_1AndOlder_h - movedWithinCounty_prcnt_est_1AndOlder_w,
        hispToWhiteMoversWithinState_diff_est = movedWithinState_prcnt_est_1AndOlder_h - movedWithinState_prcnt_est_1AndOlder_w,
        hispToWhiteMoversDiff_diff_est = movedDiff_prcnt_est_1AndOlder_h - movedDiff_prcnt_est_1AndOlder_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|movedPastYear_prcnt_est_1AndOlder$|Movers_diff|yearMoved_median"))

################################################################################
# Child variables.
df_child <-
    acs_df |>
    mutate(
        receiveCpaSnapSSI_prcnt_est_ages17AndYounger = children_in_hh_with_publicAsstOrSnapOrSSI / total_nr_children_in_hh,
        notInSchoolOrLf_prcnt_est_ages16to19 =
            (
                not_in_school_or_labor_force_HSGrad_male +
                    not_in_school_or_labor_force_notHSGrad_male +
                    not_in_school_or_labor_force_HSGrad_female +
                    not_in_school_or_labor_force_notHSGrad_female
            ) / total_pop_16_to_19,
        liveInSingleParentHh_prcnt_est_ages17AndYounger =
                (total_nr_children_in_maleAloneHH + total_nr_children_in_femaleAloneHH) / total_nr_children_in_hh_exclude_marriage,
        doNotLiveWithParent_prcnt_est_ages17AndYounger =
            (total_nr_children_in_grandparentHH + total_nr_children_in_otherRelativesHH + total_nr_children_in_fosterHH) /
            total_nr_children_in_hh_exclude_marriage,
        nr_not_in_poverty_black =
            if_else(
                year > 2012,
                black_abovePoverty_under6 + black_abovePoverty_6to11 + black_abovePoverty_12to17,
                black_abovePoverty_under5 + black_abovePoverty_age5 + black_abovePoverty_6to11 + black_abovePoverty_12to17
            ),
        nr_in_poverty_black =
            if_else(
                year > 2012,
                black_poverty_under6 + black_poverty_6to11 + black_poverty_12to17,
                black_poverty_under5 + black_poverty_age5 + black_poverty_6to11 + black_poverty_12to17
            ),
        inPoverty_prcnt_est_ages17AndYounger_b = nr_in_poverty_black / (nr_in_poverty_black + nr_not_in_poverty_black),
        nr_not_in_poverty_white =
            if_else(
                year > 2012,
                white_abovePoverty_under6 + white_abovePoverty_6to11 + white_abovePoverty_12to17,
                white_abovePoverty_under5 + white_abovePoverty_age5 + white_abovePoverty_6to11 + white_abovePoverty_12to17
            ),
        nr_in_poverty_white =
            if_else(
                year > 2012,
                white_poverty_under6 + white_poverty_6to11 + white_poverty_12to17,
                white_poverty_under5 + white_poverty_age5 + white_poverty_6to11 + white_poverty_12to17
            ),
        inPoverty_prcnt_est_ages17AndYounger_w = nr_in_poverty_white / (nr_in_poverty_white + nr_not_in_poverty_white),
        nr_not_in_poverty_hisp =
            if_else(
                year > 2012,
                hisp_abovePoverty_under6 + hisp_abovePoverty_6to11 + hisp_abovePoverty_12to17,
                hisp_abovePoverty_under5 + hisp_abovePoverty_age5 + hisp_abovePoverty_6to11 + hisp_abovePoverty_12to17
            ),
        nr_in_poverty_hisp =
            if_else(
                year > 2012,
                hisp_poverty_under6 + hisp_poverty_6to11 + hisp_poverty_12to17,
                hisp_poverty_under5 + hisp_poverty_age5 + hisp_poverty_6to11 + hisp_poverty_12to17
            ),
        inPoverty_prcnt_est_ages17AndYounger_h = nr_in_poverty_hisp / (nr_in_poverty_hisp + nr_not_in_poverty_hisp),
        blackToWhiteChildPoverty_diff_est = inPoverty_prcnt_est_ages17AndYounger_b - inPoverty_prcnt_est_ages17AndYounger_w,
        hispToWhiteChildPoverty_diff_est = inPoverty_prcnt_est_ages17AndYounger_h - inPoverty_prcnt_est_ages17AndYounger_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|receiveCpa|notInSchool|liveInSingle|doNotLive|diff_est"))

################################################################################
# Marriage by race.
df_marriage_by_race <-
    acs_df |>
    mutate(
        married_prcnt_est_15AndOlder_b = (black_male_married + black_female_married) / total_pop_age15_black,
        married_prcnt_est_15AndOlder_w = (white_male_married + white_female_married) / total_pop_age15_white,
        married_prcnt_est_15AndOlder_h = (hisp_male_married + hisp_female_married) / total_pop_age15_hisp,
        everMarried_prcnt_est_15AndOlder_b =
            (black_male_separated + black_male_widowed + black_male_divorced + black_female_separated + black_female_widowed + black_female_divorced) /
            total_pop_age15_black,
        everMarried_prcnt_est_15AndOlder_w =
            (white_male_separated + white_male_widowed + white_male_divorced + white_female_separated + white_female_widowed + white_female_divorced) /
            total_pop_age15_white,
        everMarried_prcnt_est_15AndOlder_h =
            (hisp_male_separated + hisp_male_widowed + hisp_male_divorced + hisp_female_separated + hisp_female_widowed + hisp_female_divorced) /
            total_pop_age15_hisp,
        blackToWhiteMarried_diff_est = married_prcnt_est_15AndOlder_b - married_prcnt_est_15AndOlder_w,
        blackToWhiteEverMarried_diff_est = everMarried_prcnt_est_15AndOlder_b - everMarried_prcnt_est_15AndOlder_w,
        hispToWhiteMarried_diff_est = married_prcnt_est_15AndOlder_h - married_prcnt_est_15AndOlder_w,
        hispToWhiteEverMarried_diff_est = everMarried_prcnt_est_15AndOlder_h - everMarried_prcnt_est_15AndOlder_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Fertility
df_fertility <-
    acs_df |>
    mutate(
        nrBirthsPer10000Unmarried_rate_est_ages15to50_allRaces_f =
            nr_unmarriedWomen_gaveBirth_ages15to50 * 10000 /
            (nr_unmarriedWomen_gaveBirth_ages15to50 + nr_unmarriedWomen_didNotBirth_ages15to50),
        nrBirthsPer10000Unmarried_rate_est_ages15to50_b_f =
            nr_unmarriedWomen_gaveBirth_ages15to50_black * 10000 /
            (nr_unmarriedWomen_gaveBirth_ages15to50_black + nr_unmarriedWomen_didNotBirth_ages15to50_black),
        nrBirthsPer10000Unmarried_rate_est_ages15to50_w_f =
            nr_unmarriedWomen_gaveBirth_ages15to50_white * 10000 /
            (nr_unmarriedWomen_gaveBirth_ages15to50_white + nr_unmarriedWomen_didNotBirth_ages15to50_white),
        nrBirthsPer10000Unmarried_rate_est_ages15to50_h_f =
            nr_unmarriedWomen_gaveBirth_ages15to50_hisp * 10000 /
            (nr_unmarriedWomen_gaveBirth_ages15to50_hisp + nr_unmarriedWomen_didNotBirth_ages15to50_hisp),
        blackToWhiteUnmarriedBirthRate_diff_est =
            nrBirthsPer10000Unmarried_rate_est_ages15to50_b_f - nrBirthsPer10000Unmarried_rate_est_ages15to50_w_f,
        hispToWhiteUnmarriedBirthRate_diff_est =
            nrBirthsPer10000Unmarried_rate_est_ages15to50_h_f - nrBirthsPer10000Unmarried_rate_est_ages15to50_w_f
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|diff_est"))

################################################################################
# Health insurance.
df_health_insurance <-
    acs_df |>
    mutate(
        hasHealthInsurance_prcnt_est_allAges_b =
            (has_healthInsurance_under19_black + has_healthInsurance_19to64_black + has_healthInsurance_above65_black) / total_pop_civilianNonInst_black,
        hasHealthInsurance_prcnt_est_allAges_w =
            (has_healthInsurance_under19_white + has_healthInsurance_19to64_white + has_healthInsurance_above65_white) / total_pop_civilianNonInst_white,
        hasHealthInsurance_prcnt_est_allAges_h =
            (has_healthInsurance_under19_hisp + has_healthInsurance_19to64_hisp + has_healthInsurance_above65_hisp) / total_pop_civilianNonInst_hisp,
        blackToWhiteHealthInsurance_diff_est = hasHealthInsurance_prcnt_est_allAges_b - hasHealthInsurance_prcnt_est_allAges_w,
        hispToWhiteHealthInsurance_diff_est = hasHealthInsurance_prcnt_est_allAges_h - hasHealthInsurance_prcnt_est_allAges_w,
        hasHealthInsurance_prcnt_est_under18_b = has_healthInsurance_under19_black / black_6to18_healthInsurance_denom,
        hasHealthInsurance_prcnt_est_under18_w = has_healthInsurance_under19_white / white_6to18_healthInsurance_denom,
        hasHealthInsurance_prcnt_est_under18_h = has_healthInsurance_under19_hisp / hisp_6to18_healthInsurance_denom,
        blackToWhiteChildHealthInsurance_diff_est = hasHealthInsurance_prcnt_est_under18_b - hasHealthInsurance_prcnt_est_under18_w,
        hispToWhiteChildHealthInsurance_diff_est = hasHealthInsurance_prcnt_est_under18_h - hasHealthInsurance_prcnt_est_under18_w
    ) |>
    select(matches("^year$|full_fips|^state$|^county$|prcnt_est|diff_est"))

################################################################################
# Save results.
df_final <-
    reduce(
        list(
            df_child, df_edu, df_fertility, df_housing_costs,
            df_housing_problems, df_housing_values, df_income_inequality,
            df_labor, df_marriage_by_race, df_median_income, df_mobility,
            df_occupants_per_room_by_race, df_paoc, df_paoc_race, df_poverty,
            df_renters_by_race, df_snap, df_vehicle, df_welfare
        ),
        function(x, y) {
            full_join(x, y, by = c("year", "full_fips", "state", "county"))
        }
    )

write_csv(df_final, file.path(save_dir, "acs_county_clean.csv"))
gzip(file.path(save_dir, "acs_county_clean.csv"), remove = T, overwrite = T)
