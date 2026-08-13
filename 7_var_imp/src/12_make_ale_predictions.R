library(ale)
library(here)
library(dplyr)
library(purrr)
library(stringr)
library(workflows)
dir <- here("7_var_imp", "output")
read_dir <- here("5_model_estimation", "output", "all")

################################################################################
# Read in the set-up data and the models.
################################################################################
df_reconstructed_values <- readRDS(file.path(dir, "df_for_prediction.rds"))

files_model_wfs <-
    list.files(
        read_dir, pattern = "^fitted_model_(xgboost|ols)", full.names = T
    )

model_names <- str_extract(files_model_wfs, "xgboost[0-9]|ols[0-9]")
model_wfs <- map(files_model_wfs, readRDS) |> set_names(model_names)

################################################################################
# Clean variables in the training data set.
################################################################################
# Variables with range [0, 100] which need to be transformed into [0, 1].
vars01_t <-
    c(
        "grossRentPrcntIncome_median_est",
        "grossMortgagePrcntIncome_median_est", "hhCostsPrcntIncome_median_est",
        "hhShareOfIncome_1stQuintile_est", "hhShareOfIncome_2ndQuintile_est",
        "hhShareOfIncome_3rdQuintile_est", "hhShareOfIncome_4thQuintile_est",
        "hhShareOfIncome_top5Prcnt_est", "poverty_prcnt_est_0to17",
        "poverty_prcnt_est_allAges"
    )

# Variables with the range [0, 1].
vars01 <-
    c(
        "receiveCpaSnapSSI_prcnt_est_ages17AndYounger",
        "notInSchoolOrLf_prcnt_est_ages16to19",
        "liveInSingleParentHh_prcnt_est_ages17AndYounger",
        "hhCostsBetween10and15PrcntIncome_prcnt_est",
        "hhCostsBetween15and20PrcntIncome_prcnt_est",
        "hhCostsBetween20and25PrcntIncome_prcnt_est",
        "hhCostsBetween25and30PrcntIncome_prcnt_est",
        "hhCostsBetween30and35PrcntIncome_prcnt_est",
        "hhCostsBetween35and40PrcntIncome_prcnt_est",
        "hhCostsBetween40and50PrcntIncome_prcnt_est",
        "hhCostsAbove50PrcntIncome_prcnt_est", "hhNoPhone_prcnt_est",
        "hhKitchenProblems_prcnt_est", "hhBathroomProblems_prcnt_est",
        "hhBetweeen51and100PersonPerRoom_prcnt_est",
        "hhBetweeen101and150PersonPerRoom_prcnt_est",
        "hhBetweeen151and200PersonPerRoom_prcnt_est",
        "hhAbove200PersonPerRoom_prcnt_est", "hhWith1Problem_prcnt_est",
        "hhWith2Problem_prcnt_est", "hhWith3Problem_prcnt_est",
        "hhWith4Problem_prcnt_est", "hhLessThan50Value_prcnt_est",
        "hhBetween50And100Value_prcnt_est", "hhBetween100And150Value_prcnt_est",
        "hhBetween150And200Value_prcnt_est",
        "hhBetween200And300Value_prcnt_est",
        "hhBetween300And500Value_prcnt_est",
        "hhBetween500And1000Value_prcnt_est", "giniCoefficient_index_est",
        "movedPastYear_prcnt_est_1AndOlder", "hhOwnsNoVehicle_prcnt_est",
        "hhOwns1Vehicle_prcnt_est", "hhOwns2Vehicle_prcnt_est",
        "hhOwns3Vehicle_prcnt_est", "hhOwns4Vehicle_prcnt_est",
        "hhReceiveCpaSnap_prcnt_est", "lessThanHs_prcnt_est_25older",
        "hs_prcnt_est_25older", "someCollege_prcnt_est_25older",
        "gini_simpson_index_hhIncome", "shannon_index_scaled_hhIncome",
        "lfpr_prcnt_est_16andOlder", "epr_prcnt_est_16andOlder",
        "marriageNotSeparated_prcnt_est_15andOlder",
        "everMarried_prcnt_est_15andOlder", "vacantHousingUnits_prcnt_est",
        "liveInRental_prcnt_est_allAges",
        "ratioIncomeToPovertyBelow75_prcnt_est_povertyUniverse",
        "ratioIncomeToPoverty75to99_prcnt_est_povertyUniverse",
        "ratioIncomeToPoverty100to124_prcnt_est_povertyUniverse",
        "ratioIncomeToPoverty125to149_prcnt_est_povertyUniverse",
        "ratioIncomeToPoverty150to174_prcnt_est_povertyUniverse",
        "ratioIncomeToPoverty175to199_prcnt_est_povertyUniverse",
        "singleMom_prcnt_est", "singleParent_prcnt_est", "gini_simpson_index",
        "shannon_index_scaled", "pop_prcnt_est_allAges_w",
        "pop_prcnt_est_allAges_b", "pop_prcnt_est_allAges_h",
        "pop_prcnt_est_15to24_allRaces_m", "pop_prcnt_est_15to24_b_m",
        "pop_prcnt_est_15to24_h_m", "governor", "president", "senate",
        "ur_prcnt_est_16andOlder", "delta_index_black", "delta_index_hisp",
        "delta_index_white", "delta_index_edu", "delta_index_income",
        "dissimilarity_index_edu", "dissimilarity_index_hhIncome",
        "dissimilarity_index_WhiteHisp", "dissimilarity_index_WhiteBlack",
        "multigroup_entropy_edu", "multigroup_entropy_hhIncome",
        "multigroup_entropy_race", "norm_exposure_index_edu",
        "norm_exposure_index_hhIncome", "norm_exposure_index_WhiteBlack",
        "norm_exposure_index_WhiteHisp", "gini_simpson_index_edu",
        "shannon_index_scaled_edu", "hhIncomebelow30_prcnt_est",
        "hhIncomefrom30to60_prcnt_est", "hhIncomefrom60to100_prcnt_est",
        "hhIncomefrom100to150_prcnt_est"
    )

# Variables w/ range [-10000, 10000] which need to be transformed into [-1, 1].
vars11_t <-
    c(
        "blackToWhiteUnmarriedBirthRate_diff_est",
        "hispToWhiteUnmarriedBirthRate_diff_est"
    )

# Variables with range [-1, 1].
vars11 <-
    c(
        "blackToWhiteChildPoverty_diff_est", "hispToWhiteChildPoverty_diff_est",
        "blackToWhiteCollege_diff_est", "hispToWhiteCollege_diff_est",
        "lfprBlackToWhite_diff_est_16andOlder",
        "urBlackToWhite_diff_est_16andOlder",
        "lfprHispToWhite_diff_est_16andOlder",
        "urHispToWhite_diff_est_16andOlder", "blackToWhiteMarried_diff_est",
        "blackToWhiteEverMarried_diff_est", "hispToWhiteMarried_diff_est",
        "hispToWhiteEverMarried_diff_est", "blackToWhiteMovers_diff_est",
        "hispToWhiteMovers_diff_est", "ratioBlackWhiteOvercrowding_diff_est",
        "ratioHispWhiteOvercrowding_diff_est", "blackToWhiteSingleMom_diff_est",
        "hispToWhiteSingleMom_diff_est", "blackToWhiteSingleParent_diff_est",
        "hispToWhiteSingleParent_diff_est", "blackToWhitePoverty_diff_est",
        "hispToWhitePoverty_diff_est", "hhSnapBlackToWhite_diff_est",
        "hhSnapHispToWhite_diff_est", "blackToWhiteRenters_diff_est",
        "hispToWhiteRenters_diff_est"
    )

# Variables with range [-1, Inf) to be transformed into [0, Inf).
varsInf_t <-
    c(
        "medianHhIncomeBlackToWhite_prcnt_est",
        "medianHhIncomeHispToWhite_prcnt_est"
    )

# Variables with range [0, Inf).
varsInf <-
    c(
        "homeValue_25thPtile_est", "homeValue_median_est",
        "homeValue_75thPtile_est", "yearMoved_median_est",
        "nr_businesses_per10k", "nr_socialAssociations_min_per10k",
        "nr_socialSupportServices_min_per10k",
        "nr_allSocialBeautyBusinesses_min_per10k",
        "nr_socialAssociations_max_per10k",
        "nr_socialSupportServices_max_per10k",
        "nr_allSocialBeautyBusinesses_max_per10k", "pop_nr_est",
        "lq_black", "lq_white", "lq_hisp", "pop_density", "hhIncome_median_est",
        "lq_governor", "lq_president", "lq_senate",
        "spatial_proximity_edu_b001", "spatial_proximity_edu_b01",
        "spatial_proximity_income_b01", "spatial_proximity_income_b001",
        "spatial_proximity_race_b001", "spatial_proximity_race_b01",
        "nr_homicides_max_per100k", "nr_homicides_min_per100k",
        "nr_homicides_max_3yr_avg_per100k", "nr_homicides_min_3yr_avg_per100k",
        "nr_homicides_max_5yr_avg_per100k", "nr_homicides_min_5yr_avg_per100k",
        "nr_new_businesses_per10k", "violent_crime_per100k",
        "property_crime_per100k", "all_crimes_per100k"
    )

# Variables which do not need to be transformed.
vars_noT <-
    c(
        "hou_chamber", "sen_chamber", "hou_dem", "hou_rep", "hou_dem_mean",
        "hou_rep_mean", "sen_dem", "sen_rep", "sen_dem_mean", "sen_rep_mean",
        "h_distance", "s_distance", "h_dem_sd", "s_dem_sd", "h_rep_sd",
        "s_rep_sd", "h_chamber_sd", "s_chamber_sd"     
    )

# Used for variables bounded by [0, 1] (transform back to original scale).
logit_untransf <- function(col) {exp(col) / (1 + exp(col))}

################################################################################
# Make predictions using ALE.
################################################################################
df_predictions <-
    df_reconstructed_values |>
    mutate(
        ale_xgboost =
            pmap(
                list(df_ale, ale_predict_func, ale_pc1_var_name),
                function(data_arg, pred_fun_arg, xvar_arg, model_arg) {
                    print(xvar_arg)
                    start <- Sys.time()
                    ale <-
                        ALE(
                            data = data_arg,
                            model = model_arg,
                            pred_fun = pred_fun_arg,
                            x_cols = xvar_arg,
                            output_stats = F,
                            parallel = 0,
                            max_num_bins = 30
                        )
                    end <- Sys.time()
                    print(end - start)
                    return(ale)
                },
                model_arg = model_wfs[["xgboost1"]]
            ),
        ale_ols =
            pmap(
                list(df_ale, ale_predict_func, ale_pc1_var_name),
                function(data_arg, pred_fun_arg, xvar_arg, model_arg) {
                    print(xvar_arg)
                    start <- Sys.time()
                    ale <-
                        ALE(
                            data = data_arg,
                            model = model_arg,
                            pred_fun = pred_fun_arg,
                            x_cols = xvar_arg,
                            output_stats = F,
                            parallel = 0,
                            max_num_bins = 30
                        )
                    end <- Sys.time()
                    print(end - start)
                    return(ale)
                },
                model_arg = model_wfs[["ols1"]]
            )
    ) |>
    select(variable, variable_dirty, ale_xgboost, ale_ols)

saveRDS(df_predictions, file.path(dir, "ale_predictions.rds"))
