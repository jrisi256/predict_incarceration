library(here)
library(dplyr)
library(purrr)
library(tibble)
library(rsample)
source(here("functions.R"))
read_dir <- here("5_model_estimation", "output", "all")
out_dir <- here("7_var_imp", "output")

################################################################################
# Read in train data, clusters, and models.
################################################################################
split <- readRDS(file.path(read_dir, "split.rds"))
train <- training(split)

df_hcluster06 <-
    readRDS(here("7_var_imp", "output", "list_hcluster06.rds")) |>
    enframe(name = "group_06", value = "variable") |>
    rename(variables = variable, variable = group_06) |>
    mutate(variable_dirty = variable) |>
    std_var_names() |>
    mutate(
        variable =
            pmap(
                list(variables, variable),
                function(vs, v) {
                    if_else(length(vs) > 1, paste0(v, " (cluster)"), v)
                }
            ) |>
            unlist()
    )

df_variable_importance <-
    readRDS(here("7_var_imp", "output", "df_variable_importance.rds"))

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

# Used to squeeze variables with 0s and 1s so we can use the logit function.
smithson_verkuilen_squeeze_01 <- function(col, n) {(col * (n - 1) + 0.5) / n}

# Used for variables bounded by [0, 1] (PCA may give out-of-bound values).
logit_transf <- function(col) {log(col / (1 - col))}

# Squeeze vars. w/ -1s and 1s to use inverse hyperb. tan. (aka Fisher's Transf).
smithson_verkuilen_squeeze_11 <- function(col, n) {col * ((n - 1) / n)}

# Used for variables bounded by [-1, 1] (PCA may give out-of-bound values).
fisher_transf <- function(col) {0.5 * log((1 + col) / (1 - col))}

# Used for variables bounded by [0, Inf] (PCA may give out-of-bound values).
arcSinH_transf <- function(col) {log(col + sqrt(col ^ 2 + 1))}

train_clean <-
    train |>
    mutate(
        across(all_of(vars01_t), function(col) {col = col / 100}),
        across(all_of(vars11_t), function(col) {col = col / 10000}),
        across(all_of(varsInf_t), function(col) {col = col + 1}),
        # Squeeze then logit.
        across(
            all_of(c(vars01, vars01_t)),
            function(col) {logit_transf(smithson_verkuilen_squeeze_01(col, n()))}
        ),
        # Squeeze then fisher.
        across(
            all_of(c(vars11, vars11_t)),
            function(col) {fisher_transf(smithson_verkuilen_squeeze_11(col, n()))}
        ),
        # No need to squeeze, only apply the transformation.
        across(
            all_of(c(varsInf, varsInf_t)), function(col) {arcSinH_transf(col)}
        )
    )

################################################################################
# Estimate PCA models + reconstruct true values for each variable based on PC1.
################################################################################
make_custom_predict_func <- function(pca_model, cluster_vars) {
    function(object, newdata, type = NULL) {
        # Extract the perturbed PC1 values generated by the ale package.
        pc1_values <- newdata$PC1
        
        # Hold PC2 and PC3 constant at 0 (which is the mean).
        Z <- matrix(0, nrow = nrow(newdata), ncol = length(cluster_vars))
        Z[, 1] <- pc1_values
        
        # Eigenvectors (how much each var. contributes to PC1).
        # Z = XW, Z is PCs, X is orig. data, W is rotation matrix.
        # Have Z and W, need X, we multiply by inverse (transpose).
        reconst <- Z %*% t(pca_model$rotation)
        
        # Reverse scaling by multiplying by std. dev.
        reconst_raw_sd <- sweep(reconst, 2, pca_model$scale, "*")
        
        # Reverse centering by adding mean.
        reconst_raw_mean <- sweep(reconst_raw_sd, 2, pca_model$center, "+")
        reconst_df <- as_tibble(reconst_raw_mean)
        colnames(reconst_df) <- cluster_vars
        
        for(col in colnames(reconst_df)) {
            if(col %in% c(vars01, vars01_t)) {logit_untransf(reconst_df[[col]])}
            else if(col %in% c(vars11, vars11_t)) {tanh(reconst_df[[col]])}
            # Still might get values 0, simple fix.
            else if (col %in% varsInf) {pmax(0, sinh(reconst_df[[col]]))}
            # Similarly, might get values below -1. Simple fix.
            else if (col %in% varsInf_t) {pmax(-1, sinh(reconst_df[[col]]) - 1)}
            else if (col %in% vars01_t) {reconst_df[[col]] * 100}
            else if (col %in% vars11_t) {reconst_df[[col]] * 10000}
        }
        
        # Add back in reconstructed data.
        reconst_df <- bind_cols(select(newdata, -PC1), reconst_df)
        
        # Make predictions.
        preds <- predict(object, new_data = reconst_df)
        return(preds$.pred)
    }
}

pred_func <- function(object, newdata, type = NULL) {
    preds <- predict(object, new_data = newdata)
    return(preds$.pred)
}

df_pca_analysis <-
    df_hcluster06 |>
    filter(variable %in% df_variable_importance$variable) |>
    rowwise() |>
    filter(length(variables) != 1) |>
    ungroup() |>
    mutate(
        pca_model =
            map(
                variables,
                function(cols, df) {prcomp(df[, cols], center = T, scale = T)},
                df = train_clean
            ),
        pc1 = map(pca_model, function(m) {m$x[, "PC1"]}),
        ale_pc1_var_name = "PC1",
        ale_predict_func =
            pmap(list(pca_model, variables), make_custom_predict_func),
        df_ale =
            pmap(
                list(pc1, variables),
                function(pc, vars, df) {
                    df |> mutate(PC1 = pc) |> select(-all_of(vars))
                },
                df = train_clean
            ),
        pc1_loadings =
            map(
                pca_model,
                function(m) {
                    matrix <- m$rotation[, "PC1"]
                    tibble(variable = names(matrix), loading = unname(matrix))
                }
            ),
        pc1_pv =
            map(
                pca_model,
                function(m) {
                    summary(m)$importance[, "PC1"][["Proportion of Variance"]]
                }
            ),
        # Get seq. of PC1s which will be turned back to orig. data.
        pc1_seq =
            map(
                pc1,
                function(pc) {pc1_seq = seq(min(pc), max(pc), length.out = 30)}
            ),
        reconstructed_values =
            pmap(
                list(pc1_seq, variables, pca_model),
                function(pc_seq, cols, m) {
                    # Hold PC2 and PC3 constant at 0 (which is the mean).
                    Z <- matrix(0, nrow = length(pc_seq), ncol = length(cols))
                    Z[, 1] <- pc_seq
                    
                    # Eigenvectors (how much each var. contributes to PC1).
                    # Z = XW, Z is PCs, X is orig. data, W is rotation matrix.
                    # Have Z and W, need X, we multiply by inverse (transpose).
                    reconst <- Z %*% t(m$rotation)
                    
                    # Reverse scaling by multiplying by std. dev.
                    reconst_raw_sd <- sweep(reconst, 2, m$scale, "*")
                    
                    # Reverse centering by adding mean.
                    reconst_raw_mean <- sweep(reconst_raw_sd, 2, m$center, "+")
                    return(as_tibble(reconst_raw_mean))
                }
            )
    )

# Variables which are not in clusters do not need to have PCA analysis done.
df_reconstructed_values_noCluster <-
    df_hcluster06 |>
    filter(variable %in% df_variable_importance$variable) |>
    rowwise() |>
    filter(length(variables) == 1) |>
    ungroup() |>
    mutate(
        pc1 = map(variable_dirty, function(var, df) {df[[var]]}, df = train),
        df_ale = list(train),
        ale_pc1_var_name = unlist(variable_dirty),
        ale_predict_func = list(pred_func),
        reconstructed_values_orig =
            map(
                variables,
                function(col, df) {
                    tibble(
                        "{col}" :=
                            seq(min(df[[col]]), max(df[[col]]), length.out = 30)
                    )
                },
                df = train
            ),
        pc1_seq =
            map(reconstructed_values_orig, function(df) {unname(unlist(df))})
    )

# Used for variables bounded by [0, 1] (transform back to original scale).
logit_untransf <- function(col) {exp(col) / (1 + exp(col))}

df_reconstructed_values <-
    df_pca_analysis |>
    mutate(
        reconstructed_values_orig =
            map(
                reconstructed_values,
                function(df) {
                    df |>
                        mutate(
                            across(
                                any_of(c(vars01, vars01_t)),
                                function(c) {logit_untransf(c)}
                            ),
                            across(
                                any_of(c(vars11, vars11_t)),
                                function(c) {tanh(c)}
                            ),
                            # Still might get values 0, simple fix.
                            across(
                                any_of(c(varsInf)),
                                function(c) {pmax(0, sinh(c))}
                            ),
                            # Similarly, might get values below -1. Simple fix.
                            across(
                                any_of(varsInf_t),
                                function(c) {pmax(-1, sinh(c) - 1)}
                            ),
                            across(any_of(vars01_t), function(col) {col * 100}),
                            across(any_of(vars11_t), function(c) {c * 10000})
                        )
                }
            )
    ) |>
    bind_rows(df_reconstructed_values_noCluster)

saveRDS(df_reconstructed_values, file.path(out_dir, "df_for_prediction.rds"))
