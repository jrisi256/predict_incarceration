library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(stringr)
library(rsample)
library(workflows)
dir <- here("7_var_imp", "output")
read_dir <- here("5_model_estimation", "output", "all")

################################################################################
# Read in the set-up data, training data, and the models.
################################################################################
df_reconstructed_values <- readRDS(file.path(dir, "df_for_prediction.rds"))

files_model_wfs <-
    list.files(
        read_dir, pattern = "^fitted_model_(xgboost|ols)", full.names = T
    )

model_names <- str_extract(files_model_wfs, "xgboost[0-9]|ols[0-9]")
model_wfs <- map(files_model_wfs, readRDS) |> set_names(model_names)

split <- readRDS(file.path(read_dir, "split.rds"))
train <- training(split)

################################################################################
# Make predictions.
################################################################################
make_predictions <- function(df_seq, df_train, model_wf, model) {
    pred_df <- df_train |> select(matches("full_fips|^year$"))
    print(colnames(df_seq))
    # For each sequence in the independent variable(s)...
    for(i in 1:nrow(df_seq)) {
        print(i)
        # For each variable in our cluster...
        for (col in colnames(df_seq)) {
            # Set every value in current variable equal to current sequence.
            df_train[[col]] <- df_seq[[col]][i]
        }
        # Make predictions (w/ current sequence values for target variables).
        preds <-
            predict(model_wf[[model]], new_data = df_train) |>
            rename_with(function(col) {paste0("pred", i)})
        
        pred_df <- pred_df |> bind_cols(preds)
    }
    return(pred_df)
}

calc_prediction_stats <- function (df_pred) {
    df_pred |>
        pivot_longer(
            cols = -matches("fips|year"),
            values_to = "pred",
            names_to = "seq"
        ) |>
        summarise(
            p10 = quantile(pred, 0.1),
            p25 = quantile(pred, 0.25),
            p50 = quantile(pred, 0.5),
            p75 = quantile(pred, 0.75),
            p90 = quantile(pred, 0.9),
            mean = mean(pred),
            .by = seq
        )
}

df_predictions <-
    df_reconstructed_values |>
    mutate(
        predictions_xgboost1 = 
            map(
                reconstructed_values_orig,
                make_predictions,
                df_train = train,
                model_wf = model_wfs,
                model = "xgboost1"
            ),
        prediction_distribution_xgboost1 =
            map(predictions_xgboost1, calc_prediction_stats),
        predictions_ols =
            map(
                reconstructed_values_orig,
                make_predictions,
                df_train = train,
                model_wf = model_wfs,
                model = "ols1"
            ),
        prediction_distribution_ols =
            map(predictions_ols, calc_prediction_stats)
    )

saveRDS(df_predictions, file.path(dir, "pdp_predictions.rds"))
