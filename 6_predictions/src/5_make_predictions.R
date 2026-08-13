library(here)
library(purrr)
library(dplyr)
library(earth)
library(rsample)
library(stringr)
library(baguette)
library(workflows)
library(yardstick)
read_dir <- here("5_model_estimation", "output", "all")
save_dir <- here("6_predictions", "output")

################################################################################
# Read in models and data.
################################################################################
model_files <- list.files(read_dir, pattern = "^fitted_model", full.names = T)
model_names <- str_extract(model_files, "fitted_model_.*")
split <- readRDS(file.path(read_dir, "split.rds"))
test <- testing(split)
model_metrics <- metric_set(rmse, mae, rsq)

pwalk(
    list(model_files, model_names),
    function(path_to_file, model_name, test_df, eval_func, dir) {
        # Read in model.
        model <- readRDS(path_to_file)
        model_col <- str_remove_all(model_name, ".rds")
            
        # Make predictions.
        preds <-
            predict(model, test_df) |>
            rename("{model_col}" := .pred) |>
            bind_cols(
                select(
                    test_df,
                    matches("^state$|^county$|_fips|^year$|total_prison")
                )
            )
            
        saveRDS(preds, file.path(dir, paste0("predictions_", model_name)))
            
        # Evaluate predictive performance.
        performance <-
            eval_func(
                preds,
                truth = "total_prison_adm_rate15to64",
                estimate = model_col
            ) |>
            select(-.estimator) |>
            mutate(model = model_col)
        
        saveRDS(performance, file.path(dir, paste0("performance_", model_name)))
    },
    test_df = test,
    eval_func = model_metrics,
    dir = save_dir
)
