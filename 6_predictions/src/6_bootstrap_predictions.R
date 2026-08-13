library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(rsample)
library(stringr)
library(yardstick)
library(workflows)
read_dir <- here("6_predictions", "output")

################################################################################
# Read in prediction data.
################################################################################
model_metrics <- metric_set(rmse, mae, rsq)
pred_files <- list.files(read_dir, pattern = "^predictions", full.names = T)
pred_df <-
    map(pred_files, readRDS) |>
    reduce(
        function(df1, df2) {
            full_join(
                df1,
                select(df2, -total_prison_adm_rate15to64),
                by = c("state", "county", "year", "full_fips")
            )
        }
    )

################################################################################
# Construct bootstrap samples and calculate performance metrics.
################################################################################
args <- commandArgs(trailingOnly = T)
#args <- c(1, 100)

# Generate bootstrap sample for this batch.
set.seed(args[1])
bootstrap_samples <- bootstraps(pred_df, times = args[2])

# Calculate performance of each model on each bootstrap sample.
bootstrap_results <-
    pmap(
        list(bootstrap_samples$splits, as.list(1:nrow(bootstrap_samples))),
        function(split, bootstrap, eval_func, seed) {
            # Retrieve bootstrap sample.
            boot_df <- analysis(split)
            
            # Calculate performance for each model in current bootstrap.
            performance_df <-
                boot_df |>
                pivot_longer(
                    cols = -matches("^state$|^county$|_fips|^year$|prison"),
                    names_to = "model",
                    values_to = "pred"
                ) |>
                group_by(model) |>
                model_metrics(
                    truth = total_prison_adm_rate15to64,
                    estimate = pred
                ) |>
                ungroup() |>
                mutate(id = paste0(seed, "_", bootstrap)) |>
                select(-.estimator)
        },
        eval_func = model_metrics,
        seed = args[1]
    ) |>
    bind_rows()

saveRDS(
    bootstrap_results,
    file.path(read_dir, paste0("bootstrap_", args[1], ".rds"))
)
