library(here)
library(tune)
library(broom)
library(dials)
library(rsample)
library(parsnip)
library(recipes)
library(yardstick)
library(workflows)

################################################################################
# Load in objects necessary to perform cross-validation.
#args <- commandArgs(trailingOnly = T)
args <- c("hisp", 1)
save_dir <- here("5_feature_selection", "output", args[1])
grid_lhcube <- readRDS(file.path(save_dir, "grid_lhcube.rds"))
lmreg_workflow <- readRDS(file.path(save_dir, "lmreg_workflow.rds"))
vfold <- readRDS(file.path(save_dir, "vfold.rds"))
vfold_sub <- vfold[1:2, ]
class(vfold_sub) <- class(vfold)

################################################################################
# Perform cv and save model results to ensure stability of coefficients.
model_metrics <- metric_set(rmse, mae)

extract_coef <- function(workflow_arg) {
    workflow_arg |> extract_fit_parsnip() |> tidy()
}

log_file <- file(file.path(save_dir, paste0("log_", args[2], ".txt")), open = "w")
sink(log_file, type = "output")
sink(log_file, type = "message")

cross_validation <-
    lmreg_workflow |>
    tune_grid(
        vfold_sub,
        grid = grid_lhcube,
        metrics = model_metrics,
        control = control_grid(verbose = T, extract = extract_coef)
    )

sink(type = "message")
sink(type = "output")
close(log_file)

saveRDS(
    cross_validation, file.path(save_dir, paste0("cross_validation_", args[2], ".rds"))
)
