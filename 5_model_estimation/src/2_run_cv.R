library(here)
library(tune)
library(broom)
library(dials)
library(aorsf)
library(bonsai)
library(rsample)
library(parsnip)
library(recipes)
library(baguette)
library(partykit)
library(yardstick)
library(workflows)
library(workflowsets)

################################################################################
# Load in objects necessary to perform cross-validation.
args <- commandArgs(trailingOnly = T)
#args <- c("all", "standardized_bagMars", 1:2)
save_folder <- args[1]
model <- args[2]
fold <- as.numeric(args[3:length(args)])
fold_str <- paste(fold, collapse = "-")
save_dir <- here("5_model_estimation", "output", args[1])

workflow_set <-
    readRDS(file.path(save_dir, "workflow_set.rds")) |>
    filter(wflow_id == model)

vfold <- readRDS(file.path(save_dir, "vfold.rds"))
vfold_sub <- vfold[fold, ]
class(vfold_sub) <- class(vfold)
rm(vfold)

################################################################################
# Perform cross validation.
model_metrics <- metric_set(rmse, mae)
options(future.globals.maxSize = Inf)

log_file <-
    file(
        file.path(save_dir, paste0("log_", fold_str, "_", model, ".txt")),
        open = "w"
    )

sink(log_file, type = "output")
sink(log_file, type = "message")

start_time <- Sys.time()
cross_validation <-
    workflow_set |>
    workflow_map(
        fn = "tune_grid",
        seed = 1106,
        resamples = vfold_sub,
        metrics = model_metrics,
        control = control_grid(verbose = T)
    )
end_time <- Sys.time()
run_time <- end_time - start_time

sink(type = "message")
sink(type = "output")
close(log_file)

saveRDS(
    cross_validation,
    file.path(save_dir, paste0("cv_", fold_str, "_", model, ".rds"))
)
