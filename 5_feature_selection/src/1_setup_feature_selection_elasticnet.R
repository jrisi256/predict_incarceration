library(here)
library(tune)
library(readr)
library(broom)
library(dplyr)
library(dials)
library(rsample)
library(parsnip)
library(recipes)
library(yardstick)
library(workflows)
args <- commandArgs(trailingOnly = T)
#args <- c("all", "data_no_missing.csv", 10, "total_prison_adm_rate15to64")
#args <- c("black", "data_no_missing_black.csv", 5, "black_to_white_prison_adm_rate")
#args <- c("hisp", "data_no_missing_hisp.csv", 5, "hisp_to_white_prison_adm_rate")
save_dir <- here("5_feature_selection", "output", args[1])
if(!dir.exists(save_dir)) {dir.create(save_dir)}

################################################################################
# Read in full data.
data <- read_csv(here("4_clean_missing_data", "output", args[2]))

################################################################################
# Set train and test sets.
set.seed(69)
split <- group_initial_split(data, prop = 0.8, group = full_fips)
saveRDS(split, file.path(save_dir, "split.rds"))
train <- training(split)
test <- testing(split)

################################################################################
# Set model and engine.
lmreg_model <-
    linear_reg(penalty = tune(), mixture = tune()) |>
    set_engine("glmnet")

################################################################################
# Set up cross-validation.
lmreg_hyperparameters <-
    lmreg_model |>
    extract_parameter_set_dials() |>
    update(mixture = mixture(c(0, 1)), penalty = penalty(c(-6, 0.5)))

set.seed(420)
grid_lhcube <-
    lmreg_hyperparameters |>
    grid_space_filling(size = 1000, type = "latin_hypercube")
saveRDS(grid_lhcube, file.path(save_dir, "grid_lhcube.rds"))

set.seed(1102)
vfold <- group_vfold_cv(train, group = full_fips, v = as.numeric(args[3]), repeats = 3)
saveRDS(vfold, file.path(save_dir, "vfold.rds"))

################################################################################
# Set recipe and data pre-processing steps and create workflow.
recipe_prison_admissions <-
    recipe(train) |>
    update_role(args[4], new_role = "outcome") |>
    update_role(
        -matches("^state$|^county$|full_fips|^year$|prison"),
        new_role = "predictor"
    ) |>
    update_role(
        matches("^state$|^county$|full_fips|^year$"),
        new_role = "id"
    ) |>
    step_normalize(all_predictors())

lmreg_workflow <-
    workflow() |>
    add_model(lmreg_model) |>
    add_recipe(recipe_prison_admissions)
saveRDS(lmreg_workflow, file.path(save_dir, "lmreg_workflow.rds"))
