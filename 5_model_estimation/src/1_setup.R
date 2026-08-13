library(here)
library(tune)
library(readr)
library(broom)
library(dplyr)
library(dials)
library(bonsai)
library(rsample)
library(parsnip)
library(recipes)
library(ggplot2)
library(baguette)
library(yardstick)
library(workflows)
library(workflowsets)

args <- c("all", "data_no_missing.csv", 10, "total_prison_adm_rate15to64")
save_folder <- args[1]
table_name <- args[2]
nr_folds <- as.numeric(args[3])
outcome <- args[4]
save_dir <- here("5_model_estimation", "output", save_folder)
if(!dir.exists(save_dir)) {dir.create(save_dir)}

################################################################################
# Read in full data.
data <- read_csv(here("4_clean_missing_data", "output", table_name))

################################################################################
# Set train and test sets.
set.seed(69)
split <- group_initial_split(data, prop = 0.8, group = full_fips)
saveRDS(split, file.path(save_dir, "split.rds"))
train <- training(split)
test <- testing(split)

################################################################################
# Create data processing recipe.
recipe <-
    recipe(train) |>
    update_role(all_of(outcome), new_role = "outcome") |>
    update_role(
        -matches("^state$|^county$|full_fips|^year$|prison"),
        new_role = "predictor"
    ) |>
    update_role(
        matches("^state$|^county$|full_fips|^year$"),
        new_role = "id"
    ) |>
    step_normalize(all_predictors())

################################################################################
# Set-up models + engines.
spec_ols <- linear_reg() |> set_engine("lm") |> set_mode("regression")

spec_lmreg <-
    linear_reg(penalty = tune(), mixture = tune()) |>
    set_engine("glmnet") |>
    set_mode("regression")

spec_bagMars <-
    bag_mars(
        prod_degree = tune(), prune_method = "backward", num_terms = tune()
    ) |>
    set_engine("earth", times = 50) |>
    set_mode("regression")

spec_rf <-
    rand_forest(trees = 500, min_n = tune(), mtry = tune()) |>
    set_engine("ranger") |>
    set_mode("regression")

spec_crf <-
    rand_forest(trees = 500, min_n = tune(), mtry = tune()) |>
    set_engine("partykit") |>
    set_mode("regression")

spec_orf <-
    rand_forest(trees = 300, min_n = tune(), mtry = tune()) |>
    set_engine("aorsf", split_min_stat = tune()) |>
    set_mode("regression")

spec_boost <-
    boost_tree(
        trees = 500, stop_iter = 50, tree_depth = tune(), learn_rate = tune(),
        min_n = tune(), loss_reduction = tune(), sample_size = tune()
    ) |>
    set_engine("xgboost", colsample_bytree = tune(), counts = F) |>
    set_mode("regression")

################################################################################
# Set up search space for cross-validation.
set.seed(420)

grid_lhcube_lmreg <-
    spec_lmreg |>
    extract_parameter_set_dials() |>
    update(mixture = mixture(c(0, 1)), penalty = penalty(c(-6, 1))) |>
    grid_space_filling(size = 200, type = "latin_hypercube")

grid_lhcube_bagMars <-
    spec_bagMars |>
    extract_parameter_set_dials() |>
    update(
        num_terms = num_terms(c(18, 184)), prod_degree = prod_degree(c(1, 2))
    ) |>
    grid_space_filling(size = 20, type = "latin_hypercube")

grid_lhcube_rf <-
    spec_rf |>
    extract_parameter_set_dials() |>
    update(
        mtry = mtry_prop(range = c(0.1, 0.3)),
        min_n = min_n(range = c(2, 60))
    ) |>
    grid_space_filling(size = 100, type = "latin_hypercube")

grid_lhcube_crf <-
    spec_crf |>
    extract_parameter_set_dials() |>
    update(mtry = mtry_prop(range = c(0.1, 0.3))) |>
    grid_space_filling(size = 50, type = "latin_hypercube")

grid_lhcube_orf <-
    spec_orf |>
    extract_parameter_set_dials() |>
    update(
        mtry = mtry(range = c(18, 62)),
        split_min_stat =
            new_quant_param(
                type = "double",
                range = c(0, 1),
                inclusive = c(T, T),
                label = c(split_min_stat = "Minimum Split Statistic")
            )
    ) |>
    grid_space_filling(size = 75, type = "latin_hypercube")

grid_lhcube_boost <-
    spec_boost |>
    extract_parameter_set_dials() |>
    update(colsample_bytree = mtry_prop(range = c(0.1, 0.3))) |>
    grid_space_filling(size = 150, type = "latin_hypercube")

set.seed(42)
vfold <- group_vfold_cv(train, group = full_fips, v = nr_folds, repeats = 1)
saveRDS(vfold, file.path(save_dir, "vfold.rds"))

################################################################################
workflow_set <-
    workflow_set(
        preproc = list(standardized = recipe),
        models =
            list(
                ols = spec_ols, glmnet = spec_lmreg, bagMars = spec_bagMars,
                rf = spec_rf, crf = spec_crf, orf = spec_orf,
                xgboost = spec_boost
            )
    ) |>
    option_add(id = "standardized_glmnet", grid = grid_lhcube_lmreg) |>
    option_add(id = "standardized_bagMars", grid = grid_lhcube_bagMars) |>
    option_add(id = "standardized_rf", grid = grid_lhcube_rf) |>
    option_add(id = "standardized_crf", grid = grid_lhcube_crf) |>
    option_add(id = "standardized_orf", grid = grid_lhcube_orf) |>
    option_add(id = "standardized_xgboost", grid = grid_lhcube_boost)

saveRDS(workflow_set, file.path(save_dir, "workflow_set.rds"))
