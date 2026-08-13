library(here)
library(purrr)
library(dplyr)
library(stringr)
library(rsample)
library(DALEXtra)
library(workflows)
args <- commandArgs(trailingOnly = T)
#args <- c(1)
read_dir <- here("5_model_estimation", "output", "all")
out_dir <- here("7_var_imp", "output", "perm_importance")
if(!dir.exists(out_dir)) {dir.create(out_dir, recursive = T)}

################################################################################
# Read in models, test data, and variable groupings.
################################################################################
files_xgboost <- list.files(read_dir, pattern = "model_xgboost", full.names = T)
names_xgboost <- str_extract(files_xgboost, "xgboost[0-9]")

model_ols <- readRDS(file.path(read_dir, "fitted_model_ols1.rds"))
models_xgboost <- map(files_xgboost, readRDS)
models <-
    as.list(c(models_xgboost, list(model_ols))) |>
    set_names(c(names_xgboost, "ols1"))

split <- readRDS(file.path(read_dir, "split.rds"))
test <- testing(split)

list_hcluster06 <- readRDS(here("7_var_imp", "output", "list_hcluster06.rds"))
list_hcluster07 <- readRDS(here("7_var_imp", "output", "list_hcluster07.rds"))

################################################################################
# Create bootstrapped test set.
################################################################################
set.seed(args[1])
test_boot <- test |> slice_sample(n = nrow(test), replace = T)

################################################################################
# Turn into explainer objects.
################################################################################
explainers <-
    pmap(
        list(models, names(models)),
        function(model, model_name, test_df) {
            explain_tidymodels(
                model,
                data = test_df,
                y = test_df$total_prison_adm_rate15to64,
                label = model_name
            )
        },
        test_df = test_boot
    )

################################################################################
# Calculate permutation importance.
################################################################################
vars <-
    colnames(test)[
        !str_detect(colnames(test), "full_fips|^county$|^state$|^year$|prison")
    ]

perm_importance <-
    map(
        explainers,
        function(explainer, vars_arg, seed) {
            set.seed(seed)
            model_parts(
                explainer,
                variables = vars_arg,
                B = 1,
                type = "ratio",
                N = NULL
            )
        },
        vars_arg = vars,
        seed = args[1]
    ) |>
    bind_rows() |>
    filter(!(variable %in% c("_full_model_", "_baseline_"))) |>
    mutate(permutation = args[1])

saveRDS(perm_importance, file.path(out_dir, paste0("perm_", args[1], ".rds")))

perm_importance07 <-
    map(
        explainers,
        function(explainer, vars_arg, seed, var_group) {
            set.seed(seed)
            model_parts(
                explainer,
                variables = vars_arg,
                variable_groups = var_group,
                B = 1,
                type = "ratio",
                N = NULL
            )
        },
        vars_arg = vars,
        seed = args[1],
        var_group = list_hcluster07
    ) |>
    bind_rows() |>
    filter(!(variable %in% c("_full_model_", "_baseline_"))) |>
    mutate(permutation = args[1])

saveRDS(perm_importance07, file.path(out_dir, paste0("perm07_", args[1], ".rds")))

perm_importance06 <-
    map(
        explainers,
        function(explainer, vars_arg, seed, var_group) {
            set.seed(seed)
            model_parts(
                explainer,
                variables = vars_arg,
                variable_groups = var_group,
                B = 1,
                type = "ratio",
                N = NULL
            )
        },
        vars_arg = vars,
        seed = args[1],
        var_group = list_hcluster06
    ) |>
    bind_rows() |>
    filter(!(variable %in% c("_full_model_", "_baseline_"))) |>
    mutate(permutation = args[1])

saveRDS(perm_importance06, file.path(out_dir, paste0("perm06_", args[1], ".rds")))
