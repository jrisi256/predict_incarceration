#!/bin/bash

# Args:
# folder name to store results
# data to load
# number of folds
# outcome column name

Rscript -e 'renv::run("1_setup_feature_selection_elasticnet.R", args = c("all", "data_no_missing.csv", 10, "total_prison_adm_rate15to64"), project = "~/Documents/dissertation")'
Rscript -e 'renv::run("1_setup_feature_selection_elasticnet.R", args = c("black", "data_no_missing_black.csv", 5, "black_to_white_prison_adm_rate"), project = "~/Documents/dissertation")'
Rscript -e 'renv::run("1_setup_feature_selection_elasticnet.R", args = c("hisp", "data_no_missing_hisp.csv", 5, "hisp_to_white_prison_adm_rate"), project = "~/Documents/dissertation")'