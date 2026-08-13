library(here)
library(purrr)
library(bonsai)
library(stringr)
library(recipes)
library(rsample)
library(baguette)
library(workflows)
set.seed(1102)
options(future.globals.maxSize = Inf)

################################################################################
# Read in workflows and data.
read_dir <- here("5_model_estimation", "output", "all")
workflows <- readRDS(file.path(read_dir, "workflows_final.rds"))
split <- readRDS(file.path(read_dir, "split.rds"))
train <- training(split)
args <-
    commandArgs(trailingOnly = T) %>%
    paste("^", ., "$", collapse = "|", sep = "")

# args <-
#     c("ols_1", "glmnet_1", "glmnet_2", "glmnet_3", "glmnet_4", "glmnet_5") %>%
#     paste("^", ., "$", collapse = "|", sep = "")

################################################################################
# Fit models on training data.
target_workflows <- workflows[str_detect(names(workflows), args)]
rm(workflows)
rm(split)
gc()

pwalk(
    list(target_workflows, names(target_workflows)),
    function(workflow, name, train_df, dir) {
        print(paste0("Starting ", name))
        start_time <- Sys.time()
        fitted_model <- fit(workflow, data = train_df)
        end_time <- Sys.time()
        run_time <- end_time - start_time
            
        print(paste0("Finished: ", name))
        print(paste0("Estimation time: ", run_time, " ", units(run_time)))
        print(paste0("Finished at ", Sys.time()))
        
        print("Saving model...")
        saveRDS(
            fitted_model,
            file.path(
                dir,
                paste0("fitted_model_", str_remove_all(name, "_"), ".rds")
            )
        )
        cat("\n")
        gc()
    },
    train_df = train,
    dir = read_dir
)
