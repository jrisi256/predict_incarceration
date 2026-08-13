library(here)
library(tune)
library(purrr)
library(dplyr)
library(tidyr)
library(GGally)
library(stringr)
library(ggplot2)
library(xgboost)
library(workflows)
library(workflowsets)

################################################################################
# Read in cross-validation results.
cv_dir <- here("5_model_estimation", "output", "all")
cv_files <- list.files(cv_dir, pattern = "cv_[0-9].*rds", full.names = T)
cv_names <- str_remove(str_extract(cv_files, "cv_[0-9].*rds"), ".rds")
cv_workflows <- map(cv_files, readRDS) |> bind_rows()

################################################################################
cv_results <-
    pmap(
        list(cv_workflows$result, cv_workflows$wflow_id),
        function(result_df, model_id) {
            pmap(
                list(result_df$.metrics, result_df$id),
                function(metric_df, fold_id) {
                    metric_df |>
                        mutate(fold_id = fold_id) |>
                        select(-.config, -.estimator)
                }
            ) |>
                bind_rows() |>
                mutate(model_id = model_id)
        }
    ) |>
    set_names(cv_names)

################################################################################
# Gather together the results of cross-validation for each hyper parameter.
cv_hp_results <-
    map(
        cv_results,
        function(df) {
            df |>
                pivot_longer(
                    cols = -matches("estimate|metric|fold_id|model_id"),
                    names_to = "hyperp_name",
                    values_to = "hyperp_value"
                )
        }
    ) |>
    bind_rows()

hyperparameters <- unique(cv_hp_results$hyperp_name)

# Find the prediction error for each hyperparameter value (in isolation).
avg_results_per_hp <-
    map(
        hyperparameters,
        function(hyperparameter_arg, result_df) {
            result_df |>
                filter(hyperp_name == hyperparameter_arg) |>
                summarise(
                    mean = mean(.estimate), sd = sd(.estimate), n = n(),
                    .by = c(model_id, .metric, hyperp_value)
                ) |>
                mutate(
                    se = sd / sqrt(n),
                    moe = se * qnorm(0.975),
                    ci_upper_95 = mean + moe,
                    ci_lower_95 = mean - moe
                ) |>
                arrange(.metric, mean) |>
                mutate(hyperparameter = hyperparameter_arg)
        },
        result_df = cv_hp_results
    ) |>
    bind_rows()

################################################################################
# Plot the hyperparameter space to see if results converged on some optimum.
graphs_avg_results_per_hp <-
    map(
        hyperparameters,
        function(hp_arg, df) {
            if(hp_arg %in% c("loss_reduction", "learn_rate", "penalty")) {
                df$hyperp_value <- log10(df$hyperp_value)
            }
            
            df |>
                filter(hyperparameter == hp_arg) |>
                ggplot(aes(x = hyperp_value, y = mean)) +
                geom_point() +
                facet_wrap(~ model_id + .metric, scale = "free", ncol = 2) +
                theme_bw() +
                labs(
                    x = "Hyperparameter value",
                    y = "Mean Prediction Error",
                    title = hp_arg
                )
        },
        df = avg_results_per_hp
    )

graph_dir <- here("5_model_estimation", "graphs", "cv")
if(!dir.exists(graph_dir)) {dir.create(graph_dir)}

pwalk(
    list(graphs_avg_results_per_hp, hyperparameters),
    function(graph, name) {
        ggsave(
            file.path(graph_dir, paste0(name, ".png")),
            graph,
            width = 9,
            height = 6.5
        )
    }
)

################################################################################
results_per_model <- function(df, hps) {
    df |>
        summarise(
            mean = mean(.estimate),
            sd = sd(.estimate),
            n = n(),
            .by = c(".metric", "model_id", all_of(hps))
        ) |>
        mutate(
            se = sd / sqrt(n),
            moe = se * qnorm(0.975),
            ci_upper_95 = mean + moe,
            ci_lower_95 = mean - moe
        ) |>
        mutate(id = paste0(model_id, "_", cur_group_id()), .by = all_of(hps))
}

graph_hp <- function(df, x_var, y_var, title_str) {
    logged_hps <- c("loss_reduction", "learn_rate", "penalty")
    if(x_var %in% logged_hps) {df[[x_var]] <- log10(df[[x_var]])}
    if(y_var %in% logged_hps) {df[[y_var]] <- log10(df[[y_var]])}
    
    df |>
        ggplot(aes(x = .data[[x_var]], y = .data[[y_var]])) +
        geom_point(aes(color = scaled_mean, alpha = -scaled_mean), size = 5) +
        facet_wrap(~.metric) +
        theme_bw() +
        labs(title = title_str)
}

###################################################### GLM net specific graphs.
results_glm <-
    cv_results[str_detect(names(cv_results), "glm")] |>
    bind_rows() |>
    results_per_model(c("mixture", "penalty")) |>
    mutate(
        scaled_mean = ((mean - min(mean)) / (max(mean) - min(mean))) ^ (1/3),
        .by = .metric
    )

graph_glmnet <- graph_hp(results_glm, "mixture", "penalty", "GLMNet")

ggsave(
    file.path(graph_dir, "cv_result_glmnet.png"),
    graph_glmnet,
    width = 8,
    height = 4
)

################################################### Bagged MARS specific graphs.
results_bMars <-
    cv_results[str_detect(names(cv_results), "bag")] |>
    bind_rows() |>
    results_per_model(c("num_terms", "prod_degree"))

graph_bagMars <-
    results_bMars |>
    mutate(prod_degree = as.character(prod_degree)) |>
    ggplot(aes(x = num_terms, y = mean)) +
    geom_point(aes(color = prod_degree)) +
    geom_line(aes(color = prod_degree, group = prod_degree)) +
    facet_wrap(~.metric, scale = "free_y") +
    theme_bw() +
    labs(title = "Bagged MARS")

ggsave(
    file.path(graph_dir, "cv_result_bagMars.png"),
    graph_bagMars,
    width = 8,
    height = 4
)

################################################# Random forest specific graphs.
results_rf <-
    cv_results[str_detect(names(cv_results), "_rf$")] |>
    bind_rows() |>
    results_per_model(c("min_n", "mtry")) |>
    mutate(
        scaled_mean = ((mean - min(mean)) / (max(mean) - min(mean))),
        .by = .metric
    )

graph_rf <- graph_hp(results_rf, "min_n", "mtry", "Random Forest")

ggsave(
    file.path(graph_dir, "cv_result_rf.png"), graph_rf, width = 8, height = 4
)

############################################ Conditional forest specific graphs.
results_crf <-
    cv_results[str_detect(names(cv_results), "_crf$")] |>
    bind_rows() |>
    results_per_model(c("min_n", "mtry")) |>
    mutate(
        scaled_mean = ((mean - min(mean)) / (max(mean) - min(mean))),
        .by = .metric
    )

graph_crf <- graph_hp(results_crf, "min_n", "mtry", "Conditional Random Forest")

ggsave(
    file.path(graph_dir, "cv_result_crf.png"), graph_crf, width = 8, height = 4
)

################################################ Oblique forest specific graphs.
results_orf <-
    cv_results[str_detect(names(cv_results), "_orf$")] |>
    bind_rows() |>
    results_per_model(c("min_n", "mtry", "split_min_stat")) |>
    mutate(
        scaled_mean = ((mean - min(mean)) / (max(mean) - min(mean))) ^ (1/3),
        .by = .metric
    )

graph_orf_1 <- graph_hp(results_orf, "min_n", "mtry", "Oblique Random Forest")
graph_orf_2 <- graph_hp(results_orf, "min_n", "split_min_stat", "Oblique Random Forest")
graph_orf_3 <- graph_hp(results_orf, "mtry", "split_min_stat", "Oblique Random Forest")
names_orf <-
    list(
        "cv_result_orf_minN_mtry.png", "cv_result_orf_minN_splitMinStat.png",
        "cv_result_orf_mtry_splitMinStat.png"
    )

pwalk(
    list(list(graph_orf_1, graph_orf_2, graph_orf_3), names_orf),
    function(graph, name, dir) {
        ggsave(file.path(dir, name), graph, width = 8, height = 4)
    },
    dir = graph_dir
)

######################################### Boosted decision tree specific graphs.
results_xgboost <-
    cv_results[str_detect(names(cv_results), "_xgboost$")] |>
    bind_rows() |>
    results_per_model(
        c(
            "colsample_bytree", "learn_rate", "loss_reduction", "min_n",
            "sample_size", "tree_depth"
        )
    ) |>
    mutate(
        learn_rate_log = log10(learn_rate),
        loss_reduction_log = log10(loss_reduction),
        scaled_mean = ((mean - min(mean)) / (max(mean) - min(mean))) ^ (1/6),
        .by = .metric
    )

graph_xgboost <-
    results_xgboost |>
    ggpairs(
        upper = "blank",
        diag = "blank",
        columns =
            c(
                "colsample_bytree", "learn_rate_log", "loss_reduction_log",
                "min_n", "sample_size", "tree_depth"
            ),
        mapping = aes(color = scaled_mean, alpha = -scaled_mean),
        lower = list(continuous = wrap("points", size = 1.5))
    ) +
    theme_bw() +
    theme(strip.text = element_text(size = 4))

ggsave(
    file.path(graph_dir, "cv_result_xgboost.png"),
    graph_xgboost,
    width = 8,
    height = 4
)

############################################################### Overall results.
results_overall <-
    bind_rows(
        results_bMars, results_glm, results_rf, results_crf, results_orf,
        results_xgboost
    ) |>
    select(.metric, model_id, id, mean, ci_upper_95, ci_lower_95) |>
    arrange(.metric, model_id, mean)

order <- results_overall |> filter(.metric == "rmse") |> pull(id)

results_overall <-
    results_overall |>
    mutate(id = factor(id, levels = order))

graph_overall <-
    ggplot(results_overall, aes(x = id, y = mean)) +
    geom_point(stat = "identity") +
    geom_errorbar(aes(ymin = ci_lower_95, ymax = ci_upper_95)) +
    theme_bw() +
    theme(axis.text.y = element_blank()) +
    facet_wrap(~ .metric + model_id, scales = "free", ncol = 6) +
    coord_flip()

ggsave(
    file.path(graph_dir, "cv_result_all.png"),
    graph_overall,
    width = 16,
    height = 10
)

################################################################################
# Find best hyperparameter values.
workflow_set <- readRDS(file.path(cv_dir, "workflow_set.rds"))

# Find best hyper parameter values.
find_best_hp <- function(df, model) {
    df |>
        arrange(.metric, mean) |>
        slice(1:5, .by = .metric) |>
        distinct(id, .keep_all = T) |>
        select(
            -any_of(
                c(
                    "model_id", ".metric", "mean", "sd", "n", "se", "moe", "id",
                    "ci_upper_95", "ci_lower_95", "scaled_mean", "learn_rate_log",
                    "loss_reduction_log"
                )
            )
        )
}

best_hp_glm <- find_best_hp(results_glm)
best_hp_bagMars <- find_best_hp(results_bMars)
best_hp_rf <- find_best_hp(results_rf)
best_hp_crf <- find_best_hp(results_crf)
best_hp_orf <- find_best_hp(results_orf)
best_hp_xgboost <- find_best_hp(results_xgboost)

################################################################################
# Finalize workflows with best hyperparameter values.
my_finalize_workflow <- function(row_nr, workflow_set, hp_results) {
    workflow <- workflow_set$info[[1]]$workflow[[1]]
    workflow_final <- workflow |> finalize_workflow(slice(hp_results, row_nr))
    return(workflow_final)
}

######################################################################## GLMNet.
workflow_glmnet <- workflow_set |> filter(wflow_id == "standardized_glmnet")
row_nrs_glm <- 1:nrow(best_hp_glm)
names(row_nrs_glm) <- paste0("glmnet_", row_nrs_glm)

workflows_glmnet <-
    map(
        row_nrs_glm,
        my_finalize_workflow,
        workflow_set = workflow_glmnet,
        hp_results = best_hp_glm
    )

################################################################### Bagged MARS.
workflow_bagMars <- workflow_set |> filter(wflow_id == "standardized_bagMars")
row_nrs_bagMars <- 1:nrow(best_hp_bagMars)
names(row_nrs_bagMars) <- paste0("bagMars_", row_nrs_bagMars)

workflows_bagMars <-
    map(
        row_nrs_bagMars,
        my_finalize_workflow,
        workflow_set = workflow_bagMars,
        hp_results = best_hp_bagMars
    ) |>
    map(
        function(wf) {
            model_spec <- extract_spec_parsnip(wf)
            updated_model_spec <- update(model_spec, times = 250)
            updated_wf <- update_model(wf, updated_model_spec)
        }
    )

################################################################## Random forest
workflow_rf <- workflow_set |> filter(wflow_id == "standardized_rf")
row_nrs_rf <- 1:nrow(best_hp_rf)
names(row_nrs_rf) <- paste0("rf_", row_nrs_rf)

workflows_rf <-
    map(
        row_nrs_rf,
        my_finalize_workflow,
        workflow_set = workflow_rf,
        hp_results = best_hp_rf
    ) |>
    map(
        function(wf) {
            model_spec <- extract_spec_parsnip(wf)
            updated_model_spec <- update(model_spec, trees = 2000)
            updated_wf <- update_model(wf, updated_model_spec)
        }
    )

##################################################### Conditional random forest.
workflow_crf <- workflow_set |> filter(wflow_id == "standardized_crf")
row_nrs_crf <- 1:nrow(best_hp_crf)
names(row_nrs_crf) <- paste0("crf_", row_nrs_crf)

workflows_crf <-
    map(
        row_nrs_crf,
        my_finalize_workflow,
        workflow_set = workflow_crf,
        hp_results = best_hp_crf
    ) |>
    map(
        function(wf) {
            model_spec <- extract_spec_parsnip(wf)
            updated_model_spec <- update(model_spec, trees = 2000)
            updated_wf <- update_model(wf, updated_model_spec)
        }
    )

######################################################### Oblique random forest.
workflow_orf <- workflow_set |> filter(wflow_id == "standardized_orf")
row_nrs_orf <- 1:nrow(best_hp_orf)
names(row_nrs_orf) <- paste0("orf_", row_nrs_orf)

workflows_orf <-
    map(
        row_nrs_orf,
        my_finalize_workflow,
        workflow_set = workflow_orf,
        hp_results = best_hp_orf
    ) |>
    map(
        function(wf) {
            model_spec <- extract_spec_parsnip(wf)
            updated_model_spec <- update(model_spec, trees = 1000)
            updated_wf <- update_model(wf, updated_model_spec)
        }
    )

######################################################### Boosted decision tree.
workflow_xgboost <- workflow_set |> filter(wflow_id == "standardized_xgboost")
row_nrs_xgboost <- 1:nrow(best_hp_xgboost)
names(row_nrs_xgboost) <- paste0("xgboost_", row_nrs_xgboost)

workflows_xgboost <-
    map(
        row_nrs_xgboost,
        my_finalize_workflow,
        workflow_set = workflow_xgboost,
        hp_results = best_hp_xgboost
    )

################################################################## Save results.
workflow_ols <- workflow_set |> filter(wflow_id == "standardized_ols")
workflow_ols <- list("ols_1" = workflow_ols$info[[1]]$workflow[[1]])

workflows_final <-
    as.list(
        c(
            workflow_ols, workflows_bagMars, workflows_crf, workflows_glmnet,
            workflows_rf, workflows_orf, workflows_xgboost
        )
    )

saveRDS(workflows_final, file.path(cv_dir, "workflows_final.rds"))
