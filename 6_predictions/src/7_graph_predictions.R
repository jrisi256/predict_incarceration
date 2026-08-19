library(here)
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(ggplot2)
read_dir <- here("6_predictions", "output")
graph_dir <- here("6_predictions", "graphs")

################################################################################
# Read in performance data.
################################################################################
perf_files <- list.files(read_dir, pattern = "^performance", full.names = T)
perf_df <-
    map(perf_files, readRDS) |> bind_rows() |>
    mutate(model = str_remove(model, "fitted_model_"))

bootstrap_files <- list.files(read_dir, pattern = "^bootstrap", full.names = T)
bootstrap_df <-
    map(bootstrap_files, readRDS) |>
    bind_rows() |>
    mutate(model = str_remove(model, "fitted_model_"))

################################################################################
# Graph results.
################################################################################
bootstrap_summ <-
    bootstrap_df |>
    summarise(
        ci_lower_999 = quantile(.estimate, 0.0005),
        ci_upper_999 = quantile(.estimate, 0.9995),
        .by = c("model", ".metric")
    ) |>
    inner_join(perf_df, by = c("model", ".metric")) |>
    mutate(model_type = str_remove_all(model, "fitted_model_|[0-9]")) |>
    arrange(.metric, -.estimate) |>
    mutate(
        model_type =
            case_when(
                model_type == "bagMars" ~ "Bagged MARS",
                model_type == "crf" ~ "Conditional Random Forest",
                model_type == "glmnet" ~ "GLMNet",
                model_type == "ols" ~ "OLS",
                model_type == "orf" ~ "Oblique Random Forest",
                model_type == "rf" ~ "Random Forest",
                model_type == "xgboost" ~ "XGBoost"
            ),
        .metric =
            case_when(
                .metric == "mae" ~ "Mean Absolute Error",
                .metric == "rmse" ~ "Root-mean Squared Error",
                .metric == "rsq" ~ "Coefficient of Determination"
            )
    )

order <- bootstrap_summ |> filter(.metric == "Root-mean Squared Error") |> pull(model)
bootstrap_summ <- bootstrap_summ |> mutate(model = factor(model, levels = order))

graph <-
    ggplot(bootstrap_summ, aes(x = model, y = .estimate)) +
    geom_point(aes(color = model_type), size = 3) +
    geom_errorbar(
        aes(
            ymin = ci_lower_999,
            ymax = ci_upper_999,
            color = model_type,
            linetype = model_type
        )
    ) +
    theme_bw() +
    coord_flip() +
    facet_wrap(~.metric, scale = "free_x") +
    labs(
        x = "Statistic",
        y = "Model",
        title = "Comparison of predictive performance across models",
        color = "Model Type",
        linetype = "Model Type"
    ) +
    theme(axis.text = element_text(size = 12))

ggsave(
    file.path(graph_dir, "model_performance.png"), graph, width = 12, height = 7
)

################################################################################
# Compare Xgboost models to all other models.
################################################################################
perf_wide <-
    perf_df |>
    pivot_wider(
        id_cols = ".metric", names_from = "model", values_from = ".estimate"
    )

bootstrap_wide <-
    bootstrap_df |>
    pivot_wider(
        id_cols = c("id", ".metric"),
        names_from = "model",
        values_from = ".estimate"
    )

xgboost_cols <-
    colnames(bootstrap_wide)[str_detect(colnames(bootstrap_wide), "xgboost")]

perf_diff <-
    map_dfc(
        xgboost_cols,
        function(xgboost_col, df) {
            xgboost_model_name <- str_extract(xgboost_col, "xgboost[1-9]")
            
            df |>
                mutate(
                    across(
                        -matches("^id$|.metric|xgboost"),
                        function(col) {.data[[xgboost_col]] - col},
                        .names = paste0("{.col}_", xgboost_model_name)
                    ),
                    .keep = "none"
                )
        },
        df = perf_wide
    ) |>
    bind_cols(select(perf_wide, .metric)) |>
    pivot_longer(
        cols = -matches(".metric"),
        names_to = c("comp_model", "xgboost_model"),
        values_to = "estimate",
        names_pattern = "(.*)_(.*)"
    )

bootstrap_diff <-
    map_dfc(
        xgboost_cols,
        function(xgboost_col, df) {
            xgboost_model_name <- str_extract(xgboost_col, "xgboost[1-9]")
            
            df |>
                mutate(
                    across(
                        -matches("^id$|.metric|xgboost"),
                        function(col) {.data[[xgboost_col]] - col},
                        .names = paste0("{.col}_", xgboost_model_name)
                    ),
                    .keep = "none"
                )
        },
        df = bootstrap_wide
    ) |>
    bind_cols(select(bootstrap_wide, id, .metric)) |>
    pivot_longer(
        cols = -matches("^id$|.metric"),
        names_to = c("comp_model", "xgboost_model"),
        values_to = "diff",
        names_pattern = "(.*)_(.*)"
    )

summ_diff <-
    bootstrap_diff |>
    summarise(
        ci_lower_999 = quantile(diff, 0.0005),
        ci_upper_999 = quantile(diff, 0.9995),
        .by = c(".metric", "comp_model", "xgboost_model")
    ) |>
    inner_join(perf_diff, by = c(".metric", "comp_model", "xgboost_model")) |>
    mutate(comp_model_type = str_remove_all(comp_model, "[0-9]")) |>
    mutate(
        comp_model_type =
            case_when(
                comp_model_type == "bagMars" ~ "Bagged MARS",
                comp_model_type == "crf" ~ "Conditional Random Forest",
                comp_model_type == "glmnet" ~ "GLMNet",
                comp_model_type == "ols" ~ "OLS",
                comp_model_type == "orf" ~ "Oblique Random Forest",
                comp_model_type == "rf" ~ "Random Forest",
                comp_model_type == "xgboost" ~ "XGBoost"
            ),
        .metric =
            case_when(
                .metric == "mae" ~ "Mean Absolute Error",
                .metric == "rmse" ~ "Root-mean Squared Error",
                .metric == "rsq" ~ "Coefficient of Determination"
            )
    )

graph_diff <-
    summ_diff |>
    filter(.metric == "Coefficient of Determination") |>
    ggplot(aes(x = comp_model, y = estimate)) +
    geom_point(aes(color = comp_model_type)) +
    geom_errorbar(
        aes(
            ymin = ci_lower_999,
            ymax = ci_upper_999,
            color = comp_model_type,
            linetype = comp_model_type
        )
    ) +
    theme_bw() +
    coord_flip() +
    facet_wrap(~.metric + xgboost_model, scale = "free_x", nrow = 2) +
    geom_hline(yintercept = 0) +
    labs(
        color = "Model Type",
        linetype = "Model Type",
        x = "Difference in predictive power",
        y = "Comparison Model",
        title = "Comparison of best performing model (XGBoost) with all other models"
    ) +
    theme(
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12)
    )

ggsave(
    file.path(graph_dir, "model_diff.png"), graph_diff, width = 14, height = 12
)

################################################################################
# Sub-graph for presentation.
################################################################################
graph_diff_pres <-
    summ_diff |>
    filter(
        .metric == "Coefficient of Determination", xgboost_model == "xgboost1"
    ) |>
    ggplot(aes(x = comp_model, y = estimate)) +
    geom_point(aes(color = comp_model_type), size = 3) +
    geom_errorbar(
        aes(
            ymin = ci_lower_999,
            ymax = ci_upper_999,
            color = comp_model_type,
            linetype = comp_model_type
        ),
        linewidth = 1
    ) +
    theme_bw() +
    coord_flip() +
    facet_wrap(~.metric, scale = "free_x", nrow = 2) +
    geom_hline(yintercept = 0) +
    labs(
        color = "Model Type",
        linetype = "Model Type",
        x = "Difference in predictive power",
        y = "Comparison Model",
        title = "Comparison of best performing model (XGBoost) with all other models"
    ) +
    theme(
        axis.text = element_text(size = 11),
        strip.text = element_text(size = 12),
        axis.title = element_text(size = 12),
        legend.text = element_text(size = 12)
    )
