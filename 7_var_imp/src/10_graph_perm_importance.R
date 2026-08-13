library(here)
library(lme4)
library(dplyr)
library(purrr)
library(tidyr)
library(tibble)
library(stringr)
library(ggplot2)
library(stringr)
library(rsample)
library(workflows)
library(performance)
source(here("functions.R"))
read_dir <- here("7_var_imp", "output", "perm_importance")
graph_dir <- here("7_var_imp", "graphs")
out_dir <- here("7_var_imp", "output")

################################################################################
# Read in permutation importance, data, and models.
################################################################################
files_perm_imp <- list.files(read_dir, full.names = T, pattern = "perm_")
df_perm_imp <- map(files_perm_imp, readRDS) |> bind_rows()

files_perm_imp_07 <- list.files(read_dir, full.names = T, pattern = "perm07_")
df_perm_imp_07 <- map(files_perm_imp_07, readRDS) |> bind_rows()

files_perm_imp_06 <- list.files(read_dir, full.names = T, pattern = "perm06_")
df_perm_imp_06 <- map(files_perm_imp_06, readRDS) |> bind_rows()

df_hcluster06 <-
    readRDS(here("7_var_imp", "output", "list_hcluster06.rds")) |>
    enframe(name = "group_06", value = "variable") |>
    unnest(c(variable))

df_hcluster07 <-
    readRDS(here("7_var_imp", "output", "list_hcluster07.rds")) |>
    enframe(name = "group_07", value = "variable") |>
    unnest(c(variable))

################################################################################
# Summarize results.
################################################################################
summarise_perm_imp <- function(df) {
    df |>
        summarise(
            mean = mean(dropout_loss),
            n = n(),
            ci_lower_95 = quantile(dropout_loss, 0.025),
            ci_upper_95 = quantile(dropout_loss, 0.975),
            .by = c(variable, label)
        ) |>
        assign_var_category() |>
        mutate(mean_var_category = mean(mean), .by = c(label, var_category)) |>
        arrange(label, -mean) |>
        mutate(rank = row_number(), .by = label) |>
        arrange(label, mean_var_category, mean)
}

df_summ_perm_imp <- summarise_perm_imp(df_perm_imp)
df_summ_perm_imp_07 <- summarise_perm_imp(df_perm_imp_07)
df_summ_perm_imp_06 <- summarise_perm_imp(df_perm_imp_06)

################################################################################
# Assess level of agreement among different xgboost models.
################################################################################
model_var_import <-
    lmer(
        dropout_loss ~ label + (1 | variable),
        data = df_perm_imp |> filter(label != "ols1")
    )

model_var_import_07 <-
    lmer(
        dropout_loss ~ label + (1 | variable),
        data = df_perm_imp_07 |> filter(label != "ols1")
    )

model_var_import_06 <-
    lmer(
        dropout_loss ~ label + (1 | variable),
        data = df_perm_imp_06 |> filter(label != "ols1")
    )

icc_result <- icc(model_var_import)
icc_result_07 <- icc(model_var_import_07)
icc_result_06 <- icc(model_var_import_06)

# Graph results.
order <- df_summ_perm_imp |> filter(label == "xgboost1") |> pull(variable)

graph_xgboost_comp <-
    df_summ_perm_imp |>
    filter(label != "ols1") |>
    mutate(variable = factor(variable, levels = order)) |>
    std_var_names() |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = label)) +
    theme_bw() +
    facet_wrap(~var_category, scale = "free_y") +
    theme(
        axis.text.y = element_text(size = 5),
        strip.text = element_text(size = 6)
    ) +
    labs(color = "Model", x = "Permutation Importance", y = "Variable")

ggsave(
    file.path(graph_dir, "graph_xgboost_comp.png"),
    graph_xgboost_comp,
    height = 10,
    width = 18
)

order_07 <- df_summ_perm_imp_07 |> filter(label == "xgboost1") |> pull(variable)

graph_xgboost_comp_07 <-
    df_summ_perm_imp_07 |>
    filter(label != "ols1") |>
    mutate(variable = factor(variable, levels = order_07)) |>
    std_var_names() |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = label)) +
    theme_bw() +
    facet_wrap(~var_category, scale = "free_y") +
    theme(
        axis.text.y = element_text(size = 5),
        strip.text = element_text(size = 6)
    ) +
    labs(color = "Model", x = "Permutation Importance", y = "Variable")

ggsave(
    file.path(graph_dir, "graph_xgboost_comp07.png"),
    graph_xgboost_comp_07,
    height = 10,
    width = 18
)

order_06 <- df_summ_perm_imp_06 |> filter(label == "xgboost1") |> pull(variable)

graph_xgboost_comp_06 <-
    df_summ_perm_imp_06 |>
    filter(label != "ols1") |>
    mutate(variable = factor(variable, levels = order_06)) |>
    std_var_names() |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = label)) +
    theme_bw() +
    facet_wrap(~var_category, scale = "free_y") +
    theme(
        axis.text.y = element_text(size = 5),
        strip.text = element_text(size = 6)
    ) +
    labs(color = "Model", x = "Permutation Importance", y = "Variable")

ggsave(
    file.path(graph_dir, "graph_xgboost_comp06.png"),
    graph_xgboost_comp_06,
    height = 10,
    width = 18
)

################################################################################
# Changing variable imp. for individual vs. grouped permutation importance.
################################################################################
# How did the var. imp. scores changing going from no groups to 0.7 clusters?
df_summ_perm_imp_hclust <-
    df_hcluster07 |>
    filter(variable != group_07) |>
    left_join(
        select(df_summ_perm_imp, variable, mean, label), by = "variable"
    ) |>
    rename(ind_mean = mean) |>
    left_join(
        select(df_summ_perm_imp_07, variable, mean, label),
        by = c("group_07" = "variable", "label")
    ) |>
    rename(group_07_mean = mean) |>
    mutate(diff = group_07_mean - ind_mean) |>
    arrange(label, group_07, -diff)

graph_noCluster_07_comp <-
    df_summ_perm_imp_hclust |>
    ggplot(aes(x = diff)) +
    geom_density() +
    facet_wrap(~label, scale = "free") +
    theme_bw() +
    labs(
        x = "Difference in permutation importance",
        y = "Density",
        title = "Difference in perm. imp. (No groups to 0.7 cluster)"
    )

ggsave(
    file.path(graph_dir, "graph_noCluster_07_comp.png"),
    graph_noCluster_07_comp,
    height = 10,
    width = 18
)

# How did the var. imp. scores changing going from 0.7 to 0.6 clusters?
df_summ_perm_imp_hclust_groups <-
    full_join(df_hcluster07, df_hcluster06, by = "variable") |>
    group_by(group_06) |>
    filter(!all(group_07 == group_06)) |>
    ungroup() |>
    select(-variable) |>
    distinct() |>
    left_join(
        select(df_summ_perm_imp_07, variable, mean, label),
        by = c("group_07" = "variable")
    ) |>
    rename(group_07_mean = mean) |>
    left_join(
        select(df_summ_perm_imp_06, variable, mean, label),
        by = c("group_06" = "variable", "label")
    ) |>
    rename(group_06_mean = mean) |>
    mutate(diff = group_06_mean - group_07_mean) |>
    arrange(label, group_06, -diff) |>
    rename(
        "Group (0.6) Perm. Imp." = group_06_mean,
        "Group (0.7) Perm. Imp." = group_07_mean,
        variable = group_07
    ) |>
    std_var_names()

graph_cluster_07_06_comp <-
    df_summ_perm_imp_hclust_groups |>
    ggplot(aes(x = diff)) +
    geom_density() +
    facet_wrap(~label, scale = "free") +
    theme_bw() +
    labs(
        x = "Difference in permutation importance",
        y = "Density",
        title = "Difference in perm. imp. (0.7 to 0.6 cluster)"
    )

ggsave(
    file.path(graph_dir, "graph_07_06_comp.png"),
    graph_cluster_07_06_comp,
    height = 10,
    width = 18
)

order_groups <-
    df_summ_perm_imp_hclust_groups |>
    filter(label == "xgboost1") |>
    pull(variable)

graph_07_06_comp_var <-
    df_summ_perm_imp_hclust_groups |>
    filter(label == "xgboost1") |>
    mutate(variable = factor(variable, levels = order_groups)) |>
    pivot_longer(
        cols = matches("Perm. Imp."),
        values_to = "mean_perm_imp",
        names_to = "grouping"
    ) |>
    ggplot(aes(x = mean_perm_imp, y = variable)) +
    geom_point(aes(shape = grouping), alpha = 0.4, size = 3) +
    geom_line(aes(color = group_06, group = variable)) +
    theme_bw() +
    labs(
        x = "Permutation Importance",
        y = "Group (0.7 Cluster)",
        color = "Group (0.6 Cluster)",
        shape = "Group",
        title = "Change in permutation importance (0.7 to 0.6 clusters)"
    )

ggsave(
    file.path(graph_dir, "graph_07_06_comp_var.png"),
    graph_07_06_comp_var,
    height = 10,
    width = 18
)

################################################################################
# Graph ranked variable importance.
################################################################################
order_var_imp_xgboost1 <-
    df_summ_perm_imp_06 |>
    filter(label == "xgboost1") |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    pull(variable)

graph_xgboost_top_var <-
    df_summ_perm_imp_06 |>
    filter(label == "xgboost1") |>
    filter(ci_lower_95 > 1) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    mutate(variable = factor(variable, levels = order_var_imp_xgboost1)) |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = var_category)) +
    geom_errorbar(
        aes(color = var_category, xmin = ci_lower_95, xmax = ci_upper_95)
    ) +
    theme_bw() +
    labs(
        x = "Permutation Importance",
        y = "Variable",
        color = "Category",
        title = "Statistically Significantly Predictive Variables (XGBoost-1)"
    ) +
    theme(
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
    )

ggsave(
    here(graph_dir, "graph_xgboost_top_var.png"),
    graph_xgboost_top_var,
    height = 10,
    width = 16
)

graph_xgboost_bottom_var <-
    df_summ_perm_imp_06 |>
    filter(label == "xgboost1") |>
    filter(ci_lower_95 <= 1) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    mutate(variable = factor(variable, levels = order_var_imp_xgboost1)) |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = var_category)) +
    geom_errorbar(
        aes(color = var_category, xmin = ci_lower_95, xmax = ci_upper_95)
    ) +
    theme_bw() +
    labs(
        x = "Permutation Importance",
        y = "Variable",
        color = "Category",
        title = "Non-Statistically Significantly Predictive Variables (XGBoost-1)"
    )

ggsave(
    here(graph_dir, "graph_xgboost_bottom_var.png"),
    graph_xgboost_bottom_var,
    height = 10,
    width = 16
)

order_var_imp_ols <-
    df_summ_perm_imp_06 |>
    filter(label == "ols1") |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    pull(variable)

graph_ols_top_var <-
    df_summ_perm_imp_06 |>
    filter(label == "ols1") |>
    filter(ci_lower_95 > 1) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    mutate(variable = factor(variable, levels = order_var_imp_ols)) |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = var_category)) +
    geom_errorbar(
        aes(color = var_category, xmin = ci_lower_95, xmax = ci_upper_95)
    ) +
    theme_bw() +
    labs(
        x = "Permutation Importance",
        y = "Variable",
        color = "Category",
        title = "Statistically Significantly Predictive Variables (OLS)"
    ) +
    theme(
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 12)
    )

ggsave(
    here(graph_dir, "graph_ols_top_var.png"),
    graph_ols_top_var,
    height = 10,
    width = 16
)

graph_ols_bottom_var <-
    df_summ_perm_imp_06 |>
    filter(label == "ols1") |>
    filter(ci_lower_95 <= 1) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    mutate(variable = factor(variable, levels = order_var_imp_ols)) |>
    ggplot(aes(x = mean, y = variable)) +
    geom_point(aes(color = var_category)) +
    geom_errorbar(
        aes(color = var_category, xmin = ci_lower_95, xmax = ci_upper_95)
    ) +
    theme_bw() +
    labs(
        x = "Permutation Importance",
        y = "Variable",
        color = "Category",
        title = "Non-Statistically Significantly Predictive Variables (OLS)"
    )

ggsave(
    here(graph_dir, "graph_ols_bottom_var.png"),
    graph_ols_bottom_var,
    height = 10,
    width = 16
)

################################################################################
# Assess level of agreement between OLS and XGBoost models.
################################################################################
df_summ_perm_imp_wide <-
    df_summ_perm_imp_06 |>
    pivot_wider(
        id_cols = c(variable), names_from = label, values_from = mean
    ) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    )

df_summ_perm_imp_diff_wide <-
    df_perm_imp_06 |>
    pivot_wider(
        id_cols = c(variable, permutation),
        names_from = label,
        values_from = dropout_loss
    ) |>
    mutate(
        across(
            matches("xgboost"),
            function(col) {col - ols1},
            .names = "{.col}_diff_ols1"
        )
    ) |>
    summarise(
        across(
            matches("_diff_ols1$"),
            function(col) {quantile(col, 0.025)},
            .names = "{.col}_ci_lower_95"
        ),
        across(
            matches("_diff_ols1$"),
            function(col) {mean(col)},
            .names = "{.col}_mean"
        ),
        across(
            matches("_diff_ols1$"),
            function(col) {quantile(col, 0.975)},
            .names = "{.col}_ci_upper_95"
        ),
        .by = c(variable)
    ) |>
    relocate(matches("xgboost6"), .after = variable) |>
    relocate(matches("xgboost5"), .after = variable) |>
    relocate(matches("xgboost4"), .after = variable) |>
    relocate(matches("xgboost3"), .after = variable) |>
    relocate(matches("xgboost2"), .after = variable) |>
    relocate(matches("xgboost1"), .after = variable) |>
    assign_var_category() |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    arrange(-xgboost1_diff_ols1_mean)

order_wide <- df_summ_perm_imp_diff_wide |> pull(variable)

vars_xgboost1_sig <-
    df_summ_perm_imp_06 |>
    filter(label == "xgboost1", ci_lower_95 > 1) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    pull(variable)

vars_ols_sig <-
    df_summ_perm_imp_06 |>
    filter(label == "ols1", ci_lower_95 > 1) |>
    std_var_names() |>
    mutate(
        variable =
            if_else(
                variable %in% df_hcluster06$group_06,
                paste0(variable, " (cluster)"),
                variable
            )
    ) |>
    pull(variable)

vars_diff_sig <-
    df_summ_perm_imp_diff_wide |>
    filter(
        xgboost1_diff_ols1_ci_lower_95 > 0 | xgboost1_diff_ols1_ci_upper_95 < 0
    ) |>
    pull(variable)

df_importantBoth_notDiff <-
    df_summ_perm_imp_diff_wide |>
    filter(
        variable %in% vars_ols_sig,
        variable %in% vars_xgboost1_sig,
        !(variable %in% vars_diff_sig)
    ) |>
    mutate(category = "Significant in both (not significantly different)")

df_importantBoth_diff <-
    df_summ_perm_imp_diff_wide |>
    filter(
        variable %in% vars_ols_sig,
        variable %in% vars_xgboost1_sig,
        variable %in% vars_diff_sig,
        variable %in% (df_summ_perm_imp_wide |> filter(xgboost1 >= 1.004) |> pull(variable))
    ) |>
    mutate(category = "Significant in both (significantly different)")

df_importantXG_diff <-
    df_summ_perm_imp_diff_wide |>
    filter(
        !(variable %in% vars_ols_sig),
        variable %in% vars_xgboost1_sig,
        variable %in% vars_diff_sig
    ) |>
    mutate(category = "Significant only in XGBoost")

df_importantOLS_diff <-
    df_summ_perm_imp_diff_wide |>
    filter(
        variable %in% vars_ols_sig,
        !(variable %in% vars_xgboost1_sig),
        variable %in% vars_diff_sig,
        variable %in% (df_summ_perm_imp_wide |> filter(ols1 >= 1.04) |> pull(variable))
    ) |>
    mutate(category = "Significant only in OLS")

df_important <-
    bind_rows(
        df_importantBoth_diff, df_importantBoth_notDiff, df_importantXG_diff,
        df_importantOLS_diff
    ) |>
    mutate(
        variable = factor(variable, levels = order_wide),
        difference = if_else(xgboost1_diff_ols1_mean > 0, "OLS", "XGBoost-1")
    ) |>
    left_join(df_summ_perm_imp_wide, by = "variable") |>
    pivot_longer(
        cols = c("xgboost1", "ols1"),
        names_to = "model",
        values_to = "perm_imp"
    ) |>
    mutate(model = if_else(model == "ols1", "OLS", "XGBoost-1"))

saveRDS(
    df_important |> distinct(variable, category),
    file.path(out_dir, "df_variable_importance.rds")
)

graph_xgboost_ols_comp <-
    df_important |>
    ggplot(aes(y = variable, x = perm_imp)) +
    geom_point(aes(color = model, shape = model), size = 4) +
    geom_line(aes(group = variable, lty = difference)) +
    theme_bw() +
    labs(
        x = "Difference in permutation variable importance",
        y = "Variable",
        title = "Comparison of variable importance: OLS vs. XGBoost-1",
        lty = "Model w/ higher importance",
        color = "Model",
        shape = "Model"
    ) +
    facet_wrap(~category, scale = "free_y") +
    theme(
        axis.text = element_text(size = 13),
        legend.text = element_text(size = 13),
        strip.text = element_text(size = 13)
    )

ggsave(
    here(graph_dir, "graph_xgboost_ols_comp.png"),
    graph_xgboost_ols_comp,
    height = 12,
    width = 20
)

df_corr <-
    map(
        paste0("xgboost", 1:6),
        function(col_name, df) {
            col <- df[[col_name]]
            ols <- df[["ols1"]]
            corr_spear <- cor(col, ols, method = "spearman")
            corr_ken <- cor(col, ols, method = "kendall")
            return(tibble(model = col_name, spear = corr_spear, ken = corr_ken))
        },
        df = df_summ_perm_imp_wide
    ) |>
    bind_rows()

corr_spearman <- signif(df_corr |> filter(model == "xgboost1") |> pull(spear), 3)
corr_kendall <- signif(df_corr |> filter(model == "xgboost1") |> pull(ken), 3)

graph_corr_xgboost1_ols <-
    ggplot(df_summ_perm_imp_wide, aes(y = ols1, x = xgboost1)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0) +
    theme_bw() +
    labs(
        y = "OLS permutation variable importance",
        x = "XGBoost-1 permutation variable importance",
        title = "Correlation of permutation variable importances: OLS vs. XGBoost-1",
        caption =
            paste0(
                "Spearman Correlation: ", corr_spearman, "\n",
                "Kendall's Tau: ", corr_kendall
            )
    )

ggsave(
    here(graph_dir, "graph_corr_xgboost1_ols.png"),
    graph_corr_xgboost1_ols,
    height = 10,
    width = 16
)
