library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(ggplot2)
library(stringr)
library(patchwork)
source(here("functions.R"))
read_dir <- here("7_var_imp", "output")
vars_switch <-
    c(
        "% Black (cluster)", "% Republican (cluster)", "Marriage (cluster)",
        "Nr. Total Social Capital Orgs. (cluster)", "Crime (cluster)",
        "Racial/ethnic diversity (cluster)", "Poverty + Low Education (cluster)",
        "% Uniquely Republican (cluster)", "Dissimilarity Index (Education, Income) (cluster)",
        "State-level Republican ideology (cluster)"
    )

################################################################################
# Read in variable predictions and variable importance.
################################################################################
df_pdp_predictions <- readRDS(file.path(read_dir, "pdp_predictions.rds"))
df_var_imp <-
    readRDS(file.path(read_dir, "df_variable_importance.rds")) |>
    mutate(
        category =
            case_when(
                category == "Significant in both (significantly different)" &
                    variable %in% c(
                        "State-level political ideology (cluster)",
                        "% Republican (cluster)",
                        "Political Polarization (cluster)",
                        "State-level Democratic ideology (cluster)",
                        "Nr. Total Social Capital Orgs. (cluster)",
                        "# of businesses per 10k people"
                    ) ~ "Significant in both (significantly different) (Political Ideology + Social Capital)",
                category == "Significant in both (significantly different)" &
                    !(variable %in% c(
                        "State-level political ideology (cluster)",
                        "% Republican (cluster)",
                        "Political Polarization (cluster)",
                        "State-level Democratic ideology (cluster)",
                        "Nr. Total Social Capital Orgs. (cluster)",
                        "# of businesses per 10k people"
                    )) ~ "Significant in both (significantly different) (Wealth, socioeconomic status, racial diversity + family composition)",
                T ~ category
            )
    )

df_preds <- df_pdp_predictions |> full_join(df_var_imp, by = "variable")

################################################################################
# Fix principal components so they align more with cluster titles.
################################################################################
df_preds <-
    df_preds |>
    mutate(
        pc1 =
            pmap(
                list(variable, pc1),
                function(v, pc) {if(v %in% vars_switch) {-pc} else {pc}}
            ),
        pc1_seq =
            pmap(
                list(variable, pc1_seq),
                function(v, pc_seq) {
                    if(v %in% vars_switch) {-pc_seq} else {pc_seq}
                }
            ),
        pc1_loadings =
            pmap(
                list(variable, pc1_loadings),
                function(v, df_load) {
                    if(!is.null(df_load)) {
                        if(v %in% vars_switch) {
                            df_load |> mutate(loading = -loading)
                        } else {df_load}
                    } else {df_load}
                }
            )
    )

################################################################################
# Ceteris Paribus Plot and ICE graph.
################################################################################
delta_preds <-
    df_preds |>
    filter(variable == "Delta Index (Race, Education, Income) (cluster)")

middlesex_nj <-
    delta_preds$predictions_xgboost1[[1]] |>
    filter(full_fips == "34023", year == 2018) |>
    pivot_longer(
        cols = matches("pred"), names_to = "index", values_to = "prediction"
    ) |>
    mutate(pc1 = delta_preds$pc1_seq[[1]]) |>
    select(-index)

ggplot(middlesex_nj, aes(x = pc1, y = prediction)) +
    geom_point() +
    geom_line() +
    theme_bw() +
    labs(
        title = "Middlesex County, NJ in 2018",
        x = "Delta Index (cluster)",
        y = "Prediction"
    )

nj <-
    delta_preds$predictions_xgboost1[[1]] |>
    filter(str_sub(full_fips, 1, 2) == "34") |>
    pivot_longer(
        cols = matches("pred"), names_to = "index", values_to = "prediction"
    ) |>
    select(-index) |>
    mutate(pc1 = rep(delta_preds$pc1_seq[[1]], 189))

ggplot(nj, aes(x = pc1, y = prediction)) +
    geom_point(alpha = 0.1) +
    geom_line(aes(group = paste0(full_fips, year)), alpha = 0.1) +
    theme_bw() +
    labs(
        title = "All counties, all years in NJ",
        x = "Delta Index (cluster)",
        y = "Prediction"
    )

################################################################################
# PDP graphs for each variable.
################################################################################
df_pdp_preds <-
    pmap(
        list(
            df_preds$prediction_distribution_xgboost1,
            df_preds$prediction_distribution_ols,
            df_preds$pc1_seq,
            df_preds$variable,
            df_preds$category
        ),
        function(pdp_xg, pdp_ols, pc_seq, var, cat) {
            # Combine variable importance effects across PDP models.
            df_pdp <-
                bind_rows(
                    bind_cols(
                        select(pdp_xg, -seq) |> mutate(model = "XGBoost-1"),
                        tibble(PC1 = pc_seq)
                    ),
                    bind_cols(
                        select(pdp_ols, -seq) |> mutate(model = "OLS"),
                        tibble(PC1 = pc_seq)
                    )
                ) |>
                pivot_longer(
                    cols = c(-PC1, -model),
                    values_to = "prediction",
                    names_to = "stat"
                ) |>
                filter(stat == "mean") |>
                select(-stat) |>
                mutate(var = var, category = cat)
        }
    ) |>
    bind_rows()

df_rug <-
    pmap(
        list(df_preds$pc1, df_preds$category, df_preds$variable),
        function(pc, cat, variable) {
            df <- tibble(PC1 = pc) |> mutate(category = cat, var = variable)
        }
    ) |>
    bind_rows()

min_pred <- min(df_pdp_preds |> filter(model == "XGBoost-1") |> pull(prediction), na.rm = TRUE)
max_pred <- max(df_pdp_preds |> filter(model == "XGBoost-1") |> pull(prediction), na.rm = TRUE)

pdp_graphs <-
    map(
        unique(df_pdp_preds$var),
        function(variable, df, df_rug_arg, load) {
            # Subset to specific variable
            df <- df |> filter(var == variable)
            df_rug_arg <- df_rug_arg |> filter(var == variable)
            
            # Main graph.
            graph_main <-
                ggplot(df, aes(x = PC1, y = prediction)) +
                geom_point(aes(color = model)) +
                geom_line(aes(color = model, group = model)) +
                geom_rug(
                    data = df_rug_arg,
                    aes(x = PC1),
                    inherit.aes = F,
                    alpha = 0.05,
                    sides = "b"
                ) +
                theme_bw() +
                theme(title = element_text(size = 11)) +
                labs(
                    x = variable,
                    y = "Prediction",
                    title = "Partial Dependence Graph",
                    color = "Model"
                ) +
                coord_cartesian(ylim = c(min_pred, max_pred))
            
            # Retrieve loading and pv for current variable.
            df_load <- load$pc1_loadings[[which(load$variable == variable)]]
            pv <- load$pc1_pv[[which(load$variable == variable)]]
                        
            # If loading exists, create the sub-plot and inset it.
            if (!is.null(df_load)) {
                pv <- signif(pv * 100, 2)
                title <-
                    paste0("Loadings (PC1 accounts for ", pv, "% of variance)")
                
                df_load <-
                    df_load |>
                    mutate(dir = if_else(loading > 0, "+", "-")) |>
                    std_var_names()
                
                # Create inset for variable loadings.
                graph_sub <-
                    df_load |>
                    ggplot(aes(x = reorder(variable, loading), y = loading)) +
                    geom_point(aes(color = dir), show.legend = F, size = 3) +
                    coord_flip() +
                    scale_color_manual(
                        values = c("+" = "green", "-" = "red")
                    ) +
                    theme_bw() +
                    theme(
                        plot.background = element_blank(),
                        axis.text.y = element_text(size = 11),
                        title = element_text(size = 10)
                    ) +
                    labs(x = NULL, y = NULL, title = title)
                
                # Combine main plot and sub-plot.
                graph_final <-
                    graph_main / graph_sub +
                    plot_layout(heights = c(8, 2))
                
            } else {graph_final <- graph_main}
        },
        df = df_pdp_preds,
        df_rug_arg = df_rug,
        load = df_preds
    ) |>
    set_names(unique(df_pdp_preds$var))

names(pdp_graphs) <- str_replace(names(pdp_graphs), "%", "Prcnt.")
names(pdp_graphs) <- str_replace_all(names(pdp_graphs), "/", "-")

pwalk(
    list(pdp_graphs, names(pdp_graphs)),
    function(g, name) {
        ggsave(
            here("7_var_imp", "graphs", "pdp_graphs", paste0(name, ".png")),
            g,
            height = 8,
            width = 10
        )
    }
)
