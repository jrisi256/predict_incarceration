library(ale)
library(here)
library(dplyr)
library(purrr)
library(tidyr)
library(forcats)
library(ggplot2)
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
df_ale_predictions <- readRDS(file.path(read_dir, "ale_predictions.rds"))
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
    
df_preds <-
    full_join(
        df_ale_predictions,
        df_pdp_predictions,
        by = c("variable", "variable_dirty")
    ) |>
    full_join(df_var_imp, by = "variable")

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
# Compare and contrast PDP and ALE predictions.
################################################################################
compare_ale_pdp <-
    function(
        ale_xg, ale_ols, pdp_xg, pdp_ols, pc_seq, ale_var_name, cluster, pcs,
        loadings, pv
    ) {
        # Set-up variables.
        df_dist <- tibble(observed_PC1 = pcs)
        ale_var_name <- paste0(ale_var_name, ".ceil")

        # Combine variable importance effects across PDP models.
        df_pdp <-
            bind_rows(
                bind_cols(
                    select(pdp_xg, -seq) |>
                        mutate(model = "XGBoost-1", pred_type = "PDP"),
                    tibble(PC1 = pc_seq)
                ),
                bind_cols(
                    select(pdp_ols, -seq) |>
                        mutate(model = "OLS", pred_type = "PDP"),
                    tibble(PC1 = pc_seq)
                )
            ) |>
            pivot_longer(
                cols = c(-PC1, -model, -pred_type),
                values_to = "prediction",
                names_to = "stat"
            ) |>
            filter(stat == "mean") |>
            select(-stat) |>
            rename_with(function(col) {ale_var_name}, .cols = PC1)

        # Combine variable importance effects across ALE models.
        df_ale <-
            bind_rows(
                get(ale_xg) |> mutate(model = "XGBoost-1", pred_type = "ALE"),
                get(ale_ols) |> mutate(model = "OLS", pred_type = "ALE")
            ) |>
            select(matches("ceil"), .y_mean, model, pred_type) |>
            rename(prediction = .y_mean)

        # Combine PDP and ALE.
        df_combined <- bind_rows(df_ale, df_pdp)

        # Create the caption.
        if(!is.null(loadings)) {
            caption <-
                pmap(
                    loadings |> std_var_names(),
                    function(variable, loading) {
                        paste0(variable, ": ", signif(loading, 3))
                    }
                ) |>
                paste(collapse = "\n")

            caption <-
                paste0(
                    "Proportion of Variance explained by PC1: ",
                    signif(pv, 3), "\n\n", "Variable loadings onto PC1\n",
                    caption
                )
        } else {caption <- ""}

        ggplot(df_combined, aes(x = .data[[ale_var_name]], y = prediction)) +
            geom_point(aes(color = model)) +
            geom_line(aes(color = model)) +
            facet_wrap(~pred_type) +
            theme_bw() +
            geom_rug(
                data = df_dist,
                aes(x = observed_PC1),
                inherit.aes = F,
                alpha = 0.05,
                sides = "b"
            ) +
            labs(
                x = cluster,
                y = "Prediction",
                caption = caption,
                color = "Model"
            )
    }

graphs_compare_ale_pdp <-
    pmap(
        list(
            df_preds$ale_xgboost,
            df_preds$ale_ols,
            df_preds$prediction_distribution_xgboost1,
            df_preds$prediction_distribution_ols,
            df_preds$pc1_seq,
            df_preds$ale_pc1_var_name,
            df_preds$variable,
            df_preds$pc1,
            df_preds$pc1_loadings,
            df_preds$pc1_pv
        ),
        compare_ale_pdp
    ) |>
    set_names(df_preds$variable)

################################################################################
# Compare PDP predictions within variable importance categories.
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

graphs_pdp_preds <-
    map(
        unique(df_pdp_preds$category),
        function(cat, df, df_rug_arg, load) {
            # Subset variables in specific category.
            df <- df |> filter(category == cat)
            df_rug_arg <- df_rug_arg |> filter(category == cat)
            
            # For each variable in this category.
            vars_in_cat <- unique(df$var)
            
            # Cannot use facet_wrap w/ insets. Create our own facet wrap.
            graph_list <-
                map(
                    vars_in_cat,
                    function(v) {
                        df_var <- df |> filter(var == v)
                        df_rug_var <- df_rug_arg |> filter(var == v)
                        
                        # Main graph.
                        graph_main <-
                            ggplot(df_var, aes(x = PC1, y = prediction)) +
                            geom_point(aes(color = model)) +
                            geom_line(aes(color = model, group = model)) +
                            geom_rug(
                                data = df_rug_var,
                                aes(x = PC1),
                                inherit.aes = F,
                                alpha = 0.05,
                                sides = "b"
                            ) +
                            theme_bw() +
                            theme(title = element_text(size = 11)) +
                            labs(
                                x = NULL,
                                y = "Prediction",
                                title = v,
                                color = "Model"
                            ) +
                            coord_cartesian(ylim = c(min_pred, max_pred))
                        
                        # Retrieve loading and pv for current variable.
                        df_load <- load$pc1_loadings[[which(load$variable == v)]]
                        pv <- load$pc1_pv[[which(load$variable == v)]]
                        
                        # If loading exists, create the sub-plot and inset it.
                        if (!is.null(df_load)) {
                            pv <- signif(pv * 100, 2)
                            title <- paste0("Loadings (PC1 accounts for ", pv, "% of variance)")
                            
                            df_load <-
                                df_load |>
                                mutate(dir = if_else(loading > 0, "+", "-")) |>
                                std_var_names()
                            
                            # Create inset for variable loadings.
                            graph_sub <-
                                df_load |>
                                ggplot(
                                    aes(
                                        x = reorder(variable, loading),
                                        y = loading
                                    )
                                ) +
                                geom_point(
                                    aes(color = dir),
                                    show.legend = F,
                                    size = 3
                                ) +
                                coord_flip() +
                                scale_color_manual(
                                    values = c("+" = "green", "-" = "red")
                                ) +
                                theme_bw() +
                                theme(
                                    plot.background = element_blank(),
                                    # ols, xgboost, and sig not diff
                                    axis.text.y = element_text(size = 11),
                                    title = element_text(size = 10)
                                ) +
                                labs(x = NULL, y = NULL, title = title)
                            
                            # Combine main plot and sub-plot.
                            graph_final <-
                                graph_main / graph_sub +
                                plot_layout(heights = c(8, 2))
                            
                            return(graph_final)
                            
                        } else {return(graph_main)}
                    }
                )
            
            # Stitch together all the individual plots to mimic facet_wrap.
            graph_stitched <-
                wrap_plots(
                    graph_list,
                    # for ols, xgboost, and sig not diff
                    nrow = 3
                    #ncol = 4
                ) +
                plot_annotation(
                    title = cat,
                    theme = theme(plot.title = element_text(face = "bold", size = 14))
                ) +
                # Only one legend.
                plot_layout(guides = "collect") +
                theme(legend.position = "none")
        },
        df = df_pdp_preds,
        df_rug_arg = df_rug,
        load = df_preds
    ) |>
    set_names(unique(df_pdp_preds$category))

pwalk(
    list(graphs_pdp_preds, names(graphs_pdp_preds)),
    function(g, name) {
        ggsave(
            here("7_var_imp", "graphs", paste0(name, ".png")),
            g,
            # For xbgoost only.
            #height = 16,
            #width = 10
            # For ols and sig not diff.
            height = 16,
            width = 20
        )
    }
)

################################################################################
# Create table
################################################################################
table_pred <-
    pmap(
        list(
            df_preds$pc1_seq,
            df_preds$prediction_distribution_xgboost1,
            df_preds$prediction_distribution_ols,
            df_preds$variable,
            df_preds$category
        ),
        function(pc1s, df_pred_xg, df_pred_ols, var, cat) {
            max_index <- which(pc1s == max(pc1s))
            min_index <- which(pc1s == min(pc1s))
            
            tibble(
                Max = max(pc1s),
                Min = min(pc1s),
                `Difference XGBoost` = df_pred_xg$mean[max_index] - df_pred_xg$mean[min_index],
                `Difference OLS` = df_pred_ols$mean[max_index] - df_pred_ols$mean[min_index],
                Variable = var,
                Category = cat
            )
        }
    ) |>
    bind_rows() |>
    arrange(-abs(`Difference XGBoost`)) |>
    mutate(
        across(matches("Difference|Max|Min"), function(col) {round(col, 2)})
    )

readr::write_csv(table_pred, here("7_var_imp", "output", "table_pdp.csv"))

################################################################################
# Compare PDP predictions within variable importance categories (only XGBOOST)
################################################################################
graphs_pdp_preds_xg <-
    map(
        unique(df_pdp_preds$category),
        function(cat, df, df_rug_arg, load) {
            # Subset variables in specific category.
            df <- df |> filter(category == cat, model == "XGBoost-1")
            df_rug_arg <- df_rug_arg |> filter(category == cat)

            # For each variable in this category.
            vars_in_cat <- unique(df$var)

            # Cannot use facet_wrap w/ insets. Create our own facet wrap.
            graph_list <-
                map(
                    vars_in_cat,
                    function(v) {
                        df_var <- df |> filter(var == v)
                        df_rug_var <- df_rug_arg |> filter(var == v)

                        # Main graph.
                        graph_main <-
                            ggplot(df_var, aes(x = PC1, y = prediction)) +
                            geom_point() +
                            geom_line() +
                            geom_rug(
                                data = df_rug_var,
                                aes(x = PC1),
                                inherit.aes = F,
                                alpha = 0.05,
                                sides = "b"
                            ) +
                            theme_bw() +
                            theme(title = element_text(size = 6)) +
                            labs(x = NULL, y = "Prediction", title = v)

                        # Retrieve loading and pv for current variable.
                        df_load <- load$pc1_loadings[[which(load$variable == v)]]
                        pv <- load$pc1_pv[[which(load$variable == v)]]

                        # If loading exists, create the sub-plot and inset it.
                        if (!is.null(df_load)) {
                            pv <- signif(pv * 100, 2)
                            title <- paste0("Loadings (PC1 accounts for ", pv, "% of the variance)")

                            df_load <-
                                df_load |>
                                mutate(dir = if_else(loading > 0, "+", "-")) |>
                                std_var_names()

                            # Create inset for variable loadings.
                            graph_sub <-
                                df_load |>
                                ggplot(
                                    aes(
                                        x = reorder(variable, loading),
                                        y = loading
                                    )
                                ) +
                                geom_point(
                                    aes(color = dir),
                                    show.legend = F,
                                    size = 3
                                ) +
                                coord_flip() +
                                scale_color_manual(
                                    values = c("+" = "green", "-" = "red")
                                ) +
                                theme_minimal(base_size = 7) +
                                theme(
                                    plot.background = element_blank(),
                                    title = element_text(size = 5)
                                ) +
                                labs(x = NULL, y = NULL, title = title)

                            # Combine main plot and sub-plot.
                            graph_final <-
                                graph_main / graph_sub +
                                plot_layout(heights = c(65, 35))

                            return(graph_final)

                        } else {return(graph_main)}
                    }
                )

            # Stitch together all the individual plots to mimic facet_wrap.
            graph_stitched <-
                wrap_plots(graph_list) +
                plot_annotation(
                    title = cat,
                    theme = theme(plot.title = element_text(face = "bold", size = 14))
                )
        },
        df = df_pdp_preds,
        df_rug_arg = df_rug,
        load = df_preds
    ) |>
    set_names(unique(df_pdp_preds$category))
