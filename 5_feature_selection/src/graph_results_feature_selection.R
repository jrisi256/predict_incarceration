library(here)
library(purrr)
library(dplyr)
library(stringr)
library(ggplot2)

# Read in data.
cross_validation_workflows <-
    readRDS(here("5_feature_selection", "output", "cross_validation_workflow.rds"))

################################################################################
# Plot results of cross-validation.
cross_validation_results <-
    bind_rows(cross_validation_workflows$.metrics) |>
    mutate(.estimate = if_else(.metric == "rsq", -.estimate, .estimate)) |>
    group_by(penalty, mixture, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n(), id = unique(.config)) |>
    ungroup() |>
    mutate(se = sd / sqrt(n), moe = se * qnorm(0.975)) |>
    arrange(.metric, mean) |>
    group_by(.metric) |>
    mutate(rank = row_number()) |>
    ungroup()

# It looks like we have hit the threshold of penalty.
# Too much penalty and results suffer. However, too few penalty is also bad.
penalty_graph <-
    cross_validation_results |>
    ggplot(aes(x = log10(penalty), y = mean)) +
    geom_point() +
    facet_wrap(~.metric, scale = "free_y") +
    theme_bw()

# On the other hand, it does not look like mixture matters so much.
# In general, higher mixture (more LASSO) is bad but not necessarily so.
mixture_graph <-
    cross_validation_results |>
    ggplot(aes(x = mixture, y = mean)) +
    geom_point() +
    facet_wrap(~.metric, scale = "free_y") +
    theme_bw()

penalty_and_mixture_graph <-
    map(
        unique(cross_validation_results$.metric),
        function(metric, df) {
            df |>
                filter(.metric == metric) |>
                ggplot(aes(x = penalty, y = mixture)) +
                geom_point(
                    aes(size = mean, fill = mean, alpha = mean),
                    color = "black",
                    shape = 21
                ) +
                facet_wrap(~.metric, ) +
                scale_size_continuous(range = c(1, 10)) +
                theme_bw()
        },
        df = cross_validation_results
    )

# There is very little difference in performance for the top-performing models.
compare_results_graph <-
    cross_validation_results |>
    filter(penalty <= 1) |>
    ggplot(aes(x = rank, y = mean)) +
    geom_point() +
    geom_errorbar(aes(ymin = mean - moe, ymax = mean + moe), linewidth = 0.02) +
    facet_wrap(~.metric, scale = "free_y") +
    theme_bw()

################################################################################
# Compare feature importance.
feat_importance <-
    cross_validation_workflows$.extracts |>
    map(
        # Loop over each fold + repeat (10 folds * 3 repeats = 30 iterations)
        function(unique_fold_and_repeat) {
            pmap(
                list(
                    unique_fold_and_repeat$penalty,
                    unique_fold_and_repeat$mixture,
                    unique_fold_and_repeat$.extracts
                ),
                # Loop over each unique hyperparameter combination (1000)
                function(penalty, mixture, df_feat_importance) {
                    tibble(
                        term = df_feat_importance$term,
                        estimate = abs(df_feat_importance$estimate),
                        penalty = penalty,
                        mixture = mixture
                    )
                }
            )
        }
    ) |>
    bind_rows() |>
    filter(term != "(Intercept)")

# Calculate feature importance for each feature.
feat_importance_by_feat <-
    feat_importance |>
    filter(!(term %in% c("uninsured_prcnt_est_0to19", "uninsured_prcnt_est_under65"))) |>
    group_by(term) |>
    summarise(mean = mean(estimate), sd = sd(estimate), n = n()) |>
    ungroup() |>
    mutate(
        se = sd / sqrt(n),
        moe = qt(0.975, df = n - 1) * se,
        ci_lower = mean - moe,
        ci_upper = mean + moe
    ) |>
    mutate(
        term = tolower(term),
        category =
            case_when(
                str_detect(term, "ages17andyounger|child|_0to17|16to19") ~ "Child Characteristics",
                str_detect(term, "college|hs_") ~ "Education",
                str_detect(term, "birth") ~ "Fertility",
                str_detect(term, "costs|rentp|mortgage") ~ "Housing Costs",
                str_detect(term, "phone|problem|perroom|overcrowding|vehicle") ~ "Housing Conditions",
                str_detect(term, "value") ~ "Housing Value",
                str_detect(term, "income|ginic") ~ "Income",
                str_detect(term, "ur(_|b|h)|epr_|lfpr(_|b|h)") ~ "Labor",
                str_detect(term, "married_p|evermarried_p|evermarried_r|married_r|separated") ~ "Marriage",
                str_detect(term, "moved|movers") ~ "Residential Mobility",
                str_detect(term, "single") ~ "Family Structure",
                str_detect(term, "poverty|snap") ~ "Poverty and Welfare",
                str_detect(term, "vacant|renters|rental") ~ "House Ownership",
                str_detect(term, "businesses|social") ~ "Businesses + Social Capital + Social Support Orgs.",
                str_detect(term, "gini_simpson|shannon|pop_prcnt|pop_nr") ~ "Race and Age Demographics",
                str_detect(term, "homicides") ~ "Violence" 
            )
    )

feat_importance_by_category_graph <-
    map(
        unique(feat_importance_by_feat$category),
        function(category_arg, df) {
            df <- df |> filter(category == category_arg)
            order <- df |> arrange(mean) |> pull(term)
            
            df |>
                mutate(term = factor(term, levels = order)) |>
                ggplot(aes(x = term, y = mean), size = 3) +
                geom_point() +
                geom_segment(aes(x = term, xend = term, y = 0, yend = mean)) +
                coord_flip() +
                theme_bw() +
                theme(axis.text = element_text(size = 20))
                
        },
        df = feat_importance_by_feat
    )

feat_importance_by_allCategories <-
    feat_importance_by_feat |>
    filter(mean >= 5) |>
    group_by(category) |>
    summarise(mean = mean(mean))
