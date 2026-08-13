library(here)
library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)
library(rsample)
library(ggridges)
library(flextable)
read_dir <- here("5_model_estimation", "output", "all")
out_dir <- here("5_model_estimation", "graphs")
source(here("functions.R"))

################################################################################
# Read in training data.
################################################################################
split <- readRDS(file.path(read_dir, "split.rds"))
train <- training(split)

################################################################################
# Create descriptive table.
################################################################################
train_long <-
    train |>
    pivot_longer(
        cols = -matches("^county$|^state$|fips|^year$"),
        values_to = "value",
        names_to = "variable"
    ) |>
    assign_var_category() |>
    std_var_names()

descriptive_table <-
    train_long |>
    select(-matches("^county$|^state$|fips|^year$")) |>
    group_by(variable, var_category) |>
    summarise(
        mean = mean(value),
        sd = sd(value),
        min = min(value),
        p10 = quantile(value, seq(0, 1, 0.1))[["10%"]],
        p25 = quantile(value, seq(0, 1, 0.25))[["25%"]],
        median = median(value),
        p75 = quantile(value, seq(0, 1, 0.25))[["75%"]],
        p90 = quantile(value, seq(0, 1, 0.1))[["90%"]],
        max = max(value),
        iqr = IQR(value),
        mad = mad(value)
    ) |>
    ungroup() |>
    mutate(
        across(
            where(is.numeric),
            function(col) {
                if_else(!is.na(col), sprintf("%.3f", signif(col, 3)), "-")
            }
        )
    ) |>
    arrange(var_category, variable) |>
    rename(
        Variable = variable, Mean = mean, `Std. Dev.` = sd, `Min.` = min,
        Max = max, IQR = iqr, `Median Abs. Dev.` = mad, Median = median
    )

descriptive_flextable <-
    descriptive_table |>
    as_grouped_data(groups = "var_category") |>
    as_flextable() |>
    compose(j = 1, i = ~ !is.na(var_category), value = as_paragraph(as_chunk(var_category))) |>
    bold(j = 1, i = ~ !is.na(var_category), bold = T, part = "body") |>
    fontsize(size = 7, part = "all") |>
    set_table_properties(layout = "autofit")

save_as_docx(
    descriptive_flextable,
    path = file.path(out_dir, "descriptive_table.docx")
)

##############################################################################
##  Graph distribution of each variable (pooling across counties and time)  ##
##############################################################################
if(!dir.exists(here(out_dir, "densityplots"))) {
    dir.create(here(out_dir, "densityplots"), recursive = T)
}

densitychart <- function(df, col) {
    df <- df %>% filter(variable == col)
    raw_values <- df %>% pull(value)
    boxplot_coord <- max(density(raw_values, na.rm = T)$y) / 2
    ggplot(df, aes(x = value)) +
        geom_boxplot(aes(y = -boxplot_coord), width = boxplot_coord) +
        geom_density() +
        labs(x = col, y = "Density") +
        theme_bw()
}

walk(
    unique(descriptive_table$var_category),
    function(var_cat, df, dir) {
        df_var_cat <- df |> filter(var_category == var_cat)
        vars <- unique(df_var_cat$variable)
        
        pdf(file.path(dir, paste0(var_cat, ".pdf")), onefile = T)
        plots <- map(as.list(vars), densitychart, df = df_var_cat)
        walk(plots, print)
        dev.off()
    },
    df = train_long,
    dir = here(out_dir, "densityplots")
)

###############################################################################
##  Plot changing distributions of variables over time as ridge line plots.  ##
###############################################################################
if(!dir.exists(here(out_dir, "ridgeline_plots"))) {
    dir.create(here(out_dir, "ridgeline_plots"), recursive = T)
}

ridgeplot <- function(df, col) {
    df <- df %>% filter(variable == col)
    quantiles <- quantile(df$value, probs = seq(0, 1, 0.005), na.rm = T)
    bottom <- quantiles[["2.5%"]]
    top <- quantiles[["97.5%"]]
    
    ggplot(df, aes(x = value, y = year, group = year)) +
        geom_density_ridges(
            rel_min_height = 0.005,
            quantile_lines = T,
            quantiles = c(0.25, 0.5, 0.75)
        ) +
        labs(x = col, y = "Year") +
        theme_ridges() +
        scale_x_continuous(limits = c(bottom, top))
}

walk(
    unique(descriptive_table$var_category),
    function(var_cat, df, dir) {
        df_var_cat <- df |> filter(var_category == var_cat)
        vars <- unique(df_var_cat$variable)
        
        pdf(file.path(dir, paste0(var_cat, ".pdf")), onefile = T)
        plots <- map(as.list(vars), ridgeplot, df = df_var_cat)
        walk(plots, print)
        dev.off()
    },
    df = train_long,
    dir = here(out_dir, "ridgeline_plots")
)

###############################################################################
##  Plot changing distributions of variables over time as spaghetti plots.   ##
###############################################################################
if(!dir.exists(here(out_dir, "spaghetti_plots"))) {
    dir.create(here(out_dir, "spaghetti_plots"), recursive = T)
}

spaghettiplot <- function(df, df_summ, var_cat) {
    quantiles <- quantile(df$value, probs = seq(0, 1, 0.005), na.rm = T)
    
    df |>
        filter(value < quantiles[["97.5%"]], value > quantiles[["2.5%"]]) |>
        ggplot(aes(x = year, y = value)) +
        geom_point(aes(group = full_fips), alpha = 0.01) +
        geom_line(aes(group = full_fips), alpha = 0.01) +
        theme_bw() +
        facet_wrap(~variable, scale = "free_y") +
        geom_point(
            data = df_summ,
            aes(x = year, y = value, color = `Statistic`),
        ) +
        geom_line(
            data = df_summ,
            aes(x = year, y = value, color = `Statistic`, group = `Statistic`)
        ) +
        labs(x = "Year", y = "Value", title = var_cat) +
        scale_x_continuous(breaks = seq(2010, 2019, 1)) +
        theme(strip.text = element_text(size = 7))
}

walk(
    unique(descriptive_table$var_category),
    function(var_cat, df, dir) {
        df_var_cat <- df |> filter(var_category == var_cat)
        df_var_cat_summ <-
            df_var_cat |>
            group_by(variable, year) |>
            summarise(
                P25 = quantile(value, seq(0, 1, 0.25))[["25%"]],
                Median = median(value),
                P75 = quantile(value, seq(0, 1, 0.25))[["75%"]]
            ) |>
            pivot_longer(
                cols = c(-variable, -year),
                names_to = "Statistic",
                values_to = "value"
            )
        
        vars <- unique(df_var_cat$variable)
        
        ggsave(
            file.path(dir, paste0(var_cat, ".png")),
            spaghettiplot(df_var_cat, df_var_cat_summ, var_cat),
            height = 9,
            width = 14
        )
    },
    df = train_long,
    dir = here(out_dir, "spaghetti_plots")
)

###############################################################################
# Characterize the variation as mostly within or between counties.
###############################################################################
if(!dir.exists(here(out_dir, "between_within_plots"))) {
    dir.create(here(out_dir, "between_within_plots"), recursive = T)
}

between_within <-
    train_long |>
    group_by(variable) |>
    mutate(grand_mean = mean(value)) |>
    group_by(variable, full_fips) |>
    mutate(county_mean = mean(value)) |>
    ungroup() |>
    mutate(
        within_component = value - county_mean,
        between_component = county_mean - grand_mean
    ) |>
    group_by(variable, var_category) |>
    summarise(
        var_total = var(value),
        var_between = var(county_mean),
        var_within = var(within_component),
        prop_between = var_between / var_total
    ) |>
    arrange(prop_between)

order <- between_within |> pull(variable)

between_within_ordered <-
    between_within |>
    mutate(variable = factor(variable, levels = order))

graph_between_within_overall <-
    ggplot(between_within_ordered, aes(x = prop_between)) +
    geom_density() +
    theme_bw() +
    geom_vline(
        xintercept = quantile(between_within$prop_between, probs = c(0.5))
    ) +
    labs(
        x = "Proportion of variance that is between counties (vs. within)",
        y = "Density",
        title = "Comparing within vs. between variance for counties for each variable"
    )

ggsave(
    here(out_dir, "between_within_plots", "between_within_plot.png"),
    graph_between_within_overall,
    height = 9,
    width = 14
)

walk(
    unique(between_within_ordered$var_category),
    function(var_cat, df, dir) {
        graph <-
            df |>
            filter(var_category == var_cat) |>
            ggplot(aes(x = prop_between, y = variable)) +
            geom_point() +
            theme_bw() +
            labs(
                x = "Proportion of variance that is between counties (vs. within)",
                y = "Variable",
                title = var_cat
            )
        
        ggsave(
            file.path(dir, paste0(var_cat, ".png")),
            graph,
            height = 9,
            width = 14
        )
    },
    df = between_within_ordered,
    dir = here(out_dir, "between_within_plots")
)
