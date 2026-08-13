library(here)
library(dplyr)
library(rsample)
out_dir <- here("7_var_imp", "output")
read_dir <- here("5_model_estimation", "output", "all")

################################################################################
# Read in train data.
################################################################################
split <- readRDS(file.path(read_dir, "split.rds"))
train <- training(split)
train_iv_only <- train |> select(-matches("^state$|^county$|_fips|^year$|prison"))

################################################################################
# Estimate hierarchical clusters.
################################################################################
matrix_corr <- cor(train_iv_only, method = "spearman")
matrix_dist <- as.dist(1 - abs(matrix_corr))
model_hcluster <- hclust(matrix_dist, method = "complete")
hclusters07 <- cutree(model_hcluster, h = 0.3)
hclusters06 <- cutree(model_hcluster, h = 0.4)

df_hclusters07 <-
    tibble(hcluster = unname(hclusters07), variable = names(hclusters07)) |>
    mutate(
        group =
            case_when(
                hcluster == 1 ~ "Deep Poverty",
                hcluster == 3 ~ "Single Parent HH",
                hcluster == 4 ~ "Black v. White Poverty",
                hcluster == 5 ~ "Hisp. v. White Poverty",
                hcluster == 10 ~ "Low Housing Costs",
                hcluster == 17 ~ "Rent burdened",
                hcluster == 29 ~ "Home Values",
                hcluster == 33 ~ "Income Inequality",
                hcluster == 34 ~ "Concentrated Wealth",
                hcluster == 51 ~ "Black v. White Single Parent",
                hcluster == 52 ~ "Hisp. v. White Single Parent",
                hcluster == 63 ~ "Educational diversity",
                hcluster == 64 ~ "Poverty",
                hcluster == 67 ~ "Income diversity",
                hcluster == 68 ~ "Work force participation",
                hcluster == 69 ~ "Marriage",
                hcluster == 78 ~ "Nr. Social Associations",
                hcluster == 79 ~ "Nr. Social Support Services",
                hcluster == 80 ~ "Nr. Total Social Capital Orgs.",
                hcluster == 81 ~ "Population + Exposure Index",
                hcluster == 82 ~ "Racial/ethnic diversity",
                hcluster == 83 ~ "% Black",
                hcluster == 84 ~ "% Hisp.",
                hcluster == 88 ~ "Homicides",
                hcluster == 89 ~ "Homicides (Max Averaged)",
                hcluster == 90 ~ "Homicides (Min Averaged)",
                hcluster == 91 ~ "% Republican",
                hcluster == 92 ~ "% Uniquely Republican",
                hcluster == 96 ~ "Property Crime",
                hcluster == 98 ~ "Delta Index (Race, Education, Income)",
                hcluster == 99 ~ "Dissimilarity Index (Education, Income)",
                hcluster == 103 ~ "Spatial Proximity (Education)",
                hcluster == 104 ~ "Spatial Proximity (Income)",
                hcluster == 105 ~ "Spatial Proximity (Race)",
                hcluster == 106 ~ "State-level political ideology",
                hcluster == 107 ~ "State-level Democratic ideology",
                hcluster == 108 ~ "State-level Republican ideology",
                hcluster == 109 ~ "Political Polarization",
                T ~ variable
            )
    )

list_hcluster07 <- split(df_hclusters07$variable, df_hclusters07$group)
saveRDS(list_hcluster07, file.path(out_dir, "list_hcluster07.rds"))

df_hclusters06 <-
    tibble(hcluster = unname(hclusters06), variable = names(hclusters06)) |>
    mutate(
        group =
            case_when(
                hcluster == 1 ~ "Deep Poverty",
                hcluster == 3 ~ "Single Parent HH",
                hcluster == 4 ~ "Black v. White Poverty",
                hcluster == 5 ~ "Hisp. v. White Poverty",
                hcluster == 10 ~ "Low/High Housing Costs",
                hcluster == 13 ~ "Moderately High Housing Costs",
                hcluster == 14 ~ "Rent burdened",
                hcluster == 19 ~ "Not Overcrowded",
                hcluster == 25 ~ "Home Values",
                hcluster == 28 ~ "Income Inequality",
                hcluster == 29 ~ "Concentrated Wealth",
                hcluster == 40 ~ "Residential Stability",
                hcluster == 45 ~ "Black v. White Single Parent",
                hcluster == 46 ~ "Hisp. v. White Single Parent",
                hcluster == 49 ~ "Low/High Vehicle Ownership",
                hcluster == 53 ~ "Poverty + Low Education",
                hcluster == 56 ~ "Educational diversity",
                hcluster == 59 ~ "Income diversity",
                hcluster == 60 ~ "Work Participation + Vacant Housing",
                hcluster == 61 ~ "Marriage",
                hcluster == 68 ~ "Nr. Total Social Capital Orgs.",
                hcluster == 69 ~ "Nr. Social Support Services",
                hcluster == 70 ~ "Population + Exposure Index",
                hcluster == 71 ~ "Racial/ethnic diversity",
                hcluster == 72 ~ "% Black",
                hcluster == 73 ~ "% Hisp.",
                hcluster == 76 ~ "Homicides",
                hcluster == 77 ~ "Homicides (Max Averaged)",
                hcluster == 78 ~ "Homicides (Min Averaged)",
                hcluster == 79 ~ "% Republican",
                hcluster == 80 ~ "% Uniquely Republican",
                hcluster == 83 ~ "Crime",
                hcluster == 84 ~ "Delta Index (Race, Education, Income)",
                hcluster == 85 ~ "Dissimilarity Index (Education, Income)",
                hcluster == 86 ~ "Dissimilarity Index (Race)",
                hcluster == 88 ~ "Spatial Proximity (Education)",
                hcluster == 89 ~ "Spatial Proximity (Income)",
                hcluster == 90 ~ "Spatial Proximity (Race)",
                hcluster == 91 ~ "State-level political ideology",
                hcluster == 92 ~ "State-level Democratic ideology",
                hcluster == 93 ~ "State-level Republican ideology",
                hcluster == 94 ~ "Political Polarization",
                T ~ variable
            )
    )

list_hcluster06 <- split(df_hclusters06$variable, df_hclusters06$group)
saveRDS(list_hcluster06, file.path(out_dir, "list_hcluster06.rds"))
