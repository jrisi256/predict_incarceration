library(here)
library(tune)
library(readr)
library(tidyr)
library(dplyr)
library(purrr)
library(dials)
library(future)
library(stringr)
library(recipes)
library(rsample)
library(parsnip)
library(ggplot2)
library(finetune)
library(yardstick)
library(workflows)
library(workflowsets)

################################################################################
# Read in full data.
data <- read_csv(here("4_clean_missing_data", "output", "data_no_missing.csv"))

################################################################################
# Set train and test sets.
set.seed(69)
split <- group_initial_split(data, prop = 0.8, group = full_fips)
train <- training(split)
test <- testing(split)

################################################################################
# Model fitting with parsnip.
ind_vars <- train |> select(-matches("^state$|^county$|full_fips|^year$|total_prison"))
dep_vars <- train |> select("total_prison_adm_rate")

# Model fitting with Parsnip.
# Step 1: Specify the model (e.g., linear regression, random forest, GAM, etc.)
# Step 2: Specify the engine i.e., the package you want to use to estimate the model.
# Step 3: Set the mode i.e., numeric outcomes or categorical outcomes.

lm_model <- linear_reg() |> set_engine("lm")
lm_fit_parsnip <- lm_model |> fit_xy(x = ind_vars, y = dep_vars)
predictions_parsnip <- predict(lm_fit_parsnip, test)

################################################################################
# Workflows allow one to bundle data processing steps with the modeling process.
# Step 1: You need to set your parsnip model object (model + engine).
# Step 2: One can then add variables and/or the formula directly.
workflow <-
    workflow() |>
    add_model(lm_model) |>
    add_variables(
        outcome = total_prison_adm_rate,
        predictors = -matches("^state$|^county$|full_fips|^year$|total_prison")
    )

lm_fit_workflow <- fit(workflow, train)
predictions_workflow <- predict(lm_fit_workflow, test)

# The workflowsets package allows one to create many different workflows by
# combining sets of pre-processing steps with sets of model specifications.
preprocessing <-
    list(
        "prison_admissions" =
            workflow_variables(
                total_prison_adm_rate,
                -matches("^state$|^county$|full_fips|^year$|total_prison")
            ),
        "prison_population" =
            workflow_variables(
                total_prison_pop_rate,
                -matches("^state$|^county$|full_fips|^year$|total_prison")
            )
    )

# A resulting workflow set will cross every pre-processing step with every
# model type. It will return a data frame where each row represents specific
# information about a specific workflow. The actual workflow itself is stored
# in the info column and then in the resulting data frame, it is in the workflow
# column.
workflow_set <-
    workflow_set(preproc = preprocessing, models = list("lm" = lm_model))

estimated_models <-
    workflow_set |>
    mutate(
        fit = map(info, function(df) {fit(df$workflow[[1]], train)}),
        predictions = map(fit, function(fitted_model) {predict(fitted_model, test)})
    )

################################################################################
# Recipes allow one to capture any data transformations and bundle it with your
# modeling workflow. The goal is really to prevent any data leakage, remember.
# So it's important, as much as possible, to properly separate operations done
# on train and test. Even as something as simple as normalizing the data.
# The key to recipes are the step functions which apply data transformation
# steps to the appropriate columns.
recipe <-
    recipe(train) |>
    update_role(total_prison_adm_rate, new_role = "outcome") |>
    update_role(-matches("^state$|^county$|full_fips|^year$|total_prison"), new_role = "predictor") |>
    update_role(matches("^state$|^county$|full_fips|^year$"), new_role = "id") |>
    step_normalize(all_predictors())

# You can also add recipes to workflows thus combining the pre-processing steps
# with your modeling steps.
workflow_with_recipe <-
    workflow() |>
    add_model(lm_model) |>
    # Note that when we add the recipe, the independent and dependent variables
    # are also added so you do not need to explicitly set them yourself.
    add_recipe(recipe)

lm_fit_with_recipe <- fit(workflow_with_recipe, train)
predictions_recipe <- predict(lm_fit_with_recipe, test)

################################################################################
# We can use the yardstick package to determine model efficacy.
model_results <-
    test |>
    select(matches("^state$|^county$|full_fips|^year$"), total_prison_adm_rate) |>
    mutate(
        predicted_adm_parsnip = predictions_parsnip$.pred,
        predicted_adm_recipe = predictions_recipe$.pred,
        predicted_adm_workflow = predictions_workflow$.pred
    )

model_metrics <- metric_set(rmse, rsq, mae)

model_performance <-
    model_metrics(
        model_results,
        truth = total_prison_adm_rate,
        estimate = predicted_adm_recipe
    )

################################################################################
# Let's get into cross-validation, shall we?
# Here we conduct an aside where we estimate a random forest model.
rf_model <-
    rand_forest(trees = 50) |>
    set_engine("ranger") |>
    set_mode("regression")

rf_workflow <-
    workflow() |>
    add_model(rf_model) |>
    add_variables(
        outcome = total_prison_adm_rate,
        predictors = -matches("^state$|^county$|full_fips|^year$|total_prison")
    )

rf_fit <- rf_workflow |> fit(train)

rf_fit_predictions <- predict(rf_fit, test)
model_results <- model_results |> mutate(predictions_rf = rf_fit_predictions$.pred)

model_performance_rf <-
    model_metrics(
        model_results,
        truth = total_prison_adm_rate,
        estimate = predictions_rf
    )

# Now let's actually get into cross-validation. For some reason, tidymodels
# calls k-fold cross-validation v-fold. The cross-validation performance metrics
# are averaged across all estimated models and predictions. As a reminder, having
# more folds (preferred) tends to decrease bias but increase variance. To decrease
# variance, it is generally preferred to simply repeat the fold process. If you
# wanted to reduce variance, you could also decrease the number of folds, but
# this has the unfortunate side-effect of increasing bias.
validation_folds <- vfold_cv(train, v = 10, repeats = 3)

# Allows you to control certain settings for the cross-validation process.
cv_settings <- control_resamples(save_pred = T, save_workflow = T)

# Here is where we are actually going to set-up the cross-validation.
# This returns a data frame with each train/test split, the predictions from each
# fitted model, and the performance metrics. As well as anything else you specify.
plan(sequential)
start <- Sys.time()
rf_fit_with_cv <-
    rf_workflow |>
    # This replaces the normal fit function call.
    fit_resamples(
        resamples = validation_folds, control = cv_settings, metrics = model_metrics
    )
end <- Sys.time()
sequential_time <- end - start

# Thankfully, there are convenience functions which aggregate the performance
# metrics across all the re-samples.
rf_cv_performance <- collect_metrics(rf_fit_with_cv)
rf_cv_performance_non_agg <- collect_metrics(rf_fit_with_cv, summarize = F)
# And convenience functions which aggregate the predictions which is pretty cool.
rf_cv_predictions <- collect_predictions(rf_fit_with_cv)
# These convenience functions just make it easier to interact with the fitted
# object which, at first glance, is unwieldy because it is gnarly nested data
# frame.

# Cross-validation is also very easy to make run in parallel.
parallel::detectCores(logical = T) # Check number of the cores available.
plan(multisession, workers = 2)
start_p <- Sys.time()
rf_fit_with_cv_parallel <-
    rf_workflow |>
    # This replaces the normal fit function call.
    fit_resamples(
        resamples = validation_folds, control = cv_settings, metrics = model_metrics
    )
end_p <- Sys.time()
parallel_time <- end_p - start_p

################################################################################
# Let's compare different models and use cross-validation to see which one
# does better within the training set. For this exercise, we will see if we can
# predict admissions or population better.
# Step 1. Create your recipes.
recipe_admissions <-
    recipe(train) |>
    update_role(total_prison_adm_rate, new_role = "outcome") |>
    update_role(-matches("^state$|^county$|full_fips|^year$|total_prison"), new_role = "predictor") |>
    update_role(matches("^state$|^county$|full_fips|^year$"), new_role = "id") |>
    step_normalize(all_predictors())

recipe_population <-
    recipe(train) |>
    update_role(total_prison_pop_rate, new_role = "outcome") |>
    update_role(-matches("^state$|^county$|full_fips|^year$|total_prison"), new_role = "predictor") |>
    update_role(matches("^state$|^county$|full_fips|^year$"), new_role = "id") |>
    step_normalize(all_predictors())

recipes <-
    list(
        "prison_admissions" = recipe_admissions,
        "prison_population" = recipe_population
    )

# Step 2. Create a workflow set which is just a cross of recipes and models.
model_comparison <-
    workflow_set(
        preproc = recipes,
        models = list("lm" = lm_model, "rf" = rf_model)
    )

# Step 3. We want to run cross-validation on each of these workflows now. How
# can do that efficiently? Using workflow_map. You supply the workflow argument.
# You supply the function you want to apply to that workflow. Then you supply the
# arguments to the function you will be applying to each workflow.
validation_folds <- vfold_cv(train, v = 3, repeats = 2)

model_comparison_cv <-
    model_comparison |>
    workflow_map(
        fn = "fit_resamples",
        verbose = T,
        seed = 69,
        resamples = validation_folds,
        control = cv_settings,
        metrics = model_metrics
    )

# We can use the convenience functions from before to collect performance metrics
# and what not.
model_comparison_predictions <-
    collect_predictions(model_comparison_cv) |>
    pivot_longer(cols = matches("total_prison"), names_to = "outcome", values_to = "value") |>
    filter(!is.na(value))

ggplot(model_comparison_predictions, aes(x = value, y = .pred)) +
    geom_point() +
    facet_wrap(~outcome+model) +
    theme_bw() +
    geom_abline(slope = 1, intercept = 0, color = "red")

model_comparison_metrics <-
    collect_metrics(model_comparison_cv) |>
    mutate(outcome = if_else(str_detect(wflow_id, "admissions"), "adm", "pop"))

ggplot(model_comparison_metrics, aes(x = model, y = mean)) +
    geom_point(aes(color = outcome)) +
    geom_errorbar(aes(ymin = mean - std_err, ymax = mean + std_err, color = outcome), width = 0.1) +
    facet_wrap(~.metric, scale = "free_y") +
    theme_bw()

################################################################################
# Now let's get into the true reason for the season. Cross-validation for
# hyperparameter tuning. Two general strategies are: 1) grid searches and 2)
# iterative search. Grid searches involve generating a set of hyperparameters
# and traversing through the space to find the optimal configuration. The main
# problems are: A) generating a representative space of hyperparameters (how
# do you know you've covered the entire terrain so to speak), and B) ensuring
# your space is parsimonious so you can efficiently travel through the space.
# On the other hand, you have iterative search which is usually where you use prior
# hyperparameter states to inform your current choice of hyperparameters. Usually,
# it is quite computationally expensive to utilize this approach. Hybrid solutions
# are quite popular and can work well. One starts with a grid search and proceeds
# with an iterative search after finding a good set of initial starting values.

# In regards to tidymodels specific features, there are two kinds of hyperparameters.
# The first are main hyperparameters which are usually the hyperparameters that
# are tuned the most, and they are commonly available in multiple engines e.g.,
# the rand_forest() model has main arguments trees, min_n, and mtry since these
# are the most commonly optimized parameters, and they are available in multiple
# packages. I.e., a harmonized naming system has been created to remove inconsistencies
# across engines. A secondary set of hyperparameters are engine (or package) specific.
# These are either infrequently optimized or are only available in certain engines
# (i.e., packages). E.g., the ranger engine (package) contains the gain regularization
# hyperparameter. In practice, this will look something like the following code.
hyperparam_example <-
    rand_forest(trees = 2000, min_n = 10) |>
    set_engine("ranger", regularization.factor = 0.5)

# How do we mark which arguments should be optimized? We can use the tune()
# function like below.
hyperparam_example_tune <-
    rand_forest(trees = 2000, min_n = tune()) |>
    set_engine("ranger", regularization.factor = tune())

# To get a list of hyperparameters we are tuning, we can use the following code.
list_of_tune <- extract_parameter_set_dials(hyperparam_example_tune)

# Note that we can use the tune() function to assign unique IDs to a hyperparameter
# when the same hyperparameter is being tuned in different places. This can
# happen when preprocessing the data.
hyperparam_example_tune_named <-
    rand_forest(trees = 2000, min_n = tune("min_n"), mtry = tune()) |>
    set_engine("ranger", regularization.factor = tune("reg_factor"))

list_of_tune_named <- extract_parameter_set_dials(hyperparam_example_tune_named)

# For some hyperparameters (like mtry), the range of values it can take on is
# data-dependent (e.g., mtry governs how many columns to sub-sample for each tree).
# You can use the finalize() function on a recipe, and the final range of values
# can be obtained.

################################################################### Grid Search
############################## Regular grids.
# Regular grids combines each hyperparameter (with its corresponding set of
# possible values) factorially i.e., all combinations of all values are generated.
rand_forest_tune <-
    rand_forest(trees = 30, min_n = tune()) |>
    set_engine("ranger", regularization.factor = tune()) |>
    set_mode("regression")

rand_forest_hyperparams <- rand_forest_tune |> extract_parameter_set_dials()

# The grid_regular function is used to create a factorial grid of all possible
# hyperparameter values. The "levels" argument is used to designate how many
# values to test for each hyperparameter which will generated an equally spaced
# number of values between the min and max values for the hyperparameter.
grid_regular_3 <- grid_regular(rand_forest_hyperparams, levels = 3)
grid_regular_custom <-
    grid_regular(
        rand_forest_hyperparams,
        levels = c(min_n = 3, regularization.factor = 4)
    )

################################ Irregular grids.
# You can create an irregular grid through random sampling. Generates independent
# random numbers by pulling from a uniform distribution with min and max equal to
# the min and max of the hyperparameter.
grid_random <- rand_forest_hyperparams |> grid_random(size = 1000)

# However, one can also use space-filling designs. There are many different
# algorithms with different optimization criteria, but their general goal is to
# find a configuration of points that cover the parameter space with the smallest
# chance of overlap.
grid_lhcube <-
    rand_forest_hyperparams |>
    grid_space_filling(size = 1000, type = "latin_hypercube")

################################################################################
# Let's get to testing these results and searching the hyperparameter space.
rand_forest_workflow <-
    workflow() |>
    add_model(rand_forest_tune) |>
    add_recipe(recipe_admissions)

rand_forest_tune_vfold <- vfold_cv(train, v = 2, repeats = 2)

# tune_grid() is the workhorse function here. The first argument is either a 
# model or a workflow. If the first argument is a model, the second argument is
# a recipe or formula. You also need to pass an rsample resampling object.
# It's actually quite similar to the fit_resamples function.
rand_forest_tune_results <-
    rand_forest_workflow |>
    tune_grid(rand_forest_tune_vfold, grid = grid_regular_3, metrics = model_metrics)

# There is a convenience function to examine results, but I prefer to do it myself
# so I know what's really happening under the hood.
results <- show_best(rand_forest_tune_results, metric = "rmse", n = 9)
results_manual <-
    bind_rows(rand_forest_tune_results$.metrics) |>
    group_by(min_n, regularization.factor, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n()) |>
    ungroup() |>
    mutate(std_error = sd / sqrt(n)) |>
    filter(.metric == "rmse") |>
    arrange(mean)

# After inspecting the values, we can choose the hyperparameters we think
# would be the best fit and insert them back into our workflow.
final_rand_forest_workflow <-
    rand_forest_workflow |>
    finalize_workflow(tibble(min_n = 2, regularization.factor = 1))

final_rand_forest_estimated_model <- final_rand_forest_workflow |> fit(train)
final_rand_forest_predictions <- predict(final_rand_forest_estimated_model, test)
final_rand_forest_predictions_df <- test |> mutate(predictions_rf = final_rand_forest_predictions$.pred)

model_performance_rf_tune <-
    model_metrics(
        final_rand_forest_predictions_df,
        truth = total_prison_adm_rate,
        estimate = predictions_rf
    )

# One last type of grid search to consider are racing methods. These do not really
# promise better performance but generally run much faster than normal grid searches.
# One problematic issue with grid searches is they need to be fit across all
# resamples before any tuning parameters can be evaluated. It would be helpful
# to do an interim analysis and eliminate truly bad parameter candidate sets.
# Basically, the tuning process evaluates all models on an initial subset of
# resamples. Based on their performance, some candidates get dropped. The most
# common approach is to calculate how much worse (or better) each candidate
# set is relative to the current best. Any candidate parameter set whose
# confidence interval overlaps with 0 (or is less strictly less than 0) would be
# dropped since it would not be definitive that that candidate parametr set is
# better than the current best. And for example, if this happened in a 10-fold
# cross-validation scenario, this might happen after 3 folds. There are a variety
# of different racing methods. Here, we use ANOVA model racing to test for the
# statistical significance of the difference.
rand_forest_tune_race_results <-
    rand_forest_workflow |>
    tune_race_anova(
        rand_forest_tune_vfold,
        grid = grid_regular_3,
        metrics = model_metrics,
        control = control_race(verbose = T, verbose_elim = T)
    )
results_race_results <-
    bind_rows(rand_forest_tune_race_results$.metrics) |>
    group_by(min_n, regularization.factor, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n()) |>
    ungroup() |>
    mutate(std_error = sd / sqrt(n)) |>
    filter(.metric == "rmse") |>
    arrange(mean)

# A note on parallel processing. Normally, there are two loops so to speak.
# The outer loop loops over each re-sample, and the inner loop loops over each
# hyperparameter set. By default, parallel processing only occurs on the outer loop.
# This may not be the most efficient set-up, though, if 1) the pre-processing/data cleaning
# steps are not very computationally demanding (since these are done in the outer loop),
# and/or 2) you have a lot of nodes available for work. In the control functions
# for the tune() function, the argument parallel_over controls how parallel
# processing is done. The default is parallel_over = "resamples". An alternative
# is to cross the number of re-samples with the number of hyperparameter sets to
# create one big set. These allows one to leverage more nodes. However, as mentioned
# above, if one's preprocessing steps are computationally demanding, then this
# will actually be less efficient because these preprocessing steps will actually
# need to be executed quite a few more times.

############################################################### Iterative search
# There are many different algorithms for how to conduct an iterative search. I
# will only be going over two, but they are definitely two of the most popular.
############################################# Bayesian optimization.
# The basic idea is that the current hyperparameter resampling results are fed
# into a predictive model which predicts what new values would yield the best
# results. This this then repeats after some set of number of iterations or after
# no more improvements (within some threshold) occur. The primary concerns when
# using this method are: 1) how to create the predictive model, and 2) how to select
# parameters predicted by the model. Fitting the predictive model can be tricky
# at times, and it can be quite computationally expensive especially when you
# have a large number of tuning parameters. However, a benefit of this approach
# is that a full probability model is specified so predictions have a mean and a
# variance (reflecting model confidence). One common way to create the model is
# using Gaussian process models, but there are other ways to create this model.

# Once you have figured out how to model the data, you need to decide how you are
# going to pick the next set of hyperparameter values to evaluate. One common approach
# is to create a large candidate set (perhaps using a space-filling design) and
# then predict the mean and variance for each set. The question becomes how to 
# weight the mean vs. the variance? One simple and naive approach is to pick
# the set of hyperparameters with the largest variance. This ensures, each time,
# you are exploring new space not previously explored (and thus you do not miss
# the true global optimal point). A better approach is to use the expected improvement
# acquisition function. Using the mean and variance, you can plot a distribution
# of possible values and the probability associated with each (following a Gaussian
# process because this is a Gaussian process model). With this method, you may
# find values that have a lower mean, but have more weight in their distribution
# in the area of the curve that is better than the current best hyperparameter set.
# Thus, the expected improvement is better. This is a more intelligent process than
# simply choosing the value with the highest mean or variance each time.

# In tidymodels, the Bayesian optimization approach is implemented using the
# tune_bayes() function. It is very similar to tune_grid() with some additional
# arguments.
# iter controls the maximum number of search iterations.
# initial can be an integer or an object produced by tune_grid().
## Used for creating the first candidate set. An integer specifies the number of
## levels in a space-filling grid.
# objective specifies which acquisition function should be used.
# param_info can be used to change the range of parameters.
# control takes the control_bayes() function as an argument which considers
## no_improve --> Stop the search if improved parameters are not found within no_improve iterations.
## uncertain --> Take an uncertainty sample if there is no improvement within uncertain iterations.
### Basically, pick the next candidate set with very large variation.

ctrl <- control_bayes(verbose = T)

rand_forest_bo <-
    rand_forest_workflow |>
    tune_bayes(
        resamples = rand_forest_tune_vfold,
        metrics = model_metrics,
        initial = rand_forest_tune_results,
        iter = 25,
        control = ctrl
    )

rand_forest_bo_iteration <-
    rand_forest_bo |>
    mutate(
        metric_df =
            pmap(
                list(.metrics, .iter),
                function(df, iter_nr) {df |> mutate(iter = iter_nr)}
            )
    )

rand_forest_bo_results <-
    bind_rows(rand_forest_bo_iteration$metric_df) |>
    group_by(min_n, regularization.factor, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n()) |>
    ungroup() |>
    mutate(std_error = sd / sqrt(n)) |>
    filter(.metric == "rmse") |>
    arrange(mean)

rand_forest_bo_results_iteration_gains <-
    bind_rows(rand_forest_bo_iteration$metric_df) |>
    filter(iter != 0) |>
    group_by(iter, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n()) |>
    ungroup() |>
    mutate(std_error = sd / sqrt(n)) |>
    filter(.metric == "rmse") |>
    arrange(iter)

################################################# Simulated annealing.
# Essentially, this is a random walk. Each new candidate parameter value is a 
# small pertubation of the previous value. If it performs better, it is accepted
# as the new best. However, even it performs worse it can become the new best
# value, so to speak. How? Well, the degree to which it is worse is proportional
# how likely it is to be accepted. And then the deeper into the search we go,
# the less likely the search algorithm is to accept worse values. This forms the
# probability of the value being accepted.

# One important user-specified values is the cooling coefficient which is found
# as cooling_coef in control_sim_anneal(). Lower values (ranges from 0 to 1), lead
# to a higher probability of bad values being accepted (since it is multiplied)
# so lower numbers lead to the algorithm being more forgiving of bad results.

# It then continues for a set number of iterations or halts if only worse values
# are found after some smaller set of iterations. You can also set a restart
# threshold which revisits the globally best parameter after a string of failures
# and starts anew.

# Another important detail is how you plan on perturbing hyperparameter values.
# One popular method is generalized simulated annealing. It works by specifying a
# radius or distance parameter which creates a neighborhood of values to consider.
# A value is chosen randomly within the neighborhood. And so on and so forth.

# Arguments to the control_sim_anneal() function.
# no_improve: Stop the search if no improved results are found within no_improve iterations.
# restart: The number of iterations with no new best results before starting from the previous best.
# radius: A numeric vector ranging from (0, 1) which define the min and max radius of the local neighborhood.
# flip: Probability that defines the chance of altering the value of a categorical parameter.
# cooling_coef: See above.

ctrl_sa <- control_sim_anneal(verbose = T, no_improve = 25)

rand_forest_sa_results <-
    rand_forest_workflow |>
    tune_sim_anneal(
        resamples = rand_forest_tune_vfold,
        metrics = model_metrics,
        initial = rand_forest_tune_results,
        iter = 100,
        control = ctrl_sa
    )

rand_forest_sa_iteration <-
    rand_forest_sa_results |>
    mutate(
        metric_df =
            pmap(
                list(.metrics, .iter),
                function(df, iter_nr) {df |> mutate(iter = iter_nr)}
            )
    )

rand_forest_sa_results <-
    bind_rows(rand_forest_sa_iteration$metric_df) |>
    group_by(min_n, regularization.factor, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n()) |>
    ungroup() |>
    mutate(std_error = sd / sqrt(n)) |>
    filter(.metric == "rmse") |>
    arrange(mean)

rand_forest_sa_results_iteration_gains <-
    bind_rows(rand_forest_sa_iteration$metric_df) |>
    filter(iter != 0) |>
    group_by(iter, .metric) |>
    summarise(mean = mean(.estimate), sd = sd(.estimate), n = n()) |>
    ungroup() |>
    mutate(std_error = sd / sqrt(n)) |>
    filter(.metric == "rmse") |>
    arrange(iter)

rand_forest_sa_results_param_gains <-
    bind_rows(rand_forest_sa_iteration$metric_df) |>
    filter(iter != 0) |>
    distinct(min_n, regularization.factor, iter)
