################################################################################
# results_best <-
#     results_manual |>
#     mutate(mean = if_else(.metric == "rsq", -mean, mean)) |>
#     group_by(.metric) |>
#     filter(mean == min(mean)) |>
#     filter(.metric == "rmse")
# 
# final_results <-
#     lmreg_workflow |>
#     finalize_workflow(results_best) |>
#     last_fit(split = split)
# 
# final_model <-
#     final_results |>
#     extract_workflow() |>
#     extract_fit_parsnip()
# 
# coefficients <- tidy(final_model)