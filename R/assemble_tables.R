assemble_comparison <- function(mi_results, mi_ate_results) {
  comparison_marginal <- mi_results |>
    dplyr::mutate(intervention = dplyr::case_when(intervention == 0 ~ "No distress",
                                                  intervention == 1 ~ "Distress")) |>
    dplyr::relocate("intervention") |>
    dplyr::left_join(tmle_marginal_means, by = "intervention")

  comparison_ate <- mi_ate_results |>
    dplyr::filter(intervention == 1) |>
    dplyr::mutate(intervention = dplyr::recode_values(intervention, 1 ~ "Distress")) |>
    dplyr::rename(estimand = intervention) |>
    dplyr::relocate("estimand") |>
    dplyr::left_join(tmle_ate_results, by = "estimand")

  return(list(
    marginal = comparison_marginal,
    ate = comparison_ate
  ))
}