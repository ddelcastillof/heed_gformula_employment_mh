make_wide <- function(df, id_col, time_col, base_cols, outcome, ..., static = FALSE) {
  
  require(rlang)
  
  outcome_name <- as_name(ensym(outcome))
  
  t_conf <- enquos(...)
  
  t_max <- max(df |> pull({{time_col}}))
  t_min <- min(df |> pull({{time_col}}))
  
  df_out <- df |> 
    select({{id_col}}, {{time_col}}, {{base_cols}}, !!!t_conf, {{outcome}}) |> 
    pivot_wider(
      id_cols = c({{id_col}}, {{base_cols}}), 
      names_from = {{time_col}}, 
      values_from = -c({{id_col}}, {{time_col}}, {{base_cols}})) |> 
    select(
      {{base_cols}},
      ends_with("0"),
      ends_with("1"),
      ends_with("2"),
      ends_with("3"),
      ends_with("4"),
      ends_with("5"),
      ends_with("6"),
      ends_with("7"),
      ends_with("8"),
      ends_with("9")
    )
  
  if (static) {
      intermediate_cols <- paste0(outcome_name, "_", (t_min + 1):(t_max - 1))
      df_out <- df_out |> select(-any_of(intermediate_cols))
    }

  attr(df_out, "n_tvars") <- length(t_conf)
  attr(df_out, "n_base") <- length(df[1,] |> select({{base_cols}}))
  
  df_out
}

make_predictor_matrix <- function(return_vals) {
  n_tvars <- attr(return_vals, "n_tvars")
  n_base <- attr(return_vals, "n_base")
  n_vars <- length(return_vals) + 1
  
  p_mat <- matrix(0, ncol = n_vars, nrow = n_vars)
  
  p_mat[(n_base + 1):n_vars, 1:(n_base)] <- 1
  
  p_mat
  
  
  for (n in 1:(n_vars - n_base)) {
    p_mat[n_base + n, max(0, n_base + n - n_tvars):(n_base + n - 1)] <-
      1
  }
  
  p_mat[n_vars, 1:(n_vars - 1)] <- 1
  
  rownames(p_mat) <- colnames(p_mat) <- c(colnames(return_vals), "regime")
  
  p_mat
  
}

pool_gform_estimates <- function(long_out) {
  
  n_runs <- max(long_out$`.imp`)
  
  long_out |> 
    select(.imp, gform_out) |> 
    unnest(gform_out) |> 
    janitor::clean_names() |> 
    filter(imp != 0) |> 
    summarise(
      gform_effect = mean(g_form_mean),
      var_w = safely(~mean(.x^2), na_dbl)(mean_se)$result,
      var_b = var(g_form_mean),
      .by = "interv") |> 
    mutate(
      var_t = var_w + var_b + var_b/n_runs,
      gform_se = sqrt(var_t)
    ) |> 
    select(interv, gform_effect, gform_se) |> 
    mutate(
      gform_ll = gform_effect + gform_se * qnorm(0.025),
      gform_ul = gform_effect + gform_se * qnorm(0.975),
    )
  
}

set_n_periods <- function (newdf, pool, intvar, intvals, time_name, t) {
  
  if(!is.na(intvals[[t + 1]])){
    
    set(newdf, j = intvar, value = intvals[[t + 1]])
    
  }
  
}
