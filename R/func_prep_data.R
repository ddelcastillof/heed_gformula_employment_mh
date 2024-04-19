

make_wide <- function(df, id_col, time_col, base_cols, outcome, ...) {
  
  require(rlang)
  
  t_conf <- enquos(...)
  
  t_max <- max(df |> pull({{time_col}}))
  
  df_out <- df |> 
    select({{id_col}}, {{time_col}}, {{base_cols}}, {{outcome}}, !!!t_conf) |> 
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
      ends_with("9"),
      -starts_with(quo_name(quo({{outcome}}))),
      matches(paste(quo_name(quo({{outcome}})), t_max, sep = "_"))
    )
  
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
  
  p_mat
  
}
