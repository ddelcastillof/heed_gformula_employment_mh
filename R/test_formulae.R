box::use(glue[glue])

rownames(predictor_matrix) |> 
  map(\(variable) {
    

    period <- str_extract(variable, "\\d$")
    lag1 <- as.character(as.numeric(period) - 1)
    
    if (is.na(period)) return("")
    if (period == "0") return("")
    
    dep_vars <- which(predictor_matrix[variable,] == 1) |> 
      names()
    
    baseline_vars <- str_subset(dep_vars, "_base$")
    
    update_vars <- str_subset(dep_vars, "_\\d$") |> 
      str_replace_all(
        glue("(.*)_{lag1}"), "lag1_\\1"
      ) |> 
      str_replace_all(
        glue("_{period}"), ""
      )
    
    if (length(update_vars) + length(baseline_vars) == 0) return("")
    
    glue("{variable_stat} ~ {baselines} +\n\t{updates}\n", 
         variable_stat = str_remove(variable, "_\\d$"),
         baselines = paste(baseline_vars, collapse = " + "),
         updates = paste(update_vars, collapse = " + ")) 
    
  }) |> 
  unique() |> 
  reduce(c) |> 
  cat(sep = ",\n\n")
