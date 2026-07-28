# Exclusion / participant-flow diagnostics for the STROBE flowchart.
# From clean_data(): call BEFORE the intermediate-variable drop
diagnose_exclusions <- function(raw_data) {
  library(data.table)
  n_all_w2_10   <- raw_data[wave %in% 2:10, uniqueN(pidp)]
  n_not_w2      <- n_all_w2_10 - raw_data[wave == 2L, uniqueN(pidp)]
  n_emp_any     <- raw_data[wave %in% 2:10 & !is.na(econ_emp_bin), uniqueN(pidp)]
  n_age_excl    <- raw_data[wave == 2L & (age_dv < 25 | age_dv > 64), uniqueN(pidp)]
  message(sprintf("All respondents waves 2-10:                  %d", n_all_w2_10))
  message(sprintf("  Excluded: no wave 2 response:              %d", n_not_w2))
  message(sprintf("  Employed or not-employed (any wave 2-10):  %d", n_emp_any))
  message(sprintf("  Excluded: age outside 25-64 at wave 2:     %d", n_age_excl))
  invisible(raw_data)
}

# From preproc_data(): call after the balanced-panel expansion (exp_data).
diagnose_response <- function(exp_data) {
  library(data.table)
  message(sprintf("  At wave 2 with response == 1:              %d", exp_data[wave == 2L & response == 1L, uniqueN(pidp)]))
  # response == 0 mixes two things; report them separately for the flowchart
  if ("nonresponse_reason" %in% names(exp_data)) {
    message(sprintf("  At wave 2, interviewed but age outside 25-64: %d",
      exp_data[wave == 2L & nonresponse_reason == "age_out_of_range", uniqueN(pidp)]))
    message(sprintf("  At wave 2, not interviewed:                %d",
      exp_data[wave == 2L & nonresponse_reason == "not_interviewed", uniqueN(pidp)]))
  }
  invisible(exp_data)
}

# From preproc_data(): call after the baseline join (pop_data).
diagnose_analytical_samples <- function(pop_data) {
  library(data.table)
  w2_resp <- pop_data[wave == 2L & response == 1L, pidp]
  message(sprintf("  3-wave analytical sample (wave 2 & 5, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 5L & response == 1L, pidp]))))
  message(sprintf("  4-wave analytical sample (wave 2 & 6, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 6L & response == 1L, pidp]))))
  message(sprintf("  5-wave analytical sample (wave 2 & 7, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 7L & response == 1L, pidp]))))
  invisible(pop_data)
}
