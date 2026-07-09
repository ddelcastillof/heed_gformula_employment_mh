# Exclusion / participant-flow diagnostics for the STROBE flowchart.
library(data.table)

# From clean_data(): call BEFORE the intermediate-variable drop
diagnose_exclusions <- function(raw_data) {
  n_all_w2_10   <- raw_data[wave %in% 2:10, uniqueN(pidp)]
  n_not_w2      <- n_all_w2_10 - raw_data[wave == 2L, uniqueN(pidp)]
  n_emp_any     <- raw_data[wave %in% 2:10 & !is.na(econ_emp_bin), uniqueN(pidp)]
  n_excl_status <- raw_data[wave %in% 2:10 & (econ_ltsick == 1L | econ_student == 1L | econ_retire == 1L), uniqueN(pidp)]
  n_age_excl    <- raw_data[wave == 2L & (age_dv < 25 | age_dv > 65), uniqueN(pidp)]
  message(sprintf("All respondents waves 2-10:                  %d", n_all_w2_10))
  message(sprintf("  Excluded: no wave 2 response:              %d", n_not_w2))
  message(sprintf("  Employed or not-employed (any wave 2-10):  %d", n_emp_any))
  message(sprintf("  Excluded: ever retired/student/LTS (2-10): %d", n_excl_status))
  message(sprintf("  Excluded: age outside 25-65 at wave 2:     %d", n_age_excl))
  invisible(raw_data)
}

# From preproc_data(): call after the balanced-panel expansion (exp_data).
diagnose_response <- function(exp_data) {
  message(sprintf("  At wave 2 with response == 1:              %d", exp_data[wave == 2L & response == 1L, uniqueN(pidp)]))
  invisible(exp_data)
}

# From preproc_data(): call after the baseline join (pop_data).
diagnose_analytical_samples <- function(pop_data) {
  w2_resp <- pop_data[wave == 2L & response == 1L, pidp]
  message(sprintf("  3-wave analytical sample (wave 2 & 5, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 5L & response == 1L, pidp]))))
  message(sprintf("  4-wave analytical sample (wave 2 & 6, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 6L & response == 1L, pidp]))))
  message(sprintf("  5-wave analytical sample (wave 2 & 7, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 7L & response == 1L, pidp]))))
  message(sprintf("  6-wave analytical sample (wave 2 & 8, response==1): %d",
    length(intersect(w2_resp, pop_data[wave == 8L & response == 1L, pidp]))))
  invisible(pop_data)
}
