library(gfoRmula)
source("R/data_import_and_cleaning.R")
source("custom_helpers.R")
library(dtplyr)

pop_data |> 
  lazy_dt() |> 
  filter(wave %in% 9:10) |> 
  mutate(t0 = t0 - 3) |> 
  filter(!if_any(everything(), is.na))

# super-simple version ----------------------------------------------------
late_pop <-
  pop_data[wave %in% 9:10][, `:=`(t0 = t0 - 3)]

missings <-
  late_pop[is.na(sf12mcs_dv) |
             is.na(sf12pcs_dv) | is.na(sex_dv_base) |
             is.na(econ_emp_bin), .(pidp)]

late_pop <- late_pop[!missings, on = .(pidp)]

outcome_type <- "continuous_eof"

covnames <-
  c(
    "econ_emp_bin",
    "sf12pcs_dv"
  )

outcome_name <- "sf12mcs_dv"
intvars <- c("econ_emp_bin")

covtypes <- c(
  "binary", # econ_emp_bin
  "normal" # sf12pcs_dv
)

basecovs <- c(
  "sex_dv_base"
)

ymodel <- sf12mcs_dv ~  sex_dv_base +  # Baseline - sex
  sf12pcs_dv + lag1_sf12pcs_dv +       # Time-varying confounders
  econ_emp_bin + lag1_econ_emp_bin     # Time-varying exposures


covparams <- list(
  covmodels = c(
    econ_emp_bin ~ sex_dv_base +
      sf12pcs_dv + lag1_sf12pcs_dv + 
      lag1_econ_emp_bin,
    sf12pcs_dv ~ sex_dv_base +
      lag1_sf12pcs_dv + 
      lag1_econ_emp_bin
  )
)

# laggedvars(Lsf12mcs_dv Lsf12pcs_dv Lage_dv Llog_income Lecon_emp_bin Lecon_dist intdaty_lag)

lagged_variables <- c(
  "sf12pcs_dv",
  "econ_emp_bin"
)


histories <- c(lagged)
histvars <- list(lagged_variables)

nsimul <- 2000 # Monte Carlo sample size
ncores <- min(32, parallel::detectCores() - 1)



interventions <- tibble(econ_emp_bin1 = 0:1,
                        econ_emp_bin2 = 0:1) |> 
  expand.grid() |>
  t() |>
  as_tibble(.name_repair = "minimal") |>
  unclass() |>
  map(~list(c(static, .x)))

intvars <- as.list(rep("econ_emp_bin", length(interventions)))

int_description <-
  map(interventions, ~paste(.x[[1]][-1], collapse = "-") |> paste0("econ_emp_bin:", x = _))

time_points <- 2

gform_cont_eof <- gformula(obs_data = late_pop,
                           outcome_type = outcome_type,
                           id = "pidp",
                           time_name = "t0",
                           time_points = time_points,
                           covnames = covnames,
                           outcome_name = outcome_name,
                           covtypes = covtypes,
                           covparams = covparams,
                           # covpredict_custom = covpredict_custom,
                           ymodel = ymodel,
                           
                           intvars = intvars,
                           interventions = interventions,
                           int_descript = int_description,
                           
                           histories = histories,
                           histvars = histvars,
                           basecovs = basecovs,
                           seed = 1234,
                           parallel = FALSE,
                           # nsamples = 200, # Number of bootstrap samples
                           # nsimul = nsimul,
                           ref_int = 1
)

gform_cont_eof



# manual calcs ------------------------------------------------------------

library(DiagrammeR)

grViz("digraph {
  graph []
  node [fontname=Arial]
  A1 [label=<Empl<SUB>0</SUB>>]
  A2 [label=<Empl<SUB>1</SUB>>]
  L1 [label=<PCS<SUB>0</SUB>>]
  L2 [label=<PCS<SUB>1</SUB>>]
  Y [label='MCS']
  S [label='Sex', color=grey]
  edge []
  S -> A1, A2, L1, L2, Y [color=grey]
  A1 -> A2
  A1 -> Y
  L1 -> L2, A2 -> Y
  L1 -> A1 -> L2 -> A2
  {rank = same; L1; L2; Y}
  {rank = same; A1; A2}
  {rank = min; L1; L2; Y}
  {rank = max; S}
}")

grViz("digraph {
  graph []
  node [fontname=Arial]
  A1 [label=<Empl<SUB>0</SUB>>]
  A2 [label=<Empl<SUB>1</SUB>>]
  L1 [label=<PCS<SUB>0</SUB>>]
  L2 [label=<PCS<SUB>1</SUB>>]
  Y [label='MCS']
  edge []
  A1 -> A2
  A1 -> Y
  L1 -> L2, A2 -> Y
  L1 -> A1 -> L2 -> A2
  {rank = same; L1; L2; Y}
  {rank = same; A1; A2}
  {rank = min; L1; L2; Y}
}")



library(magrittr)

pop_df <- as_tibble(late_pop) |> 
  select(-wave) |> 
  pivot_wider(id_cols = c(pidp, ends_with("base")), names_from = t0, values_from = sf12mcs_dv:econ_dist) |> 
  select(
    Y = sf12mcs_dv_1,
    A1 = econ_emp_bin_0,
    A2 = econ_emp_bin_1,
    L1 = sf12pcs_dv_0,
    L2 = sf12pcs_dv_1,
    sex = sex_dv_base
  )

# Try it by intuition -----------------------------------------------------

grViz("digraph {
  graph []
  node [fontname=Arial]
  A1 
  A2 
  L1 
  L2 
  Y 
  edge []
  A1 -> A2
  A1 -> Y
  L1 -> L2, A2 -> Y
  L1 -> A1 -> L2 -> A2
  {rank = same; L1; L2; Y}
  {rank = same; A1; A2}
  {rank = min; L1; L2; Y}
}")

main_mod <- lm(Y ~ sex + L1 + A1 + L2 + A2, data = pop_df)

modl2 <- pop_df %$%
  lm(L2 ~ sex + L1 + A1)

data_to_test <- tibble(
  intervention = c("0-0", "1-1"),
  data = map(0:1, \(set_val) mutate(pop_df, across(starts_with("A"), ~set_val)))
) |> 
  bind_rows(
    tibble(intervention = "natural", data = list(pop_df)),
    x = _
  ) 

data_to_test |> 
  mutate(
    data_update = map(data, ~mutate(.x, L2 = predict(modl2, newdata = .x))),
    outcomes = map(data_update, ~predict(main_mod, newdata = .x)),
    gform_mean = map_dbl(outcomes, mean)
    ) 

full_ints <- tibble(A1 = 0:1, A2 = 0:1) |>
  expand.grid() |> 
  mutate(intervention = paste(A1, A2, sep = "-")) |> 
  nest(new_ints = -intervention) |> 
  mutate(
    data_newvals = list(pop_df |>  select(-A1, -A2)),
    data_newvals = map2(new_ints, data_newvals, bind_cols)
  ) |> 
  bind_rows(
    tibble(intervention = "natural", data_newvals = list(pop_df)),
    x = _
  ) 

full_ints |> 
  mutate(
    # Predict confounder at t2 based on t1 vars
    data_update = map(data_newvals, ~mutate(.x, L2 = predict(modl2, newdata = .x))),
    # re-do predicton from main model based on updated L2s
    outcomes = map(data_update, ~predict(main_mod, newdata = .x)),
    # means of new values
    gform_mean = map_dbl(outcomes, mean)
  ) 

# by MI method ------------------------------------------------------------

library(gFormulaMI)

regimes <- tibble(a = 0:1, b = 0:1) |>
  expand.grid() |>
  arrange(a, b) |>
  t() |>
  as_tibble() |>
  unclass() |> unname()



imps <- gFormulaImpute(
  pop_df,
  M = 40,
  nSim = 100000,
  trtVars = c("A1", "A2"),
  trtRegimes = regimes
)


fits <- imps %$%
  lm(Y ~ factor(regime))

outvals <- syntheticPool(fits)

outvals |> 
  as_tibble() |> 
  rownames_to_column("Intervention") |> 
  mutate(Estimate = ifelse(Intervention == 1, Estimate,  Estimate[Intervention == 1] + Estimate) |> round(1))




