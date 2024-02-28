# df <- haven::read_dta("T:/projects/HEED/Data/heed_gform_example_andy.dta")

# redo with harry dat -----------------------------------------------------
df2 <- haven::read_dta("T:/projects/HEED/Data/heed_gform_example.dta")

library(tidyverse)
library(data.table)
library(gfoRmula)

dt2 <- df2 |> 
  rename(y3 = y) |> 
  mutate(id = row_number()) |> 
  select(-u) |> 
  pivot_longer(l0:y3, names_to = c("var", "t0"), values_to = "val", names_pattern = "(.)(.)") |> 
  pivot_wider(names_from = var, values_from = val) |> 
  mutate(t0 = as.integer(t0)) |> 
  as.data.table()


time_points <- 4

# Simple interventions
# intvars <- list('a', 'a')
# interventions <- list(list(c(static, rep(0, time_points))),
#                       list(c(static, rep(1, time_points))))
# int_description <- c('Never treat', 'Always treat')

# Complex interventions
intvars <-  as.list(rep("a", 16))

interventions <- tibble(a = 0:1, b = 0:1, c = 0:1, d = 0:1) |>
  expand.grid() |>
  arrange(a, b, c, d) |> 
  # mutate(e = 0) |>
  t() |>
  as_tibble() |>
  unclass() |>
  map(~list(c(static, .x)))

int_description <-
  map(interventions, ~paste(.x[[1]][2:5], collapse = "-") |> paste0("Unemployed:", x = _))

gf_out <- gformula(
  dt2,
  outcome_type = "continuous_eof", # eofu
  id = "id", # i(i)
  outcome_name = "y", # out(y)
  covnames = c("l", "a"), # com(y:regress, l:logit, a:logit)
  covtypes = c("binary", "binary"),  # see above
  covparams = list(
    covmodels = c(
      # y ~ a_lag + a_lag2 # ... eq(y:a_lag ...)
      l ~ lag1_a + lag1_l,
      a ~ l + lag1_a
    )
  ),
  time_name = "t0", #t(t)
  time_points = time_points,
  intvars = intvars,
  interventions = interventions,
  int_descript = int_description,
  histories = c(lagged),  # remember to add cumavg
  histvars = list(c("a", "l")),
  ymodel = y ~ a + l + lag1_a + lag2_a + lag3_a + lag1_l + lag2_l + lag3_l,
  seed = 1234,
  parallel = TRUE,
  nsamples = 20, # Number of bootstrap samples 
  nsimul = 2000,
  ncores = 6
)

gf_out

