library(tidyverse)

pop_data |> 
  ggplot(aes(wave, fill = factor(response))) +
  stat_count()

pop_data |> 
  select(-pidp, -response, -t0) |> 
  summarise(across(everything(), ~sum(is.na(.x))/n()), .by = "wave") |> 
  pivot_longer(-wave, names_to = "var", values_to = "p_miss") |> 
  ggplot(aes(x = factor(wave), y = p_miss)) +
  geom_col() +
  facet_wrap(~var)

pop_data |> 
  filter(wave == 1, response == 1) |> 
  select(pidp) |> 
  left_join(pop_data, by = "pidp") |> 
  filter(wave == 10, response == 1) |> 
  select(pidp) |> 
  left_join(pop_data, by = "pidp") |> 
  select(-pidp, -response, -t0) |> 
  summarise(across(everything(), ~sum(is.na(.x))/n()), .by = "wave") |> 
  pivot_longer(-wave, names_to = "var", values_to = "p_miss") |> 
  ggplot(aes(x = factor(wave), y = p_miss)) +
  geom_col() +
  facet_wrap(~var)

pres1 <- pop_data |> 
  filter(response == 1) |> 
  select(pidp, wave1 = wave)

pres1 |> 
  filter(wave1 == 2) |> 
  inner_join(pres1 |> filter(wave1 == 6), by = join_by(pidp))

pres1 |> 
  rename(wave2 = wave1) |> 
  left_join(pres1, by = "pidp", relationship = "many-to-many") |> 
  count(wave1, wave2) |> 
  filter(wave1 < wave2) |> 
  mutate(n_waves = wave2 - wave1 + 1) |> 
  arrange(desc(n_waves), desc(n)) |> View()

test_wave <- function(wave_n) function(wave) sum(wave <= wave_n)

waves_allowing_5_obs <- pres1 |> 
  summarise(firstwave = min(wave1), lastwave = max(wave1), .by = pidp) |> 
  mutate(n_waves = lastwave - firstwave + 1) |> 
  filter(n_waves >= 5) |> 
  mutate(
    valid_wave = map2(firstwave, lastwave, \(first, last) {
      
      map(1:10, \(wave_n) {
        first <= wave_n & (last - wave_n) > 3
      }) |> 
        reduce(c) |> 
        tibble(wave = 1:10, valid = _)
      
    })
  ) |> 
  unnest(valid_wave) |> 
  filter(valid)

waves_allowing_5_obs |> count(wave)

late_pop |> 
  group_by(pidp) |> 
  summarise(n_missing = sum(is.na(sf12mcs_dv)), base_missing = all(is.na(sf12mcs_dv_base))) |> 
  filter(n_missing == 4, base_missing)

wide_data |> 
  summarise(across(everything(), ~sum(is.na(.x)/n()))) |> 
  pivot_longer(everything()) |> 
  arrange(value)


# Do micey parallels?

library(furrr)

summary(late_pop)

imputed_late <- mice::mice(late_pop[,-c(1:3)], defaultMethod = c("norm", "logreg", "pmm", "pmm"), m = 5, maxit = 5)
full_impute <- mice::complete(imputed_late, action = "long", include = TRUE)

long_dat <- cbind(late_pop[,1:3], full_impute)

plan(multisession, workers = 6)

gform_long <- long_dat |>
  nest(data = -.imp) |>
  mutate(gform_out = map(data, function(data) {
    
    data$econ_emp_bin <- as.integer(as.character(data$econ_emp_bin))
    
    gform_cont_eof <- gformula(
      obs_data = data,
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
      
      # covfits_custom = c(NA, NA, NA, NA, incr_fit),
      # covpredict_custom = covpredict_custom,
      
      histories = histories,
      histvars = histvars,
      basecovs = basecovs,
      seed = 1234,
      # parallel = TRUE,
      # nsamples = nsamples,
      # Number of bootstrap samples
      # ncores = ncores,
      ref_int = 1
    )
    
    gform_cont_eof$result
    
  }))

plan(sequential)

gform_long[,-'data'] |> 
  unnest(gform_out) |> 
  janitor::clean_names() |> 
  filter(imp != 0) |> 
  summarise(g_form_mean = mean(g_form_mean), .by = "interv")
