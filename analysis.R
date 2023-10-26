library(haven)
library(dplyr)
library(plm)
library(gfoRmula)

lagged_variables <- c("econ_emp",
                      "intdaty_dv",
                      "home_owner",
                      "mastat_dv",
                      "dnc",
                      "gor_dv",
                      "ghqcase4",
                      "log_income",
                      "econ_dist",
                      "sf12pcs_dv"
)

diff_variables <- c("log_income")

data <- read_stata("./Test_Harry14.dta")
data <- data |> select(-c("age_sq")) # we generate age_sq when needed

data <-
  data %>% mutate(across(
    c("pidp", "ppid", "hidp", "wave", "pns1pid", "pns2pid"),
    as.integer
  ),
  across(
    c(
      "hours",
      "wage_hour",
      "lwage_hour",
      "swage_hour",
      "dimlwt",
      "wgt",
      "econ_realequivwinct",
      "econ_realequivwinc",
      "log_income",
      "inc_increase",
      "inc_decrease",
      "weight_house",
      "samp_medianinc",
      "samp_poverty",
      "exp_povgap",
      "inc_winsann",
      "econ_realequivinc",
      "econ_realequivinct",
      "yplgrs_dv",
      "econ_incchange",
      "econ_percent",
      "inc_wins",
      "econ_realnetinc",
      "econ_cpi",
      "indinus_xw",
      "indinui_xw",
      "hhdenub_xw",
      "hhdenus_xw",
      "indinub_lw",
      "indinus_lw",
      "indinub_xw",
      "indinui_lw",
      "indscus_lw"
    ),
    as.numeric
  ),)

data <- data %>% mutate(across(matches("scsf"), as.integer),
                        across(
                          c(
                            "jbstat",
                            "age_dv",
                            "racel_dv",
                            "gor_dv",
                            "mastat_dv",
                            "depchl_dv",
                            "bame",
                            "home_owner",
                            "finnow",
                            "finfut",
                            "sex_dv",
                            "intdaty_dv",
                            "sf1",
                            "sf2a",
                            "sf2b",
                            "sf3a",
                            "sf3b",
                            "sf4a",
                            "sf4b",
                            "sf5",
                            "sf6a",
                            "sf6b",
                            "sf6c",
                            "sf7",
                            "sclfsato",
                            "econ_savings",
                            "dlltsd",
                            "econ_benefits",
                            "econ_incdec",
                            "econ_incquint",
                            "ydses_c10",
                            "ydses_c5",
                            "house_type",
                            "dnc2",
                            "dnc",
                            "econ_poverty",
                            "exp_poverty",
                            "exp_incchange",
                            "exp_emp",
                            "econ_emp",
                            "econ_educ",
                            "econ_retire",
                            "econ_ltsick",
                            "econ_empqal",
                            "econ_state",
                            "econ_dist",
                            "ghqcase3",
                            "ghqcase4",
                            "ghq_severe",
                            "nonepar_dv",
                            "tenure_dv",
                            "hhtype_dv",
                            "hiqual_dv",
                            "scghq1_dv",
                            "scghq2_dv"
                          ),
                          as.integer
                        ),
)

raw_columns <- c(
  "jbhrs",
  "jshrs",
  "j2hrs",
  "fimnlabgrs_dv",
  "fiyrinvinc_dv",
  "sf12pcs_dv",
  "sf12mcs_dv",
  "fihhmnnet1_dv",
  "fihhmnsben_dv",
  "ieqmoecd_dv",
  "_merge",
  "depChild",
  "depChild2"
)

panel_data <- pdata.frame(data, c('pidp', 'wave'))

if (length(lagged_variables) == 1) {
  panel_data[paste("lag1", lagged_variables[1], sep = "_")] <- diff(panel_data[, lagged_variables])
} else {
  for (column_name in lagged_variables) {
    panel_data[paste("lag1", column_name, sep = "_")] <- diff(panel_data[, c(column_name)])
  }
  #lags <- data.frame(lapply(as.list(panel_data[, lagged_variables], keep.attributes = TRUE), plm::lag))
  #colnames(lags) <- paste(colnames(lags), "lag1", sep = "_")
  #panel_data <- cbind(panel_data, lags)
}


if (length(diff_variables) == 1) {
  panel_data[paste("diff1", diff_variables[1], sep = "_")] <- diff(panel_data[, diff_variables])
} else {
  for (column_name in diff_variables) {
    panel_data[paste("diff1", column_name, sep = "_")] <- diff(panel_data[, c(column_name)])
  }
  #diffs <- data.frame(lapply(as.list(panel_data[, diff_variables], keep.attributes = TRUE), diff))
  #colnames(diffs) <- paste(colnames(diffs), "diff1", sep = "_")
  #panel_data <- cbind(panel_data, diffs)
}
# TODO make sure there is no OPCODEs in these variables

data <- as.data.frame(panel_data)

# 0 "Emp_both"
# 1 "Emp_stu"
# 2 "Emp_notemp" 
# 3 "Emp_ret" 
# 4 "Stu_emp" 
# 5 "Stu_both" 
# 6 "Stu_notemp" 
# 7 "Stu_ret" 
# 8 "Notemp_emp" 
# 9 "Notemp_stu" 
# 10 "Notemp_both" 
# 11 "Notemp_ret" 
# 12 "Ret_emp" 
# 13 "Ret_stu" 
# 14 "Ret_nonemp" 
# 15 "Ret_both"

data <- data |> mutate(
  lag1_econ_econ = (econ_emp - 1) * 4 + lag1_econ_emp - 1,
wnw_race = ifelse(racel_dv %in% 1:4, 0, ifelse(racel_dv %in% 5:97, 1, NA)),
econ_emp_bin = case_when(econ_emp == 1 ~ 0, econ_emp == 3 ~ 1)
)

panel_data <- pdata.frame(data, c('pidp', 'wave'))
panel_data["lag1_econ_emp_bin"] <- diff(panel_data[, c("econ_emp_bin")])
data <- as.data.frame(panel_data)


data <- data[data$age_dv <= 65 & data$age_dv >= 25, ]
data <- data |> mutate(ukcount = case_when(gor_dv <= 10 & gor_dv >= 1~ 1, gor_dv == 11 ~ 2, gor_dv == 12 ~ 3, gor_dv == 13 ~ 4))

panel_data <- pdata.frame(data, c('pidp', 'wave'))
panel_data["lag1_ukcount"] <- diff(panel_data[, c("ukcount")])
data <- as.data.frame(panel_data)
