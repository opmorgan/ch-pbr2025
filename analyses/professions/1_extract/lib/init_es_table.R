## Dependencies: tidyverse
library(tidyverse)

## This table should contain every column from processed input data, and columns for summaries.
init_es_table <- function() {
  es <- tibble(
               study = character(),
               profession = character(),
               pop = character(),
               creative_group = character(),
               control_group = character(),
               h = character(), # handedness measure
               rl_sm = character(),
               n_creative = integer(), # n creative professionals
               n_left_creative = integer(),
               n_right_creative = integer(),
               n_control = integer(), # n controls
               n_left_control = integer(),
               n_right_control = integer(),
               PL_creative = numeric(), # percentage lefties (creative)
               PL_control = numeric(), # percentage lefties (control)
               es_type = factor(),
               lower = numeric(),
               effect = numeric(),
               upper = numeric(),
               chi_sq = numeric(),
               p = numeric(),
               correction = logical(), # Correction in epitest::oddsratio?
               method = character(), # Method from epitest::oddsratio
               notes = character(),
               comparison_details = character(), #details on pop
               pref_success = character(),
               include = logical(),
               # exclude because unnecessary Right/Nonright comparison?
               exclude_rnr = logical(),
               model_obj = list()
               )
}
