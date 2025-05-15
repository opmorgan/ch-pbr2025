library(tidyverse)

## Given a row with a pooled Odds ratio estimate and 95% CI
## (in columns "effect", "lower", and "upper"),
## Find standard error.
find_pooled_se <- function(poolest_row) {
  effect <- poolest_row %>% .[["effect"]]
  lower <- poolest_row %>% .[["lower"]]
  upper <- poolest_row %>% .[["upper"]]
  odds_est <- tibble(lower = lower, effect = effect, upper = upper)

  ## Need to find the pooled SE, too
  ## First, convert back to log odds
  log_est  <- odds_est %>% mutate(across(, function(x) log(x)))

  ## Then, find SE in log odds
  log_se <- ci_to_se(lower = log_est$lower, upper = log_est$upper, alpha = 0.05)
  log_se_bounds <- tibble(
                          lower = (log_est$effect - (.5 * log_se)),
                          effect = (log_est$effect),
                          upper = (log_est$effect + (.5 * log_se))
                          )

  ## Then, convert back to Odds ratio
  odds_se_bounds <- log_se_bounds %>% mutate(across(, function(x) exp(x)))
  odds_se <- odds_se_bounds$upper - odds_se_bounds$lower
  return(odds_se)
}

find_log_se  <- function(poolest_row) {
  effect <- poolest_row %>% .[["effect"]]
  lower <- poolest_row %>% .[["lower"]]
  upper <- poolest_row %>% .[["upper"]]
  odds_est <- tibble(lower = lower, effect = effect, upper = upper)

  ## Need to find the pooled SE, too
  ## First, convert back to log odds
  log_est  <- odds_est %>% mutate(across(, function(x) log(x)))

  ## Then, find SE in log odds
  log_se <- ci_to_se(lower = log_est$lower, upper = log_est$upper, alpha = 0.05)
  return(log_se)
}
