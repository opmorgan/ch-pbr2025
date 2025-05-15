## Dependencies: epitools, tidyverse, psych
requireNamespace("epitools")
library(tidyverse)
requireNamespace("psych") # To find CI around cohen's D

#### Calculate odds ratio and 95% CI with epitools::oddratio()
## Output a tibble with: es_type, lower, effect, upper, model_obj, correction,
## method, and chi^2, p.
##
## Coding scheme for epitools
## "exposed=0": control group
## "exposed=1": creative group
## "disease=0": right/strong handers
## "disease=1": left/mixed handers
##
#### Format for input table:
##
##            Handedness
## Condition  right left (or mixed/strong)
## Control    A     B
## Creative   C     D
##
## Example usage:
## prop_matrix <- matrix(c(A, B, C, D), nrow = 2, ncol = 2, byrow = T)
## epitools::oddsratio(prop_matrix, method = "wald")
## Here, a value greater than one will mean a higher rate of left-handers in the
## creative group.
find_OR <- function(prop_matrix) {
  ## The method "wald" uses cross-multiplication
  or_fit <- epitools::oddsratio(prop_matrix, method = "wald")
  est_tbl <- or_fit$measure %>% as.data.frame() %>% as_tibble() %>% slice(2)
  ## Calculate chi^2 with Yates' continuity correction
  chisq <- chisq.test(prop_matrix)
  or_tbl <- tibble(es_type = "OR", lower = est_tbl$lower,
                   effect = est_tbl$estimate, upper = est_tbl$upper,
                   chi_sq = chisq$statistic %>% .[["X-squared"]],
                   p = chisq$p.value,
                   correction = or_fit$correction,
                   method = attr(or_fit, which = "method")
                   )
  return(or_tbl)
}

#### Calculate Odds Ratio, score-based-CIs with PropCIs::orscoreci
## Agresti, A. (2013). An introduction to categorical data analysis (3rd ed). Wiley-Interscience.
## "Score" CIs are described on pp. 78-79.
## Input: a 1-row tibble with columns "n_creative," "n_left_creative", "n_control", "n_left_control"
find_OR_score <- function(proc_row) {
  ## PropCIS::orscoreci calculates the OR as: [p1(1-p1)/(p2(1-p2))]
  ## So, for a lefty advantage to be represented as greater than 1,
  ## "x1" should be the number of creative lefties, "n1" total creative.
  ## "x2" should be the number of control lefties, "n2" total controls.
  x1 <- proc_row$n_left_creative
  n1 <- proc_row$n_creative
  x2 <- proc_row$n_left_control
  n2 <- proc_row$n_control
  or_est <- ((x1 / (n1 - x1)) / (x2 / (n2 - x2)))
  or_fit <- PropCIs::orscoreci(x1, n1, x2, n2, conf.level = .95)
  ## Do a chi-squared test, too
  x <- matrix(c(x1, (n1 - x1), x2, (n2 - x2)), nrow = 2, ncol = 2)
  chisq <- chisq.test(x)
  or_tbl <- tibble(es_type = "OR", lower = or_fit$conf.int[[1]],
                   effect = or_est, upper = or_fit$conf.int[[2]],
                   chi_sq = chisq$statistic %>% .[["X-squared"]] %>% round(2),
                   p = chisq$p.value,
                   method = "Score"
                   )
  return(or_tbl)
}

#### Calculate relative risk ratio and 95% CI
## Output a tibble with: es_type, lower, effect, upper, model_obj, correction,
## method (no chi^2, p)
##
## Coding scheme for epitools
## "exposed=0": control group
## "exposed=1": creative group
## "disease=0": right/strong handers
## "disease=1": left/mixed handers
##
#### Format for input table:
##
##            Handedness
## Condition  right left (or mixed/strong)
## Control    A     B
## Creative   C     D
##
## Example usage:
## prop_matrix <- matrix(c(A, B, C, D), nrow = 2, ncol = 2, byrow = T)
## epitools::oddsratio(prop_matrix, method = "wald")
## Here, a value greater than one will mean a higher rate of left-handers in the
## creative group.
find_RR <- function(prop_matrix) {
  ## The method "wald" matches my hand calculations
  ## method "boot" gives the same point estimate, with wider CI.
  ## For our analysis, more conservation  to use method
  ## that gives narrower CI.
  rr_fit <- epitools::riskratio(prop_matrix, method = "wald")
  est_tbl <- rr_fit$measure %>% as.data.frame() %>% as_tibble() %>% slice(2)
  chisq.test(prop_matrix)
  rr_tbl <- tibble(es_type = "RR", lower = est_tbl$lower,
                   effect = est_tbl$estimate, upper = est_tbl$upper,
                   correction = rr_fit$correction,
                   method = attr(rr_fit, which = "method")
                   )
  return(rr_tbl)
}

## Calcualte 95% CI around a frequency
## (Use for left-hander incidence estimates with no control group)
## Input: total n, n lefties, confidence level.
find_freq <- function(n_total, n_left, conf.level = .95) {
  freq_fit <- epitools::binom.exact(x = n_left, n = n_total)
  est_tbl <- freq_fit %>% as_tibble() %>%
    mutate(lower = lower*100, proportion = proportion*100, upper = upper*100)
  freq_tbl <- tibble(es_type = "freq", lower = est_tbl$lower,
                   effect = est_tbl$proportion, upper = est_tbl$upper,
                   )

  return(freq_tbl)
}


## Given means and SDs, find Cohen's D with CI
## "m1" should be the mean handedness for the creative group.
find_d <- function(m1, m2, sd1, sd2, n1, n2) {
  d <- msd_to_d(m1, sd1, m2, sd2)
  esci <- psych::cohen.d.ci(d = d, n1 = n1, n2 = n2, alpha = .05) %>%
  as_tibble()
  d_tbl <- tibble(es_type = "d", lower = esci$lower,
                  effect = esci$effect, upper = esci$upper)
  return(d_tbl)
}

## Given means and SDs, find Cohen's D
msd_to_d <- function(m1, sd1, m2, sd2) {
  d <- (m1 - m2) / (sqrt((sd1^2 + sd2^2) / 2))
  return(d)
}

