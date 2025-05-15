## Dependencies: MBESS, tidyverse

#### Calculate CI for Pearson's rho
## Given pearson's rho, convert to z' using Fisher's transform
## Source: "How to compute confidence interval for Pearson’s r? A brief guide"
##          (Medium blog post by Shan Dou, May 30, 2018)
##    https://shandou.medium.com/how-to-compute-confidence-interval-for-pearsons-r-a-brief-guide-951445b9cb2d
##    Archived at https://perma.cc/UMM5-VV5U
## See also: https://en.wikipedia.org/wiki/Fisher_transformation
##    Archived at https://perma.cc/HLQ5-TUVX
## (1) Helper function: Convert rho to z'
rho_to_zprime <- function(rho) {
  ## Double check this equation with a second source
  z <- 0.5 * log((1 + rho) / (1 - rho))
  return(z)
}
## (2) Helper function: Given Fisher's z' and n, find CI around z'
## Depends on: tibble
zprime_ci <- function(zprime, n, alpha = 0.05) {
  ## Find z for 95% CI given alpha - positive value leaving 1/2 alpha tail
  zcritical <- qnorm((alpha / 2), lower.tail = F)
  se <- 1 / sqrt(n - 3)
  lower <- (zprime - zcritical * se)
  upper <- (zprime + zcritical * se)
  return(tibble::tibble(lower = lower, zprime = zprime, upper = upper))
}
## Helper function: Convert z' to rho
zprime_to_rho <- function(zprime) {
  rho <- tanh(zprime)
  return(rho)
}
## (3) Main function: Find CI around rho, given n
## Returns a tibble with columns: lower, upper, rho
rho_ci <- function(rho, n, alpha = 0.05) {
  zprime <- rho_to_zprime(rho)
  zprime_ci <- zprime_ci(zprime, n, alpha = 0.05)
  rho_ci <- zprime_ci %>%
    zprime_to_rho %>%
    rename(rho = zprime)
  return(rho_ci)
}


#### Calculate CI for eta^2 or R^2
## Given R2/eta^2 and the effect direction, find rho
r2_to_rho <- function(r2, sign = "pos") {
  rho <- sqrt(r2)
  if (sign == "pos") {
    return(rho)
  } else if (sign == "neg") {return(rho*(-1))
  }
}


## Given partial rho, n, and number of regressors k, find CI for partial rho
## Returns a tibble with columns: lower, upper, rho
## Depends on: MBESS
rho_p_ci <- function(rho_p, N, K, conf.level = 0.95) {
  rho_p_ci <- MBESS::ci.R(R = rho_p, N = N, K = K, conf.level = conf.level)

  ## Put output into tibble with columns: lower, upper, effect (partial rho)
  rho_p_ci <- rho_p_ci %>%
    as_tibble() %>%
    mutate(effect = rho_p) %>%
    rename(lower = Lower.Conf.Limit.R, upper = Upper.Conf.Limit.R) %>%
    select(lower, upper, effect)

  return(rho_p_ci)
}


#### Calculate 95 CI from p-value
## Source: BMJ 2011;343:d2090
##   https://www.bmj.com/content/343/bmj.d2090
##   Archived at https://perma.cc/E3ZG-Y9RG
## Inputs: p-value ("p"), point estimate ("effect").
## Returns a tibble with columns: lower, upper, effect
p_to_ci <- function(p, effect) {
  ## (1) Find z from p
  z <- -0.862 + sqrt(0.743 - 2.404 * log(p))
  ## (2) Find SE from estimate, z
  se <- abs(effect/z)
  ## (3) Find CI from est, SE
  lower <- effect - (1.96 * se)
  upper <- effect + (1.96 * se)
  ci_effect <- tibble::tibble(lower = lower, effect = effect, upper = upper)
  return(ci_effect)
}


#### To estimate CIs around Cohen's D, I will use the Psych package:
## psych::cohen.d.ci()

