## Calculate CI around proportions
## I will Use the Wilson ("score") method,
## recommended by Agresti and Coull (1998) and Brown et al. (2001) for small samples.
## https://www.itl.nist.gov/div898/handbook/prc/section2/prc241.htm
## https://www.rdocumentation.org/packages/PropCIs/versions/0.3-0/topics/scoreci
## This blog post compares the Wilson and Agresti-Coull methods:
## https://towardsdatascience.com/five-confidence-intervals-for-proportions-that-you-should-know-about-7ff5484c024f
## Input: x = number of hits; n = total number of observations.
library(PropCIs)
find_prop_score <- function(x, n, conf_level = 0.95) {
  prop_fit <- scoreci(x, n, conf.level = conf_level)
  prop_tbl <- tibble(lower = prop_fit$conf.int[[1]],
                     effect = x / n,
                     upper = prop_fit$conf.int[[2]]
                     )
  return(prop_tbl)
}

