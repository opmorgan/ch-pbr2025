## Convert 95% interval to units of standard error
## Input: n, lower, upper, alpha (for 95% CI, alpha = 0.05)
## https://handbook-5-1.cochrane.org/chapter_7/7_7_3_2_obtaining_standard_deviations_from_standard_errors_and.htm
## [Archive: https://perma.cc/6JW8-YMLF]
## Output: SEM
ci_to_se <- function(n, lower, upper, alpha = 0.05) {
  len_ci_se <- qnorm(1-(alpha/2)) - qnorm(alpha/2)
  len_ci <- upper - lower
  se <- len_ci/len_ci_se
  return(se)
}

## Convert SE to "inverse variance" (1/SEM^2)
## Source: https://training.cochrane.org/handbook/current/chapter-10#_Ref531256048
## [Archive: https://perma.cc/K5CC-A7CN]
## Another Source: https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pooling-es.html
## [Archive: https://perma.cc/96PS-FN7Z]
se_to_inv_var <- function(se) {
  inv_var <- 1/se^2
  return(inv_var)
}
