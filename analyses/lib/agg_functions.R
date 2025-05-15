#### Construct aggregate sd, following Cochrane Handbook's formula
## "...the rather complex-looking formula for the SD produces the SD of
## outcome measurements as if the combined group had never been divided
## into two. This SD is different from the usual pooled SD that is used
## to compute a confidence interval for a MD or as the denominator in
## computing the SMD. This usual pooled SD provides a within-subgroup SD
## rather than an SD for the combined group, so provides an underestimate
## of the desired SD."
## Source:  Cochrane Handbook for Systematic Reviews of Interventions
##          Version 6.2, 2021
##          Section 6.5.2.10: Combining Groups
##  Archived at https://perma.cc/6TKA-PHXT
agg_mean_cochrane <- function(n1, n2, m1, m2) {
  m <- ( (n1 * m1) + (n2 * m2) ) / (n1 + n2)
  return(m)
}
agg_sd_cochrane <- function(sd1, sd2, n1, n2, m1, m2) {
  sd <- sqrt(
             (
              (n1-1)*sd1^2
              + (n2-1)*sd2^2
              + ( (n1*n2)*(m1^2+m2^2-2*m1*m2) / (n1+n2) )
             )
             / (n1+n2-1)
  )
  return(sd)
}

## Aggregate many means and sds, given means, sds, and ns
## Input: a tibble with three columns of equal length: n, m, and sd
## Output: a tibble with three 1-long columns: n_agg, m_agg, and sd_agg
agg_means_sds <- function(input_tbl) {

  proc <- input_tbl %>%
    mutate(
           n_cum = as.numeric(NA),
           m_cum = as.numeric(NA),
           sd_cum = as.numeric(NA)
           )

  for (j in 1:dim(proc)[1]) {
    ## TODO: check that all three input vectors are the same length
    if (j == 1) {
      proc[j, "n_cum"] <- proc[[j, "n"]]
      proc[j, "m_cum"] <- proc[[j, "m"]]
      proc[j, "sd_cum"] <- proc[[j, "sd"]]
    } else if (j > 1) {
      ## cumulative n equals the sum of n in rows 1:j
      proc[j, "n_cum"] <- sum(proc[1:j, "n"])

      ## m is calcualted based on previous cumulative n, m, current n, m
      n1 <- proc[[j - 1, "n_cum"]]
      n2 <- proc[[j, "n"]]
      m1 <- proc[[j - 1, "m_cum"]]
      m2 <- proc[[j, "m"]]

      proc[j, "m_cum"] <- agg_mean_cochrane(n1, n2, m1, m2)

      ## sd is calculated based on previous cumulative sd, m, n vs. current
      sd1 <- proc[[j - 1, "sd_cum"]]
      sd2 <- proc[[j, "sd"]]
      proc[j, "sd_cum"] <- agg_sd_cochrane(sd1, sd2, n1, n2, m1, m2)
    }
  }

out <- proc %>%
  tail(1) %>%
  select(n_cum, m_cum, sd_cum) %>%
  rename(n_agg = n_cum, m_agg = m_cum, sd_agg = sd_cum)
}
