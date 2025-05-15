## Initialize summary table
## A tibble with columns:
## task, es_type, measure
## N (estimates), N_s (number of studies), n (observations)
## lower, effect, upper, p
## model (fixed/random), partials (separate/combined), notes
init_pool_table <- function() {
  pool_table <- tibble(task = factor(levels = c("RAT", "AUT", "TTCT-F", "TTCT-V")),
                       es_type = as.character(),
                       measure = as.character(),
                       N = as.numeric(),
                       N_stu = as.numeric(),
                       # n_obs = as.numeric(), # Would have to match these after metagen
                       n = as.numeric(), ## Number of participants
                       n1 = as.numeric(), ## n righties
                       n2 = as.numeric(), ## n_lefties
                       lower = as.numeric(),
                       effect = as.numeric(),
                       upper = as.numeric(),
                       p = as.numeric(),
                       I2 = as.character(),
                       partials = factor(levels = c("separate", "collapsed")),
                       model_type = factor(levels = c("fixed", "random")),
                       label = as.character(),
                       metagen_obj = lst(),
                       )
  return(pool_table)
}

