## Dependencies: tidyverse, common/pretty_table.R, (metagen)

#### Calculate pooled ES for specified subgroups, "manually"

## How to calculate summaries using fixed effects model,
## following:
## Harrer, M., Cuijpers, P., Furukawa, T.A., & Ebert, D.D. (2021).
## Doing Meta-Analysis with R: A Hands-On Guide. Boca Raton,
## FL and London: Chapman & Hall/CRC Press. ISBN 978-0-367-61007-4.
## https://bookdown.org/MathiasHarrer/Doing_Meta_Analysis_in_R/pooling-es.html
## [Archive: https://perma.cc/96PS-FN7Z)]
##
## 1. Find weights: inverse variance
##     i) Find standard error of ES estimate (use 95% CI)
##     i) Find variance (standard error squared)
##     i) Find inverse variance (1/variance)
## 1. Estimate pooled effect size as the weighted average of each study's effect size
##     i) Multiple each study's effect size with its weight.
##     i) Sum these products across each effect
##     i) Divide by the sum of all individual weights

## Given a tibble with individual effect sizes,
## return a tibble with pooled effect sizes.
## es_individual is a tibble with a row for each effect size with each
##    group specified in the vector "groups",
##    effect size ("effect') and 95% CI ("lower" and "upper")
## group is a vector specifying the groups pool across (e.g., task, measure)
## "fixed_or_random" specifies whether to use a fixed or random effects model.
## It can have the values "fixed" (default) or "random"
## Returns a tibble with a row for each pooled effect size,
##     and columns for number of studies pooled (N),
##                     n in each study (n)
##                     each grouping var,
##                      lower, effect, and upper.

## Confidnece intervals are probably calculated INCORRECTLY here.
## They are found by pooling individual confidence estimates with the
## same formula as effect sizes.
## TODO: Refactor to make a function that just pools effect size, given a table
## of ES estimates. Leave grouping out of the function.
pool_es_fixed  <- function(es_individual,
                           groups = c("task", "measure", "es_type"),
                           fixed_or_random = "fixed") {
  if (fixed_or_random == "fixed") {

    ## (1) Multiply each study's effect size with its weight.
    es_pool_1 <- es_individual %>%
      mutate(w_lower_1 = lower * inv_var,
             w_effect_1 = effect * inv_var,
             w_upper_1 = upper * inv_var
             )
    ## (debug) Inspect result
    # es_pool_1 %>%
    #   select(study, task, measure, es_type, lower, effect, upper,
    #          inv_var, w_lower_1, w_effect_1, w_upper_1)

    ## (2) Sum these products across each effect
    es_pool_2 <- es_pool_1 %>%
      group_by(across(groups)) %>%
      summarize(N = n(), n = sum(n),
                w_lower_2 = sum(w_lower_1),
                w_effect_2 = sum(w_effect_1),
                w_upper_2 = sum(w_upper_1),
                ## (3-pre) find the sum of individual weights
                sum_inv_var = sum(inv_var),
                .groups = "drop"
                )
    ## (debug) inspect result)
    # es_pool_1 %>% filter(task == "AUT", measure == "Fluency", es_type == "d")
    # es_pool_2
    # es_pool_2$sum_inv_var

    ## (3) Divide by the sum of all individual weights
    es_pool_3 <- es_pool_2 %>%
      mutate(w_lower_3 = w_lower_2 / sum_inv_var,
             w_effect_3 = w_effect_2 / sum_inv_var,
             w_upper_3 = w_upper_2 / sum_inv_var
             )

    es_pooled <- es_pool_3 %>%
      rename(lower = w_lower_3,
             effect = w_effect_3,
             upper = w_upper_3,
             inv_var = sum_inv_var
             ) %>%
    select(task, measure, es_type, N, n, inv_var, lower, effect, upper)

    return(es_pooled)

  } else if (fixed_or_random == "random)") {
    #TODO: write function for random effects pooling
  }
}

#### Combine tibble of pooled effect sizes with
## table of individual effect sizes
combined_es <- function(es_pooled, es_individual, groups) {
  es_combo <- es_pooled %>%
  mutate(study = "_pooled") %>%
  select(study, everything()) %>%
  bind_rows(es_individual) %>%
  arrange(desc(study)) %>%
  arrange(across(groups)) %>%
  mutate(study = study  %>%
         recode("_pooled" = "Pooled")
        )
  return(es_combo)
}



#### Summarize and combine metagen results

## Helper function: find number of unique studies from metagen object
find_n_stu <- function(metagen_obj) {
  N_stu <- metagen_obj$studlab %>% unique() %>% length()
  return(N_stu)
}

## Summarize a metagen result in a 1-row tibble
## Input: a metagen object, model type, "partials"
## task, measure, es type
## Return: a one-row tibble with meta-analysis results
metagen_makerow <- function(metagen_obj, model_type = "fixed", partials = "",
                            task = "", measure = "", es_type = "", label = "",
                            n_var = NA, n1_var = NA, n2_var = NA) {
  if (model_type == "fixed") {
    out_row <- tibble(
                   model_type = "fixed",
                   N = metagen_obj$k,
                   lower = metagen_obj$lower.fixed,
                   effect = metagen_obj$TE.fixed,
                   upper = metagen_obj$upper.fixed,
                   p = metagen_obj$pval.fixed
                   )
  } else if (model_type == "random") {
    out_row <- tibble(
                   model_type = "random",
                   N = metagen_obj$k,
                   lower = metagen_obj$lower.random,
                   effect = metagen_obj$TE.random,
                   upper = metagen_obj$upper.random,
                   p = metagen_obj$pval.random
                   )
  }

  I2 <- str_c( metagen_obj$I2 %>% numformat(2), " [",
              metagen_obj$lower.I2 %>% numformat(2), ", ",
              metagen_obj$upper.I2 %>% numformat(2), "]")

  N_stu <- find_n_stu(metagen_obj)

  out_row[["I2"]] = I2
  out_row[["N_stu"]] = N_stu
  out_row[["partials"]] = partials
  out_row[["task"]] = task
  out_row[["measure"]] = measure
  out_row[["es_type"]] = es_type
  out_row[["label"]] = label
  out_row[["metagen_obj"]] = lst(metagen_obj)
  
  ## Add n participants
  out_row[["n"]] = n_var
  out_row[["n1"]] = n1_var
  out_row[["n2"]] = n2_var

  out_row <- out_row %>%
    select(task, measure, es_type, N, N_stu, n, n1, n2,
           lower, effect, upper, p, I2, partials,
           model_type, label, metagen_obj)

  return(out_row)
}
