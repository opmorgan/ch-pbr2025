## Dependencies: tidyverse, lib/tables.R/pretty_table(), meta
library(tidyverse)
requireNamespace("meta")

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


#### Summarize and combine meta-analysis results

## Helper function: find number of unique studies from metagen object
find_n_stu <- function(metagen_obj) {
  N_stu <- metagen_obj$studlab %>% unique() %>% length()
  return(N_stu)
}

## Summarize a metagen result in a 1-row tibble
## Input: a metagen object, model type,
## profession, es type
## Return: a one-row tibble with meta-analysis results
metagen_makerow <- function(metagen_obj, model_type = "random",
                            profession = "", es_type = "",
                            label = "") {
  if (model_type == "fixed") {
    out_row <- tibble(
                   model_type = "fixed",
                   N = metagen_obj$k,
                   ## Transform metagen's logodds to odds
                   lower = exp(metagen_obj$lower.fixed),
                   effect = exp(metagen_obj$TE.fixed),
                   upper = exp(metagen_obj$upper.fixed),
                   p = metagen_obj$pval.fixed
                   )
  } else if (model_type == "random") {
    out_row <- tibble(
                   model_type = "random",
                   N = metagen_obj$k,
                   lower = exp(metagen_obj$lower.random),
                   effect = exp(metagen_obj$TE.random),
                   upper = exp(metagen_obj$upper.random),
                   p = metagen_obj$pval.random
                   )
  }

  I2 <- str_c( metagen_obj$I2 %>% numformat(2), " [",
              metagen_obj$lower.I2 %>% numformat(2), ", ",
              metagen_obj$upper.I2 %>% numformat(2), "]")

  N_stu <- find_n_stu(metagen_obj)

  out_row[["I2"]] = I2
  out_row[["N_stu"]] = N_stu
  out_row[["profession"]] = profession
  out_row[["es_type"]] = es_type
  out_row[["label"]] = label
  out_row[["metagen_obj"]] = lst(metagen_obj)

  out_row <- out_row %>%
    select(profession, es_type, N, N_stu,
           lower, effect, upper, p, I2,
           model_type, label, metagen_obj)

  return(out_row)
}

## Summarize a glmer result in a 1-row tibble
## calculate OR, Ci, I^2 on model with all random effects
## (Use metabin with vanilla random effects model to find I^2)
## Study, id, handedness comparison, and population are included as random effects
glmm_makerow <- function(glm_sub_prof, meta_sub_prof,
                            profession = "", es_type = "OR",
                            model_type = "random (all)",
                            label = "glmer") {

  if (model_type == "random (all)") {
    ## Add study, id, handedness comparison, and population as random effects
    ## For S/M, "handedness comparison" only has multiple values for music and art. Check that there are multiple values.
    if (length(unique(glm_sub_prof[["comparison"]])) > 1) {
      glm1 <- lme4::glmer(data = glm_sub_prof,
                          handedness ~ group + (1|study) + (1|est_id) + (1|comparison) + (1|pop), family = "binomial")
    } else {
    ## If not, don't try to include "comparison" in the model.
    glm1 <- lme4::glmer(data = glm_sub_prof,
                        handedness ~ group + (1|study) + (1|est_id) + (1|pop), family = "binomial",
                        control=glmerControl(optimizer="bobyqa"))
    }
  }

  if (model_type == "random") {
    ## Include only ID as a random effect (similar to meta/metafor estimates)
    glm1 <- lme4::glmer(data = glm_sub_prof,
                        handedness ~ group + (1|est_id), family = "binomial")
  }


  # p-value is in the 8th cell of "coefficients"
  p_val <- glm1 %>% summary() %>% .[["coefficients"]] %>% .[8]

  ## Find CI
  ## This shows the odds ratio for controls / creative (backwards from what we want to print)
  em <- glm1 %>% emmeans(~ group, type = "response", level = .95) %>% pairs() %>% confint()


  ## Find I^2 with metabin
  ## Run model
  m1 <- meta::metabin(data = meta_sub_prof,
                studlab = study,
                n.e = n_creative,
                event.e = n_left_creative,
                n.c = n_control,
                event.c = n_left_control,
                method = "GLMM",
                sm = "OR",
                MH.exact = F
  )

  I2 <- str_c(m1$I2 %>% numformat(2), " [",
              m1$lower.I2 %>% numformat(2), ", ",
              m1$upper.I2 %>% numformat(2), "]")

 out_row <- tibble(
                   model_type = model_type,
                   N = m1$k,
                   ## To get the ratio of the proportion of left handers for cerative/controls,
                   ## divide 1 by estimate.
                   ## Be sure to flip upper and lower as well
                   lower = (1 / em$asymp.UCL),
                   effect = (1 / em$odds.ratio),
                   upper = (1 / em$asymp.LCL),
                   p = p_val
    )

  N_stu <- find_n_stu(m1)

  out_row[["I2"]] = I2
  out_row[["N_stu"]] = N_stu
  out_row[["profession"]] = profession
  out_row[["es_type"]] = es_type
  out_row[["label"]] = label
  out_row[["metagen_obj"]] = lst(m1)

  out_row <- out_row %>%
    select(profession, es_type, N, N_stu,
           lower, effect, upper, p, I2,
           model_type, label, metagen_obj)

  return(out_row)
}
