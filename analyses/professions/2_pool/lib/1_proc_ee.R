## Depends on: tidyverse
library(tidyverse)

## Source required project libraries
source(here("analyses", "lib", "conversions.R"))

## Define function to process extracted effect size data
proc_ee <- function(ee) {
  ee_proc <- ee %>%
    ## remove NAs (studies that report "null" with no stats)
    filter(!is.na(effect)) %>%
    ## Remove excluded estimates
    ## (e.g., redundant estimates from the same study)
    filter(include == 1 & exclude_rnr == 0) %>%
    ## Only use odds ratio estimates
    filter(es_type == "OR") %>%
    ## Calculate SE, "inverse variance" for each study
    ## (For pooling based on precomplied ORs with metagen)
    ## Depends on: {pool_dir}/lib/conversions.R
    mutate(se = ci_to_se(n = n, lower = lower, upper = upper, alpha = 0.05),
           inv_var = se_to_inv_var(se)
    )

  ## Recode Art/Architecture to have an entry in each separate category

  ## Depends on: {pool_dir}/lib/recode_slash_vars.R
  prof_vars_slash <- c("Art/Architecture", "Art/Music", "Art/Science")
  prof_vars_new <- list(c("Art", "Architecture"), c("Art", "Music"),
                        c("Art", "Science"))
  for (j in seq_along(prof_vars_slash)) {
    ee_proc <- recode_slash_vars(ee_proc, prof_var_slash = prof_vars_slash[[j]],
                                 prof_vars_new = prof_vars_new[[j]])
  }

  ee_proc <- ee_proc %>% filter(profession != "Science")

  ## Add an id column to ee_proc (an id for each row)
  ## This should be included as a random effect when pooling with glmer
  ee_proc <- ee_proc %>% tibble::rowid_to_column("est_id")

  ## Save the data
  readr::write_csv(ee_proc, here(pool_dir, "data", "proc", "ee_proc.csv"))
}


#### Helper function to recode Art/Architecture to have an entry in each separate category

## Define function to create new rows from variables with slashes in them
## prof_var_slash <- "Art/Architecture"
## prof_vars_new <- c("Art", "Architecture")
## prof_var_slash and prof_vars_new must be the same lengfh
## prof_vars new contains vectors, each of which specify the new variable names
## for the row that will be duplicated
recode_slash_vars <- function(ee_proc, prof_var_slash, prof_vars_new) {
  slash_rows <- ee_proc %>% filter(profession == prof_var_slash)
  # Initialize tibble to store new rows
  new_rows <- ee_proc %>% slice(0)
  for (j in 1:dim(slash_rows)[1]) {
    new_row_1 <- slash_rows %>% slice(j) %>% mutate(profession = prof_vars_new[[1]])
    new_row_2 <- slash_rows %>% slice(j) %>% mutate(profession = prof_vars_new[[2]])
    new_rows <- new_rows %>% add_row(new_row_1) %>% add_row(new_row_2)
  }
  ee_proc <- ee_proc %>%
    subset(profession != prof_var_slash) %>%
    add_row(new_rows)
  return(ee_proc)
}
