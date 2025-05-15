requireNamespace("psych")
library(tidyverse)

replicate_goodman <- function(handedness_dta) {
  ## Calculate effect sizes
  ## Initialize results table
  es <- tibble(
               sample = character(),
               rl_sm = character(),
               n1 = integer(),
               n2 = integer(),
               m1 = integer(),
               m2 = integer(),
               es_type = factor(),
               lower = numeric(),
               effect = numeric(),
               upper = numeric(),
               p = numeric(),
               measure = character(),
               notes = character(),
  )

  es_type = "Cohen's D"

  ## Make map with sample names and their codes
  sample_map <- tibble(names = list("US",    "NLSY79", "NLSY97"),
                       codes = list(c(4, 5), c(4),      c(5)))

  #### Loop through each sample
  for (sample_name in sample_map$names) {
    ## Find sample code linked to name
    sample_code <- sample_map %>%
      filter(names == sample_name) %>%
      .[["codes"]] %>% unlist()

    ## Filter by sample
    hand_sub <- hand %>% filter(sample %in% sample_code)

    #### Loop through each handedness comparison
    rl_sm_vars <- c("SR/SL", "R/L")
    for (rl_sm in rl_sm_vars) {
      ## Subset handedness groups
      if (rl_sm == "SR/SL") {
        #### Use goodman's "leftness" coding (leaving out mixed handers)
        ## count 1 as "left"; 0 as "right"; omit anything in-between.
        hand_sub <- hand_sub %>% mutate(handedness = case_when(
                                                   leftness == 1 ~ "left",
                                                   leftness == 0 ~ "right"
                                                   )
        )
        hand_sub_right <- hand_sub %>% filter(handedness == "right")
        hand_sub_left <- hand_sub %>% filter(handedness == "left")
      } else if (rl_sm == "R/L") {
        #### Use goodman's "leftness" coding (leaving in mixed handers)
        ## This is the analysis goodman reported.
        ## count 1 as "left"; less than 1 as "right"
        hand_sub <- hand_sub %>% mutate(handedness = case_when(
                                                   leftness > 0 ~ "left",
                                                   leftness == 0 ~ "right"
                                                   )
        )
        hand_sub_right <- hand_sub %>% filter(handedness == "right")
        hand_sub_left <- hand_sub %>% filter(handedness == "left")
      }

      ## Find mean, sd, and n for each group
      ## 1 codes right, 2 codes left
      m1 <- hand_sub_right %>% .[["occ_creative"]] %>% mean(na.rm = T)
      m2 <- hand_sub_left %>% .[["occ_creative"]] %>% mean(na.rm = T)
      sd1 <- hand_sub_right %>% .[["occ_creative"]] %>% sd(na.rm = T)
      sd2 <- hand_sub_left %>% .[["occ_creative"]] %>% sd(na.rm = T)
      n1 <- hand_sub_right %>% drop_na(occ_creative) %>% dim() %>% .[[1]]
      n2 <- hand_sub_left %>% drop_na(occ_creative) %>% dim() %>% .[[1]]

      d <- msd_to_d(m1 = m1, m2 = m2, sd1 = sd1, sd2 = sd2)

      esci <- psych::cohen.d.ci(d = d, n1 = n1, n2 = n2, alpha = .05) %>%
        as_tibble()

      ## Find p-value
      t_test <- t.test(formula = occ_creative ~ handedness, data = hand_sub)
      p_value <- t_test$p.value

      es <- es %>%
        add_row(sample = sample_name,
                n1 = n1, n2 = n2, m1 = m1, m2 = m2, es_type = es_type,
                lower = esci$lower, effect = esci$effect, upper = esci$upper,
                p = p_value, rl_sm = rl_sm, measure = "Originality/Inductive reasoning")
    }
  }
return(es)
}
