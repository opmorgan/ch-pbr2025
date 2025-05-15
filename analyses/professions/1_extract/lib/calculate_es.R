###### Calculate summary ES estimates and CI from input data

#### Main function: calculate ES from processed input data
## Depends on: lib/calculate_es.R/
##             create_cond_vars(), create_hand_vars(),
##             create_prop_matrix(), create_es_row_base()
##
## Input "proc" must be a tibble containing the columns:
## n_creative, n_left_creative,
## n_control, n_left control,
## PL_creative, n_right_creative, PR_control,
## PL_control, n_right_control, PR_control
## i.e., the output of lib/proc.R/calc_convenience_vars().
calculate_es <- function(input, study_label, OR = T, RR = T, freq = F, d = F) {
  ## Initialize table to hold results
  es_tbl <- init_es_table()

  ## For frequency/odds data, add calculated variables
  if(OR == T | RR == T | freq == T) {
    proc <- calc_convenience_vars(input, study_label)
  }

  ## For continuous data, just need means and sds.
  if(d == T) {
    proc <- input %>% mutate(study = study_label,
                             PL_creative = n_creative,
                             PL_control = n_control
                             )
  }

  ## Extract effect size from each processed input row
  for (j in 1:dim(proc)[1]) {

    ## Subset input spreadsheet to one row
    proc_row <- proc %>% slice(j)

    if(d == T) {
      ## Check that there is a control group; if not, cannot calculate d.
      if(proc_row$control_group %in% c("None", "none", NA)) {
           stop("Cannot calculate Cohen's D: no control group")
      } else {
          ## Calculate Cohen's D with CI, add to study results table
          d_tbl <- find_d(m1 = proc_row$m_creative, m2 = proc_row$m_control,
                          sd1 = proc_row$sd_creative, sd2 = proc_row$sd_control,
                          n1 = proc_row$n_creative, n2 = proc_row$n_control)
          es_row <- proc_row %>% select(-m_creative, -m_control,
                                        -sd_creative, -sd_control)
          es_tbl <- es_tbl %>% add_row(add_column(es_row, d_tbl))
      }
    }

    if (freq == T) {
      ## Check that "control_group" equals "none" or "NA")
      if (proc_row$control_group %in% c("None", "none", NA)) {
      ## Calculate CI around frequency, add to study results table
      freq_tbl <- find_freq(n_total = proc_row$n_creative,
                            n_left = proc_row$n_left_creative)
      es_tbl <- es_tbl %>% add_row(add_column(proc_row, freq_tbl))
      } else {
        stop("Cannot calculate frequency (no control) because input contains a control group.")
      }
    }

    if (OR == T | RR == T) {
      ## Check that there is a control group; if not, cannot calculate OR/RR.
      if(proc_row$control_group %in% c("None", "none", NA)) {
           stop("Cannot calculate OR/RR: no control group")
      } else {
        ## Create frequency table labels
        cond_vars <- create_cond_vars(proc_row)
        hand_vars <- create_hand_vars(proc_row)

        ##  Create and inspect frequency table
        prop_matrix <- create_prop_matrix(proc_row, cond_vars, hand_vars)
        print(prop_matrix)

        if (OR == T) {
          ## Calculate Odds Ratio, add to study results table
          or_tbl <- find_OR_score(proc_row)
          es_tbl <- es_tbl %>% add_row(add_column(proc_row, or_tbl))
        }

        if (RR == T) {
          ## Calculate Risk Ratio, add to study results table
          rr_tbl <- find_RR(prop_matrix)
          es_tbl <- es_tbl %>% add_row(add_column(proc_row, rr_tbl))
        }
      }
    }
  }

  return(es_tbl)
}

#### (Helper 1) Process data: i.e., calculate convenience variables
## Input must be a tibble containing the colums:
## n_creative, n_left_creative,
## n_control, n_left control.
## From these variables, calculate:
## PL_creative, n_right_creative, PR_control
## PL_control, n_right_control, PR_control
calc_convenience_vars <- function(input_tbl, study_label) {
  proc <- input_tbl %>%
    mutate(study = study_label,
           PL_creative = (n_left_creative / n_creative) * 100,
           PL_control = (n_left_control / n_control) * 100,
           n_right_creative = n_creative - n_left_creative,
           n_right_control = n_control - n_left_control)

  return(proc)
}

#### (Helper 2) Initialize row to add to study results table
create_es_row_base <- function(proc_row, study_var) {
  es_row_base <- tibble(
         study = study, profession = proc_row$profession, pop = proc_row$pop,
         h = proc_row$h, rl_sm = proc_row$rl_sm,
         creative_group = proc_row$creative_group,
         control_group = proc_row$control_group,
         n_creative = proc_row$n_creative, n_control = proc_row$n_control,
         # n_left_creative = proc_row$n_left_creative, #### WHY CANT I ADD THIS COLUMN????????????????????????????????
         # n_left_creative = proc_row$n_left_creative, n_left_control = proc_row$n_left_control,
         # n_right_creative = proc_row$n_right_creative, n_right_control = proc_row$n_right_control,
         PL_creative = proc_row$PL_creative, PL_control = proc_row$PL_control,
         ## Add comparison details to notes
         notes = proc_row$comparison_details,
         include = proc_row$include, exclude_rnr = proc_row$exclude_rnr
         )
}

#### (Helper 3a) Create condition labels for proportion tables (these labels = group names)
create_cond_vars <- function(proc_row) {
  cond_vars <- c(proc_row$control_group, ## Control condition must go first
                 proc_row$creative_group)
  cond_vars <- factor(cond_vars, levels = cond_vars)
  return(cond_vars)
}

#### Helper 3b) Create handedness labels for proprtion tables
create_hand_vars <- function(proc_row) {
  if (proc_row$rl_sm == "R/L") {
    hand_vars <- c("Right", "Left") ## Right/Strong must go first
  } else if (proc_row$rl_sm == "S/M") {
    hand_vars <- c("Strong", "Mixed")
  }
  hand_vars <- factor(hand_vars, levels = hand_vars)
  return(hand_vars)
}

#### (Helper 4) Initialize proportion table
init_prop_table <- function(cond_vars, hand_vars) {
  prop <- tibble(Condition = cond_vars,
               Handedness = hand_vars,
               n = rep(NA, 2)) %>%
        expand(Condition, Handedness, n)
  return(prop)
}

#### (Helper 5) Create proportion tibble
  ## Calculate frequencies, put them in proportion table formatted for epitools
  ##
  ##            Handedness
  ## Condition  right left (or mixed/strong)
  ## Control    A     B
  ## Creative   C     D
create_prop_matrix <- function(proc_row, cond_vars, hand_vars) {
  ## Initialize proportion table
  prop <- tibble(Condition = cond_vars,
               Handedness = hand_vars,
               n = rep(NA, 2)) %>%
           expand(Condition, Handedness, n)

  ## Specify A, B, C, and D and put them in a labeled tibble
  prop <- prop %>%
    mutate(
     n = case_when(
       ## (A) Find number of right/strong-handed controls
       Condition == cond_vars[[1]] & Handedness == hand_vars[[1]]
       ~ proc_row$n_right_control,
       ## (B) Find number of left/mixed-handed controls
       Condition == cond_vars[[1]] & Handedness == hand_vars[[2]]
       ~ proc_row$n_left_control,
       ## (C) Find number of right-handed creatives
       Condition == cond_vars[[2]] & Handedness == hand_vars[[1]]
       ~ proc_row$n_right_creative,
       ## (D) Find number of left-handed creatives
       Condition == cond_vars[[2]] & Handedness == hand_vars[[2]]
       ~ proc_row$n_left_creative
      )
  )
  prop_matrix <- matrix(prop[["n"]], nrow = 2, ncol = 2, byrow = T)
  dimnames(prop_matrix) <- list("Conditions" = cond_vars,
                                "Handedness" = hand_vars)
  return(prop_matrix)
}

display_study_es <- function(es_study) {
  es_study %>% pretty_table(color_effects = T, color_domain = c(0, 2),
                            palette_effects = c("#b8e186", "#f7f7f7", "#f1b6da"),
                            color_na = "#f1b6da") ## Hack for ratio scale
}
