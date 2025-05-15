extract_counts <- function(hand_proc, codes) {
  ## (a) Add column with profession groups (Art, Achitecture, Music, Control)
  ## and columns "creative group" (specific profesisons, aggregates), "control group"
  codes <- codes %>% mutate(occ = code, profession = profession_group, creative_group = occupation)

  hand_coded <- left_join(hand_proc, codes) %>%
    ## Control group: all subjects not in the Art, Architecture, or Music groups
    mutate(profession = replace_na(profession, "Control"))

  #### Find counts for right/left in each occupation
  counts <- hand_coded %>%
    group_by(rl_sm, profession, creative_group, sample, handedness) %>%
    summarize(n = n())

  counts <- counts %>%
    pivot_wider(names_from = handedness, names_glue = "n_{handedness}", values_from = n) %>%
    mutate(n_left = replace_na(n_left, 0)) %>%
    mutate(n_total = n_right + n_left)

  ## (b) Find number of lefties and total for control group
  control_counts <- counts %>%
    ungroup %>%
    filter(profession == "Control") %>%
    rename(n_right_control = n_right,
           n_left_control = n_left,
           n_control = n_total) %>%
    mutate(control_group = "Non-art, architecture, music") %>%
    select(-profession, -creative_group)

  ## (c) Find number of lefties and total for each creative group
  creative_counts <- counts %>%
    ungroup %>%
    filter(profession %in% c("Art", "Architecture", "Music")) %>%
    rename(n_right_creative = n_right,
           n_left_creative = n_left,
           n_creative = n_total)

    ## Combine creative, control group counts
    ## Format and add variables for effect extraction
    ## (ch/analyses/professions/extract.Rmd)
    counts_proc <- left_join(creative_counts, control_counts) %>%
      mutate(pop = "Professionals",
             pref_success = "mix",
             comparison_details = case_when(
                                            sample == 4 & rl_sm == "R/L" ~ "R/L",
                                            sample == 4 & rl_sm == "S/M" ~ "(SR+SL)/M",
                                            sample == 5 & rl_sm == "R/L" ~ "SR/SL",
                                            sample == 5 & rl_sm == "S/M" ~ "(SR+SL)/M"
                                            ),
             h = case_when(
                           sample == 4 ~ "1i_2pt",
                           sample == 5 ~ "3i_3pt"
                           ),
             include = 0,
             exclude_rnr = 0
             ) %>%
    select(pop, profession, creative_group, control_group,
           rl_sm, n_creative, n_left_creative,
           n_control, n_left_control,
           h, pref_success, comparison_details,
           include, exclude_rnr, sample)

    return(counts_proc)
}
