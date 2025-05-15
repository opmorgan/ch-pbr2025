## SR/SL excludes mixed handers (there are far more of them in the 1997 sample, because the 1979 sample only gave two choices -- mixed handers are those who answered with a different handedness in different survey years). (Using Goodman's leftness variable: 0 is right, 1 is left; any in between are excluded.)
## S/M compares strong righties and lefties to mixedies. (Using Goodman's "leftness" variable: 0 or 1 counts as strong; any in between counts as mixed.
## S/M handers are coded as "right" and "left" in the "handedness" variable to make programming easier.
## R/L compares strong righties to non-righties in both samples. (Using Goodman's "lefty" variable: handedness is coded as 0 (right) or left (1), rounding "leftness" to the nearest integer.)


code_handedness <- function(hand) {
  ## Create columns for handedness counts
  hand_srsl <- hand %>%
    mutate("rl_sm" = "SR/SL")
  hand_sm <- hand %>%
    mutate("rl_sm" = "S/M")
  hand_rl <- hand %>%
    mutate("rl_sm" = "R/L")
  hand_proc <- bind_rows(hand_srsl, hand_sm, hand_rl) %>%
    mutate(handedness = "") %>%
  ## Populate handedness variable
    mutate(handedness = case_when(
                            rl_sm == "SR/SL" & leftness == 0 ~ "right", # Right handers (exclude mixedies)
                            rl_sm == "SR/SL" & leftness == 1 ~ "left",
                            rl_sm == "SR/SL" & leftness < 1 & leftness > 0 ~ "exclude",
                            rl_sm == "S/M" & leftness %in% c(0, 1) ~ "right", # Strong handers
                            rl_sm == "S/M" & (0 < leftness) & (leftness < 1) ~ "left", # Mixed handers
                            rl_sm == "R/L" & lefty == 0 ~ "right",
                            rl_sm == "R/L" & lefty == 1 ~ "left"
                            )
      )

  hand_proc <- hand_proc %>% filter(handedness != "exclude")
}
