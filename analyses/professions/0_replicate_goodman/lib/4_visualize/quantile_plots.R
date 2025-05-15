## Depends on analyses/professions/lib/es_functions.R
find_quantile_counts <- function(hand_data, n_quantiles) {

  ## Add variable for quantile
  hand_data <- hand_data %>%
    mutate(quantile = as.factor(ntile(desc(occ_creative), n_quantiles)))

  ## Count n and percent leftire by quantile
  counts <- hand_data %>%
    group_by(quantile, handedness) %>%
    dplyr::summarize(n = n()) %>%
    pivot_wider(names_from = handedness,
                names_glue = "n_{handedness}", values_from = n) %>%
    mutate(n_left = replace_na(n_left, 0)) %>%
    mutate(n_total = n_right + n_left) %>%
    mutate(percent_left =  (n_left / n_total) * 100) %>%
    ## Depends on analyses/professions/lib/es_functions.R
    mutate(lower = (find_prop_score(n_left, n_total) %>% .[["lower"]]) * 100,
           upper = (find_prop_score(n_left, n_total) %>% .[["upper"]]) * 100)
    return(counts)
}


find_total_count <- function(counts) {
  ## Find overall mean proportion
  total_count <- counts %>%
    ungroup() %>%
    dplyr::summarize(n_left = sum(n_left),
              n_right = sum(n_right),
              n_total = sum(n_total),
              percent_left = (sum(n_left) / sum(n_total)) * 100) %>%
    ## Depends on analyses/professions/lib/es_functions.R
    mutate(lower = (find_prop_score(n_left, n_total) %>% .[["lower"]]) * 100,
           upper = (find_prop_score(n_left, n_total) %>% .[["upper"]]) * 100)
  return(total_count)
}


## X axis: n bins, one for each quantile of creativity ratings.
## Then, to visualize:
## Y axis: (1) Dot showing proportion of lefties
## Y axis: (2) Line error bounds showing 95% CI
## - Add horizontal line sample mean proportion, with error bounds
graph_quantile_counts <- function(counts, y_limits = c(5, 15), title = "") {
  n_quantiles <- length(counts$quantile)

  g <- ggplot(data = counts, aes(x = fct_rev(quantile), y = percent_left)) +
    ## Hack for error "Discrete value... continuous scale"
    geom_point(shape = NA) +
    ## Add ribbon, line showing sample mean and CI
    geom_ribbon(aes(x = 1:n_quantiles,
                    ymin = total_count$lower, ymax = total_count$upper),
                fill = "gray70", alpha = .2) +
    geom_line(aes(x = 1:n_quantiles, y = total_count$percent_left),
              size = .1, color = "gray10", linetype = "dashed") +
    ## Add error bars
    geom_errorbar(aes(ymin = lower, ymax = upper), width = .3) +
    labs(title = title,
         x = "Originality/Inductive Reasoning Quantile",
         y = "% Left-handed") +
    geom_point() +
    scale_y_continuous(limits = y_limits, expand = c(0, 0))
  g <- g %>% gg_style()
}


print_quantile_counts <- function(counts) {
  counts %>%
    arrange(quantile) %>%
    mutate(ci = str_c("[", lower, ", ", upper, "]")) %>%
    select(quantile, n_total, n_left, n_right, percent_left, ci) %>%
    rename(Quantile = quantile, n = n_total, `% left` = percent_left,
           `n (left)` = n_left, `n (right)` = n_right, `95% CI` = ci) %>%
    head(20) %>%
    pretty_table(digits = 2)
}
