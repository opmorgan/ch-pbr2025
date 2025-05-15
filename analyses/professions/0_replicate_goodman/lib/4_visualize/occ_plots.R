## Depends on analyses/professions/lib/es_functions.R
find_occ_counts <- function(data) {
  counts <- data %>%
    group_by(occ, handedness) %>%
    dplyr::summarize(n = n(),
              occ_creative = first(occ_creative)) %>%
    pivot_wider(names_from = handedness,
                names_glue = "n_{handedness}", values_from = n) %>%
    mutate(n_left = replace_na(n_left, 0)) %>%
    mutate(n_right = replace_na(n_right, 0)) %>%
    mutate(n_total = n_right + n_left) %>%
    mutate(percent_left =  (n_left / n_total) * 100) %>%
    ## Depends on analyses/professions/lib/es_functions.R
    mutate(lower = (find_prop_score(n_left, n_total) %>% .[["lower"]]) * 100,
           upper = (find_prop_score(n_left, n_total) %>% .[["upper"]]) * 100)
    return(counts)
}

## Plot proportion of left handedness for each profession rating,
## from least to most creative.
## X axis: professions, ordered from least to most creative.
## Y Axis: proportion left handed
## - Size of dot indicates N
graph_occ_counts <- function(counts, title = "", geom_smooth = T) {
  g <- ggplot(data = counts, aes(x = occ_creative,
                                 y = percent_left, group_by(occ))) +
    labs(title = title,
         x = "Originality/Inductive Reasoning (Z score)",
         y = "% Left-handed") +
    geom_point(alpha = .1, aes(size = n_total), show.legend = F) +
    scale_size_area(name = NULL) +
    {if (geom_smooth == T)
    geom_smooth(color = "gray10", size = .1, alpha = .2, method = "glm")
    }
  g <- g %>% gg_style()
  g <- g + theme(aspect.ratio = 1/1.618)
  return(g)
}
