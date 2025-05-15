## Dependencise: ggplot, gghalves
## Visualize group comparison: right vs. left handers  and job creativity rating
## X axis: two bins, right and left handers
## Y axis: (1) Dots showing creativity rating for each person?
## Y axis: (2) Box plots showing medians, quartiles
## Y axis: (3) Dot with line error bounds showing mean and SD
## Y axis: (4) Violin plots alongside

make_replication_plot <- function(data, title = "") {
  g <- data %>% ggplot(aes(x = handedness, y = occ_creative,
                          color = handedness, fill = handedness)) +
    # geom_quasirandom(alpha = 0.1, varwidth = T, show.legend = F) +
    geom_half_violin(side = "r", alpha = 0.5,
                     show.legend = F) +
    geom_half_boxplot(side = "l", alpha = 0.2, size = 0.2,
                 varwidth = T,
                 outlier.shape = NA,
                 coef = 0,
                 show.legend = F) +
    stat_summary(geom = "point", fun = mean, color = "black",
                 size = .5, show.legend = F) +
    stat_summary(geom = "linerange", fun.data = mean_cl_normal, size = .7,
                color = "black") +
    scale_x_discrete(labels = c("Left", "Right")) +
    scale_y_continuous(limits = c(-3, 3), expand = c(0, 0)) +
    labs(x = "Handedness",
         y = "ONET Originality/Inductive Reasoning (Z scores)",
         title = title)
  g <- g %>% gg_color() %>% gg_style()
  g <- g + theme(aspect.ratio = 2 / 1)
}
