## Functions to style plots
gg_style <- function(g) {
  g_styled <- g +
    theme_minimal(base_size = 10) +
    theme(aspect.ratio = 1 / 1,
          panel.border = element_rect(colour = "black", fill = NA, size = .4),
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          plot.background = element_rect(fill = "white", color = "white"),
          panel.background = element_rect(fill = "white")
    )
    return(g_styled)
}

gg_color <- function(g, plot_colors) {
  if (missing(plot_colors)) {
    # By default, use colorblind-safe categorical palette
    plot_colors <- c("#4477AA", "#EE6677", "#CCBB44", "#66CCEE",
                     "#AA3377", "#228833", "#BBBBBB")
  }
  g_colored <- g +
    scale_color_manual(values = plot_colors) +
    scale_fill_manual(values = plot_colors)
  return(g_colored)
}
