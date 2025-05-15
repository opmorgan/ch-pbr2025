## Functions to style plots
funnel_plot_style <- function(g) {
  g_styled <- g +
    theme_minimal(base_size = 10) +
    theme(aspect.ratio = 1 / 1,
          axis.ticks.x = element_blank(),
          panel.grid.minor = element_blank(),
          legend.title = element_blank(),
          plot.title = element_text(hjust = 0.5)
    )
    return(g_styled)
}

gg_color <- function(g, plot_colors) {
  if (missing(plot_colors)) {
    # By default, use colorblind-safe categorical palette
    plot_colors <- c("#228833", "#AA3377", "#EE6677", "#CCBB44",
                     "#66CCEE", "#4477AA", "#BBBBBB")
  }
  g_colored <- g +
    scale_color_manual(values = plot_colors) +
    scale_fill_manual(values = plot_colors)
  return(g_colored)
}


## Make funnel plot for a profession
prof_funnel_plot <- function(prof_var, forest_table) {
  sub <- forest_table %>%
    filter(profession == prof_var) %>%
    arrange(desc(study)) # put the diamond at the bottom.


  ## Convert everything to log odds
  sub <- sub %>% mutate(lower = log(lower),
                        effect = log(effect),
                        upper = log(upper)
                        )
  ## Find SE (log odds) for every estimate
  sub <- sub %>% mutate(se = ci_to_se(lower = lower, upper = upper))

  ## Prepare data for scatter-plotting
  plot_data <- sub %>% filter(!is.na(study))
  # plot_data %>% select(profession, study, lower, effect, upper, se)
  plot_data

  ## Assign the pooled effect and its SE
  poolest_row <- sub %>% filter(is.na(study))
  estimate <- poolest_row$effect
  se <- poolest_row$se

  ## Make label showing N (n studies)
  n_text <- str_c("N = ", poolest_row$N, "(", poolest_row$N_stu, ")")
  # n_grob = textGrob(label = n_text, x = .95, y = 0.95,
  #     just = c("right", "top"),
  #     gp=gpar(fontface = "italic", col = "gray90",size = 3))

  ## Store a vector of values that spans the range from 0
  ## to the max value of SE in your dataset.
  ## Make the increment small enough (I choose 0.001)
  ## to ensure your whole range of data is captured
  ## Ignore se "Inf"
  range_sub <- plot_data %>% filter(is.finite(se))
  se_seq <- seq(0, max(range_sub$se), 0.001)

  ## Now, compute vectors of the lower-limit and upper limit values for
  ## the 95% CI region, using the range of SE that you generated in the
  ## previous step, and the stored value of your meta-analytic estimate.
  alpha <- 0.05
  lower95 <- estimate - (qnorm(1-(alpha/2)) * se_seq)
  upper95 <- estimate + (qnorm(1-(alpha/2)) * se_seq)

  ## You can do this for a 99% CI region too
  alpha <- 0.01
  lower99 <- estimate - (qnorm(1-(alpha/2)) * se_seq)
  upper99 <- estimate + (qnorm(1-(alpha/2)) * se_seq)

  ## And finally, do the same thing except now calculating the
  ## confidence interval for your meta-analytic estimate based on
  ## the stored value of its standard error
  alpha <- 0.05
  pooled_lower95 <- estimate - (qnorm(1-(alpha/2)) * se) # poolest_row$lower
  pooled_upper95 <- estimate + (qnorm(1-(alpha/2)) * se) # poolest_row$upper

  #Now, put all of those calculated values into one data frame (called 'dfCI').
  #You might get a warning about '...row names were found from a short variable...'
  #You can ignore it.
  dfCI <- data.frame(lower95, upper95, lower99, upper99,
                    se_seq, estimate, pooled_lower95, pooled_upper95)

  #Now we can actually make the funnel plot.
  #Using your original data-frame, map standard error to your x-axis (for now) and Zr to your y-axis
  funnel_plot <- ggplot(aes(x = se, y = effect), data = plot_data) +
    ## Add your data-points to the scatterplot
    geom_point(shape = 21, size = 4, aes(fill = rl_sm), alpha = .5) +
    ## Give the x- and y- axes informative labels
    xlab('Standard Error') + ylab('Log Odds')+
    ## Now using the 'dfCI' data-frame we created, plot dotted lines corresponding
    ## to the lower and upper limits of your 95% CI region,
    ## And dashed lines corresponding to your 99% CI region
    geom_line(aes(x = se_seq, y = lower95), linetype = 'solid', data = dfCI) +
    geom_line(aes(x = se_seq, y = upper95), linetype = 'solid', data = dfCI) +
    geom_line(aes(x = se_seq, y = lower99), linetype = 'dashed', data = dfCI) +
    geom_line(aes(x = se_seq, y = upper99), linetype = 'dashed', data = dfCI) +
    ## Plot line through pooled point estimate
    geom_segment(aes(x = min(se_seq), y = estimate,
                 xend = max(se_seq), yend = estimate),
                 linetype = "solid", data = dfCI) +
    ## Now plot dotted lines corresponding to the 95% CI of your meta-analytic estimate
    # geom_segment(aes(x = min(se_seq), y = pooled_lower95, xend = max(se_seq),
    #                  yend = pooled_lower95), linetype = 'dashed', data = dfCI) +
    # geom_segment(aes(x = min(se_seq), y = pooled_upper95, xend = max(se_seq),
    #                  yend = pooled_upper95), linetype = 'dashed', data = dfCI) +
    ## Reverse the x-axis ordering (se) so that the tip of the funnel will appear
    ## at the top of the figure once we swap the x- and y-axes...
    scale_x_reverse() +
    ## Transform y axis to odds
    # scale_y_continuous(trans = 'log10') +
    ## Specify the range and interval for the tick-marks of the y-axis (Zr);
    ## And now we flip the axes so that SE is on y- and effect is on x-
    coord_flip(clip = "off", expand = T) +
    ## Add n
    # geom_text(aes(label = n_text), vjust = "inward", hjust = "inward") +
    annotate(geom = "text", label = n_text, x = -Inf, y = Inf, vjust = "inward", hjust = "inward") +
    ## Style
    labs(title = prof_var)

  funnel_plot <- funnel_plot %>% funnel_plot_style() %>% gg_color()

  ## Save figure
  figure_name <- str_c(prof_var, ".png")
  figure_path <- here(funnel_dir, "figures", figure_name)
  ggsave(figure_path, plot = funnel_plot, width=5, height=5, dpi=300)

  return(funnel_plot)
}
