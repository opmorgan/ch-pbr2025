## Dependencies: gt, tidyverse
library(gt)
library(tidyverse)

## Make a table with gt() where all numbers are rounded to 3 digits
pretty_table <- function(table, title = NULL, digits = 3,
                         groupname_col = NULL, color_effects = F,
                         color_domain = c(-100, 100),
                         palette_effects = c("#f1b6da", "#f7f7f7", "#b8e186"),
                         color_na = "#f7f7f7"
                         ) {
  if (color_effects == F) {
    gt(table, groupname_col = groupname_col) %>%
      tab_header(title = title) %>%
      fmt_missing(columns = everything(), missing_text = "") %>%
      fmt_number(columns = where(is.numeric),
                 drop_trailing_zeros = T,
                 decimals = digits)
  } else if (color_effects == T) {
    gt(table, groupname_col = groupname_col) %>%
      tab_header(title = title) %>%
      fmt_missing(columns = everything(), missing_text = "") %>%
      fmt_number(columns = where(is.numeric),
                 drop_trailing_zeros = T,
                 decimals = digits) %>%
      data_color(
        columns = c(lower, effect, upper),
        colors = scales::col_bin(
            bins = 3,
            na.color = color_na,
          palette = palette_effects,
          domain = color_domain)
      )
  }
}
