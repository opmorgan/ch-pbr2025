#### Check that the function ci_to_se() gives expected output by generating se, CI from a dataset, then converting the CI to se.
## Dependencies: palmerpenguins, tidyverse
library(palmerpenguins)
library(tidyverse)

data <- tibble(variable = character(),
               n = numeric(),
               se_raw = numeric(),
               lower = numeric(),
               effect = numeric(),
               upper = numeric()
)

variable <- "bill_length_mm"
data <- data  %>%
  add_row(variable = variable,
          n = length(penguins[[variable]]),
          se_raw = sd(penguins[[variable]], na.rm=T)/sqrt(n),
          effect = mean(penguins[[variable]], na.rm = T),
          lower = effect - (1.96 * se_raw),
          upper = effect + (1.96 * se_raw)
  )
data %>%
  mutate(se = ci_to_se(n = n, lower = lower, upper = upper, alpha = 0.05)) %>%
  select(se_raw, lower, effect, upper, se)
## se_raw should equal se



#### Create table showing pooled effect sizes from various analyses
## Dependencies: gt, tidyverse, common/lib/format_p.R
pool_table_gt <- function(in_table, title = "Meta-analysis results",
                          groupname_col = "label") {
  in_table %>%
  select(-metagen_obj) %>%
  format_p() %>%
  arrange(label, model_type, task, measure, es_type, effect) %>%
  pretty_table(title = title, groupname_col = groupname_col) %>%
  data_color(
             columns = c(lower, effect, upper),
             colors = scales::col_bin(
                                      bins = 3,
                                      na.color = "#f7f7f7",
                                      palette = c(
                                                  "#f1b6da",
                                                  "#f7f7f7",
                                                  "#b8e186"),
                                      domain = c(-2.3, 2.3))
  ) %>%
  tab_style(
            style = cell_text(whitespace = "nowrap"),
            locations = cells_body(columns = c(I2, measure))
            )
}

#### Print summaries of many metagen objects to console
##TODO: Print full, per study output in pretty tables
print_metagen_summaries <- function(disp_tbl) {
  dim(disp_tbl)[[1]]
  for (j in 1:dim(disp_tbl)[[1]]) {
    disp_row <- disp_tbl %>% filter(model_type == "random") %>% slice(j)
    disp_row %>% select(task, es_type, measure, N, N_stu, partials) %>% print()
    disp_row %>%
      .[["metagen_obj"]] %>%
      .[["metagen_obj"]] %>%
      summary() %>%
      print()
  }
}

#### Round number and remove leading zero, output as string
numformat <- function(x, digits = 2) {
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}
