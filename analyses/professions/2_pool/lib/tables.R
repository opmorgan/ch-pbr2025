## Initialize summary table
## A tibble with columns:
## task, es_type, measure
## N (estimates), N_s (number of studies), n (observations)
## lower, effect, upper, p
## model (fixed/random), notes
init_pool_table <- function() {
  pool_table <- tibble(profession = factor(levels = c("Architecture",
                                                      "Art", "Music")),
                       es_type = as.character(),
                       N = as.numeric(),
                       N_stu = as.numeric(),
                       # n_obs = as.numeric(), # Would have to match these after metagen
                       lower = as.numeric(),
                       effect = as.numeric(),
                       upper = as.numeric(),
                       p = as.numeric(),
                       I2 = as.character(),
                       model_type = factor(levels = c("fixed", "random")),
                       label = as.character(),
                       metagen_obj = lst(),
                       )
  return(pool_table)
}

#### Create gt table showing pooled effect sizes from various analyses
## Dependencies: gt, tidyverse,
## {proj}/analyses/lib/util.R/format_p(),
## {proj}/analyses/lib/util.R/numformat()
source(here("analyses", "lib", "util.R"))
pool_table_gt <- function(in_table, title = "Meta-analysis results",
                          groupname_col = "label") {
  in_table %>%
  # select(-metagen_obj) %>%
  format_p() %>%
  arrange(model_type, label, profession, es_type, effect) %>%
  pretty_table(title = title, groupname_col = groupname_col) %>%
  data_color(
             columns = c(lower, effect, upper),
             colors = scales::col_bin(
                                      bins = 3,
                                      na.color = "#f7f7f7",
                                      palette = c(
                                                  "#b8e186",
                                                  "#f7f7f7",
                                                  "#f1b6da"
                                                  ),
                                      domain = c(-1, 3))
  ) %>%
  tab_style(
            style = cell_text(whitespace = "nowrap"),
            locations = cells_body(columns = c(I2))
            )
}

#### Print summaries of many metagen objects to console
##TODO: Print full, per study output in pretty tables
print_metagen_summaries <- function(disp_tbl) {
  dim(disp_tbl)[[1]]
  for (j in 1:dim(disp_tbl)[[1]]) {
    disp_row <- disp_tbl %>% filter(model_type == "random") %>% slice(j)
    disp_row %>% select(profession, es_type, N, N_stu) %>% print()
    disp_row %>%
      .[["metagen_obj"]] %>%
      .[["metagen_obj"]] %>%
      summary() %>%
      print()
  }
}
