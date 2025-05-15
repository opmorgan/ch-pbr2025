## Depends on: tidyverse
library(tidyverse)

## Define function to create (or load) glm table (one row for each observation)
## Requires the global variable: pool_dir
make_glm_df <- function(ee_proc) {
  #### Check if a cached (saved) table exists. If so, load it.
  glm_data_filename <- "glm_data.csv"
  glm_data_filepath <- here(pool_dir, "data", "proc", glm_data_filename)
  if (file.exists(glm_data_filepath)) {
      print(str_c("Loading existing glm data table found at ",
                  glm_data_filepath))
      glm_data <- readr::read_csv(glm_data_filepath)
  } else {
  #### Otherwise, create it.
      print(str_c("No existing glm data table found at ", glm_data_filepath,
                  ". Creating new glm data table..."))


    #### Prepare table with a row for each observation. Columns:
    ## study, profession, rl_sm, pop, comparison,
    ## group (creative/control), handedness (left/right)
    ## (group and handedness are the IV and DV; rl_sm is a filtering variable;
    ## all other columns are variables you want to model as random effects)
    glm_data <- ee_proc %>%
      slice(0) %>%
      select(est_id, study, profession, rl_sm, pop, comparison) %>%
      mutate(group = as.character(), handedness = as.numeric())


    #### For each row, generate a table with n_creative + n_control rows:
    ## (n_left_creative) rows with "group = creative", "handedness = 1" (creative, left)
    ## (n_creative - n_left_creative) rows with "group = creative", "handedness = 0" (creative, right)
    ## (n_right_control) rows with "group = control", "handedness = 1" (control, left)
    ## (n_control - n_left_control) rows with "group = creative", "handedness = 0" (right)

    ## Define function to add rows to glm data table, given a row from extract effects and
    ## the desired n, group, and handedness
    ## (n is based on n_creative, n_left_creative, n_control, n_left_control),
    add_glm_rows <- function(glm_data, ee_row, n, group, handedness) {
        glm_row <- ee_row %>%
          select(est_id, study, profession, rl_sm, pop, comparison) %>%
          mutate(group = group, handedness = handedness)
        glm_rows <- glm_row[rep(1, n), ]
        glm_data <- glm_data %>% tibble::add_row(glm_rows)
    }

    ## Loop through each row (each extracted effect)
    #ee_row <- ee_proc %>% slice(2)
    #ee_row

    for (j in 1:dim(ee_proc)[1]) {
      ee_row <- ee_proc %>% slice(j)
      str_c(j, "/", dim(ee_proc)[1], ". Study: ", ee_row$study,
            ". Profession: ", ee_row$profession) %>%
        print()

      ## Make (n_left_creative) rows with "group = creative", "handedness = 1"
      ## (creative, left)
      glm_data <- glm_data %>%
        add_glm_rows(ee_row, n = ee_row[["n_left_creative"]],
                     group = "creative", handedness = 1)

      ## Make (n_creative - n_left_creative) rows with "group = creative", "handedness = 0"
      ## (creative, right)
      glm_data <- glm_data %>%
        add_glm_rows(ee_row, n = (ee_row[["n_creative"]] - ee_row[["n_left_creative"]]),
                     group = "creative", handedness = 0)

      ## Make (n_right_control) rows with "group = control", "handedness = 1" (control, left)
      glm_data <- glm_data %>%
        add_glm_rows(ee_row, n = ee_row[["n_left_control"]],
                     group = "control", handedness = 1)

      ## Make (n_control - n_left_control) rows with "group = creative", "handedness = 0" (right)
      glm_data <- glm_data %>%
        add_glm_rows(ee_row, n = (ee_row[["n_control"]] - ee_row[["n_left_control"]]),
                     group = "control", handedness = 0)

    }

    ## Save dataframe
    readr::write_csv(glm_data, here(pool_dir, "data", "proc",
                                    glm_data_filename))
    print(str_c("New glm data table saved at ", glm_data_filepath))

  }
  return(glm_data)
}
