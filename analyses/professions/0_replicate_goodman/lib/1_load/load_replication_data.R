library(haven)
load_replication_data <- function(file_path = here(replication_data_dir, "handedness.dta")) {
  handedness_dta <- haven::read_dta(file_path)
}
