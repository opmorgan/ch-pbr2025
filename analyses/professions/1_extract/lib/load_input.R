#### Load input file
## Depends on: readr
load_input <- function(input_filename) {
  input_path <- here(extract_dir, "data", "input", input_filename)
  input <- readr::read_csv(input_path)
}
