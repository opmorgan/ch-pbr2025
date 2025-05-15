write_counts <- function(counts_proc) {
  ## Create spreadsheet with counts of left/right, mixed/strong handers
  ## for each occupation, for each dataset.
  ## (Formatted to use as input in ch/analyses/professions/extract.Rmd)

  nlsy79 <- counts_proc %>%
    filter(sample == 4) %>%
    select(-sample)

  write_csv(nlsy79, here(output_dir, "nlsy79.csv"))

  nlsy97 <- counts_proc %>%
    filter(sample == 5) %>%
    select(-sample)

  write_csv(nlsy97, here(output_dir, "nlsy97.csv"))
}
