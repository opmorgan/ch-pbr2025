## Dependencies: tidyverse
library(tidyverse)

## Format p-values to display nicely in tables
## Input: a tibble with a column named "p"
format_p <- function(tbl) {
  tbl  %>%
    ## display p as "<.001" if it is less than 0.001; or else, round.
    mutate(p = case_when(
                         (p < 0.001) ~ "<.001",
                         (0.001 <= p & p < 0.01) ~
                           as.character(p %>% numformat(3)), ## Changed from round() Apr 27
                         (0.01 <= p) ~
                           as.character(p  %>% numformat(2))
        )
    )
}

format_p.value <- function(tbl) {
  tbl  %>%
    ## display p as "<.001" if it is less than 0.001; or else, round.
    mutate(p.value = case_when(
                         (p.value < 0.001) ~ "<.001",
                         (0.001 <= p.value & p.value < 0.01) ~
                           as.character(p.value %>% numformat(3)), ## Changed from round() Apr 27
                         (0.01 <= p.value) ~
                           as.character(p.value  %>% numformat(2))
        )
    )
}

## Generate string reporting p-value, like "p = .04" or "p < .001"
## Input: a tibble with a column named "p"
## Output: a copy of the tibble with a new column "p_string"
format_p_string <- function(tbl) {
  tbl  %>%
    ## display p as "<.001" if it is less than 0.001; or else, round.
    mutate(p_string = case_when(
                         (p < 0.001) ~ "p < .001",
                         (0.001 <= p & p < 0.01) ~
                           str_c("p = ", as.character(p %>% round(3))),
                         (0.01 <= p) ~
                           str_c("p = ", as.character(p  %>% round(2)))
        )
    )
}

#### Round number and remove leading zero, output as string
numformat <- function(x, digits = 2) {
    ncode <- paste0("%.", digits, "f")
    sub("^(-?)0.", "\\1.", sprintf(ncode, x))
}
