This repo contains data, analysis scripts, task scripts, and figures for the paper:

Morgan, O. P., Zhao, Siyi, and Casasanto, D. (2025). Handedness and creativity: Facts and fictions. _Psychonomic Bulletin & Review_.

This repo is archived at: [https://osf.io/mc5gy](https://osf.io/xhpjy/)

# Setup and dependencies

All analysis code depends on the ```R``` programming language. It has been tested with version 4.3.2.

## R dependencies

R dependencies are managed with [renv](https://rstudio.github.io/renv/articles/renv.html). To set up the project with all dependencies, install ```R``` [using the instructions for your operating system](https://www.r-project.org/), clone this repository, then run the following commands from the R console.

1. Install renv:

```install.packages("renv")```

2. Install project dependencies. Run from the project (top-level) directory:

```renv::restore()```

```renv``` will load dependencies that are already installed on your system, and install and load those that aren't.


# Analyses

The folder ```analyses/``` contains all analysis data, scripts, and output. ```analyses/lib/``` contains common functions used across analyses. 

## Divergent thinking tasks meta-analysis

The folder ```analyses/tasks``` contains scripts used to extract, pool, and visualize data from meta-analyzed studies. The following scripts are designed to be run in order:

- ```1_extract/extract.Rmd``` extracts effect sizes from the raw data reported in each paper. Raw data used to calculate (or extract) effect sizes from each study is recorded in ```1_extract/data/input```. The folder ```1_extract/lib``` contains supporting functions, and ```1_extract/components```includes component ```.Rmd``` files called in ```1_extract/extract.Rmd```. The report ```1_extract/extract.html``` shows the results of effect extraction.
- ```2_pool/pool.Rmd```pools the effect sizes extracted in ```1_extract/extract.Rmd```. The report ```2_pool/pool.Rmd``` shows the results of effect size pooling.
- ```3_forest/forest.Rmd``` created forest plots using the output of ```2_pool/pool.Rmd```. The script ```3_forest/forest_display.Rmd``` displays the resulting plots in its output report, ```3_forest/forest_display.html```.

## Creative professions meta-analysis

The folder ```analyses/professions``` contains scripts used to extract, pool, and visualize data from studies in the creative Professions meta-analysis. It follows the same directory structure as ```analyses/tasks```. The following scripts are designed to be run in order:

- ```1_extract/extract.Rmd``` (Extract effects)
- ```2_pool/pool.Rmd``` (Pool effect sizes)
- ```3_funnel/funnel.Rmd``` (Make funnel plots)


## Re-analyses of Goodman (2014) and Zickert et al. (2018)

The folder ```0_replicate_goodman/``` contains scripts used to replicate, extend, and visualize analyses from Goodman (2014).[^1] The folder ```data/replication_data/Data-appendix--Joshua-Goodman/``` includes Goodman's summary data, ```handedness.dta```, downloaded from the replication dataset the author made available [here](https://perma.cc/FH75-2F45). This data is described in Goodman’s replication data readme (included here in the same directory as ```handedness.dta```) and in Goodman's (2014) [appendix](https://perma.cc/FF3E-WRDP). Analyses are run with the script ```0_replicate_goodman/main.Rmd```.

The folder ```0_replicate_zickert/``` contains scripts used to replicate, extend, and visualize analyses from Zickert et al. (2018).[^2] The folder ```data``` includes Zickert et al.'s (2018) replication dataset[^3], ```analysis_data.csv```, which the authors made available [here](https://perma.cc/75BW-5L3U). Analyses are run with the script ```0_replicate_zickert/reproduce_zickert.Rmd```

# PRISMA flowcharts

The folder ```prisma_flowcharts/``` contains flowcharts created with PRISMA's shiny app, as well as the MARS/PRISMA resources that were consulted.

# Literature search data

The folder ```lit_search/``` contains the output of the Google Scholar searches used in the divergent thinking tasks meta-analysis. Automated searches were conducted using the tool [Publish or Perish](https://perma.cc/D6U8-B9SH). Spreadsheets were created by manually combining search results with <250 character search terms (Google Scholar's maximum) to create the logical search terms described in the manuscript. Then, studies were screened and coded according to the inclusion criteria described in the manuscript, as recorded in the spreadsheets ```GS-AUT.tsv```, ```GS-RAT.tsv```, and ```GS-TTCT.tsv```.

# Publication figures

All figures shown in the manuscript can be found in the folder ```publication_figures/```. These figures were generated using code in ```analyses/```, and edited using [Inkscape](https://perma.cc/V4C9-ZYV2) (e.g., to add titles and combine multiplot figures).


# References

[^1]: Goodman, J. (2014). The wages of sinistrality: Handedness, brain structure, and human capital accumulation. Journal of Economic Perspectives, 28(4), 193–212. [https://doi.org/10.1257/jep.28.4.193](https://doi.org/10.1257/jep.28.4.193)

[^2]: Zickert, N., Geuze, R. H., van der Feen, F. E., & Groothuis, T. G. G. (2018). Fitness costs and benefits associated with hand preference in humans: A large internet study in a Dutch sample. Evolution and Human Behavior, 39(2), 235–248. [https://doi.org/10.1016/j.evolhumbehav.2018.01.001](https://doi.org/10.1016/j.evolhumbehav.2018.01.001)

[^3]: Zickert, N., Geuze, R. H., van der Feen, F. E., & Groothuis, T. G. G. (2018). Replication data for: Fitness costs and benefits associated with hand preference in humans. DataverseNL. [10.34894/yotusm](10.34894/yotusm)



