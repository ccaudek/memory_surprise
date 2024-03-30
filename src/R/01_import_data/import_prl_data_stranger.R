# R script for importing the PRL data in the "stranger" conditions, where
# the raw data are saved in a folder with a different structure than the
# "self" data.

suppressPackageStartupMessages({
  library("readxl")
  library("readr")
  library("dplyr")
  library("here")
})

source(here::here("scr", "R", "functions", "prl_funs.R"))

# Generate interim RDS files for the "stranger" condition
load_psychtoolkit_files()

d <- write_prl_raw_data_list()

saveRDS(
  d,
  here::here("data", "prep", "prl", "interim", "stranger_raw_prl_data.rds")
)

# eof -----
