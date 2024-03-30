###
##
# R script for joining all RDS files with the raw data of the PRL task. 
##
###


suppressPackageStartupMessages({
  library("readxl")
  library("readr")
  library("dplyr")
  library("here")
})


# "stranger" condition
stranger_data <- readRDS(
  here::here("data", "prep", "prl", "interim", "stranger_raw_prl_data.rds")
)
names(stranger_data)

# "self" condition
control_self_data <- readRDS(
  here::here("data", "prep", "prl", "interim", "control_self_raw_prl_data.rds")
)
surprise_self_data <- readRDS(
  here::here("data", "prep", "prl", "interim", "surprise_self_raw_prl_data.rds")
)

self_data <- rbind(control_self_data, surprise_self_data) |> 
  dplyr::rename(
    "code_psytoolkit" = "psytoolkit_code"
  )
names(self_data)

# Bind self and stranger conditions
raw_df <- rbind(stranger_data, self_data)

# 
raw_df$user_id <- raw_df$user_id |> 
  dplyr::recode(
    "ch_be_1986_3_11_139_f" = "ch_be_1986_03_11_139_f" , 
    "gi_gh_1958_11_15_917_m" = "gi_gh_1958_11_15_317_m",
    "gi_gh_1968_11_15_317_m" = "gi_gh_1958_11_15_317_m",
    "lo_bu_1997_9_4_956_m" = "lo_bu_1997_09_04_956_m",
    "ch_bp_1972_2_ 3_126_f" = "ch_bp_1972_02_03_126_f", 
    "el_sc_1997_04_16_742_f" = "es_sc_1997_04_16_742_f"
  )

saveRDS(
  raw_df,
  here::here(
    "data", "prep", "prl", "prl_raw_data.rds"
  )
)

# eof ---

