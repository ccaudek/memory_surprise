################################################################################

# Memory and surprise
# Author: Corrado Caudek (corrado.caudek@unifi.it)

# MASTER FILE to source all other scripts
# Running this file reproduces the entire project workflow.

################################################################################


###
##
# PRL data preparation
##
###

# Source functions.
source("src/R/functions/prl_funs.R")

# Run data preparation script to generate file with all PRL raw data.
# here::here("data", "prep", "prl", "interim", "combined_prl_raw_data.csv")
source("src/R/01a_import_data/01_import_prl_data.R")

# Generate input for hddmrl.
source("src/R/01a_import_data/02_pre_processing.R")


###
##
# PRL analysis
##
###

# Scripts in the Python folder



###
##
# Memory data preparation
##
###

