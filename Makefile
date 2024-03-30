# Define variables
R_SCRIPTS_DIR := src/R
DATA_DIR := data
RAW_DATA_DIR := $(DATA_DIR)/prep/prl/interim
RAW_DATA_FILE := $(RAW_DATA_DIR)/combined_prl_raw_data.csv

# Define targets and dependencies
all: $(RAW_DATA_FILE)

$(RAW_DATA_FILE): $(R_SCRIPTS_DIR)/01a_import_data/01_import_prl_data.R
	Rscript $<

# Define phony targets
.PHONY: all
