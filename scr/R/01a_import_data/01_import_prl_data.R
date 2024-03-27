# Import PRL data.
# 

# Load necessary libraries
library(dplyr)
library(here)

# Using the `here` package to set the raw folder path more reliably
# Make sure you have the here package installed using install.packages("here")
raw_folder_path <- here::here("data", "raw")

# Get a list of all subject folders within the "raw" folder
subject_folders <- list.files(path = raw_folder_path, full.names = TRUE)

# Initialize an empty data frame to store the combined data
combined_prl_data <- data.frame()

# Loop through each subject folder
for (subject_folder in subject_folders) {
  cat("Processing folder:", subject_folder, "\n")
  
  # Recursively list all files to manually filter for PRL CSV files later
  all_files <- list.files(path = subject_folder, full.names = TRUE, recursive = TRUE)
  
  # Manually filter for PRL CSV files
  prl_csv_files <- all_files[grep("PRL.*\\.csv$", all_files)]
  
  if (length(prl_csv_files) > 0) {
    cat("CSV files found:", length(prl_csv_files), "\n")
  } else {
    cat("No CSV files found in this folder.\n")
  }
  
  # Process each PRL CSV file
  for (prl_file in prl_csv_files) {
    condition <- gsub(".*[\\\\/](PRL.*?)[\\\\/].*", "\\1", prl_file)
    condition <- gsub(" ", "_", condition)
    
    temp_data <- read.csv(prl_file)
    temp_data$Condition <- condition
    
    combined_prl_data <- rbind(combined_prl_data, temp_data)
  }
}

if (nrow(combined_prl_data) > 0) {
  # If data has been added, rearrange the columns to have the Condition column first
  combined_prl_data <- combined_prl_data %>% select(Condition, everything())
  print(head(combined_prl_data))
} else {
  cat("No data has been combined. Please check the file paths and patterns.\n")
}

# Write the combined data frame to a new CSV file
write.csv(
  combined_prl_data, 
  here::here("data", "prep", "prl", "interim", "combined_prl_raw_data.csv"), 
  row.names = FALSE
)


