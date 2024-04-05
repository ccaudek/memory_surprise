library(readxl)
library(dplyr)
library(stringr)
library(here)



extract_participant_code <- function(excel_path) {
  info <- read_excel(excel_path, n_max = 2)
  participant <- info$nome_1
  surname <- info$cognome_1
  year <- as.character(info$anno_1)
  month <- sprintf("%02d", info$mese_1)
  day <- sprintf("%02d", info$giorno_1)
  phone <- as.character(info$cellulare_1)
  gender <- ifelse(info$sesso_1 == 2, "m", "f")
  participant_code <- paste(participant, surname, year, month, day, phone, gender, sep = "_")
  return(participant_code)
}









standardize_txt_data <- function(txt_data, num_columns, base_column_names) {
  current_num_columns <- ncol(txt_data)
  
  # If the current number of columns is more than expected, remove the extra columns
  if (current_num_columns > num_columns) {
    txt_data <- txt_data[, 1:num_columns]
  }
  
  # If the current number of columns is less than expected, add NA columns to match
  if (current_num_columns < num_columns) {
    for (i in (current_num_columns + 1):num_columns) {
      txt_data[, i] <- NA
    }
    names(txt_data) <- base_column_names
  }
  
  return(txt_data)
}



process_subject_folder <- function(
    subject_folder, combined_data, base_column_names, expected_num_columns) {
  memory_folders <- list.dirs(path = subject_folder, full.names = TRUE, recursive = FALSE)
  memory_folders <- memory_folders[grep("MEMORY", memory_folders)]
  
  for (memory_folder in memory_folders) {
    folder_parts <- unlist(strsplit(basename(memory_folder), " "))
    is_self <- folder_parts[2]
    is_surprise <- folder_parts[3]
    
    files <- list.files(memory_folder, full.names = TRUE)
    excel_file <- files[grepl(".xlsx", files) & !grepl("~\\$", basename(files))]
    txt_file <- files[grepl(".txt", files)]
    
    if (length(excel_file) > 1) {
      excel_file <- excel_file[!grepl("~\\$", basename(excel_file))]
    }
    
    participant_code <- extract_participant_code(excel_file[1])
    
    txt_data <- read.delim(txt_file[1], header = FALSE, sep = " ", stringsAsFactors = FALSE)
    txt_data <- standardize_txt_data(txt_data, expected_num_columns, base_column_names)
    names(txt_data) <- base_column_names
    
    txt_data$participant_code <- participant_code
    txt_data$is_self <- is_self
    txt_data$is_surprise <- is_surprise
    
    combined_data <- rbind(combined_data, txt_data)
  }
  return(combined_data)
}



# Define the expected number of columns in the .txt files without the extra column for STRANGER
expected_num_columns <- 10
base_txt_column_names <- c(
  "memory", "rt", "is_old_chosen", "is_old", "column5", "column6", 
  "column7", "column8", "column9", "column10")

# Set the path to the 'raw' directory containing the subject folders
raw_directory <- here::here("data", "raw")
setwd(raw_directory)

# Initialize the final combined dataframe
combined_data <- data.frame(matrix(ncol = expected_num_columns, nrow = 0))
colnames(combined_data) <- base_txt_column_names

# List all the subject folders
subject_folders <- list.dirs(full.names = TRUE, recursive = FALSE)

# Process each subject folder
for (subject_folder in subject_folders) {
  combined_data <- process_subject_folder(subject_folder, combined_data, base_txt_column_names, expected_num_columns)
}

combined_data$participant_code <- tolower(combined_data$participant_code)

length(unique(combined_data$participant_code))

# Write the combined dataframe to a CSV file
write.csv(
  combined_data, 
  here::here(
    "data", "prep", "memory", "memory_data.csv"
  ),
  row.names = FALSE
)

# eof ---



