library("readxl")
library("readr")
library("dplyr")
library("here")

# This script requires that there is only ONE file in each "experiment_data"
# folder ! ! !


FOLDER_NAME <- "surprise_self"

# List all folders starting with 'prl_' (i.e., for all subjects)
folder_list <- list.dirs(
  path = paste0(here::here("data", "raw", "memory"), "/", FOLDER_NAME), 
  full.names = FALSE, 
  recursive = FALSE
)
folder_list <- folder_list[grep("^mem_", folder_list)]


# for testing purposes:
# folder_name <- folder_list[1]

# Function to process each prl_XXX folder
process_folder <- function(folder_name) {
  # Set your working directory to the one that contains the 'prl' folder
  mydir <- here::here("data", "raw", "memory", FOLDER_NAME)
  
  # Path to the data.xlsx file
  excel_file_path <- paste0(mydir, "/", folder_name, "/data.xlsx")
  # Read the excel file
  excel_data <- read_excel(excel_file_path)
  
  # Create the first string from the 'participant' column
  participant_str <- excel_data$participant
  
  # Create the second string by concatenating columns 2 to 8
  # Convert last column's 1 to 'f' and 2 to 'm'
  second_str <- apply(excel_data[, 2:8], 1, function(row) {
    row[length(row)] <- ifelse(row[length(row)] == 1, "f", "m")
    paste(row, collapse = "_")
  })
  
  # Path to the .txt file
  txt_file_path <- list.files(
    path = paste0(
      here::here("data", "raw", "memory"), "/", FOLDER_NAME, "/", 
      folder_name, "/experiment_data"),
    # pattern = "memory_task_.*\\.txt$", 
    full.names = TRUE
  )
  
  # Read the .txt file
  txt_data <- rio::import(txt_file_path)

  # Add the two strings as new columns
  txt_data$participant_str <- participant_str
  txt_data$second_str <- second_str

  # is_correct : 1 = correct
  #              2 = error
  #              3 = no response
  
  # image_chosen : 1 = the subject has chosen the "old" image
  #                2 = the subject has chosen the "new" image
  #                3 = no response
  
  columns_names <- c(
    "task", "rt", "is_correct", "image_chosen", "intertrial_delay",
    "x_coord_old_img", "x_coord_new_img", "temp1", "old_img_number",
    "new_img_number", "psytoolkit_code", "user_id"
  )
  
  colnames(txt_data) <- columns_names
  
  df <- txt_data |> 
    dplyr::select(!starts_with("temp"))
  
  return(df)
}


# Apply process_folder to each element of folder_list
list_of_dfs <- lapply(folder_list, process_folder)

# Combine all dataframes into one
combined_df <- do.call(rbind, list_of_dfs)

# saveRDS(
#   combined_df,
#   here::here(
#     "data", "prep", "memory", "raw_memory_data.RDS"
#   )
# )

saveRDS(
  combined_df,
  if (FOLDER_NAME == "surprise_self"){
    here::here(
      "data", "prep", "memory", "surprise_self_raw_prl_data.RDS"
    )
  } else if (FOLDER_NAME == "control_self") {
    here::here(
      "data", "prep", "memory", "control_self_raw_prl_data.RDS"
    )
  } else if (FOLDER_NAME == "surprise_stranger") {
    here::here(
      "data", "prep", "memory", "surprise_stranger_raw_prl_data.RDS"
    )
  } else if (FOLDER_NAME == "control_stranger") {
    here::here(
      "data", "prep", "memory", "control_stranger_raw_prl_data.RDS"
    )
  } else {
    stop("Invalid value of FOLDER_NAME")
  }
)


# eof ----
