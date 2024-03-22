suppressPackageStartupMessages({
  library("readxl")
  library("readr")
  library("dplyr")
  library("here")
})

# This script requires that there is only ONE file in each "experiment_data"
# folder ! ! !

FOLDER_NAME <- "surprise_stranger" # "surprise_self", "control_self"

# List all folders starting with 'prl_' (i.e., for all subjects)
folder_list <- list.dirs(
  path = paste0(here::here("data", "raw", "prl"), "/", FOLDER_NAME), 
  full.names = FALSE, 
  recursive = FALSE
)
folder_list <- folder_list[grep("^prl_", folder_list)]

# Function to process each prl_XXX folder
process_folder <- function(folder_name) {
  # Set your working directory to the one that contains the 'prl' folder
  mydir <- here::here("data", "raw", "prl", FOLDER_NAME)
  
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
      here::here("data", "raw", "prl"), "/", FOLDER_NAME, "/", 
      folder_name, "/experiment_data"),
    # pattern = "prl_*\\.txt$", 
    # pattern = "prl_*\\.txt$", 
    full.names = TRUE
  )
  
  # Read the .txt file
  txt_data <- rio::import(txt_file_path)

  # Add the two strings as new columns
  txt_data$participant_str <- participant_str
  txt_data$second_str <- second_str
  
  # momentary happiness (quanto ti senti felice in questo momento?)
  # -1 : no response
  # 1 : minimum
  # 5 : maximum
  
  # rt_happiness : RT in trials in which the question "quanto ti senti felice 
  # in questo momento?" has been presented. 0 otherwise.
  
  # feedback : 1 positive, 2 negative
  
  # chosen_stimulus : 1 orange background, 2 : white background
  
  # video_number : 0 means that no video has been shown in that trial
  
  columns_names <- c(
    "epoch", "temp1", "is_self", "is_surprise", "trial", "momentary_happiness", 
    "rt_happiness", "temp2", "most_rewarded_image", "rt", "feedback", 
    "chosen_stimulus", "video_number", "inter_trial_delay", "x_coord_orange", 
    "x_coord_white", "cumulative_total_reward", "orange_img_number", 
    "white_img_number", "temp3", "temp4", "temp5", "psytoolkit_code", "user_id"
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

saveRDS(
  combined_df,
  if (FOLDER_NAME == "surprise_self"){
    here::here(
      "data", "prep", "prl", "interim", "surprise_self_raw_prl_data.RDS"
    )
  } else if (FOLDER_NAME == "control_self") {
    here::here(
      "data", "prep", "prl",  "interim", "control_self_raw_prl_data.RDS"
    )
  } else {
    stop("Invalid value of FOLDER_NAME")
  }
)


# eof ----






fbk <- ifelse(combined_df$feedback == 1, 1, 0)
fbk <- fbk[-200]
combined_df$new_fbk <- c(0, fbk)

combined_df$momentary_happiness <- ifelse(
  combined_df$momentary_happiness == -1, NA,
  combined_df$momentary_happiness 
)

combined_df |> 
  group_by(new_fbk) |> 
  summarize(
    y = mean(momentary_happiness, na.rm = TRUE)
  )

