# Pre-processing PRL raw data

library(tidyverse)
library(tidyr)
library(here)
library(rio)
library(lme4)
library(brms)


raw_df <- rio::import(
  here::here("data", "prep", "prl", "interim", "combined_prl_raw_data.csv")
)

# Sanitize names.Replace all "." with "_" in the column names
colnames(raw_df) <- gsub("\\.", "_", colnames(raw_df))

# Convert all column names to lowercase
colnames(raw_df) <- tolower(colnames(raw_df))

# Check the new column names
glimpse(raw_df)

# Custom function to fill NAs, skipping the first three entries for each subject
fill_mood_values <- function(df) {
  mood_values <- df$mood_slider_value
  for (i in 1:length(mood_values)) {
    if (is.na(mood_values[i]) && i > 3) {
      mood_values[i] <- mood_values[i-1]
    }
  }
  df$mood_slider_value <- mood_values
  return(df)
}

# Apply the function to each subject's data
raw_df <- raw_df %>%
  group_by(subject_code) %>%
  group_modify(~ fill_mood_values(.)) %>%
  ungroup()

# Check the results for the mood_slider_value column
print(raw_df$mood_slider_value)

# Custom function to fill the remaining NAs with the first available value 
# for each subject
fill_with_first_available <- function(df) {
  # Find the first non-NA mood_slider_value for the subject
  first_non_na <- df$mood_slider_value[which(!is.na(df$mood_slider_value))[1]]
  
  # If there is a non-NA value, fill the NAs with it
  if (!is.na(first_non_na)) {
    df$mood_slider_value[is.na(df$mood_slider_value)] <- first_non_na
  }
  
  return(df)
}

# Apply the function to each subject's data
raw_df <- raw_df %>%
  group_by(subject_code) %>%
  group_modify(~ fill_with_first_available(.)) %>%
  ungroup()

# Check the results for the mood_slider_value column
print(raw_df$mood_slider_value)

# Custom function to fill empty strings with the previous non-empty value
fill_video_names <- function(df) {
  video_names <- df$video_file_name
  for (i in 2:length(video_names)) {
    if (video_names[i] == "" && i > 1) {
      video_names[i] <- video_names[i-1]
    }
  }
  df$video_file_name <- video_names
  return(df)
}

# Apply the function to each subject's data
raw_df <- raw_df %>%
  group_by(subject_code) %>%
  group_modify(~ fill_video_names(.)) %>%
  ungroup()

# Check the results for the video_file_name column
print(raw_df$video_file_name[1:30])

raw_df |> 
  group_by(video_type) |> 
  summarize(
    acc = mean(feedback_received),
    mrt = mean(reaction_time, trim= 0.1)
  )

# Wrangle chosen_image
raw_df <- raw_df %>%
  mutate(
    # Remove ".png" and "_old" from the chosen_image
    chosen_image = gsub("\\.png", "", chosen_image),
    chosen_image = gsub("_old", "", chosen_image),
    # Extract the number between "_" and ".png" and save it in img_number
    img_number = as.integer(gsub(".*_(\\d+)$", "\\1", chosen_image)),
    # Keep only "white" or "orange" in the chosen_image column, "_old" removed above
    chosen_image = gsub("_(\\d+)$", "", chosen_image)
  )

raw_df <- raw_df %>%
  mutate(most_rewarded_stimulus_in_epoch = tolower(most_rewarded_stimulus_in_epoch))

raw_df$subj_name <- str_replace(raw_df$subject_code, "((^[^_]+_[^_]+)).*", "\\1")

# Create the new columns
raw_df <- raw_df %>%
  mutate(
    is_surprise = case_when(
      condition %in% c("PRL_SELF_SURPRISE", "PRL_STRANGER_SURPRISE") ~ "surprise",
      TRUE ~ "control"
    ),
    is_self = case_when(
      grepl("^PRL_SELF", condition) ~ "self",
      grepl("^PRL_STRANGER", condition) ~ "stranger",
      TRUE ~ NA_character_ # This should ideally never happen but is good practice to handle unexpected cases
    )
  )

# Generate the cond column
raw_df <- raw_df %>%
  mutate(cond = paste(is_surprise, is_self, sep = "_"))


# nosurprise_df <- raw_df |> 
#   dplyr::filter(video_type == "not_surprising_videos")

# This is used for the behavioral indices
# df <- data.frame(
#   subjID = as.numeric(factor(raw_df$subject_code)),
#   choice = ifelse(raw_df$chosen_image == "orange", 1, 2),
#   outcome = ifelse(raw_df$feedback_received == TRUE, 1, -1),
#   condition = raw_df$video_type,
#   rt = raw_df$reaction_time,
#   trial = raw_df$trial_number
# )


# This is used for hddmrl
df <- data.frame(
  subjID = as.numeric(factor(raw_df$subj_name)),
  choice = ifelse(raw_df$chosen_image == "orange", 1, 2),
  outcome = ifelse(raw_df$feedback_received == TRUE, 1, -1),
  cond = raw_df$cond,
  is_self = raw_df$is_self,
  is_surprise = raw_df$is_surprise,
  rt = raw_df$reaction_time,
  trial = raw_df$trial_number
)

df <- df %>%
  mutate(split_by = case_when(
    cond == "control_self" ~ 1,
    cond == "control_stranger" ~ 2,
    cond == "surprise_self" ~ 3,
    cond == "surprise_stranger" ~ 4,
    TRUE ~ NA_integer_ # This handles any unexpected cases
  ))


df1 <- df |> 
  mutate(
    response = ifelse(choice == 1, 1, 0),
    feedback = ifelse(outcome == 1, 1, 0),
    q_init = 0.5
  ) |> 
  dplyr::select(-c(choice, outcome)) |> 
  dplyr::rename(subj_idx = subjID)

# cond has modalities:
# "control_self", "control_stranger", "surprise_self", "surprise_stranger"
sorted_new_df <- df1 %>%
  arrange(subj_idx, cond, trial)

sorted_new_df$subj_idx <- sorted_new_df$subj_idx + 11

rio::export(
  sorted_new_df,
  here::here(
    "data", "prep", "prl", "prl_surprise_input_for_hddmrl.csv"
  )
)


raw_df |> 
  group_by(is_self, is_surprise) |> 
  summarize(
    acc = mean(feedback_received),
    rt = mean(reaction_time, trim = 0.1)
  )


# lavinia data -----------------------------------------------------------------

lavinia_data <- readRDS(
  here::here(
    "data", "prep", "prl_OLD", "prl_raw_data.RDS"
  )
)

length(unique(lavinia_data$user_id))
table(lavinia_data$is_self, lavinia_data$is_surprise)

lavinia_data <- lavinia_data |> 
  mutate(
    rt = rt / 1000
  )

lavinia_data$rt <- ifelse(
  lavinia_data$rt > 10, median(lavinia_data$rt), lavinia_data$rt)

hist(lavinia_data$rt)

lavinia_data <- lavinia_data %>%
  mutate(surprise = case_when(
    is_surprise == "surprise" ~ "surprise",
    is_surprise == "no_surprise" ~ "control",
    TRUE ~ "error" # This handles any unexpected cases
  ))

lavinia_data$is_surprise <- NULL
lavinia_data <- lavinia_data %>%
  dplyr::rename(
    is_surprise = surprise
  )

lavinia_data <- lavinia_data %>%
  mutate(cond = paste(is_surprise, is_self, sep = "_"))


lavinia_data <- lavinia_data |> 
  dplyr::rename(subj_idx = user_id)

lavinia_data <- lavinia_data |> 
  mutate(
    response = ifelse(chosen_stimulus == 1, 1, 0),
    feedback = ifelse(feedback == 1, 1, 0)
  )

lavinia_data |> 
  group_by(is_surprise, is_self) |> 
  summarize(
    acc = mean(feedback)
  )

lavinia_data$subj_idx <- as.integer(factor(lavinia_data$subj_idx))

lavinia_data <- lavinia_data %>%
  mutate(split_by = case_when(
    cond == "control_self" ~ 1,
    cond == "control_stranger" ~ 2,
    cond == "surprise_self" ~ 3,
    cond == "surprise_stranger" ~ 4,
    TRUE ~ NA_integer_ # This handles any unexpected cases
  ))

lavinia_data$q_init <- 0.5

sorted_df <- lavinia_data %>%
  arrange(subj_idx, cond, trial) |> 
  dplyr::select(
    subj_idx, cond, trial, is_surprise, is_self, response,
    feedback, rt, split_by, q_init)

foo <- rbind(sorted_df, sorted_new_df)

rio::export(
  sorted_df,
  here::here(
    "data", "prep", "prl", "prl_surprise_input_for_hddmrl.csv"
  )
)



# Behavioral indices -----------------------------------------------------------

calculate_indices <- function(data) {
  data %>%
    mutate(
      # Identify if the choice stayed the same as the previous trial
      stay = lead(choice) == choice,
      # Identify if the outcome was a win (1) or a loss (-1) in the previous trial
      prev_outcome = lag(outcome)
    ) %>%
    group_by(subjID, condition) %>%
    summarise(
      # Calculate win-stay: proportion of stays after a win
      win_stay = mean(stay & prev_outcome == 1, na.rm = TRUE),
      # Calculate lose-shift: proportion of shifts after a loss
      lose_shift = mean(!stay & prev_outcome == -1, na.rm = TRUE)
    )
}

# Apply the function to your data frame
indices_df <- calculate_indices(df)

indices_df |> 
  group_by(condition) |> 
  summarize(
    avg_win_stay = mean(win_stay),
    avg_lose_shift = mean(lose_shift)
  )

raw_df |> 
  group_by(video_type) |> 
  summarize(
    mood = mean(mood_slider_value, trim = 0.1)
  )

hist(raw_df$mood_slider_value)

fm <- lmer(mood_slider_value ~ video_type + (1 | subject_code), data = raw_df)
summary(fm)

m1 <- brm(
  mood_slider_value ~ video_type + (1 | subj_name), 
  family = student(),
  data = raw_df,
  backend = "cmdstanr"
)
pp_check(m1)
summary(m1)

marginal_effects(m1, "video_type")



m2 <- brm(
  mood_slider_value ~ video_type + 
    (video_type | subj_name) + (1 | img_number), 
  family = student(),
  data = raw_df,
  backend = "cmdstanr"
)
pp_check(m2) 
summary(m2)

marginal_effects(m1, "video_type")


