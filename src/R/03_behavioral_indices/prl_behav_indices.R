

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
