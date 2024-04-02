suppressPackageStartupMessages({
  library("readxl")
  library("readr")
  library("dplyr")
  library("here")
  library(brms)
  library(cmdstanr)
  library(lme4)
})


prl_raw_df <- readRDS(
  here::here(
    "data", "prep", "prl", "prl_raw_data.rds"
  )
)


fbk <- ifelse(prl_raw_df$feedback == 1, 1, 0)
fbk <- fbk[-length(fbk)]
prl_raw_df$new_fbk <- c(0, fbk)

prl_raw_df$momentary_happiness <- ifelse(
  prl_raw_df$momentary_happiness == -1, NA,
  prl_raw_df$momentary_happiness 
)

prl_raw_df$feedback <- ifelse(prl_raw_df$feedback == 2, 0, prl_raw_df$feedback)

fm1 <- glmer(
  feedback ~ is_surprise + is_self +
    (1 | user_id),
  family = binomial(),
  data = prl_raw_df
)
#                       Estimate Std. Error z value Pr(>|z|)   
# (Intercept)            0.33659    0.11187   3.009  0.00262 **
# is_surprisesurprise    0.04108    0.10428   0.394  0.69362   
# is_selfstranger       -0.19579    0.11437  -1.712  0.08692 . 


prl_raw_df |> 
  group_by(is_self, is_surprise) |> 
  summarize(
    acc = mean(feedback)
  )



prl_raw_df |> 
  group_by(new_fbk) |> 
  summarize(
    y = mean(momentary_happiness, na.rm = TRUE)
  )

m1 <- brm(
  momentary_happiness ~ is_surprise * is_self +
    (1 | user_id),
  family = categorical(),
  data = prl_raw_df,
  backend = "cmdstanr"
)
pp_check(m1)

summary(m1)


m2 <- brm(
  momentary_happiness ~ is_surprise +
    (1 | user_id),
  family = categorical(),
  data = prl_raw_df,
  backend = "cmdstanr"
)

