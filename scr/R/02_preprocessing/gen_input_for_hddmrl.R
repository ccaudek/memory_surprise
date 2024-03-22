
suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
})

surprise_self_df <- readRDS(
  here::here(
    "data", "prep", "prl", "surprise_self_raw_prl_data.RDS"
  )
)
control_self_df <- readRDS(
  here::here(
    "data", "prep", "prl", "control_self_raw_prl_data.RDS"
  )
)

surprise_stranger_df <- readRDS(
  here::here(
    "data", "prep", "prl", "surprise_stranger_raw_prl_data.RDS"
  )
)
control_stranger_df <- readRDS(
  here::here(
    "data", "prep", "prl", "control_stranger_raw_prl_data.RDS"
  )
)

d1 <- rbind(surprise_self_df, control_self_df)


# I want to number each subject in the data.frame so that subjects are 
# ordered sequentially, according to the order they appear in the data frame. 
# https://community.rstudio.com/t/why-does-group-indices-use-alphabetical-ordering/5452
# As suggested in the above link, I wrap group_indices in another function:
grpid = function(x) match(x, unique(x))
# then
d2 <- d1 %>% 
  mutate(subj_idx = group_indices(., user_id) %>% grpid)
# In this manner, the variable subj_idx assigns an integer to each subject;
# this integer is ordered according to the sequence in which the subjects are 
# present in the data.frame.
# table(d3$subj_idx)
# unique(d3$subj_idx)

d2$response <- d2$chosen_stimulus - 1
d2$rt / 1000
d2$split_by <- ifelse(
  d2$is_surprise == "surprise", 1, 0
)
d2$feedback <- ifelse(d2$feedback == 1, 1, 0)
d2$q_init <- 0.5

d2$momentary_happiness <- ifelse(
  d2$momentary_happiness == -1, NA, d2$momentary_happiness
)

d3 <- d2 |> 
  dplyr::select(
    subj_idx, user_id, response, is_surprise, is_self, rt, trial, split_by,
    feedback, q_init
  )

mydat <- d3 |> 
  dplyr::arrange(subj_idx, trial, is_surprise, is_self, split_by)

rio::export(
  mydat,
  here::here(
    "data", "prep", "prl", "prl_input_for_hddmrl.csv"
  )
)

# eof ----

d2 |> 
  group_by(is_surprise) |> 
  summarise(
    h = mean(momentary_happiness, na.rm = TRUE)
    )

d2 |> 
  group_by(is_surprise) |> 
  summarise(
    fdbk = mean(feedback, na.rm = TRUE)
  )

d2$inter_trial_delay <- d2$inter_trial_delay / 1000
d2$rt <- d2$rt / 1000
d2$t <- (d2$trial - 80) / 80

fm <- glmer(
  feedback ~ is_surprise + epoch + 
    ( 1 | user_id) + 
    (1 | orange_img_number) + (1 | white_img_number),
  family = binomial(link = "logit"),
  data = d2
)
summary(fm)

fm <- lmer(
  log(rt) ~ is_surprise + trial + 
    ( 1 | user_id) + 
    (1 | orange_img_number) + (1 | white_img_number),
  data = d2
)
summary(fm)
plot(fm)

fm <- glmer(rt ~ is_surprise + t + 
              (1 | user_id) + 
              (1 | orange_img_number) + (1 | white_img_number),
            data = d2, 
            family = Gamma(link = "log"))
summary(fm)

mod <- brm(
  rt ~ is_surprise + t + 
    ( is_surprise | user_id) + 
    (1 | orange_img_number) + (1 | white_img_number),
  data = d2,
  family = exgaussian(),
  backend = "cmdstanr"
)
pp_check(mod)
summary(mod)

