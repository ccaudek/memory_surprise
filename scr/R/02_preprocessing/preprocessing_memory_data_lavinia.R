suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("lme4")
})


surprise_self_df <- readRDS(
  here::here(
    "data", "prep", "memory", "surprise_self_raw_prl_data.RDS"
  )
)
surprise_self_df$is_self <- "self"
surprise_self_df$is_surprise <- "surprise"


control_self_df <- readRDS(
  here::here(
    "data", "prep", "memory", "control_self_raw_prl_data.RDS"
  )
)
control_self_df$is_self <- "self"
control_self_df$is_surprise <- "control"

surprise_stranger_df <- readRDS(
  here::here(
    "data", "prep", "memory", "surprise_stranger_raw_prl_data.RDS"
  )
)
surprise_stranger_df$is_self <- "stranger"
surprise_stranger_df$is_surprise <- "surprise"

control_stranger_df <- readRDS(
  here::here(
    "data", "prep", "memory", "control_stranger_raw_prl_data.RDS"
  )
)
control_stranger_df$is_self <- "stranger"
control_stranger_df$is_surprise <- "control"

d1 <- rbind(surprise_self_df, control_self_df)

d1$user_id <- d1$user_id |> 
  dplyr::recode(
   "gi_gh_1958_11_15_917_m" = "gi_gh_1968_11_15_317_m"
  )


# is_correct : 1 = correct
#              2 = error
#              3 = no response

# image_chosen : 1 = the subject has chosen the "old" image
#                2 = the subject has chosen the "new" image
#                3 = no response


d1$is_correct <- ifelse(
  d1$is_correct == 3, NA, 
  ifelse(d1$is_correct == 1, 1, 0)
)

d1$image_chosen <- ifelse(
  d1$image_chosen == -1, NA, 
  ifelse(d1$image_chosen == 1, "old", "new")
)

d1$image_chosen <- factor(d1$image_chosen)

d2 <- d1[d1$user_id != "ch_be_1986_3_11_139_f", ]
d3 <- d2[d2$user_id != "ch_be_1986_3_11_139_m", ]

d3 |> 
  group_by(is_surprise) |> 
  summarize(
    acc = mean(is_correct, na.rm = TRUE),
    rt = mean(rt, trim=0.1, na.rm = TRUE),
    n = n_distinct(user_id)
  )
# is_surprise   acc    rt     n
# <chr>       <dbl> <dbl> <int>
# 1 control     0.784 1708.     2
# 2 surprise    0.861 1605.     4


fm <- glmer(
  is_correct ~ is_surprise + (1 | user_id),
    # (1 | old_img_number) + (1 | new_img_number), 
  family = binomial(), 
  data = d3
  )
summary(fm)

fm <- lmer(
  log(rt) ~ is_surprise + (1 | user_id) +
     (1 | new_img_number),
  data = d1
)
summary(fm)


hist(log(d1$rt))

