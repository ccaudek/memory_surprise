suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("lme4")
})


memory_df <- rio::import(
  here::here(
    "data", "prep", "memory", "memory_data.csv"
  )
)

memory_df <- memory_df |> 
  dplyr::rename("is_correct" = "column8")

memory_df$is_correct <- ifelse(memory_df$is_old_chosen == 3, NA, memory_df$is_correct)

memory_df |> 
  group_by(is_self, is_surprise) |> 
  summarize(
    acc = mean(is_correct, na.rm= TRUE),
    mrt = mean((rt/1000), trim=0.1)
  )

hist(memory_df$rt)

# d1$user_id <- d1$user_id |> 
#   dplyr::recode(
#    "gi_gh_1958_11_15_917_m" = "gi_gh_1968_11_15_317_m"
#   )


# is_correct : 1 = correct
#              2 = error
#              3 = no response

# image_chosen : 1 = the subject has chosen the "old" image
#                2 = the subject has chosen the "new" image
#                3 = no response





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
  is_correct ~ is_surprise + (1 | participant_code),
    # (1 | old_img_number) + (1 | new_img_number), 
  family = binomial(), 
  data = memory_df
  )
summary(fm)

fm <- lmer(
  rt ~ is_surprise + is_self + (1 | participant_code),
  data = memory_df
)
summary(fm)


hist(log(d1$rt))

