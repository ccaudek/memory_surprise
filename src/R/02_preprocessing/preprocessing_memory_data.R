suppressPackageStartupMessages({
  library("tidyverse")
  library("here")
  library("lme4")
  library("mice")
})


memory_df <- rio::import(
  here::here(
    "data", "prep", "memory", "memory_data.csv"
  )
)

memory_df <- memory_df |> 
  dplyr::rename("is_correct" = "column8")

memory_df$is_correct <- ifelse(
  memory_df$is_old_chosen == 3, NA, memory_df$is_correct)

memory_df$rt <- memory_df$rt / 1000

memory_df$is_self <- factor(memory_df$is_self)
memory_df$is_surprise <- factor(memory_df$is_surprise)

# S/C (surprise/control) E/O (self/other)

memory_df <- memory_df %>%
  mutate(condition = case_when(
    is_surprise == "SURPRISE" & is_self == "SELF" ~ "SE",
    is_surprise == "SURPRISE" & is_self == "STRANGER" ~ "SO",
    is_surprise == "NOSURPRISE" & is_self == "SELF" ~ "CE",
    is_surprise == "NOSURPRISE" & is_self == "STRANGER" ~ "CO",
    TRUE ~ NA_character_  # This line handles any cases that don't match the above conditions
  ))

memory_df$condition <- factor(memory_df$condition)

memory_df$subj_idx <- as.integer(factor(memory_df$participant_code))
memory_df$response <- memory_df$is_correct

df <- memory_df |> 
  dplyr::select(subj_idx, response, participant_code, is_self, is_surprise, 
                condition, rt)

data.init = df
for (i in 1:ncol(data.init)) data.init[, i][is.na(data.init[, i])] = 1

imp <- mice(
  df, 
  method = "norm.predict", 
  m = 1, 
  data.init = data.init, 
  seed = 111
)

df1 <- complete(imp)

df1 |> 
  group_by(condition) |> 
  summarize(
    mrt = median(rt),
    acc = mean(response)
  )

# Sorting df1 by subj_idx and then by condition
df1_sorted <- df1 %>% 
  arrange(subj_idx, condition)

rio::export(
  df1_sorted,
  here::here(
    "data", "prep", "memory", "memory_surprise_input_for_hddmrl.csv"
  )
)


# eof ---





fm <- glmer(
  is_correct ~ is_self + (1 | participant_code) +
    (1 | column9) + (1 | column10), 
  data = memory_df, family = binomial(link = "logit"))

summary(fm)



m1 <- brm(
  is_correct ~ is_surprise + (is_surprise | participant_code), 
  data = memory_df, 
  family = bernoulli(),
  backend = "cmdstan"
)
summary(m1)

marginal_effects(m1, "is_self")



m2 <- brm(
  RT ~ is_self + (is_self | participant_code), 
  data = memory_df, 
  family = student(),
  backend = "cmdstan"
)
pp_check(m2)
summary(m2)



memory_df$say_old <- ifelse(
  memory_df$is_old_chosen == 1, 1, 
  ifelse(memory_df$is_old_chosen == 2, 0, NA)
)

memory_df$is_old_chosen <- NULL
memory_df$is_old <- NULL

memory_df$is_correct <- ifelse(
  is.na(memory_df$say_old), NA, memory_df$is_correct)

results <- memory_df %>%
  group_by(participant_code) %>%
  summarise(
    Hits = sum(say_old == 1 & is_correct == 1) / sum(is_old == 1), # True positives / all actual old items
    False_Alarms = sum(say_old == 1 & is_correct == 0) / sum(is_old == 0) # False positives / all actual new items
  ) %>%
  mutate(
    # Adjust hit and false alarm rates to avoid 0 and 1
    Hits_adj = pmin(pmax(Hits, 0.5/sum(is_old == 1)), 1 - 0.5/sum(is_old == 1)),
    False_Alarms_adj = pmin(pmax(False_Alarms, 0.5/sum(is_old == 0)), 1 - 0.5/sum(is_old == 0)),
    # Compute d-prime
    D_prime = qnorm(Hits_adj) - qnorm(False_Alarms_adj)
  )

results



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

