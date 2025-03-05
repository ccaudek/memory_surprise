
library(here)
library(cmdstanr)
library(dplyr)
library(tidyr)
library(posterior)
library(ggplot2)


df <- rio::import(
  here::here(
    "data", "prep", "prl", "prl_surprise_input_for_hddmrl.csv"
  )
)
unique(df$subj_idx) |> length()


df$feedback <- ifelse(df$feedback == 1, 1, -1)

# --- 1) Sort the data by subject and trial
df <- df %>% arrange(subj_idx, trial)



# --- 2) Ensure trial indices are correctly numbered for each subject
df <- df %>%
  group_by(subj_idx) %>%
  mutate(trial_index = row_number()) %>%
  ungroup()

# --- 3) Compute the number of subjects, trials, and conditions
N_subj <- length(unique(df$subj_idx))
N_trials <- max(df$trial_index)  # Use trial_index to ensure consistency
N_cond <- length(unique(df$cond))

# --- 4) Convert condition names into integer indices
cond_levels <- sort(unique(df$cond))  # Sort for consistency
df$cond_idx <- as.integer(factor(df$cond, levels = cond_levels))

# Print mapping from condition names to integer indices
for (i in seq_along(cond_levels)) {
  print(paste("Condizione:", cond_levels[i], "→ Indice:", i))
}

# --- 5) Initialize matrices with integer values (default to 0)
choice_matrix  <- matrix(0L, nrow = N_subj, ncol = N_trials)
outcome_matrix <- matrix(0L, nrow = N_subj, ncol = N_trials)
cond_idx_vector <- integer(N_subj)

# --- 6) Fill in the matrices subject by subject
subj_list <- unique(df$subj_idx)
for (s in seq_along(subj_list)) {
  this_subj <- subj_list[s]
  
  # Extract subject data
  subj_data <- df %>% filter(subj_idx == this_subj)
  
  # Ensure trials align correctly
  trial_idx <- subj_data$trial_index
  
  # Assign values (ensure response and feedback are integers)
  choice_matrix[s, trial_idx]  <- as.integer(subj_data$response)
  outcome_matrix[s, trial_idx] <- as.integer(subj_data$feedback)
  
  # Assign condition index for the subject (assuming one condition per subject)
  cond_idx_vector[s] <- subj_data$cond_idx[1]
}

# --- 7) Prepare the data list for Stan
stan_data <- list(
  N_subj         = N_subj,
  N_trials       = N_trials,
  N_cond         = N_cond,
  choice         = choice_matrix,
  outcome        = outcome_matrix,
  cond_idx       = cond_idx_vector,
  alpha_pos_mu_prior = 0.1,
  alpha_neg_mu_prior = 0.1,
  beta_mu_prior      = 2,
  decay_mu_prior     = 0.1
)

# --- 8) Check the structure before passing to Stan
str(stan_data)


# --- 1) Compile the model
mod <- cmdstan_model(
  here::here(
    "src", "R", "stan", "rw_01.stan"
    )
  )

# --- 2) Run MCMC

init_fun <- function(chain_id) {
  list(
    alpha_pos_mu    = rep(0.1, N_cond),
    alpha_neg_mu    = rep(0.1, N_cond),
    beta_mu         = rep(2.0, N_cond),
    decay_mu        = rep(0.1, N_cond),
    alpha_pos_sigma = rep(0.1, N_cond),
    alpha_neg_sigma = rep(0.1, N_cond),
    beta_sigma      = rep(0.1, N_cond),
    decay_sigma     = rep(0.1, N_cond),
    alpha_pos       = matrix(0.1, nrow = N_subj, ncol = N_cond),
    alpha_neg       = matrix(0.1, nrow = N_subj, ncol = N_cond),
    beta            = matrix(2.0, nrow = N_subj, ncol = N_cond),
    decay           = matrix(0.1, nrow = N_subj, ncol = N_cond)
  )
}

# Then pass `init = init_fun` to the sample call:
fit <- mod$sample(
  data = stan_data,
  seed = 123,
  chains = 4,
  parallel_chains = 4,
  iter_sampling = 1000,
  iter_warmup = 1000,
  init = init_fun
)

# [1] "Condizione: control_self → Indice: 1"
# [1] "Condizione: control_stranger → Indice: 2"
# [1] "Condizione: surprise_self → Indice: 3"
# [1] "Condizione: surprise_stranger → Indice: 4"

fit <- mod$variational(
  data = stan_data,
  seed = 42,
  init = init_fun,
  algorithm = "meanfield",    # or "fullrank"
  iter = 20000,               # often need more iterations for complex models
  grad_samples = 1,           # defaults often fine; adjust as needed
  elbo_samples = 100,
  output_samples = 1000,      # how many approximate posterior samples you want
  tol_rel_obj = 0.01          # stops when ELBO changes < 1% (tweak to be stricter)
)


# --- 3) Diagnose and inspect results
fit$cmdstan_diagnose()
summary_fit <- fit$summary()
print(summary_fit, n=20)      # Print first 20 rows

> cond_levels
# [1] "control_self"      "surprise_self"     "control_stranger"  "surprise_stranger"

# 1) Extract draws into an array of samples
draws_array <- fit$draws()

# 2) Convert draws to a data frame-like structure for convenient manipulation
draws_df <- as_draws_df(draws_array)

# Example: Probability alpha_pos_mu[2] > alpha_pos_mu[1]

# 1) Extract the vector of samples for each parameter
alpha_pos_mu_1 <- draws_df$`alpha_pos_mu[1]`
alpha_pos_mu_3 <- draws_df$`alpha_pos_mu[3]`
is_greater <- (alpha_pos_mu_3 > alpha_pos_mu_1)
prob_mu3_greater_than_mu1 <- mean(is_greater)
prob_mu3_greater_than_mu1


alpha_pos_mu_2 <- draws_df$`alpha_pos_mu[2]`
alpha_pos_mu_4 <- draws_df$`alpha_pos_mu[4]`
is_greater <- (alpha_pos_mu_2 > alpha_pos_mu_4)
prob_mu4_greater_than_mu2 <- mean(is_greater)
prob_mu4_greater_than_mu2

# 1) Extract the vector of samples for each parameter
alpha_neg_mu_1 <- draws_df$`alpha_neg_mu[1]`
alpha_neg_mu_3 <- draws_df$`alpha_neg_mu[3]`
is_greater <- (alpha_neg_mu_3 > alpha_neg_mu_1)
prob_mu3_greater_than_mu1 <- mean(is_greater)
prob_mu3_greater_than_mu1

alpha_neg_mu_2 <- draws_df$`alpha_neg_mu[2]`
alpha_neg_mu_4 <- draws_df$`alpha_neg_mu[4]`
is_greater <- (alpha_neg_mu_2 > alpha_neg_mu_4)
prob_mu4_greater_than_mu2 <- mean(is_greater)
prob_mu4_greater_than_mu2


## beta


# 1) Extract the vector of samples for each parameter
beta_mu_1 <- draws_df$`beta_mu[1]`
beta_mu_3 <- draws_df$`beta_mu[3]`
is_greater <- (beta_mu_3 > beta_mu_1)
prob_mu3_greater_than_mu1 <- mean(is_greater)
prob_mu3_greater_than_mu1

beta_mu_2 <- draws_df$`beta_mu[2]`
beta_mu_4 <- draws_df$`beta_mu[4]`
is_greater <- (beta_mu_2 > beta_mu_4)
prob_mu4_greater_than_mu2 <- mean(is_greater)
prob_mu4_greater_than_mu2



