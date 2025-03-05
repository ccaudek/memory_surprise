data {
  int<lower=1> N_subj;      // Number of subjects
  int<lower=1> N_trials;    // Number of trials per subject
  int<lower=1> N_cond;      // Number of conditions
  
  // Data for each subject
  array[N_subj, N_trials] int choice;   // Choices (1 or 2)
  array[N_subj, N_trials] int outcome;  // Outcomes (-1 or 1)
  array[N_subj] int cond_idx;           // Condition index for each subject
  
  // Hyperprior parameters (if desired as data inputs)
  real<lower=0> alpha_pos_mu_prior;
  real<lower=0> alpha_neg_mu_prior;
  real<lower=0> beta_mu_prior;
  real<lower=0> decay_mu_prior;
}

parameters {
  //---------------------------------------------------------------------------
  // Group-level parameters for each condition
  //---------------------------------------------------------------------------
  // <lower=0> constraints ensure these parameters never go negative
  vector<lower=0>[N_cond] alpha_pos_mu;  // Learning rate for positive outcomes
  vector<lower=0>[N_cond] alpha_neg_mu;  // Learning rate for negative outcomes
  vector<lower=0>[N_cond] beta_mu;       // Inverse temp (softmax) param
  vector<lower=0>[N_cond] decay_mu;      // Decay parameter
  
  // Between-subject variability for each condition
  vector<lower=0>[N_cond] alpha_pos_sigma;
  vector<lower=0>[N_cond] alpha_neg_sigma;
  vector<lower=0>[N_cond] beta_sigma;
  vector<lower=0>[N_cond] decay_sigma;
  
  //---------------------------------------------------------------------------
  // Individual subject parameters
  //---------------------------------------------------------------------------
  // alpha_pos[s, c], alpha_neg[s, c], etc. are drawn from group-level dists
  matrix<lower=0>[N_subj, N_cond] alpha_pos;
  matrix<lower=0>[N_subj, N_cond] alpha_neg;
  matrix<lower=0>[N_subj, N_cond] beta;
  matrix<lower=0>[N_subj, N_cond] decay;
}

model {
  //---------------------------------------------------------------------------
  // Priors for group-level parameters
  //---------------------------------------------------------------------------
  // Because alpha_pos_mu, etc., are <lower=0>, no need for T[0, ]
  alpha_pos_mu ~ normal(0.1, 0.05);
  alpha_neg_mu ~ normal(0.1, 0.05);
  beta_mu      ~ normal(2,   1);
  decay_mu     ~ normal(0.1, 0.05);
  
  // Hyperpriors for the between-subject variability
  alpha_pos_sigma ~ cauchy(0, 0.5);
  alpha_neg_sigma ~ cauchy(0, 0.5);
  beta_sigma      ~ cauchy(0, 0.5);
  decay_sigma     ~ cauchy(0, 0.5);
  
  //---------------------------------------------------------------------------
  // Subject-level parameter distributions (hierarchical)
  //---------------------------------------------------------------------------
  for (c in 1:N_cond) {
    alpha_pos[, c] ~ normal(alpha_pos_mu[c], alpha_pos_sigma[c]);
    alpha_neg[, c] ~ normal(alpha_neg_mu[c], alpha_neg_sigma[c]);
    beta[, c]      ~ normal(beta_mu[c],      beta_sigma[c]);
    decay[, c]     ~ normal(decay_mu[c],     decay_sigma[c]);
  }
  
  //---------------------------------------------------------------------------
  // Likelihood: Rescorla-Wagner with decay and asymmetric learning
  //---------------------------------------------------------------------------
  for (s in 1:N_subj) {
    // Initialize Q-values for each subject
    real Q1 = 0;  // value for option 1
    real Q2 = 0;  // value for option 2
    
    for (t in 1:N_trials) {
      // Softmax probability of choosing option 1
      real diff_Q = Q1 - Q2;
      real prob_choice1 = inv_logit(beta[s, cond_idx[s]] * diff_Q);
      
      // Bernoulli likelihood: 'choice[s, t] == 1' with probability prob_choice1
      target += bernoulli_lpmf(choice[s, t] == 1 | prob_choice1);
      
      // Q-value update depending on whether choice was 1 or 2
      if (choice[s, t] == 1) {
        // If outcome > 0, use alpha_pos; else alpha_neg
        real alpha_used = (outcome[s, t] > 0)
                           ? alpha_pos[s, cond_idx[s]]
                           : alpha_neg[s, cond_idx[s]];
        Q1 += alpha_used * (outcome[s, t] - Q1);
        Q2 *= (1 - decay[s, cond_idx[s]]);
        
      } else {
        // Chose option 2
        real alpha_used = (outcome[s, t] > 0)
                           ? alpha_pos[s, cond_idx[s]]
                           : alpha_neg[s, cond_idx[s]];
        Q2 += alpha_used * (outcome[s, t] - Q2);
        Q1 *= (1 - decay[s, cond_idx[s]]);
      }
    }
  }
}

generated quantities {
  //---------------------------------------------------------------------------
  // Derived quantities: means per condition
  //---------------------------------------------------------------------------
  vector[N_cond] mean_alpha_pos;
  vector[N_cond] mean_alpha_neg;
  vector[N_cond] mean_beta;
  vector[N_cond] mean_decay;
  
  for (c in 1:N_cond) {
    mean_alpha_pos[c] = alpha_pos_mu[c];
    mean_alpha_neg[c] = alpha_neg_mu[c];
    mean_beta[c]      = beta_mu[c];
    mean_decay[c]     = decay_mu[c];
  }
}
