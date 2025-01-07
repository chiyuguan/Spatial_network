// bym_intercept_covariates.stan
data {
  int<lower=0> N;
  array[N] int<lower=0> y; 
  vector<lower=0>[N] E; 
  int<lower=1> K;  
  matrix[N, K] X;  
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real beta0;
  vector[K] beta;
}
model {
  y ~ poisson_log(log_E + beta0 + X * beta);
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
}
generated quantities {
  vector[N] mu = exp(log_E + beta0 + X * beta);
  vector[N] log_lik;
  vector[N] RR; // New variable for RR
  real expbeta0 = exp(beta0);
  vector[K] expbeta = exp(beta);
  
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta0 + X[i] * beta);
    RR[i] = mu[i] / E[i]; //Calculate RR
  }
}
