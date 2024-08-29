// bym_intercept_only.stan
data {
  int<lower=0> N;
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N> node1; // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=N> node2; // and node1[i] < node2[i]
  
  array[N] int<lower=0> y; // count outcomes
  vector<lower=0>[N] E; // exposure
  int<lower=1> K;               // Number of covariates (K = 8)
  matrix[N, K] X;
}
transformed data {
  vector[N] log_E = log(E);
}
parameters {
  real beta0;
}
model {
  y ~ poisson_log(log_E + beta0);
  beta0 ~ normal(0, 5);
}
generated quantities {
  vector[N] mu = exp(log_E + beta0);
  vector[N] log_lik;
  vector[N] SMR; // New variable for SMR
  real expbeta0 = exp(beta0);
  
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta0);
    SMR[i] = mu[i] / y[i]; // Calculate SMR
  }
}


