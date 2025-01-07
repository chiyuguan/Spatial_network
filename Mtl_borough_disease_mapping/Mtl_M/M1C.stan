// M1_C
data {
  int<lower=0> N;
  int<lower=0> N_edges;
  array[N_edges] int<lower=1, upper=N> node1; // node1[i] adjacent to node2[i]
  array[N_edges] int<lower=1, upper=N> node2; // and node1[i] < node2[i]
  
  array[N] int<lower=0> y; // count outcomes
  vector<lower=0>[N] E; // exposure(based on population size or another baseline measure.)
  // represents the amount of time, population at risk, or some other measure of the opportunity for an event (such as a disease) to occur.
  // goal is to model the rate of these events per unit of exposure.
  
  int<lower=1> K;               // Number of covariates (K = 8)
  matrix[N, K] X;               // Predictor matrix (33 x 8)
}
transformed data {
  vector[N] log_E = log(E); 
  //For poission regression, an offset log(exposure) is typically used in situations where
  // the response variable is a count that is proportional to some measure of exposure.
}
parameters {
  real beta0; // intercept
  vector[K] beta;               // coefficients for predictors
  real<lower=0> sigma_theta;
  vector[N] theta; // heterogeneous effects
}
model {
  y ~ poisson_log(log_E + beta0 + X * beta + theta * sigma_theta); //Model with Intercept, Covariates, and Random Effects
  
  // NOTE:  no prior on phi_raw, it is used to construct phi
  // the following computes the prior on phi on the unit scale with sd = 1
  
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
  //sigma_theta ~ normal(0, 10);
 //sigma_theta ~ normal(0, 5);
 sigma_theta ~ normal(0, 2);
  theta ~ normal(0, sigma_theta);
}
generated quantities { 
  vector[N] mu = exp(log_E + beta0 + X * beta + theta * sigma_theta);
  vector[N] SMR; // New variable for SMR
  real expbeta0 = exp(beta0);
  real expsigma_theta = exp(sigma_theta);
  vector[K] expbeta = exp(beta);
                     
  vector[N] log_lik; // store the log-likelihood for each data point.
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta0 + X[i] * beta + theta[i] * sigma_theta);
    SMR[i] = mu[i] / E[i];
   //log_lik[i] = poisson_lpmf(y[i] | mu[i]);
  }
}

