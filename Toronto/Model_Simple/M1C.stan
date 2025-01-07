// M1_C
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
  real<lower=0> sigma_theta;
  vector[N] theta; // heterogeneous effects
}
model {
  y ~ poisson_log(log_E + beta0 + X * beta + theta); //Model with Intercept, Covariates, and Random Effects
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
  //sigma_theta ~ normal(0, 10);
 //sigma_theta ~ normal(0, 5);
 sigma_theta ~ cauchy(0, 1);
  theta ~ normal(0, sigma_theta);
}
generated quantities { 
  vector[N] mu = exp(log_E + beta0 + X * beta + theta);
  vector[N] RR; // New variable for RR
  real expbeta0 = exp(beta0);
  real expsigma_theta = exp(sigma_theta);
  vector[K] expbeta = exp(beta);
                     
  vector[N] log_lik; // store the log-likelihood for each data point.
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta0 + X[i] * beta + theta[i]);
    RR[i] = mu[i] / E[i]; // RR
   //log_lik[i] = poisson_lpmf(y[i] | mu[i]);
  }
}

