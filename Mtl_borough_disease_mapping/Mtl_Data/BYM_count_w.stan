// use for count weight
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
  matrix[N, N] W;  // Weight matrix (count weights)
}
transformed data {
  vector[N] log_E = log(E); 
  //For poission regression, an offset log(exposure) is typically used in situations where
  // the response variable is a count that is proportional to some measure of exposure.
}
parameters {
  real beta0; // intercept
  vector[K] beta;               // coefficients for predictors
  
  real<lower=0> tau_theta; // precision of heterogeneous effects
  real<lower=0> tau_phi; // precision of spatial effects
  
  vector[N] theta; // heterogeneous effects
  vector[N] phi; // spatial effects
}
transformed parameters {
  real<lower=0> sigma_theta = inv(sqrt(tau_theta)); // convert precision to sigma
  real<lower=0> sigma_phi = inv(sqrt(tau_phi)); // convert precision to sigma
}
model {
  y ~ poisson_log(log_E + beta0 + X * beta + phi * sigma_phi
                  + theta * sigma_theta); //Model with Intercept, Covariates, and Random Effects
  
  // NOTE:  no prior on phi_raw, it is used to construct phi
  // the following computes the prior on phi on the unit scale with sd = 1
  
 // Spatial prior with weight matrix W
  for (i in 1:N_edges) {
    target += -0.5 * W[node1[i], node2[i]] * square(phi[node1[i]] - phi[node2[i]]);
  }
  // soft sum-to-zero constraint on phi)
  sum(phi) ~ normal(0, 0.001 * N); // equivalent to mean(phi) ~ normal(0,0.001)
  
  beta0 ~ normal(0, 5);
  beta ~ normal(0, 5);
  theta ~ normal(0, 1);
  tau_theta ~ gamma(3.2761, 1.81); // Carlin WinBUGS priors
  tau_phi ~ gamma(1, 1); // Carlin WinBUGS priors
}
generated quantities { 
  vector[N] mu = exp(log_E + beta0 + X * beta + phi * sigma_phi
                     + theta * sigma_theta);
  vector[N] SMR; // New variable for SMR
  real expbeta0 = exp(beta0);
  real expsigma_theta = exp(sigma_theta);
  real expsigma_phi = exp(sigma_phi);
  vector[K] expbeta = exp(beta);
                     
  vector[N] log_lik; // store the log-likelihood for each data point.
  for (i in 1:N) {
    log_lik[i] = poisson_log_lpmf(y[i] | log_E[i] + beta0 + X[i] * beta + phi[i] * sigma_phi + theta[i] * sigma_theta);
    SMR[i] = mu[i] / E[i];
   //log_lik[i] = poisson_lpmf(y[i] | mu[i]);
  }
}
