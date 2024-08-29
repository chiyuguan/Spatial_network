# fits a spatial model using the Besag-York-Mollié (BYM) model in Stan
library(devtools)
if(!require(cmdstanr)){
  devtools::install_github("stan-dev/cmdstanr", dependencies=c("Depends", "Imports"))
}
#install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)  
#install_cmdstan()
# uses the cmdstanr package to fit a Bayesian spatial model to the Scotland disease mapping data.
options(digits=3)
# Sets the number of digits to display in the output to 3.


source("mungeCARdata4stan.R") 
# process the adjacency data
source("scotland_data.R")
# load the Scotland disease mapping data.

y = data$y;
x = 0.1 * data$x; # scaled
E = data$E;
# Extracts y (observed counts), x (socio-economic index scaled by 0.1), and E (expected counts) from the data list.

nbs = mungeCARdata4stan(data$adj, data$num);
N = nbs$N;
node1 = nbs$node1;
node2 = nbs$node2;
N_edges = nbs$N_edges;


# Combines all the necessary data into a list (data) to pass to the Stan model.
data = list(N=N,
            N_edges=N_edges,
            node1=node1,
            node2=node2,
            y=y,
            x=x,
            E=E);

# Compiles the Stan model defined in the "bym_predictor_plus_offset.stan" file using cmdstan_model.
bym_model = cmdstan_model("bym_predictor_plus_offset.stan");
bym_intercept_only_model = cmdstan_model("bym_intercept_only.stan");
bym_intercept_cov_model = cmdstan_model("bym_intercept_covariates.stan");

# Uses the sample method to fit the model with the provided data,
# using 4 parallel chains and suppressing intermediate output (refresh = 0).
output_dir <- tempdir() # Or specify a custom directory
bym_scot_stanfit = bym_model$sample(# runs the Stan sampling algorithm to estimate the posterior distribution of the model parameters.
         data = data,
         parallel_chains = 4,
         refresh=0,
         output_dir = output_dir); #default num_iterations=1000
         
bym_scot_stanfit$summary(variables = c("lp__", "beta0", "beta1",
                                       "sigma_phi", "tau_phi",
                                       "sigma_theta", "tau_theta",
                                       "mu[5]","phi[5]","theta[5]"));


# Provides summary statistics including
# the mean, standard deviation, and 95% credible intervals (0.025, 0.5, 0.975 quantiles).
bym_scot_stanfit$summary(variables = c("lp__", "beta0", "beta1",
                                       "sigma_phi", "tau_phi",
                                       "sigma_theta", "tau_theta",
                                       "mu[5]","phi[5]","theta[5]"),
                         ~quantile(.x, probs = c(0.025, 0.5, 0.975)));


### 6.20 to do:

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## Diagnostic???!! Traceplot
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
#install.packages("bayesplot")
library(bayesplot)

# Extract draws
draws <- bym_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("beta0", "beta1", "sigma_phi", "tau_phi"))

# Generate rank plots for selected parameters
mcmc_rank_hist(draws_array, pars = c("beta0", "beta1", "sigma_phi", "tau_phi"))

# Density plots for selected parameters
mcmc_dens(draws_array, pars = c("beta0", "beta1", "sigma_phi", "tau_phi"))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## how many mcmc iterations per run??? str(bym_scot_stanfit)

# Extract the number of iterations per chain
metadata <- bym_scot_stanfit$metadata()
print(metadata$iter_sampling)
print(metadata$iter_warmup)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

## warmup (burinin _ = 5000, totoal run = 10000

output_dir <- tempdir() # Or specify a custom directory
bym_scot_stanfit = bym_model$sample(
  data = data,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

bym_scot_stanfit$summary(variables = c("lp__", "beta0", "beta1",
                                       "sigma_phi", "tau_phi",
                                       "sigma_theta", "tau_theta",
                                       "mu[5]","phi[5]","theta[5]"));


# Provides summary statistics including
# the mean, standard deviation, and 95% credible intervals (0.025, 0.5, 0.975 quantiles).
bym_scot_stanfit$summary(variables = c("lp__", "beta0", "beta1",
                                       "sigma_phi", "tau_phi",
                                       "sigma_theta", "tau_theta",
                                       "mu[5]","phi[5]","theta[5]"),
                         ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
output_dir <- tempdir() # Or specify a custom directory
bym_intercept_only_model_scot_stanfit = bym_intercept_only_model$sample(
  data = data,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

bym_intercept_only_model_scot_stanfit$summary(variables = c("lp__", "beta0"));

bym_intercept_only_model_scot_stanfit$summary(variables = c("lp__", "beta0"),
                         ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

waic_result <- waic(as.matrix(bym_intercept_only_model_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_intercept_only_model_scot_stanfit$draws("log_lik")))
print(loo_result)
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
X = matrix(0.1 * data$x, ncol = 1)  # Ensure X is a matrix with one column
data2 = list(N=N,
            N_edges=N_edges,
            node1=node1,
            node2=node2,
            y=y,
            E=E,
            K = 1,
            X=X);
output_dir <- tempdir() # Or specify a custom directory
bym_intercept_cov_model_scot_stanfit = bym_intercept_cov_model$sample(
  data = data2,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

bym_intercept_cov_model_scot_stanfit$summary(variables = c("lp__", "beta0", "beta"));

bym_intercept_cov_model_scot_stanfit$summary(variables = c("lp__", "beta0", "beta"),
                                              ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

waic_result <- waic(as.matrix(bym_intercept_cov_model_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_intercept_cov_model_scot_stanfit$draws("log_lik")))
print(loo_result)

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
## read how to compute WAIC and LOOCV in stan output

library(loo)
library(cmdstanr)
# Extract log-likelihood values
log_lik <- bym_scot_stanfit$draws("log_lik")

# Convert draws to array and then to matrix
log_lik_matrix <- as.matrix(log_lik)

# Compute WAIC
waic_result <- waic(log_lik_matrix)
print(waic_result)

# Compute LOO-CV
loo_result <- loo(log_lik_matrix)
print(loo_result) #SE is NA, due to "high Pareto k values"??
#elpd_loo (Expected Log Predictive Density): the average log probability of the left-out data under the model.
#p_loo is an estimate of the effective number of parameters in the model. It's a measure of model complexity.
#

# Examine Pareto k diagnostic values
pareto_k <- pareto_k_table(loo_result)
print(pareto_k)

# Extract Pareto k values
 pareto_k_values <- loo_result$diagnostics$pareto_k
 print(pareto_k_values)

# # Identify observations with high Pareto k values
# high_pareto_k <- which(pareto_k_values > 0.7)
 
 # Extract posterior draws (needed for loo_moment_match)
 posterior_draws <- as_draws_array(bym_scot_stanfit$draws())
 
 # Use loo_moment_match if there are problematic observations (k > 0.7) ??? Not work
 if (length(high_pareto_k) > 0) {
   loo_result_mm <- loo_moment_match(loo=loo_result, post_draws = posterior_draws)
   print(loo_result_mm)
 } else {
   print("No problematic observations found.")
 }
 
