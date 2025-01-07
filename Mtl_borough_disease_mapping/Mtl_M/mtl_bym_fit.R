# fits a spatial model using the Besag-York-Mollié (BYM) model in Stan
library(devtools)
library(cmdstanr)  
#options(digits=3)
# Sets the number of digits to display in the output to 3.

source("mtl_data.R")
source("mtl_graph_data.R")

y = data$last_week_2021_new_cases_total;

x = data[, 5:11] %>% st_drop_geometry(); # 33*8 metrix for covariates
# need covariates mean centered and 1 sd scaled,
x <- scale(x, center = TRUE, scale = TRUE)
# Convert the scaled data back to a data frame
x <- as.data.frame(x)

x %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(number.cex = 0.5, tl.cex = 0.5) # Cor_plot

E = data$Expected_count_for_each_boro;

## Purpose: This function converts spatial adjacency information into a format suitable for Stan.
# Inputs:
#   nodes
#   edges
# Outputs: A list containing:
#   N: Number of regions.
#   N_edges: Number of edges in the adjacency graph.
#   node1: A vector containing the starting node of each edge.
#   node2:  A vector containing the ending node of each edge.

mungeCARdata4stan_mtl = function(adjacency_matrix) {
  N = nrow(adjacency_matrix);  # Number of regions
  diag(adjacency_matrix) <- 0 # ignore self loops
  # Find the edges in the adjacency matrix
  nedges <- which(adjacency_matrix != 0, arr.ind = TRUE)
  nedges <- nedges[nedges[, 1] < nedges[, 2], ]
  
  nedges <- data.frame(Row = rownames(adjacency_matrix)[nedges[, 1]],
                       Column = colnames(adjacency_matrix)[nedges[, 2]],
                       Value = adjacency_matrix[nedges])
  # Since the adjacency matrix is symmetric, we need to remove duplicate edges
  
  # Step 2: Replace all entries equal to 34 with 28
  
  N_edges = nrow(nedges);  #**** Total number of edges in the adjacency list
  
  node1 = nedges[, 1];  # Initialize node1 vector
  node2 = nedges[, 2];  # Initialize node2 vector
  return (list("N"=N, "N_edges"=N_edges, "node1"=node1, "node2"=node2));  # Return the processed adjacency information
}

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For traffic based
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
nbs = mungeCARdata4stan_mtl(adjacency_matrix);
N = nbs$N;
node1 = nbs$node1;
node2 = nbs$node2;
N_edges = nbs$N_edges;

# Combines all the necessary data into a list (data) to pass to the Stan model.
X = as.matrix(x)  # Ensure X is a matrix 
data = list(N=N,
            N_edges=N_edges,
            node1=node1,
            node2=node2,
            y=y,
            E=E,
            K = 7,
            X=X); # log(E) and the Poisson distribution require positive values, exclude L'Île-Dorval

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For traffic based
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# Compiles the Stan model defined in the "bym_predictor_plus_offset.stan" file using cmdstan_model.
#bym_model = cmdstan_model("bym_predictor_plus_offset.stan");
# Compile the Stan model
# Load the rstan package
library(rstan)
library(loo)
library(posterior)

#bym_model <- cmdstan_model("bym_predictor_plus_offset.stan");
bym_intercept_only_model = cmdstan_model("bym_intercept_only.stan");
bym_intercept_cov_model = cmdstan_model("bym_intercept_covariates.stan");
bym_intercept_cov_random_effect_model = cmdstan_model("Cov_plus_theta.stan");
#bym_intercept_cov_random_effect_model = cmdstan_model("bym_predictor_plus_offset.stan");

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###

## warmup (burinin _ = 5000, totoal run = 10000
# output_dir <- tempdir() # Or specify a custom directory
# bym_scot_stanfit = bym_model$sample(
#   data = data,
#   parallel_chains = 4,
#   refresh=0,
#   output_dir = output_dir, # ); default num_iterations=1000
#   iter_sampling = 5000,  # Number of post-warmup iterations per chain
#   iter_warmup = 5000);   # Number of warmup iterations per chain
# 
# bym_scot_stanfit$summary(variables = c("lp__", "beta0", "beta",
#                                        "sigma_phi", "tau_phi",
#                                        "sigma_theta", "tau_theta",
#                                        "mu[5]","phi[5]","theta[5]"));
# 
# 
# # Provides summary statistics including
# # the mean, standard deviation, and 95% credible intervals (0.025, 0.5, 0.975 quantiles).
# bym_scot_stanfit$summary(variables = c("lp__", "beta0", "beta",
#                                        "sigma_phi", "tau_phi",
#                                        "sigma_theta", "tau_theta",
#                                        "mu[5]","phi[5]","theta[5]"),
#                          ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M0: intercept only
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
output_dir <- tempdir() # Or specify a custom directory
bym_intercept_only_model_scot_stanfit = bym_intercept_only_model$sample(
  data = data,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

bym_intercept_only_model_scot_stanfit$summary(variables = c("lp__", "beta0","SMR[25]","expbeta0"));

bym_intercept_only_model_scot_stanfit$summary(variables = c("lp__", "beta0","SMR[25]","expbeta0"),
                                              ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

## For mu
bym_intercept_only_model_scot_stanfit$summary(variables = c("mu"));
bym_intercept_only_model_scot_stanfit$summary(variables = c("mu"),
                                             ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

summary_mu <- bym_intercept_only_model_scot_stanfit$summary(variables = c("mu"), ~quantile(.x, probs = c(0.025, 0.5, 0.975)))
mu_values <- summary_mu$`50%` # use the mean of mu

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "Scatter Plot of y_i vs mu_i", pch = 19, col = "blue")

library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym_intercept_only_model_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:33,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
waic_result <- waic(as.matrix(bym_intercept_only_model_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_intercept_only_model_scot_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots
library(bayesplot)

# Extract draws
draws <- bym_intercept_only_model_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("beta0"))

# Generate rank plots for selected parameters
mcmc_rank_hist(draws_array, pars = c("beta0"))

# Density plots for selected parameters
mcmc_dens(draws_array, pars = c("beta0"))
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M1: intercept_Cov
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
output_dir <- tempdir() # Or specify a custom directory
bym_intercept_cov_model_scot_stanfit = bym_intercept_cov_model$sample(
  data = data,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

# Extract the summary
summary_df <- as.data.frame(bym_intercept_cov_model_scot_stanfit$summary(variables = c("lp__", "beta0", "beta")))

# Covariate names
covariate_names <- c("i_visMin", "i_educ_noHS_25_64", "i_LICO", "i_rec_imm", "i_young", "i_old", "i_tenant")

# Create a data frame with parameter names and covariate names
param_map <- data.frame(
  parameter = paste0("beta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

# Replace parameter names with covariate names in the summary
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

summary_df <- as.data.frame(bym_intercept_cov_model_scot_stanfit$summary(variables = c("lp__", "beta0", "beta"),
                                                                         ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

param_map2 <- data.frame(
  parameter = paste0("expbeta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym_intercept_cov_model_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym_intercept_cov_model_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta"), 
                                                                         ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)


## For mu
bym_intercept_cov_model_scot_stanfit$summary(variables = c("mu"));
bym_intercept_cov_model_scot_stanfit$summary(variables = c("mu"),
                                             ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym_intercept_cov_model_scot_stanfit$summary(variables = c("mu"), ~quantile(.x, probs = c(0.025, 0.5, 0.975)))
mu_values <- summary_mu$`50%` # use the mean of mu

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "Scatter Plot of y_i vs mu_i", pch = 19, col = "blue")

library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym_intercept_cov_model_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:33,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
waic_result <- waic(as.matrix(bym_intercept_cov_model_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_intercept_cov_model_scot_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots
library(bayesplot)

# Extract draws
draws <- bym_intercept_cov_model_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

beta_length <- 7
beta_names <- paste0("beta[", 1:beta_length, "]")

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("beta0", beta_names))

# Generate rank plots for selected parameters
mcmc_rank_hist(draws_array, pars = c("beta0", beta_names))

# Density plots for selected parameters
mcmc_dens(draws_array, pars = c("beta0", beta_names))

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "beta[1]" = "i_visMin",
  "beta[2]" = "i_educ_noHS_25_64",
  "beta[3]" = "i_LICO",
  "beta[4]" = "i_rec_imm",
  "beta[5]" = "i_young",
  "beta[6]" = "i_old",
  "beta[7]" = "i_tenant"
)

# Extract the draws
draws <- bym_intercept_cov_model_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Rename the parameters in the array manually
param_names <- dimnames(draws_array)$variable
for (i in seq_along(param_names)) {
  if (param_names[i] %in% names(covariate_names)) {
    param_names[i] <- covariate_names[param_names[i]]
  }
}
dimnames(draws_array)$variable <- param_names

# Generate trace plots with the renamed parameters
mcmc_trace(draws_array, pars = c("beta0", unname(covariate_names)))

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M1_B: M1_theta
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
output_dir <- tempdir() # Or specify a custom directory
bym_intercept_cov_random_effect_model_scot_stanfit = bym_intercept_cov_random_effect_model$sample(
  data = data,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

# Extract the summary
summary_df <- as.data.frame(bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("lp__", "beta0", "beta")))

# Covariate names
covariate_names <- c("i_visMin", "i_educ_noHS_25_64", "i_LICO", "i_rec_imm", "i_young", "i_old", "i_tenant")

# Create a data frame with parameter names and covariate names
param_map <- data.frame(
  parameter = paste0("beta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

# Replace parameter names with covariate names in the summary
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

summary_df <- as.data.frame(bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("lp__", "beta0", "beta"),
                                                                         ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

param_map2 <- data.frame(
  parameter = paste0("expbeta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta"), 
                                                                         ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)


## For mu
bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("mu"));
bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("mu"),
                                             ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym_intercept_cov_random_effect_model_scot_stanfit$summary(variables = c("mu"), ~quantile(.x, probs = c(0.025, 0.5, 0.975)))
mu_values <- summary_mu$`50%` # use the mean of mu

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "Scatter Plot of y_i vs mu_i", pch = 19, col = "blue")

library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym_intercept_cov_random_effect_model_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:33,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
waic_result <- waic(as.matrix(bym_intercept_cov_random_effect_model_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_intercept_cov_random_effect_model_scot_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "beta[1]" = "i_visMin",
  "beta[2]" = "i_educ_noHS_25_64",
  "beta[3]" = "i_LICO",
  "beta[4]" = "i_rec_imm",
  "beta[5]" = "i_young",
  "beta[6]" = "i_old",
  "beta[7]" = "i_tenant"
)

# Extract the draws
draws <- bym_intercept_cov_random_effect_model_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Rename the parameters in the array manually
param_names <- dimnames(draws_array)$variable
for (i in seq_along(param_names)) {
  if (param_names[i] %in% names(covariate_names)) {
    param_names[i] <- covariate_names[param_names[i]]
  }
}
dimnames(draws_array)$variable <- param_names

# Generate trace plots with the renamed parameters
mcmc_trace(draws_array, pars = c("beta0", unname(covariate_names)))

theta_length <- 33
theta_names <- paste0("theta[", 1:theta_length, "]")

# dimnames(draws_array)
# phi_length <- 33
# phi_names <- paste0("phi[", 1:phi_length, "]")

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("tau_theta", "tau_phi"))

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))
mcmc_dens(draws_array, pars = c("tau_theta", "tau_phi"))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M2_C: Traffic based with weights=1/0
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
library(INLA)
#Build the adjacency matrix with weights=1
adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)

#The ICAR precision matrix (note! This is singular)
Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
#See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))

data2 = list(N=N,
            N_edges=N_edges,
            node1=node1,
            node2=node2,
            y=y,
            E=E,
            K = 7,
            X=x,
            scaling_factor=scaling_factor);

bym2_model = cmdstan_model("bym2_predictor_plus_offset.stan");

## warmup (burinin _ = 5000, totoal run = 10000
output_dir <- tempdir() # Or specify a custom directory
bym2_scot_stanfit = bym2_model$sample(
  data = data2,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                       "sigma", "rho",
                                       "phi[5]","theta[5]"));

param_map <- data.frame(
  parameter = paste0("betas[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                                                     "sigma", "rho",
                                                                     "phi[5]","theta[5]")))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)


# Provides summary statistics including
# the mean, standard deviation, and 95% credible intervals (0.025, 0.5, 0.975 quantiles).
bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                        "sigma", "rho", 
                                       "mu[5]","phi[5]","theta[5]"),
                         ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                                                    "sigma", "rho", 
                                                                    "phi[5]","theta[5]"),
                                                      ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

param_map2 <- data.frame(
  parameter = paste0("expbeta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta"), 
                                                                         ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

## For mu
bym2_scot_stanfit$summary(variables = c("mu"));
bym2_scot_stanfit$summary(variables = c("mu"),
                                             ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym2_scot_stanfit$summary(variables = c("mu"), ~quantile(.x, probs = c(0.025, 0.5, 0.975)))
mu_values <- summary_mu$`50%` # use the mean of mu

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "Scatter Plot of y_i vs mu_i", pch = 19, col = "blue")


library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym2_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:33,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
waic_result <- waic(as.matrix(bym2_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym2_scot_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots
# library(bayesplot)
# 
# # Extract draws
# draws <- bym2_scot_stanfit$draws()
# 
# # Convert draws to an array
# draws_array <- as.array(draws)
# 
# beta_length <- 8
# beta_names <- paste0("betas[", 1:beta_length, "]")
# theta_names <- paste0("theta[", 1:beta_length, "]")
# 
# # Generate trace plots for selected parameters
# mcmc_trace(draws_array, pars = c("beta0", beta_names,theta_names,"sigma","rho"))
# 
# # Density plots for selected parameters
# mcmc_dens(draws_array, pars = c("beta0", "beta0", beta_names,theta_names,"sigma","rho"))

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "betas[1]" = "i_visMin",
  "betas[2]" = "i_educ_noHS_25_64",
  "betas[3]" = "i_LICO",
  "betas[4]" = "i_rec_imm",
  "betas[5]" = "i_young",
  "betas[6]" = "i_old",
  "betas[7]" = "i_tenant"
)

# Extract the draws
draws <- bym2_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Rename the parameters in the array manually
param_names <- dimnames(draws_array)$variable
for (i in seq_along(param_names)) {
  if (param_names[i] %in% names(covariate_names)) {
    param_names[i] <- covariate_names[param_names[i]]
  }
}
dimnames(draws_array)$variable <- param_names

# Generate trace plots with the renamed parameters for M2
mcmc_trace(draws_array, pars = c("beta0", unname(covariate_names)))

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))

# Generate trace plots with the renamed parameters for 2B
mcmc_trace(draws_array, pars = c("sigma","rho"))

mcmc_dens(draws_array, pars = c("sigma","rho"))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M2_D: traffic based with weights=stops
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
library(INLA)
#Build the adjacency matrix with weights=1
adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=nedges$Value,symmetric=TRUE)

#The ICAR precision matrix (note! This is singular)
Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
#See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))

data2 = list(N=N,
             N_edges=N_edges,
             node1=node1,
             node2=node2,
             y=y,
             E=E,
             K = 7,
             X=x,
             scaling_factor=scaling_factor);

bym2_model = cmdstan_model("bym2_predictor_plus_offset.stan");

## warmup (burinin _ = 5000, totoal run = 10000
output_dir <- tempdir() # Or specify a custom directory
bym2_scot_stanfit = bym2_model$sample(
  data = data2,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                        "sigma", "rho", 
                                        "mu[5]","phi[5]","theta[5]"));


# Provides summary statistics including
# the mean, standard deviation, and 95% credible intervals (0.025, 0.5, 0.975 quantiles).
bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                        "sigma", "rho", 
                                        "mu[5]","phi[5]","theta[5]"),
                          ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

## For mu
bym2_scot_stanfit$summary(variables = c("mu"));
bym2_scot_stanfit$summary(variables = c("mu"),
                          ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym2_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:33,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
waic_result <- waic(as.matrix(bym2_scot_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym2_scot_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots
library(bayesplot)

# Extract draws
draws <- bym2_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

beta_length <- 8
beta_names <- paste0("betas[", 1:beta_length, "]")
theta_names <- paste0("theta[", 1:beta_length, "]")

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("beta0", beta_names,theta_names,"sigma","rho"))

# Density plots for selected parameters
mcmc_dens(draws_array, pars = c("beta0", "beta0", beta_names,theta_names,"sigma","rho"))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M2:For Contiguity based
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
source("mtl_graph_Contiguity.R")
nbs = mungeCARdata4stan_mtl(weights_matrix);
N = nbs$N;
node1 = nbs$node1;
node2 = nbs$node2;
N_edges = nbs$N_edges;

# Combines all the necessary data into a list (data) to pass to the Stan model.
X = as.matrix(x)  # Ensure X is a matrix 

library(INLA)
#Build the adjacency matrix with weights=1
adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)

#The ICAR precision matrix (note! This is singular)
Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
#See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))

data_34 = list(N=N,
               N_edges=N_edges,
               node1=node1,
               node2=node2,
               y=y,
               E=E,
               K = 7,
               X=x,
               scaling_factor=scaling_factor);

bym2_model = cmdstan_model("bym2_predictor_plus_offset.stan");

## warmup (burinin _ = 5000, totoal run = 10000
output_dir <- tempdir() # Or specify a custom directory
bym2_scot_stanfit = bym2_model$sample(
  data = data_34,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain


param_map <- data.frame(
  parameter = paste0("betas[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                                                    "sigma", "rho",
                                                                    "phi[5]","theta[5]")))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)


# Provides summary statistics including
summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                                                    "sigma", "rho", 
                                                                    "phi[5]","theta[5]"),
                                                      ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

param_map2 <- data.frame(
  parameter = paste0("expbeta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta"), 
                                                      ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

## For mu
bym2_scot_stanfit$summary(variables = c("mu"));
bym2_scot_stanfit$summary(variables = c("mu"),
                          ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym2_scot_stanfit$summary(variables = c("mu"), ~quantile(.x, probs = c(0.025, 0.5, 0.975)))
mu_values <- summary_mu$`50%` # use the mean of mu

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "Scatter Plot of y_i vs mu_i", pch = 19, col = "blue")


library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym2_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:34,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
library(loo)
library(cmdstanr)
waic_result <- waic(as.matrix(bym2_scot_stanfit$draws("log_lik")))
print(waic_result)

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "betas[1]" = "i_visMin",
  "betas[2]" = "i_educ_noHS_25_64",
  "betas[3]" = "i_LICO",
  "betas[4]" = "i_rec_imm",
  "betas[5]" = "i_young",
  "betas[6]" = "i_old",
  "betas[7]" = "i_tenant"
)

# Extract the draws
draws <- bym2_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Rename the parameters in the array manually
param_names <- dimnames(draws_array)$variable
for (i in seq_along(param_names)) {
  if (param_names[i] %in% names(covariate_names)) {
    param_names[i] <- covariate_names[param_names[i]]
  }
}
dimnames(draws_array)$variable <- param_names

# Generate trace plots with the renamed parameters for M1
mcmc_trace(draws_array, pars = c("beta0", unname(covariate_names)))

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))

# Generate trace plots with the renamed parameters for M2
mcmc_trace(draws_array, pars = c("sigma","rho"))

mcmc_dens(draws_array, pars = c("sigma","rho"))

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M2_B: For Contiguity based add loop
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
weights_matrix_loop <- weights_matrix+diag(diag(adjacency_matrix))

nbs = mungeCARdata4stan_mtl(weights_matrix_loop);
N = nbs$N;
node1 = nbs$node1;
node2 = nbs$node2;
N_edges = nbs$N_edges;

# Combines all the necessary data into a list (data) to pass to the Stan model.
X = as.matrix(x)  # Ensure X is a matrix 

library(INLA)
#Build the adjacency matrix with weights=1
adj.matrix = sparseMatrix(i=nbs$node1,j=nbs$node2,x=1,symmetric=TRUE)

#The ICAR precision matrix (note! This is singular)
Q=  Diagonal(nbs$N, rowSums(adj.matrix)) - adj.matrix
#Add a small jitter to the diagonal for numerical stability (optional but recommended)
Q_pert = Q + Diagonal(nbs$N) * max(diag(Q)) * sqrt(.Machine$double.eps)

# Compute the diagonal elements of the covariance matrix subject to the 
# constraint that the entries of the ICAR sum to zero.
#See the inla.qinv function help for further details.
Q_inv = inla.qinv(Q_pert, constr=list(A = matrix(1,1,nbs$N),e=0))

#Compute the geometric mean of the variances, which are on the diagonal of Q.inv
scaling_factor = exp(mean(log(diag(Q_inv))))

data_34_loop = list(N=N,
                    N_edges=N_edges,
                    node1=node1,
                    node2=node2,
                    y=y,
                    E=E,
                    K = 7,
                    X=x,
                    scaling_factor=scaling_factor);

bym2_model = cmdstan_model("bym2_predictor_plus_offset.stan");

## warmup (burinin _ = 5000, totoal run = 10000
output_dir <- tempdir() # Or specify a custom directory
bym2_scot_stanfit = bym2_model$sample(
  data = data_34_loop,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain


param_map <- data.frame(
  parameter = paste0("betas[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                                                    "sigma", "rho",
                                                                    "phi[5]","theta[5]")))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)


# Provides summary statistics including
summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("lp__", "beta0", "betas",
                                                                    "sigma", "rho", 
                                                                    "phi[5]","theta[5]"),
                                                      ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map)) { #i=3
  summary_df$variable <- gsub(param_map$parameter[i], param_map$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]

# Print updated summary
print(summary_df)

param_map2 <- data.frame(
  parameter = paste0("expbeta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym2_scot_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta"), 
                                                      ~quantile(.x, probs = c(0.025, 0.5, 0.975))))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

## For mu
bym2_scot_stanfit$summary(variables = c("mu"));
bym2_scot_stanfit$summary(variables = c("mu"),
                          ~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym2_scot_stanfit$summary(variables = c("mu"), ~quantile(.x, probs = c(0.025, 0.5, 0.975)))
mu_values <- summary_mu$`50%` # use the mean of mu

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "Scatter Plot of y_i vs mu_i", pch = 19, col = "blue")


library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym2_scot_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:34,
  Mean = mu_means,
  SD = mu_sds
)

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()


## For WAIC & LOO
library(loo)
library(cmdstanr)
waic_result <- waic(as.matrix(bym2_scot_stanfit$draws("log_lik")))
print(waic_result)

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "betas[1]" = "i_visMin",
  "betas[2]" = "i_educ_noHS_25_64",
  "betas[3]" = "i_LICO",
  "betas[4]" = "i_rec_imm",
  "betas[5]" = "i_young",
  "betas[6]" = "i_old",
  "betas[7]" = "i_tenant"
)

# Extract the draws
draws <- bym2_scot_stanfit$draws()

# Convert draws to an array
draws_array <- as.array(draws)

# Rename the parameters in the array manually
param_names <- dimnames(draws_array)$variable
for (i in seq_along(param_names)) {
  if (param_names[i] %in% names(covariate_names)) {
    param_names[i] <- covariate_names[param_names[i]]
  }
}
dimnames(draws_array)$variable <- param_names

# Generate trace plots with the renamed parameters for M1
mcmc_trace(draws_array, pars = c("beta0", unname(covariate_names)))

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))

# Generate trace plots with the renamed parameters for M2
mcmc_trace(draws_array, pars = c("sigma","rho"))

mcmc_dens(draws_array, pars = c("sigma","rho"))


