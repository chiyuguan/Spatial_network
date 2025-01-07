# fits a spatial model using the Besag-York-Mollié (BYM) model in Stan
library(devtools)
library(rstan)
# we recommend running this in a fresh R session or restarting your current session
install.packages("cmdstanr", repos = c('https://stan-dev.r-universe.dev', getOption("repos")))
library(cmdstanr)  
#options(digits=3)
# Sets the number of digits to display in the output to 3.

source("ON_data.R")
source("ON_newE.R")

y = plot_case$case_count; # Modify to total 2020 3-5 cases
Predictor_var$Stops <- log(Predictor_var$Stops)
x = S2 %>% st_drop_geometry(); # 94*7 metrix for covariates
# need covariates mean centered and 1 sd scaled,
x <- scale(x, center = TRUE, scale = TRUE)
# Convert the scaled data back to a data frame
x <- as.data.frame(x)

x3 = Predictor_var[, c(1:3,5:7)] %>% st_drop_geometry();
# need covariates mean centered and 1 sd scaled,
x3 <- scale(x3, center = TRUE, scale = TRUE)
# Convert the scaled data back to a data frame
x3 <- as.data.frame(x3)

x4 = Predictor_var[, c(1,3:7)] %>% st_drop_geometry();
# need covariates mean centered and 1 sd scaled,
x4 <- scale(x4, center = TRUE, scale = TRUE)
# Convert the scaled data back to a data frame
x4 <- as.data.frame(x4)

x %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(number.cex = 0.5, tl.cex = 0.5) # Cor_plot

x3 %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(number.cex = 0.5, tl.cex = 0.5) # Cor_plot

x4 %>% 
  cor(use = "complete.obs") %>% 
  corrplot::corrplot.mixed(number.cex = 0.5, tl.cex = 0.5) # Cor_plot

E = plot_case$E;

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### For traffic based
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
# Combines all the necessary data into a list (data) to pass to the Stan model.
X = as.matrix(x)  # Ensure X is a matrix 
data = list(N=94,
            y=y,
            E=E,
            K = 5,
            X=X); 

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M1_C: M2
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
set.seed(123)
bym_M1C_model = cmdstan_model("M1C.stan");
#bym_M1C_model = cmdstan_model("bym_intercept_covariates.stan");
output_dir <- tempdir() # Or specify a custom directory
bym_M1C_model_stanfit = bym_M1C_model$sample(
  data = data,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

# Extract the summary
summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("lp__", "beta0", "beta")))
# Covariate names
covariate_names <- colnames(x)

# exponential output
param_map2 <- data.frame(
  parameter = paste0("expbeta[", 1:length(covariate_names), "]"),
  covariate = covariate_names,
  stringsAsFactors = FALSE
)
summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("RR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
}
print(summary_df)

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("RR[25]", "expbeta0", "expbeta"), 
                                                          ~quantile(.x, probs = c(0.025, 0.5, 0.975))))

for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
}
print(summary_df)

bym_M1C_model_stanfit$summary(variables = c("expsigma_theta"))
bym_M1C_model_stanfit$summary(variables = c("expsigma_theta"),~quantile(.x, probs = c(0.025, 0.5, 0.975)));

library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym_M1C_model_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) 
mu_lower <- apply(posterior_mu, 2, quantile, probs = 0.025)
mu_upper <- apply(posterior_mu, 2, quantile, probs = 0.975)

mu_summary <- data.frame(
  Observation = 1:94,
  Mean = mu_means,
  Upper = mu_upper,
  Lower= mu_lower
)
library(ggplot2)
ggplot(mu_summary, aes(x = y, y = Mean)) +
  geom_point(color = "black", size = 2) + # Points for fitted vs. observed
  geom_errorbar(aes(ymin = Lower, ymax = Upper), 
                width = 0.2, color = "red") + # Error bars for credible intervals
  labs(
    title = "Fitted vs Observed with 95% Credible Intervals",
    x = "Observed (y)",
    y = "Fitted Mean (mu)"
  ) +
  theme_minimal()+
  theme(
    panel.grid = element_blank()  # Remove all grid lines
  )


## For WAIC 
library(loo)
library(posterior)
waic_result <- waic(as.matrix(bym_M1C_model_stanfit$draws("log_lik")))
print(waic_result)

# Double Check WAIC
log_lik <-as.matrix(bym_M1C_model_stanfit$draws("log_lik"))
lppd <- sum(log(colMeans(exp(log_lik))))
p_WAIC <- sum(apply(log_lik, 2, var))
WAIC <- -2 * (lppd - p_WAIC)

## For traceplots

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "beta[1]" = "Visible minorities",
  "beta[2]" = "Recent immigrants",
  "beta[3]" = "No High sch, 25-64",
  "beta[4]" = "Log of Stops",
  "beta[5]" = "Low-income cutoff"
)

# Extract the draws
draws <- bym_M1C_model_stanfit$draws()
draws_array <- as.array(draws)
param_names <- dimnames(draws_array)$variable
for (i in seq_along(param_names)) {
  if (param_names[i] %in% names(covariate_names)) {
    param_names[i] <- covariate_names[param_names[i]]
  }
}
dimnames(draws_array)$variable <- param_names

# Generate trace plots with the renamed parameters
mcmc_trace(draws_array, pars = c("beta0", unname(covariate_names), "sigma_theta"))

theta_length <- 94
theta_names <- paste0("theta[", 1:theta_length, "]")


# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("sigma_theta", theta_names))
mcmc_trace(draws_array, pars = "sigma_theta")

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names), "sigma_theta"))
mcmc_dens(draws_array, pars = c("sigma_theta", theta_names))

# For RR
posterior_RR <- bym_M1C_model_stanfit$draws(variables = 'RR')

posterior_RR <- as_draws_matrix(posterior_RR)

# Calculate P(RR > 1) for each area
P_RR_gt_1 <- colMeans(posterior_RR > 1)

# Summary of RR
RR_mean <- colMeans(posterior_RR)  # Posterior mean of RR
RR_sd <- apply(posterior_RR, 2, sd)  # Posterior SD of RR

# Combine into a dataframe
RR_summary <- data.frame(
  Area = 1:length(P_RR_gt_1),
  RR_Mean = RR_mean,
  RR_SD = RR_sd,
  P_RR_gt_1 = P_RR_gt_1
)

# Display or save results
print(RR_summary)

plot_case$P_RR_gt_1 <- RR_summary$P_RR_gt_1
ggplot(data = plot_case) +
  geom_sf(aes(fill = P_RR_gt_1, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90")   +
  labs(title = "Probability of posterior Relative Risk (RR) larger than 1",
       subtitle = "form March 2020 to May 2020",
       fill = "P(posterior RR > 1)") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )+
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M1_C: no stops
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
set.seed(123)
X3 = as.matrix(x3)  # Ensure X is a matrix 
data2 = list(N=94,
             y=y,
             E=E,
             K = 6,
             X=X3); 
bym_M1C_model = cmdstan_model("M1C.stan");
#bym_M1C_model = cmdstan_model("bym_intercept_covariates.stan");
output_dir <- tempdir() # Or specify a custom directory
bym_M1C_model_stanfit = bym_M1C_model$sample(
  data = data2,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

# Extract the summary
summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("lp__", "beta0", "beta")))

# Covariate names
covariate_names <- colnames(x3)

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

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("lp__", "beta0", "beta"),
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

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("RR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("RR[25]", "expbeta0", "expbeta"), 
                                                          ~quantile(.x, probs = c(0.025, 0.5, 0.975))))

for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

bym_M1C_model_stanfit$summary(variables = c("expsigma_theta"))
bym_M1C_model_stanfit$summary(variables = c("expsigma_theta"),~quantile(.x, probs = c(0.025, 0.5, 0.975)));

## For mu
bym_M1C_model_stanfit$summary(variables = c("mu"));
bym_M1C_model_stanfit$summary(variables = c("mu"),~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym_M1C_model_stanfit$summary(variables = c("mu"))
mu_values <- summary_mu$mean # use the mean of mu

# polygon(
#   c(mu_means, rev(mu_means)),            # X-coordinates: mu_mean and its reverse
#   c(mu_means-mu_sds, rev(mu_means+mu_sds)),          # Y-coordinates: lower CI and reversed upper CI
#   col = rgb(173, 216, 230, alpha = 100, maxColorValue = 255), # Light blue with transparency
#   border = NA                          # No border for the polygon
# )


library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym_M1C_model_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:94,
  Mean = mu_means,
  SD = mu_sds
)

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "M1 scatter Plot of y_i vs mu_i", pch = 19, col = "black")

arrows(x0 = mu_means, y0 = mu_means-mu_sds, x1 = mu_means, y1 = mu_means+mu_sds,
       length = 0.05, angle = 90, code = 3, col = "red")

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()

# plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "M1 scatter Plot of y_i vs mu_i with 95% CI", pch = 19, col = "black")
# 
# # Step 3: Overlay the 95% CI shadow
# # Add a transparent ribbon for the 95% CI
# mu_lower <- mu_summary$Mean-mu_summary$SD
# mu_upper <- mu_summary$Mean+mu_summary$SD
# lines(mu_summary$Mean, mu_lower, col = "red", lty = 2)  # 95% lower bound
# lines(mu_summary$Mean, mu_upper, col = "red", lty = 2)  # 95% upper bound
# 
# legend("topleft", legend =  "95% CI",
#        col = "red",
#        lty = 2, lwd = 1, bty = "n")

library(ggplot2)

# Assuming `mu_summary` has columns: `y_i` (observed values), `mu_mean` (fitted mean), 
# `mu_lower` (lower 95% CI), `mu_upper` (upper 95% CI)

ggplot(mu_summary, aes(x = y, y = Mean)) +
  geom_point(color = "black", size = 2) + # Points for fitted vs. observed
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2, color = "red") + # Error bars for credible intervals
  labs(
    title = "Fitted vs Observed with 95% Credible Intervals",
    x = "Observed (y)",
    y = "Fitted Mean (mu)"
  ) +
  theme_minimal()


## For WAIC & LOO
library(loo)
library(posterior)
waic_result <- waic(as.matrix(bym_M1C_model_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_M1C_model_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "beta[1]" = "Youth",
  "beta[2]" = "High school 15",
  "beta[3]" = "Low Income",
  "beta[4]" = "Recent movers 5",
  "beta[5]" = "Recent immigrants",
  "beta[6]" = "Visible minorities"
)


# Extract the draws
draws <- bym_M1C_model_stanfit$draws()

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

theta_length <- 94
theta_names <- paste0("theta[", 1:theta_length, "]")

# dimnames(draws_array)
# phi_length <- 33
# phi_names <- paste0("phi[", 1:phi_length, "]")

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("sigma_theta", theta_names))
mcmc_trace(draws_array, pars = "sigma_theta")

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))
mcmc_dens(draws_array, pars = c("sigma_theta", theta_names))

# For RR
posterior_RR <- bym_M1C_model_stanfit$draws(variables = 'RR')

posterior_RR <- as_draws_matrix(posterior_RR)

# Calculate P(RR > 1) for each area
P_RR_gt_1 <- colMeans(posterior_RR > 1)

# Summary of RR
RR_mean <- colMeans(posterior_RR)  # Posterior mean of RR
RR_sd <- apply(posterior_RR, 2, sd)  # Posterior SD of RR

# Combine into a dataframe
RR_summary <- data.frame(
  Area = 1:length(P_RR_gt_1),
  RR_Mean = RR_mean,
  RR_SD = RR_sd,
  P_RR_gt_1 = P_RR_gt_1
)

# Display or save results
print(RR_summary)

plot_case$P_RR_gt_1 <- RR_summary$P_RR_gt_1
ggplot(data = plot_case) +
  geom_sf(aes(fill = P_RR_gt_1, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90")   +
  labs(title = "Probability of posterior SMR larger than 1",
       subtitle = "form March 2020 to May 2020",
       fill = "P(posterior SMR > 1)") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )+
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )

### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### M1_C: no edu
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
### ++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++ ###
set.seed(123)
X4 = as.matrix(x4)  # Ensure X is a matrix 
data4 = list(N=94,
             y=y,
             E=E,
             K = 6,
             X=X4); 
bym_M1C_model = cmdstan_model("M1C.stan");
#bym_M1C_model = cmdstan_model("bym_intercept_covariates.stan");
output_dir <- tempdir() # Or specify a custom directory
bym_M1C_model_stanfit = bym_M1C_model$sample(
  data = data4,
  parallel_chains = 4,
  refresh=0,
  output_dir = output_dir, # ); default num_iterations=1000
  iter_sampling = 5000,  # Number of post-warmup iterations per chain
  iter_warmup = 5000);   # Number of warmup iterations per chain

# Extract the summary
summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("lp__", "beta0", "beta")))

# Covariate names
covariate_names <- colnames(x4)

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

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("lp__", "beta0", "beta"),
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

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta")))
for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

summary_df <- as.data.frame(bym_M1C_model_stanfit$summary(variables = c("SMR[25]", "expbeta0", "expbeta"), 
                                                          ~quantile(.x, probs = c(0.025, 0.5, 0.975))))

for (i in 1:nrow(param_map2)) { #i=3
  summary_df$variable <- gsub(param_map2$parameter[i], param_map2$covariate[i], summary_df$variable, fixed = TRUE)
} #treats the pattern as a fixed string rather than a regular expression. This avoids any issues with special characters like [ ]
print(summary_df)

bym_M1C_model_stanfit$summary(variables = c("expsigma_theta"))
bym_M1C_model_stanfit$summary(variables = c("expsigma_theta"),~quantile(.x, probs = c(0.025, 0.5, 0.975)));

## For mu
bym_M1C_model_stanfit$summary(variables = c("mu"));
bym_M1C_model_stanfit$summary(variables = c("mu"),~quantile(.x, probs = c(0.025, 0.5, 0.975)));

# Extract the mu_i values
summary_mu <- bym_M1C_model_stanfit$summary(variables = c("mu"))
mu_values <- summary_mu$mean # use the mean of mu

# polygon(
#   c(mu_means, rev(mu_means)),            # X-coordinates: mu_mean and its reverse
#   c(mu_means-mu_sds, rev(mu_means+mu_sds)),          # Y-coordinates: lower CI and reversed upper CI
#   col = rgb(173, 216, 230, alpha = 100, maxColorValue = 255), # Light blue with transparency
#   border = NA                          # No border for the polygon
# )


library(posterior)
library(ggplot2)
# For full model
# Extract posterior samples
posterior_samples <- bym_M1C_model_stanfit$draws(variables = 'mu')

# Convert to a matrix
posterior_mu <- as_draws_matrix(posterior_samples)

# Compute summary statistics
mu_means <- apply(posterior_mu, 2, mean) # 2: This indicates that the function should be applied to the columns. 
mu_sds <- apply(posterior_mu, 2, sd)

# Create a data frame for plotting
mu_summary <- data.frame(
  Observation = 1:94,
  Mean = mu_means,
  SD = mu_sds
)

# Create the scatter plot
plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "M1 scatter Plot of y_i vs mu_i", pch = 19, col = "black")

arrows(x0 = mu_means, y0 = mu_means-mu_sds, x1 = mu_means, y1 = mu_means+mu_sds,
       length = 0.05, angle = 90, code = 3, col = "red")

# Plot using ggplot2
library(ggplot2)

ggplot(mu_summary, aes(x = Observation, y = Mean)) +
  geom_point() +
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), width = 0.2) +
  labs(title = "Posterior Means and Standard Deviations of mu",
       x = "Observation",
       y = "mu") +
  theme_minimal()

# plot(mu_values, y, xlab = "mean of mu_i", ylab = "y_i", main = "M1 scatter Plot of y_i vs mu_i with 95% CI", pch = 19, col = "black")
# 
# # Step 3: Overlay the 95% CI shadow
# # Add a transparent ribbon for the 95% CI
# mu_lower <- mu_summary$Mean-mu_summary$SD
# mu_upper <- mu_summary$Mean+mu_summary$SD
# lines(mu_summary$Mean, mu_lower, col = "red", lty = 2)  # 95% lower bound
# lines(mu_summary$Mean, mu_upper, col = "red", lty = 2)  # 95% upper bound
# 
# legend("topleft", legend =  "95% CI",
#        col = "red",
#        lty = 2, lwd = 1, bty = "n")

library(ggplot2)

# Assuming `mu_summary` has columns: `y_i` (observed values), `mu_mean` (fitted mean), 
# `mu_lower` (lower 95% CI), `mu_upper` (upper 95% CI)

ggplot(mu_summary, aes(x = y, y = Mean)) +
  geom_point(color = "black", size = 2) + # Points for fitted vs. observed
  geom_errorbar(aes(ymin = Mean - SD, ymax = Mean + SD), 
                width = 0.2, color = "red") + # Error bars for credible intervals
  labs(
    title = "Fitted vs Observed with 95% Credible Intervals",
    x = "Observed (y)",
    y = "Fitted Mean (mu)"
  ) +
  theme_minimal()


## For WAIC & LOO
library(loo)
library(posterior)
waic_result <- waic(as.matrix(bym_M1C_model_stanfit$draws("log_lik")))
print(waic_result)

loo_result <- loo(as.matrix(bym_M1C_model_stanfit$draws("log_lik")))
print(loo_result)

## For traceplots

library(bayesplot)
library(posterior)

# Define the covariate names and their corresponding parameter names
covariate_names <- c(
  "beta[1]" = "Youth",
  "beta[2]" = "Low Income",
  "beta[3]" = "Stops",
  "beta[4]" = "Recent movers 5",
  "beta[5]" = "Recent immigrants",
  "beta[6]" = "Visible minorities"
)

# Extract the draws
draws <- bym_M1C_model_stanfit$draws()

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

theta_length <- 94
theta_names <- paste0("theta[", 1:theta_length, "]")

# dimnames(draws_array)
# phi_length <- 33
# phi_names <- paste0("phi[", 1:phi_length, "]")

# Generate trace plots for selected parameters
mcmc_trace(draws_array, pars = c("sigma_theta", theta_names))
mcmc_trace(draws_array, pars = "sigma_theta")

mcmc_dens(draws_array, pars = c("beta0", unname(covariate_names)))
mcmc_dens(draws_array, pars = c("sigma_theta", theta_names))

# For SMR
posterior_SMR <- bym_M1C_model_stanfit$draws(variables = 'SMR')

posterior_SMR <- as_draws_matrix(posterior_SMR)

# Calculate P(SMR > 1) for each area
P_SMR_gt_1 <- colMeans(posterior_SMR > 1)

# Summary of SMR
SMR_mean <- colMeans(posterior_SMR)  # Posterior mean of SMR
SMR_sd <- apply(posterior_SMR, 2, sd)  # Posterior SD of SMR

# Combine into a dataframe
SMR_summary <- data.frame(
  Area = 1:length(P_SMR_gt_1),
  SMR_Mean = SMR_mean,
  SMR_SD = SMR_sd,
  P_SMR_gt_1 = P_SMR_gt_1
)

# Display or save results
print(SMR_summary)

plot_case$P_SMR_gt_1 <- SMR_summary$P_SMR_gt_1
ggplot(data = plot_case) +
  geom_sf(aes(fill = P_SMR_gt_1, geometry = geometry)) +
  scale_fill_gradient(low = "grey10", high = "grey90")   +
  labs(title = "Probability of posterior SMR larger than 1",
       subtitle = "form March 2020 to May 2020",
       fill = "P(posterior SMR > 1)") +
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(b = 10))
  )+
  theme_void()+
  theme(
    plot.title = element_text(size = 16, face = "bold", hjust = 0.5, margin = margin(t = 10, b = 10)), # Adds top and bottom margin to the title
    plot.subtitle = element_text(size = 12, hjust = 0.5, margin = margin(t = 5, b = 10)),              # Adds margin to the subtitle
    legend.title = element_text(size = 10, face = "bold"),
    legend.text = element_text(size = 9),
    plot.margin = margin(20, 20, 20, 20)  # Adds extra space around the entire plot
  )
##################################################################################################################
##################################################################################################################
############################### Model selection
##################################################################################################################
##################################################################################################################
