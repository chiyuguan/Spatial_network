
library(dplyr)

# Load necessary packages
library(knitr)
library(kableExtra)

# Data for the table
data <- data.frame(
  Regression_Coefficient = c("Intercept", "visMin", "educ_noHS_25_64", "LICO", "Stop count","sigma_theta","sigma_phi","WAIC"),
  M1_Mean = c("0.007", "1.14", "1.03", "0.77", "", " ", " ", "65.40 "),
  M1_95CrI = c("0.007, 0.008", "1.07, 1.21", "0.97, 1.10", "0.73, 0.81", " ", " ", " ", " "),
  M2_Mean = c("0.007", "1.15", "1.04", "0.82", " ", "2.00", " ","7.50 "),
  M2_95CrI = c("0.005, 0.008", "0.90, 1.47", "0.82, 1.31", "0.66, 1.00", " ", "1.78, 2.29", " ", " "),
  M3_Mean = c("0.007", "1.19", "1.05", "0.82", " ", "1.77", "1.95 "," 7.40"),
  M3_95CrI = c("0.005, 0.008", "0.84, 1.65", "0.76, 1.41", "0.61, 1.06", " ", "1.52, 2.15", "1.49, 2.88", " "),
  M4_Mean = c("0.006", "1.02", "1.08", "0.81", "1.25", "1.74", "3.21", "7.30 "),
  M4_95CrI = c("0.005, 0.008", "0.70, 1.44", "0.81, 1.41", "0.64, 1.03", "0.90, 1.71", "1.51, 2.08 ", "1.64, 8.62", " ")
)

# Create the table
kable(data, format = "html", escape = FALSE, col.names = c("Regression Coefficient", "Mean", "95% CrI", "Mean", "95% CrI", "Mean", "95% CrI", "Mean", "95% CrI")) %>%
  kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive")) %>%
  add_header_above(c(" " = 1, "M1" = 2, "M2" = 2, "M3" = 2, "M4" = 2)) %>%
  column_spec(1, bold = TRUE) %>%
  footnote(general = "Abbreviations: CrI, credible interval; WAIC, Watanabe–Akaike information criterion; visMin, Visible minorities level; educ_noHS_25_64, No High sch for people who are 25-64; LICO, Low-income cutoff", 
           symbol = c("a Posterior mean.", 
                      "b Covariates were mean centered and scaled to 1 standard deviations,
                      and the regression coefficients were exponentiated.
                      The value of coefficients represents an borough-level relative risk of having higher Covid_19 disease counts,
                      which is the ratio of the risk with a 1-unit increase of the covariates to the risk at mean value of the covariates.",
                      "c M1 is model without random effect; M2 is model without spatial effects; M3 is BYM model based on contiguity matrix; M4 is BYM model based on interaction matrix"))

