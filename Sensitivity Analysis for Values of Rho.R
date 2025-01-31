library(googlesheets4)
library(tidyverse)
library(metafor)
library(robumeta)
library(clubSandwich)
library(weightr)
library(ggplot2)

# Load your dataset
NNMA_Data <- read_sheet("https://docs.google.com/spreadsheets/d/1cv5ftm6-XV28pZ_mN43K7HH3C7WhsPMnPsB1HDuRLE4/edit#gid=0")

# Check structure
glimpse(NNMA_Data)

# Select only the necessary columns
df <- NNMA_Data %>%
  select(NL_TX, RS_TX, FF_TX, VF_TX, SE_TX, NL_COMP, RS_COMP, FF_COMP, VF_COMP, SE_COMP,) %>%
  drop_na()  # Remove missing values


# Compute empirical correlation
rho_actual <- cor(df$X1, df$X2)

# Create a range of rho values based on perturbation of actual correlation
rho_values <- seq(rho_actual - 0.3, rho_actual + 0.3, length.out = 10)

# Initialize results storage
results <- data.frame(rho = numeric(), beta1 = numeric(), beta2 = numeric())

for (rho in rho_values) {
  # Perturb correlation (create pseudo datasets with adjusted correlation)
  df_temp <- df
  df_temp$X2 <- df$X1 * rho + df$X2 * sqrt(1 - rho^2)  # Adjust correlation
  
  # Run regression
  model <- lm(Y ~ X1 + X2, data = df_temp)
  
  # Store results
  results <- rbind(results, data.frame(rho = rho, beta1 = coef(model)[2], beta2 = coef(model)[3]))
}

# Plot sensitivity analysis results
ggplot(results, aes(x = rho)) +
  geom_line(aes(y = beta1, color = "Beta 1")) +
  geom_line(aes(y = beta2, color = "Beta 2")) +
  labs(title = "Sensitivity of Regression Coefficients to Rho",
       y = "Coefficient Value") +
  theme_minimal()