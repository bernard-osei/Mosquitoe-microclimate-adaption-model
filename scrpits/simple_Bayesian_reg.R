#install.packages("pacman")

library(readxl)
library(janitor)
library(dplyr)
library(INLA)
library(ggplot2)


#laod in the data
mos_rest <- read_excel("data/raw_diagnostics.xlsx")
#changing var names from capital letters to lower case
mos_rest <- rename_with(mos_rest, tolower)

# Trim column names to remove any trailing spaces
names(mos_rest) <- trimws(names(mos_rest))
attach(mos_rest)


###########################################################################
###############converting data levels to factors and numeric ##############

mos_rest$village <- as.factor(mos_rest$village)
mos_rest$ecological_zone <- as.factor(mos_rest$ecological_zone)
mos_rest$locality <- as.factor(mos_rest$locality)
mos_rest$locality<- as.factor(mos_rest$locality)
mos_rest$ecological_zone <- as.factor(mos_rest$ecological_zone)
mos_rest$resting_surface <- as.factor(mos_rest$resting_surface)
mos_rest$species <- as.factor(mos_rest$species)
mos_rest$sp_elisa <- as.factor(mos_rest$sp_elisa)
mos_rest$movability <-as.factor(mos_rest$movability)
mos_rest$blood_source <- as.factor(mos_rest$blood_source) 
mos_rest$sat_temp_min <- as.numeric(mos_rest$sat_temp_min) # Convert sat_temp_min to numeric
mos_rest$sat_temp_max <- as.numeric(mos_rest$sat_temp_max) # Convert sat_temp_max to numeric
mos_rest$days_count  <- as.numeric(mos_rest$days_count) # Convert days_count to numeric
mos_rest$date <- as.Date(mos_rest$date, format = "%d/%m/%Y") # Convert date to Date type
mos_rest$date_label <-format(mos_rest$date, "%d-%b-%Y") 

# Ensure temp (mosquito resting temperature) is numeric
mos_rest$temp <- as.numeric(mos_rest$temp)


#date_unique <- unique(mos_rest$date_label) # Check unique date 
#str(mos_rest)
#######################################################################


# This function replace missing values with column mean/median for specified colums
# Function to replace missing values with column median 
#and round to nearest whole number
fill_missing_values <- function(data, cols_to_fill) {
  # Ensure specified columns exist in the dataframe
  cols_to_fill <- intersect(cols_to_fill, names(data))
  
  # Convert specified columns to numeric, handling non-numeric values gracefully
  data[cols_to_fill] <- lapply(data[cols_to_fill], function(x) {
    x <- as.numeric(as.character(x)) # Convert factors/characters to numeric
    if (all(is.na(x))) return(x) # Return as is if conversion fails
    
    # Replace missing values with column median and round
    x[is.na(x)] <- round(mean(x, na.rm = TRUE))
    return(x)
  })
  return(data)
}

#using the function
mos_rest <- fill_missing_values(mos_rest,
                                c("height_from_ground",
                                  "dist_from_nearest_wall"))

ggplot(mos_rest, aes(x = temp)) +
  geom_histogram(binwidth = 5, fill = "blue", color = "black", alpha = 0.7) +
  theme_minimal() +
  ggtitle("Distribution of Mosquito Resting Site Temperature")
########################################################################
colSums(is.na(mos_rest)) #checking for missing entry
summary(mos_rest$satelite_outdoor_temp)

# Prepare data for modeling
# Add observation ID if not present
if (!"observation_id" %in% names(mos_rest)) {
  mos_rest$observation_id <- 1:nrow(mos_rest)
}

# Ensure days_count is treated as factor for random effect
mos_rest$days_count_factor <- as.integer(mos_rest$days_count)
#mos_rest$observation_id <- as.integer(mos_rest$observation_id)

# Define PC prior for random effects (penalized complexity prior)
#pc_prior <- list(prec = list(prior = "pc.prec", param = c(0, 0.05)))

# Define priors for fixed effects (Gaussian priors for continuous, default for categorical)
# Using a general prior for all continuous predictors
#fixed_prior_general <- list(mean = 0, prec = 0.1)  # General weakly informative prior

# Prior for residual error variance (control.family)
var_eps <- 0.2  # For residual error variance prior
family_prior <- list(hyper = list(prec = list(prior = "gaussian", param = c(0, var_eps))))


cit_0 <- inla(temp ~ 0 + temp_indoor,
               data = mos_rest,
               family = "gaussian",
               #control.fixed = fixed_prior_general,
               control.family = family_prior,
               control.predictor = list(compute = TRUE),
               control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                      return.marginals.predictor = TRUE))


# Model 3: MRST ~ satelite_outdoor_temp + (1|days_count_factor) + (1|observation) + (1|village)
avg_sat_0 <- inla(temp ~ 0 + satelite_outdoor_temp,
               data = mos_rest,
               family = "gaussian",
               #control.fixed = fixed_prior_general,
               control.family = family_prior,
               control.predictor = list(compute = TRUE),
               control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                      return.marginals.predictor = TRUE))



# Model 5: MRST ~ sat_temp_min + (1|days_count_factor) + (1|observation) + (1|village)
min_sat_0 <- inla(temp ~ 0 + sat_temp_min,
               data = mos_rest,
               family = "gaussian",
               #control.fixed = fixed_prior_general,
               control.family = family_prior,
               control.predictor = list(compute = TRUE),
               control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                      return.marginals.predictor = TRUE))


# Model 7: MRST ~ sat_temp_max + (1|days_count_factor) + (1|observation) + (1|village)
max_sat_0 <- inla(temp ~ 0 + sat_temp_max,
               data = mos_rest,
               family = "gaussian",
              # control.fixed = fixed_prior_general,
               control.family = family_prior,
               control.predictor = list(compute = TRUE),
               control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                      return.marginals.predictor = TRUE))



print("\ncit_0 Summary:")
print(cit_0$summary.fixed)
print(paste("WAIC:", cit_0$waic$waic))
print(paste("MLIK:", cit_0$mlik[1,1]))



print("\navg_sat_0 Summary:")
print(avg_sat_0$summary.fixed)
print(paste("WAIC:", avg_sat_0$waic$waic))
print(paste("MLIK:", avg_sat_0$mlik[1,1]))



print("\nmin_sat_0 Summary:")
print(min_sat_0$summary.fixed)
print(paste("WAIC:", min_sat_0$waic$waic))
print(paste("MLIK:", min_sat_0$mlik[1,1]))



print("\nmax_sat_0 Summary:")
print(max_sat_0$summary.fixed)
print(paste("WAIC:", max_sat_0$waic$waic))
print(paste("MLIK:", max_sat_0$mlik[1,1]))

# Model comparison
model_comparison <- data.frame(
  Model = c("cit_0", "avg_sat_0", "min_sat_0", "max_sat_0"),
  WAIC = c(cit_0$waic$waic, avg_sat_0$waic$waic, min_sat_0$waic$waic, max_sat_0$waic$waic),
  MLIK = c(cit_0$mlik[1,1], avg_sat_0$mlik[1,1], min_sat_0$mlik[1,1], max_sat_0$mlik[1,1])
)

print("\nModel Comparison Table:")
print(model_comparison)

# Compare Models 2 vs 4, 6 vs 8
print("\nComparison of Models:")
print(paste("cit_0 (Indoor temp) WAIC:", cit_0$waic$waic, "| avg_sat_0 (Sat mean) WAIC:", avg_sat_0$waic$waic))
print(paste("min_sat_0 (Sat min) WAIC:", min_sat_0$waic$waic, "| max_sat_0 (Sat max) WAIC:", max_sat_0$waic$waic))

# Identify best models
best_cit_0_4 <- ifelse(cit_0$waic$waic < avg_sat_0$waic$waic, "cit_0", "avg_sat_0")
best_min_sat_0_8 <- ifelse(min_sat_0$waic$waic < max_sat_0$waic$waic, "min_sat_0", "max_sat_0")

print(paste("\nBest model between indoor vs sat mean:", best_cit_0_4))
print(paste("Best model between sat min vs sat max:", best_min_sat_0_8))

# Compare best models: 2, 4, 6, 8
best_models_comparison <- data.frame(
  Model = c("cit_0", "avg_sat_0", "min_sat_0", "max_sat_0"),
  WAIC = c(cit_0$waic$waic, avg_sat_0$waic$waic, min_sat_0$waic$waic, max_sat_0$waic$waic),
  MLIK = c(cit_0$mlik[1,1], avg_sat_0$mlik[1,1], min_sat_0$mlik[1,1], max_sat_0$mlik[1,1]),
  Description = c("Indoor temp ", 
                  "Sat mean temp ", 
                  "Sat min temp ", 
                  "Sat max temp ")
)

print("\nComparison of best models (1, 3, 5, 7):")
print(best_models_comparison)


# Identify overall best model based on WAIC
overall_best_idx <- which.min(best_models_comparison$WAIC)
overall_best_model <- best_models_comparison$Model[overall_best_idx]
print(paste("\nOverall best model based on WAIC:", overall_best_model, 
            "with WAIC =", best_models_comparison$WAIC[overall_best_idx]))


#=============================================================================
# DISTANCE-BASED MODEL EVALUATION (RMSE Approach)
#=============================================================================

# Create model functions for each predictor
# These functions take coefficients and data as inputs, return predicted values

model_cit_0 <- function(a, data) {
  # a[1] = intercept (though we use 0 + predictor, so slope is a[1])
  a[1] * data$temp_indoor
}

model_avg_sat_0 <- function(a, data) {
  a[1] * data$satelite_outdoor_temp
}

model_min_sat_0 <- function(a, data) {
  a[1] * data$sat_temp_min
}

model_max_sat_0 <- function(a, data) {
  a[1] * data$sat_temp_max
}

# Generic distance measurement function (Root-Mean-Squared Deviation)
measure_distance <- function(model_func, coefs, data, response_col = "temp") {
  # Get predicted values
  pred <- model_func(coefs, data)
  # Get observed values
  obs <- data[[response_col]]
  # Remove NA values
  valid_idx <- !is.na(pred) & !is.na(obs)
  pred <- pred[valid_idx]
  obs <- obs[valid_idx]
  # Compute RMSE
  diff <- obs - pred
  sqrt(mean(diff ^ 2))
}

# Extract coefficients from INLA models (posterior means)
coefs_cit_0 <- c(cit_0$summary.fixed["temp_indoor", "mean"])
coefs_avg_sat_0 <- c(avg_sat_0$summary.fixed["satelite_outdoor_temp", "mean"])
coefs_min_sat_0 <- c(min_sat_0$summary.fixed["sat_temp_min", "mean"])
coefs_max_sat_0 <- c(max_sat_0$summary.fixed["sat_temp_max", "mean"])

# Compute RMSE for each model
rmse_cit_0 <- measure_distance(model_cit_0, coefs_cit_0, mos_rest)
rmse_avg_sat_0 <- measure_distance(model_avg_sat_0, coefs_avg_sat_0, mos_rest)
rmse_min_sat_0 <- measure_distance(model_min_sat_0, coefs_min_sat_0, mos_rest)
rmse_max_sat_0 <- measure_distance(model_max_sat_0, coefs_max_sat_0, mos_rest)

# Create RMSE comparison table
rmse_comparison <- data.frame(
  Model = c("cit_0", "avg_sat_0", "min_sat_0", "max_sat_0"),
  RMSE = c(rmse_cit_0, rmse_avg_sat_0, rmse_min_sat_0, rmse_max_sat_0),
  Description = c("Indoor temp", 
                  "Sat mean temp", 
                  "Sat min temp", 
                  "Sat max temp")
)

print("\n=== RMSE-Based Model Comparison ===")
print(rmse_comparison)

# Identify best model based on minimum RMSE
best_rmse_idx <- which.min(rmse_comparison$RMSE)
best_rmse_model <- rmse_comparison$Model[best_rmse_idx]
print(paste("\nBest model based on RMSE:", best_rmse_model, 
            "with RMSE =", round(rmse_comparison$RMSE[best_rmse_idx], 4)))

# Create a comprehensive comparison
comprehensive_comparison <- data.frame(
  Model = c("cit_0", "avg_sat_0", "min_sat_0", "max_sat_0"),
  Description = c("Indoor temp", "Sat mean temp", "Sat min temp", "Sat max temp"),
  WAIC = c(cit_0$waic$waic, avg_sat_0$waic$waic, min_sat_0$waic$waic, max_sat_0$waic$waic),
  RMSE = c(rmse_cit_0, rmse_avg_sat_0, rmse_min_sat_0, rmse_max_sat_0),
  Slope = c(coefs_cit_0[1], coefs_avg_sat_0[1], coefs_min_sat_0[1], coefs_max_sat_0[1])
)

print("\n=== Comprehensive Model Comparison (WAIC vs RMSE) ===")
print(comprehensive_comparison)
######################################################################################################
#-----------------------------------------------------------------------------------------------------#
#Here we want to quantify how much of the variation in mosaquito resting temperature is explained by the 
#model residual.
#calculating the total deviations 
# First, calculate SD for each model (using your approach)
# calculate_model_sd <- function(model) {
#   # Get precision for Gaussian observations
#   precision <- model$summary.hyperpar["Precision for the Gaussian observations", "mean"]
#   
#   # Convert to SD
#   variance <- 1/precision
#   sd <- sqrt(variance)
#   
#   return(sd)
# }

# Calculate SD for each model
# sd_cit_0 <- calculate_model_sd(cit_0)
# sd_avg_sat_0 <- calculate_model_sd(avg_sat_0)
# sd_min_sat_0<- calculate_model_sd(min_sat_0)
# sd_max_sat_0 <- calculate_model_sd(max_sat_0)


# Function to extract variances AND beta SD from INLA model
extract_variances <- function(model, predictor_name = NULL) {
  
  # Extract posterior mean precisions
  prec_resid  <- model$summary.hyperpar["Precision for the Gaussian observations", "mean"]
  #prec_day    <- model$summary.hyperpar["Precision for days_count_factor", "mean"]
  #prec_village<- model$summary.hyperpar["Precision for village", "mean"]
  
  # Convert to variances
  var_resid   <- 1 / prec_resid
  #var_day     <- 1 / prec_day
 # var_village <- 1 / prec_village
  
  # Create results list
  variances <- list(
    residual_variance = var_resid,
    #day_variance = var_day,
    #village_variance = var_village,
    #total_variance = var_resid + var_day + var_village,
    
    # Standard deviations
    residual_sd = sqrt(var_resid)
    #day_sd = sqrt(var_day),
    #village_sd = sqrt(var_village),
    #total_sd = sqrt(var_resid + var_day + var_village)
  )
  
  # Extract beta SD if predictor name is provided
  if (!is.null(predictor_name)) {
    beta_sd <- model$summary.fixed[predictor_name, "sd"]
    beta_mean <- model$summary.fixed[predictor_name, "mean"]
    
    variances$beta_sd <- beta_sd
    variances$beta_mean <- beta_mean
    variances$beta_ci_lower <- model$summary.fixed[predictor_name, "0.025quant"]
    variances$beta_ci_upper <- model$summary.fixed[predictor_name, "0.975quant"]
  }
  
  return(variances)
}

# Usage examples:
vars_cit_0 <- extract_variances(cit_0, predictor_name = "temp_indoor")
vars_avg_sat_0 <- extract_variances(avg_sat_0, predictor_name = "satelite_outdoor_temp")
vars_min_sat_0 <- extract_variances(min_sat_0, predictor_name = "sat_temp_min")
vars_max_sat_0 <- extract_variances(max_sat_0, predictor_name = "sat_temp_max")

#extracting the standard deviations for each model residual error or predictor SD
sd_cit_0 <- vars_cit_0$residual_sd
sd_avg_sat_0 <- vars_avg_sat_0$residual_sd
sd_min_sat_0 <- vars_min_sat_0$residual_sd
sd_max_sat_0 <- vars_max_sat_0$residual_sd

# Print for comparison
sd_comparison <- data.frame(
  Model = c("Model2 (Indoor)", "Model4 (Sat Mean)", "Model6 (Sat Min)", "Model8 (Sat Max)"),
  resi_SD = c(sd_cit_0, sd_avg_sat_0, sd_min_sat_0, sd_max_sat_0),
  beta_SD = c(vars_cit_0$beta_sd, vars_avg_sat_0$beta_sd, vars_min_sat_0$beta_sd, 
              vars_max_sat_0$beta_sd),
  WAIC = c(cit_0$waic$waic, avg_sat_0$waic$waic, 
           min_sat_0$waic$waic, max_sat_0$waic$waic)
)
print(sd_comparison)
#-----------------------------------------------------------------------------------------------------#
############################################################################################################
# ============================================================
# POSTERIOR SAMPLES IN INLA: CIT Model
# ============================================================
cit_sample <- inla.posterior.sample(2000, cit_0)
 # Names of latent effects in the first posterior sample
 #latent_names <- rownames(cit_sample[[1]]$latent)
 #hyper_names  <- names(cit_sample[[1]]$hyperpar)
 
 #intercept_idx <- grep("Intercept", latent_names)
 latent_names <- rownames(cit_sample[[1]]$latent)
 #intercept_idx <- grep("temp_indoor:", latent_names)

 # intercept_samples <- sapply(cit_sample, function(s) {
 #   as.numeric(s$latent[intercept_idx])
 # })


 pred_idx <- grep("^Predictor:", latent_names)

 eta_mat <- sapply(cit_sample, function(s) s$latent[pred_idx, 1])

 eta_summary <- data.frame(
   predictor = latent_names[pred_idx],
   mean  = apply(eta_mat, 1, mean),
   sd    = apply(eta_mat, 1, sd),
   lo50  = apply(eta_mat, 1, quantile, probs = 0.25),
   med   = apply(eta_mat, 1, quantile, probs = 0.50),
   hi50  = apply(eta_mat, 1, quantile, probs = 0.75),
   lo95  = apply(eta_mat, 1, quantile, probs = 0.025),
   hi95  = apply(eta_mat, 1, quantile, probs = 0.975)
 )

 
 pred_df <- bind_cols(mos_rest, eta_summary) |>
   arrange(temp_indoor)

 prec <- cit_0$summary.hyperpar["Precision for the Gaussian observations", "mean"]
 sigma <- sqrt(1 / prec)


 pred_df <- pred_df %>%
   mutate(
     pred_lo50 = mean + qnorm(0.25) * sigma,
     pred_hi50 = mean + qnorm(0.75) * sigma,
     pred_lo95 = mean + qnorm(0.025) * sigma,
     pred_hi95 = mean + qnorm(0.975) * sigma

   )

#########################cit model plot #######################################
plt1 <-  ggplot() +
   geom_ribbon(data = pred_df,
               aes(x = temp_indoor, ymin = pred_lo95, ymax = pred_hi95),
               fill = "steelblue", alpha = 0.20) +
   geom_ribbon(data = pred_df,
               aes(x = temp_indoor, ymin = pred_lo50, ymax = pred_hi50),
               fill = "steelblue", alpha = 0.40) +
   geom_line(data = pred_df,
             aes(x = temp_indoor, y = mean),
             colour = "black", linewidth = 1.0) +
   geom_abline(intercept = 0, slope = 1,
               colour = "darkred", linetype = "dashed", linewidth = 0.8) +
   geom_point(data = mos_rest,
              aes(x = temp_indoor, y = temp),
              colour = "black", alpha = 0.25, size = 2.2) +
   # # Annotations
   # annotate("text", x = min(temp_indoor), 
   #          y = max(pred_df$temp),
   #          label = paste0("RMSE = ", round(vars_cit_0$residual_sd, 2)),
   #          hjust = 0, vjust = 1, size = 3.5) +
   labs(
     x = "CIT (°C)",
     y = expression(paste("MRST (", degree, "C)"))
   ) +
   scale_y_continuous(breaks = seq(15, 50, 5),  limits = c(15, 50)) +
   #scale_x_continuous(breaks = seq(24, 42, 2), limits = c(24, 42)) +
   theme_bw(base_size = 12) +
   theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
         panel.grid.major.y=element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(vjust = 0.5, size = 12),
         axis.text.y = element_text(vjust = 0.5, size = 12),
         legend.text = element_text(size = 12),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12)
         #legend.title = element_text(size = 12),
         #legend.position = "right"
         )  
 print(plt1)
###############################################################################
 # ============================================================
 # POSTERIOR SAMPLES IN INLA: Average Satellite Model
 # ============================================================
 sat_0_sample <- inla.posterior.sample(2000, avg_sat_0)
 # Names of latent effects in the first posterior sample
 #latent_names <- rownames(cit_sample[[1]]$latent)
 #hyper_names  <- names(cit_sample[[1]]$hyperpar)
 
 #intercept_idx <- grep("Intercept", latent_names)
 latent_names_sat <- rownames(sat_0_sample[[1]]$latent)
 #intercept_idx_sat <- grep("satelite_outdoor_temp:", latent_names_sat)
 
 # intercept_samples_sat <- sapply(sat_0_sample, function(s) {
 #   as.numeric(s$latent[intercept_idx_sat])
 # })
 
 
 pred_idx_sat <- grep("^Predictor:", latent_names_sat)
 
 eta_mat_sat <- sapply(sat_0_sample, function(s) s$latent[pred_idx_sat, 1])
 
 eta_summary_sat <- data.frame(
   predictor = latent_names[pred_idx_sat],
   mean  = apply(eta_mat_sat, 1, mean),
   sd    = apply(eta_mat_sat, 1, sd),
   lo50  = apply(eta_mat_sat, 1, quantile, probs = 0.25),
   med   = apply(eta_mat_sat, 1, quantile, probs = 0.50),
   hi50  = apply(eta_mat_sat, 1, quantile, probs = 0.75),
   lo95  = apply(eta_mat_sat, 1, quantile, probs = 0.025),
   hi95  = apply(eta_mat_sat, 1, quantile, probs = 0.975)
 )
 
 
 pred_df_sat <- bind_cols(mos_rest, eta_summary_sat) |>
   arrange(satelite_outdoor_temp)
 
 prec_sat <- avg_sat_0$summary.hyperpar["Precision for the Gaussian observations", "mean"]
 sigma_sat <- sqrt(1 / prec_sat)
 
 
 pred_df_sat <- pred_df_sat %>%
   mutate(
     pred_lo50 = mean + qnorm(0.25) * sigma_sat,
     pred_hi50 = mean + qnorm(0.75) * sigma_sat,
     pred_lo95 = mean + qnorm(0.025) * sigma_sat,
     pred_hi95 = mean + qnorm(0.975) * sigma_sat
     
   )
 
###################################################################################
 ######################### Average Sat model plot #######################################
 plt2 <- ggplot() +
   geom_ribbon(data = pred_df_sat,
               aes(x = satelite_outdoor_temp, ymin = pred_lo95, ymax = pred_hi95),
               fill = "steelblue", alpha = 0.20) +
   geom_ribbon(data = pred_df_sat,
               aes(x = satelite_outdoor_temp, ymin = pred_lo50, ymax = pred_hi50),
               fill = "steelblue", alpha = 0.40) +
   geom_line(data = pred_df_sat,
             aes(x = satelite_outdoor_temp, y = mean),
             colour = "black", linewidth = 1.0) +
   geom_abline(intercept = 0, slope = 1,
               colour = "darkred", linetype = "dashed", linewidth = 0.8) +
   geom_point(data = mos_rest,
              aes(x = satelite_outdoor_temp, y = temp),
              colour = "black", alpha = 0.25, size = 2.2) +
   # # Annotations
   # annotate("text", x = min(satelite_outdoor_temp), 
   #          y = max(pred_df_sat$temp),
   #          label = paste0("RMSE = ", round(vars_avg_sat_0$residual_sd, 2)),
   #          hjust = 0, vjust = 1, size = 3.5) +
   labs(
     x = "SDT (°C)",
     y = expression(paste("MRST (", degree, "C)"))
   ) +
   #scale_y_continuous(limits = c(20, 50)) +
   scale_y_continuous(breaks = seq(20, 50, 5),  limits = c(20, 50)) +
   #scale_x_continuous(breaks = seq(24, 28, 1), limits = c(24, 28)) +
   theme_bw(base_size = 12) +
   theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
         panel.grid.major.y=element_blank(),
         panel.grid.minor = element_blank(),
         axis.text.x = element_text(vjust = 0.5, size = 12),
         axis.text.y = element_text(vjust = 0.5, size = 12),
         legend.text = element_text(size = 12),
         axis.title.x = element_text(size = 12),
         axis.title.y = element_text(size = 12)
         #legend.title = element_text(size = 12),
         #legend.position = "right"
   )  

print(plt2)

###############################################################################
# ============================================================
# POSTERIOR SAMPLES IN INLA: minimum Satellite Model
# ============================================================
sat_min_sample <- inla.posterior.sample(2000, min_sat_0)
# Names of latent effects in the first posterior sample
#latent_names <- rownames(cit_sample[[1]]$latent)
#hyper_names  <- names(cit_sample[[1]]$hyperpar)

#intercept_idx <- grep("Intercept", latent_names)
latent_names_sat_min <- rownames(sat_min_sample[[1]]$latent)
intercept_idx_sat_min <- grep("sat_temp_min:", latent_names_sat_min)

intercept_samples_sat_min <- sapply(sat_min_sample, function(s) {
  as.numeric(s$latent[intercept_idx_sat_min])
})

pred_idx_sat_min <- grep("^Predictor:", latent_names_sat_min)

eta_mat_sat_min <- sapply(sat_min_sample, function(s) s$latent[pred_idx_sat_min, 1])

eta_summary_sat_min <- data.frame(
  predictor = latent_names[pred_idx_sat_min],
  mean  = apply(eta_mat_sat_min, 1, mean),
  sd    = apply(eta_mat_sat_min, 1, sd),
  lo50  = apply(eta_mat_sat_min, 1, quantile, probs = 0.25),
  med   = apply(eta_mat_sat_min, 1, quantile, probs = 0.50),
  hi50  = apply(eta_mat_sat_min, 1, quantile, probs = 0.75),
  lo95  = apply(eta_mat_sat_min, 1, quantile, probs = 0.025),
  hi95  = apply(eta_mat_sat_min, 1, quantile, probs = 0.975)
)


pred_df_sat_min <- bind_cols(mos_rest, eta_summary_sat_min) |>
  arrange(sat_temp_min)

prec_sat_min <- min_sat_0$summary.hyperpar["Precision for the Gaussian observations", "mean"]
sigma_sat_min <- sqrt(1 / prec_sat_min)


pred_df_sat_min <- pred_df_sat_min %>%
  mutate(
    pred_lo50 = mean + qnorm(0.25) * sigma_sat_min,
    pred_hi50 = mean + qnorm(0.75) * sigma_sat_min,
    pred_lo95 = mean + qnorm(0.025) * sigma_sat_min,
    pred_hi95 = mean + qnorm(0.975) * sigma_sat_min
    
  )

###################################################################################
#########################sat_min model plot #######################################
plt3 <- ggplot() +
  geom_ribbon(data = pred_df_sat_min,
              aes(x = sat_temp_min, ymin = pred_lo95, ymax = pred_hi95),
              fill = "steelblue", alpha = 0.20) +
  geom_ribbon(data = pred_df_sat_min,
              aes(x = sat_temp_min, ymin = pred_lo50, ymax = pred_hi50),
              fill = "steelblue", alpha = 0.40) +
  geom_line(data = pred_df_sat_min,
            aes(x = sat_temp_min, y = mean),
            colour = "black", linewidth = 1.0) +
  geom_abline(intercept = 0, slope = 1,
              colour = "darkred", linetype = "dashed", linewidth = 0.8) +
  geom_point(data = mos_rest,
             aes(x = sat_temp_min, y = temp),
             colour = "black", alpha = 0.25, size = 2.2) +
  # # Annotations
  # annotate("text", x = min(sat_temp_min), 
  #          y = max(pred_df_sat_min$temp),
  #          label = paste0("RMSE = ", round(vars_min_sat_0$residual_sd, 2)),
  #          hjust = 0, vjust = 1, size = 3.5) +
  labs(
    x = "Minimun SDT (°C)",
    y = expression(paste("MRST (", degree, "C)"))
  ) +
  theme_bw(base_size = 12) +
  scale_y_continuous(breaks = seq(20, 50, 5),  limits = c(20, 50)) +
  #scale_x_continuous(breaks = seq(22, 26, 1), limits = c(22, 26)) +
  theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
        panel.grid.major.y=element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, size = 12),
        axis.text.y = element_text(vjust = 0.5, size = 12),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
        #legend.title = element_text(size = 12),
        #legend.position = "right"
  )  

print(plt3)

###############################################################################
# ============================================================
# POSTERIOR SAMPLES IN INLA: Maximum Satellite Model
# ============================================================
sat_max_sample <- inla.posterior.sample(2000, max_sat_0)
# Names of latent effects in the first posterior sample
#latent_names <- rownames(cit_sample[[1]]$latent)
#hyper_names  <- names(cit_sample[[1]]$hyperpar)

#intercept_idx <- grep("Intercept", latent_names)
latent_names_sat_max <- rownames(sat_max_sample[[1]]$latent)
# intercept_idx_sat_min <- grep("sat_temp_max:", latent_names_sat_min)
# 
# intercept_samples_sat_min <- sapply(sat_min_sample, function(s) {
#   as.numeric(s$latent[intercept_idx_sat_min])
# })

pred_idx_sat_max <- grep("^Predictor:", latent_names_sat_max)

eta_mat_sat_max <- sapply(sat_max_sample, function(s) s$latent[pred_idx_sat_max, 1])

eta_summary_sat_max <- data.frame(
  predictor = latent_names[pred_idx_sat_max],
  mean  = apply(eta_mat_sat_max, 1, mean),
  sd    = apply(eta_mat_sat_max, 1, sd),
  lo50  = apply(eta_mat_sat_max, 1, quantile, probs = 0.25),
  med   = apply(eta_mat_sat_max, 1, quantile, probs = 0.50),
  hi50  = apply(eta_mat_sat_max, 1, quantile, probs = 0.75),
  lo95  = apply(eta_mat_sat_max, 1, quantile, probs = 0.025),
  hi95  = apply(eta_mat_sat_max, 1, quantile, probs = 0.975)
)


pred_df_sat_max <- bind_cols(mos_rest, eta_summary_sat_max) |>
  arrange(sat_temp_max)

prec_sat_max <- max_sat_0$summary.hyperpar["Precision for the Gaussian observations", "mean"]
sigma_sat_max <- sqrt(1 / prec_sat_max)


pred_df_sat_max <- pred_df_sat_max %>%
  mutate(
    pred_lo50 = mean + qnorm(0.25) * sigma_sat_max,
    pred_hi50 = mean + qnorm(0.75) * sigma_sat_max,
    pred_lo95 = mean + qnorm(0.025) * sigma_sat_max,
    pred_hi95 = mean + qnorm(0.975) * sigma_sat_max
    
  )

###################################################################################
#########################maximum sat model plot #######################################
plt4 <- ggplot() +
  geom_ribbon(data = pred_df_sat_max,
              aes(x = sat_temp_max, ymin = pred_lo95, ymax = pred_hi95),
              fill = "steelblue", alpha = 0.20) +
  geom_ribbon(data = pred_df_sat_max,
              aes(x = sat_temp_max, ymin = pred_lo50, ymax = pred_hi50),
              fill = "steelblue", alpha = 0.40) +
  geom_line(data = pred_df_sat_max,
            aes(x = sat_temp_max, y = mean),
            colour = "black", linewidth = 1.0) +
  geom_abline(intercept = 0, slope = 1,
              colour = "darkred", linetype = "dashed", linewidth = 0.8) +
  geom_point(data = mos_rest,
             aes(x = sat_temp_max, y = temp),
             colour = "black", alpha = 0.25, size = 2.2) +
  # Annotations
  # annotate("text", x = min(sat_temp_max), 
  #          y = max(pred_df_sat_max$temp),
  #          label = paste0("RMSE = ", formatC(vars_max_sat_0$residual_sd,
  #                                            format = "f", digits = 2)),
  #          hjust = 0, vjust = 1, size = 3.5) +
  labs(
    x = "Maximun SDT (°C)",
    y = expression(paste("MRST (", degree, "C)"))
  ) +
  scale_y_continuous(breaks = seq(20, 50, 5),  limits = c(20, 50)) +
  #scale_x_continuous(breaks = seq(27, 34, 1), limits = c(27, 34)) +
  theme_bw(base_size = 12) +
  theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
        panel.grid.major.y=element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(vjust = 0.5, size = 12),
        axis.text.y = element_text(vjust = 0.5, size = 12),
        legend.text = element_text(size = 12),
        axis.title.x = element_text(size = 12),
        axis.title.y = element_text(size = 12)
        #legend.title = element_text(size = 12),
        #legend.position = "right"
  )  

print(plt4)

#===============================================================================#
###############################################################################

# Arrange
library(patchwork)
combine_plt <- (plt1| plt2 ) / (plt3|plt4) + 
  plot_layout(guides = "collect") +
  plot_annotation( 
    tag_levels = "I",
    caption = "Data Source: GAEC-BNARI",               
    #    = "Mosquito resting site temperature against temperature predictors with no random effects",
    # subtitle = paste0("Best-fit line with 50% (dark) and 95% (light) credible intervals | Dashed = 1:1 line\n",
    #                   "★ Best model (lowest RMSE): ", best_rmse_model, 
    #                   " (RMSE = ", round(rmse_comparison$RMSE[best_rmse_idx], 3), "°C)"),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 13, colour = "grey40"),
      plot.caption =  element_text(face = "italic")
    )
  )
print(combine_plt)

#===============================================================================#
# Save outputs
ggsave("figure/temp_models_nre.pdf",
       combine_plt, width = 12, height = 10, units = "in", dpi = 300)
ggsave("figure/temp_models_nre.png",
       combine_plt, width = 12, height = 10, units = "in", dpi = 300)

#================================================================================#
# Save RMSE comparison to CSV
write.csv(rmse_comparison, "table_results/rmse_model_comparison.csv", row.names = FALSE)
write.csv(comprehensive_comparison, "table_results/comprehensive_model_comparison.csv", row.names = FALSE)
print("\nRMSE comparison saved to: table_results/rmse_model_comparison.csv")
print("Comprehensive comparison saved to: table_results/comprehensive_model_comparison.csv")


# Store model parameters for each model
model_parameters <- list(
  cit_0 = cit_0$summary.fixed,
  avg_sat_0 = avg_sat_0$summary.fixed,
  min_sat_0 = min_sat_0$summary.fixed,
  max_sat_0 = max_sat_0$summary.fixed
)

print("\nModel parameters stored for further analysis.")

# Save model results to files
write.csv(model_comparison, "table_results/model_comparison.csv", row.names = FALSE)

# Create a summary of the best models
best_models_summary <- data.frame(
  Model_Name = c("Model_2", "Model_4", "Model_6", "Model_8"),
  Formula = c("MRST ~ 0 + temp_indoor + (ar1|days_count) + (1|village)",
              "MRST ~ 0 + sat_temp_mean + (ar1|days_count) + (1|village)",
              "MRST ~ 0 + sat_temp_min + (ar1|days_count) + (1|village)",
              "MRST ~ 0 + sat_temp_max + (ar1|days_count) + (1|village)"),
  WAIC = c(cit_0$waic$waic, avg_sat_0$waic$waic, min_sat_0$waic$waic, max_sat_0$waic$waic),
  MLIK = c(cit_0$mlik[1,1], avg_sat_0$mlik[1,1], min_sat_0$mlik[1,1], max_sat_0$mlik[1,1])
)

write.csv(best_models_summary, "table_results/best_models_summary.csv", row.names = FALSE)

# Function to extract fixed and random effects with credible intervals
extract_effects_with_ci <- function(model, model_name) {
  # Extract fixed effects
  fixed <- model$summary.fixed
  if (!is.null(fixed) && nrow(fixed) > 0) {
    fixed <- as.data.frame(fixed)
    fixed$Effect_Type <- "Fixed"
    fixed$Effect_Name <- rownames(fixed)
    fixed$Model <- model_name
  } else {
    fixed <- data.frame()
  }
  
  # Extract random effects (hyperparameters)
  random <- model$summary.hyperpar
  if (!is.null(random) && nrow(random) > 0) {
    random <- as.data.frame(random)
    random$Effect_Type <- "Random"
    random$Effect_Name <- rownames(random)
    random$Model <- model_name
  } else {
    random <- data.frame()
  }
  
  # Combine fixed and random effects
  if (nrow(fixed) > 0 && nrow(random) > 0) {
    # Ensure both have same columns
    all_cols <- union(names(fixed), names(random))
    for (col in all_cols) {
      if (!col %in% names(fixed)) fixed[[col]] <- NA
      if (!col %in% names(random)) random[[col]] <- NA
    }
    combined <- rbind(fixed[, all_cols, drop = FALSE], random[, all_cols, drop = FALSE])
  } else if (nrow(fixed) > 0) {
    combined <- fixed
  } else if (nrow(random) > 0) {
    combined <- random
  } else {
    return(data.frame())
  }
  
  # Select and rename relevant columns
  if (nrow(combined) > 0) {
    result <- data.frame(
      Model = combined$Model,
      Effect_Type = combined$Effect_Type,
      Effect_Name = combined$Effect_Name,
      Mean = combined$mean,
      SD = combined$sd,
      Lower_CI_0.025 = combined$`0.025quant`,
      Upper_CI_0.975 = combined$`0.975quant`,
      Median = combined$`0.5quant`,
      Mode = combined$mode
    )
    return(result)
  } else {
    return(data.frame())
  }
}

# Extract effects for all models
all_models_list <- list(
  cit_0 = cit_0, avg_sat_0 = avg_sat_0, min_sat_0 = min_sat_0, max_sat_0 = max_sat_0
)

# Apply extraction function to all models
all_effects <- dplyr::bind_rows(lapply(names(all_models_list), function(name) {
  extract_effects_with_ci(all_models_list[[name]], name)
}))

# Write comprehensive effects to CSV
write.csv(all_effects, "table_results/all_models_effects_with_ci.csv", row.names = FALSE)

# Function to extract model predictions with credible intervals for best models
extract_predictions_with_ci <- function(model, model_name, data) {
  # Get linear predictor (fitted values) with quantiles
  lp <- model$summary.linear.predictor
  
  if (!is.null(lp) && nrow(lp) > 0) {
    result <- data.frame(
      Model = rep(model_name, nrow(lp)),
      Observation_ID = 1:nrow(lp),
      Fitted_Mean = lp$mean,
      Fitted_SD = lp$sd,
      Fitted_Lower_CI_0.025 = lp$`0.025quant`,
      Fitted_Upper_CI_0.975 = lp$`0.975quant`,
      Fitted_Median = lp$`0.5quant`,
      Fitted_Mode = lp$mode
    )
    
    # Add observed response if available
    if (!is.null(data$temp)) {
      result$Observed_Temp = data$temp
    }
    if (!is.null(data$height_from_ground)) {
      result$Observed_Height = data$height_from_ground
    }
    if (!is.null(data$dist_from_nearest_wall)) {
      result$Observed_Distance = data$dist_from_nearest_wall
    }
    
    return(result)
  } else {
    return(data.frame())
  }
}

# Extract predictions for best models (Models 1, 3, 5, 7)
best_models_predictions <- do.call(rbind, list(
  extract_predictions_with_ci(cit_0, "Model_2", mos_rest),
  extract_predictions_with_ci(avg_sat_0, "Model_4", mos_rest),
  extract_predictions_with_ci(min_sat_0, "Model_6", mos_rest),
  extract_predictions_with_ci(max_sat_0, "Model_8", mos_rest)
))

# Write predictions to CSV
write.csv(best_models_predictions, "table_results/best_models_predictions_with_ci.csv", row.names = FALSE)

print("\nResults saved to results/ directory.")
print(paste("Fixed and random effects with credible intervals saved to: results/all_models_effects_with_ci.csv"))
print(paste("Total effects extracted:", nrow(all_effects)))
print(paste("Best models predictions with credible intervals saved to: results/best_models_predictions_with_ci.csv"))
print(paste("Total predictions extracted:", nrow(best_models_predictions)))


#=================================================================================================#
########################################################################################
#========================================================================================================
#this produces the results without step size but uses the unique mean Satellite temperature values between the 
#min and max sat temp
#=====================================================================================================
# Load packages
# -------
library(dplyr)
library(openxlsx)

# ------------------------------------------------------------
# Automatically extract model quantities
# ------------------------------------------------------------
beta_sat <- avg_sat_0$summary.fixed["satelite_outdoor_temp", "mean"]

residual_precision <- avg_sat_0$summary.hyperpar[
  "Precision for the Gaussian observations", "mean"]
residual_sd <- 1 / sqrt(residual_precision)

n_mosquitoes <- 431
seed_value <- 123
output_file <- "data/Simulated_mosquito_temperature_data_current.xlsx"
set.seed(seed_value)

# -------------------------
# Extract date-level observed satellite-derived temperatures
# (keep all rows, including duplicates — one per date/observation)
# -------------------------
observed_sat_temp <- mos_rest$satelite_outdoor_temp

# Remove missing values
valid_idx <- !is.na(observed_sat_temp)
observed_sat_temp <- observed_sat_temp[valid_idx]
observed_dates <- mos_rest$date[valid_idx]  # retain corresponding dates if needed

# -------------------------
# Expected means table
# -------------------------
expected_means <- data.frame(
  date = observed_dates,
  observed_date_level_SDT = observed_sat_temp,
  model_predicted_Mean_Temp = beta_sat * observed_sat_temp
)

# -------------------------
# Build wide simulated temperature table
# Rows = mosquito IDs
# Columns = each date-level observed SDT
# -------------------------
sim_matrix <- sapply(observed_sat_temp, function(temp_val) {
  estimated_mean <- beta_sat * temp_val
  rnorm(
    n = n_mosquitoes,
    mean = estimated_mean,
    sd = residual_sd
  )
})

# Convert to data frame
simulated_temp_wide <- as.data.frame(sim_matrix)

# Use date-level SDT as column names
col_names <- paste0("SDT_", format(observed_sat_temp, nsmall = 2), "_", seq_along(observed_sat_temp))
colnames(simulated_temp_wide) <- col_names

# -------------------------
# Compute mean_simulated_MRST per date-level SDT column
# -------------------------
mean_simulated_MRST <- colMeans(simulated_temp_wide)
expected_means$mean_simulated_MRST <- mean_simulated_MRST

# -------------------------
# Add mosquito IDs as first column
# -------------------------
simulated_temp_wide <- cbind(
  mosquito_ID = paste0("mosquito", 1:n_mosquitoes),
  simulated_temp_wide
)

#simulated_temp_wide <- cbind(expected_means, simulated_temp_wide)

###################################################
# # modifying 
# mos_rest_v3 <- mos_rest_v2 %>% 
#   select(ecological_zone, locality, village, date, days_count, point_x, point_y, temp,
#          temp_indoor, satelite_outdoor_temp, sat_temp_min, sat_temp_max,
#          mosquito_ID, SDT_24.11, SDT_24.23,SDT_24.90,SDT_24.94,SDT_25.26,
#          SDT_25.43, SDT_25.60,SDT_25.68,SDT_25.71,SDT_26.38,SDT_27.19, 
#          SDT_27.27,SDT_27.36,SDT_27.64, SDT_27.90)%>% 
#   group_by(days_count, date, village, ecological_zone, locality,point_x, point_y) %>% 
#   summarise(n_count = n(), 
#             mean_temp = mean(temp),
#             mean_sat_temp = mean(satelite_outdoor_temp),
#             m_sat_temp_min = mean(sat_temp_min),
#             m_sat_temp_max = mean(sat_temp_max),
#             min_SDT_24.11 = mean(SDT_24.11),
#             max_SDT_27.90 = mean(SDT_27.90)) 
# 
# mos_rest_v3 <- bind_cols(mos_rest_v3, expected_means)

#mos_rest_v2 <- cbind(mos_rest, simulated_temp_wide)

# Save CSV
# write.csv(expected_means, "tables/expected_means_simulated_sat.csv", row.names = FALSE)
# 
# 
# simulated_MRST <- read.csv("tables/simulated_sat_v1.csv")
# mean_expected_simulated_MRST <- read.csv("tables/expected_means_simulated_sat.csv")
# 
# mean_expected_simulated_MRST <- mean_expected_simulated_MRST %>% 
#   select(observed_date_level_SDT,model_predicted_Mean_Temp,mean_simulated_MRST)
# 
# 
# simulated_mrst <- cbind(simulated_MRST,mean_expected_simulated_MRST) 
# 
# 
# write.csv(simulated_mrst, "tables/simulated_sat_v3.csv")



# -------------------------
# Inputs summary sheet
# -------------------------
inputs_summary <- data.frame(
  parameter = c(
    "model",
    "beta_sat",
    "residual_sd",
    "n_mosquitoes",
    "n_date_level_observations",
    "min_observed_sat_temp",
    "max_observed_sat_temp",
    "random_seed"
  ),
  value = c(
    "temp ~ 0 + satelite_outdoor_temp",
    beta_sat,
    residual_sd,
    n_mosquitoes,
    length(observed_sat_temp),
    min(observed_sat_temp),
    max(observed_sat_temp),
    seed_value
  )
)

# -------------------------
# Save to Excel
# -------------------------
wb <- createWorkbook()
addWorksheet(wb, "Simulated_Temp")
addWorksheet(wb, "Inputs_Summary")
addWorksheet(wb, "Expected_Means")

writeData(wb, "Simulated_Temp", simulated_temp_wide)
writeData(wb, "Inputs_Summary", inputs_summary)
writeData(wb, "Expected_Means", expected_means)

saveWorkbook(wb, output_file, overwrite = TRUE)
