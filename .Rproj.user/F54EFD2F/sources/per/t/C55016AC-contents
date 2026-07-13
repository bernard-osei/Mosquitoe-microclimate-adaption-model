#install.packages("pacman")

library(readxl)
library(janitor)
library(dplyr)
library(INLA)
library(ggplot2)
library(patchwork)

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


########################################################################
colSums(is.na(mos_rest)) #checking for missing entry
summary(mos_rest)

# Prepare data for modeling
# Add observation ID if not present
if (!"observation_id" %in% names(mos_rest)) {
  mos_rest$observation_id <- 1:nrow(mos_rest)
}

# Ensure days_count is treated as factor for random effect
mos_rest$days_count_factor <- as.integer(mos_rest$days_count)
#mos_rest$observation_id <- as.integer(mos_rest$observation_id)

# Define PC prior for random effects (penalized complexity prior)
pc_prior <- list(prec = list(prior = "pc.prec", param = c(2, 0.05)))

# Define priors for fixed effects (Gaussian priors for continuous, default for categorical)
# Using a general prior for all continuous predictors
fixed_prior_general <- list(mean = 0, prec = 0.1)  # General weakly informative prior

# Prior for residual error variance (control.family)
var_eps <- 0.2  # For residual error variance prior
family_prior <- list(hyper = list(prec = list(prior = "gaussian", param = c(0, var_eps))))


model2 <- inla(temp ~ 0 + temp_indoor + 
              f(days_count_factor, model = "iid", hyper = pc_prior) + 
              f(village, model ="iid", hyper = pc_prior),
              data = mos_rest,
              family = "gaussian",
              control.fixed = fixed_prior_general,
              control.family = family_prior,
              control.predictor = list(compute = TRUE),
              control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                     return.marginals.predictor = TRUE))


# Model 3: MRST ~ satelite_outdoor_temp + (1|days_count_factor) + (1|observation) + (1|village)
model4 <- inla(temp ~ 0 + satelite_outdoor_temp + 
              f(days_count_factor, model = "iid", hyper = pc_prior) + 
              f(village, model ="iid", hyper = pc_prior),
              data = mos_rest,
              family = "gaussian",
              control.fixed = fixed_prior_general,
              control.family = family_prior,
              control.predictor = list(compute = TRUE),
              control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                     return.marginals.predictor = TRUE))



# Model 5: MRST ~ sat_temp_min + (1|days_count_factor) + (1|observation) + (1|village)
model6 <- inla(temp ~ 0 + sat_temp_min + 
              f(days_count_factor, model = "ar1", hyper = pc_prior) + 
              f(village, model ="iid", hyper = pc_prior),
              data = mos_rest,
              family = "gaussian",
              control.fixed = fixed_prior_general,
              control.family = family_prior,
              control.predictor = list(compute = TRUE),
              control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                     return.marginals.predictor = TRUE))


# Model 7: MRST ~ sat_temp_max + (1|days_count_factor) + (1|observation) + (1|village)
model8 <- inla(temp ~ 0 + sat_temp_max + 
              f(days_count_factor, model = "ar1", hyper = pc_prior) + 
              f(village, model ="iid", hyper = pc_prior),
              data = mos_rest,
              family = "gaussian",
              control.fixed = fixed_prior_general,
              control.family = family_prior,
              control.predictor = list(compute = TRUE),
              control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                     return.marginals.predictor = TRUE))



print("\nModel2 Summary:")
print(model2$summary.fixed)
print(paste("WAIC:", model2$waic$waic))
print(paste("MLIK:", model2$mlik[1,1]))



print("\nModel4 Summary:")
print(model4$summary.fixed)
print(paste("WAIC:", model4$waic$waic))
print(paste("MLIK:", model4$mlik[1,1]))



print("\nModel6 Summary:")
print(model6$summary.fixed)
print(paste("WAIC:", model6$waic$waic))
print(paste("MLIK:", model6$mlik[1,1]))



print("\nModel8 Summary:")
print(model8$summary.fixed)
print(paste("WAIC:", model8$waic$waic))
print(paste("MLIK:", model8$mlik[1,1]))

# Model comparison
model_comparison <- data.frame(
  Model = c("Model2", "Model4", "Model6", "Model8"),
  WAIC = c(model2$waic$waic, model4$waic$waic, model6$waic$waic, model8$waic$waic),
  MLIK = c(model2$mlik[1,1], model4$mlik[1,1], model6$mlik[1,1], model8$mlik[1,1])
)

print("\nModel Comparison Table:")
print(model_comparison)

#=============================================================================
# DISTANCE-BASED MODEL EVALUATION (RMSE Approach)
#=============================================================================

# Create model functions for each predictor
# These functions take coefficients and data as inputs, return predicted values

model_func_2 <- function(a, data) {
  # a[1] = slope for temp_indoor
  a[1] * data$temp_indoor
}

model_func_4 <- function(a, data) {
  # a[1] = slope for satelite_outdoor_temp
  a[1] * data$satelite_outdoor_temp
}

model_func_6 <- function(a, data) {
  # a[1] = slope for sat_temp_min
  a[1] * data$sat_temp_min
}

model_func_8 <- function(a, data) {
  # a[1] = slope for sat_temp_max
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
coefs_model2 <- c(model2$summary.fixed["temp_indoor", "mean"])
coefs_model4 <- c(model4$summary.fixed["satelite_outdoor_temp", "mean"])
coefs_model6 <- c(model6$summary.fixed["sat_temp_min", "mean"])
coefs_model8 <- c(model8$summary.fixed["sat_temp_max", "mean"])

# Compute RMSE for each model
rmse_model2 <- measure_distance(model_func_2, coefs_model2, mos_rest)
rmse_model4 <- measure_distance(model_func_4, coefs_model4, mos_rest)
rmse_model6 <- measure_distance(model_func_6, coefs_model6, mos_rest)
rmse_model8 <- measure_distance(model_func_8, coefs_model8, mos_rest)

# Create RMSE comparison table
rmse_comparison <- data.frame(
  Model = c("Model2", "Model4", "Model6", "Model8"),
  RMSE = c(rmse_model2, rmse_model4, rmse_model6, rmse_model8),
  Description = c("Indoor temp + RE", 
                  "Sat mean temp + RE", 
                  "Sat min temp + RE", 
                  "Sat max temp + RE")
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
  Model = c("Model2", "Model4", "Model6", "Model8"),
  Description = c("Indoor temp + RE", "Sat mean temp + RE", "Sat min temp + RE", "Sat max temp + RE"),
  WAIC = c(model2$waic$waic, model4$waic$waic, model6$waic$waic, model8$waic$waic),
  RMSE = c(rmse_model2, rmse_model4, rmse_model6, rmse_model8),
  Slope = c(coefs_model2[1], coefs_model4[1], coefs_model6[1], coefs_model8[1])
)

print("\n=== Comprehensive Model Comparison (WAIC vs RMSE) ===")
print(comprehensive_comparison)
###########################################################################################################
#---------------------------------------------------------------------------------------------------------#

########################################################################################################
# Function to extract variances AND beta SD from INLA model
extract_variances <- function(model, predictor_name = NULL) {
  
  # Extract posterior mean precisions
  prec_resid  <- model$summary.hyperpar["Precision for the Gaussian observations", "mean"]
  prec_day    <- model$summary.hyperpar["Precision for days_count_factor", "mean"]
  prec_village<- model$summary.hyperpar["Precision for village", "mean"]
  
  # Convert to variances
  var_resid   <- 1 / prec_resid
  var_day     <- 1 / prec_day
  var_village <- 1 / prec_village
  
  # Create results list
  variances <- list(
    residual_variance = var_resid,
    day_variance = var_day,
    village_variance = var_village,
    total_variance = var_resid + var_day + var_village,
    
    # Standard deviations
    residual_sd = sqrt(var_resid),
    day_sd = sqrt(var_day),
    village_sd = sqrt(var_village),
    total_sd = sqrt(var_resid + var_day + var_village)
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
vars_model2 <- extract_variances(model2, predictor_name = "temp_indoor")
vars_model4 <- extract_variances(model4, predictor_name = "satelite_outdoor_temp")
vars_model6 <- extract_variances(model6, predictor_name = "sat_temp_min")
vars_model8 <- extract_variances(model8, predictor_name = "sat_temp_max")

# Compare variances across models with beta SD
compare_variances <- function(model_list, model_names, predictor_names) {
  
  results <- data.frame()
  
  for (i in 1:length(model_list)) {
    vars <- extract_variances(model_list[[i]], predictor_names[i])
    
    results <- rbind(results, data.frame(
      Model = model_names[i],
      Predictor = predictor_names[i],
      Beta_Mean = vars$beta_mean,
      Beta_SD = vars$beta_sd,
      Beta_Lower = vars$beta_ci_lower,
      Beta_Upper = vars$beta_ci_upper,
      Residual_SD = vars$residual_sd,
      Day_SD = vars$day_sd,
      Village_SD = vars$village_sd,
      Total_SD = vars$total_sd
    ))
  }
  
  return(results)
}

# Usage:
model_list <- list(model2, model4, model6, model8)
model_names <- c("Model2 (Indoor)", "Model4 (Sat Mean)", 
                 "Model6 (Sat Min)", "Model8 (Sat Max)")
predictor_names <- c("temp_indoor", "satelite_outdoor_temp", 
                     "sat_temp_min", "sat_temp_max")

variance_comparison <- compare_variances(model_list, model_names, predictor_names)
print(variance_comparison)


#==================================================================================#
####################### Visualization of models ############################
####################################################################################
# Visualization: Temperature comparison plots with standardized axis (20-50 degrees)
#par(mfrow=c(2, 2))

# # Helper: build prediction data frame for a model given predictor range
build_pred_data <- function(model, predictor_col, predictor_label, n = 200) {
  beta        <- model$summary.fixed[predictor_col, "mean"]
  beta_sd     <- model$summary.fixed[predictor_col, "sd"]
  resid_sd <- sqrt(1/model$summary.hyperpar["Precision for the Gaussian observations", "mean"])
  x_seq       <- seq(20, 50, length.out = n)
  fitted_mean <- beta * x_seq 
  # 95% CI: mean ± 1.96 * SE (propagated from fixed-effect posterior)
  fitted_lo95 <- (beta - 1.96 * beta_sd) * x_seq 
  fitted_hi95 <- (beta + 1.96 * beta_sd) * x_seq
  # 50% CI: mean ± 0.674 * SE
  fitted_lo50 <- (beta - 0.674 * beta_sd) * x_seq
  fitted_hi50 <- (beta + 0.674 * beta_sd) * x_seq
  data.frame(
    x        = x_seq,
    mean     = fitted_mean,
    lo95     = fitted_lo95,
    hi95     = fitted_hi95,
    lo50     = fitted_lo50,
    hi50     = fitted_hi50
  )
}

# Helper: single uncertainty plot
make_uncertainty_plot <- function(model, obs_data, predictor_col,
                                  x_label, panel_label, waic_val, rmse_val = NULL) {
  pred_df <- build_pred_data(model, predictor_col)
  obs_df  <- data.frame(
    x = obs_data[[predictor_col]],
    y = obs_data$temp
  )
  obs_df <- obs_df[!is.na(obs_df$x) & !is.na(obs_df$y), ]
  
  # Build subtitle with RMSE if provided
  if (!is.null(rmse_val)) {
    subtitle_text <- paste0("WAIC = ", formatC(waic_val, format = "f", digits =  2),
                            " | Residual_SD = ", formatC(rmse_val,format = 'f', digits =  2), "°C")
  } else {
    subtitle_text <- paste0("WAIC = ", formatC(waic_val, format = 'f',  digits =  2))
  }

  ggplot() +
    # 95% credible band (lightest)
    geom_ribbon(data = pred_df,
                aes(x = x, ymin = lo95, ymax = hi95),
                fill = "steelblue", alpha = 0.18) +
    # 50% credible band (darker)
    geom_ribbon(data = pred_df,
                aes(x = x, ymin = lo50, ymax = hi50),
                fill = "steelblue", alpha = 0.40) +
    # Observed points
    geom_point(data = obs_df,
               aes(x = x, y = y),
               colour = "black", alpha = 0.2, size = 2.5) +
    # Best-fit line
    geom_line(data = pred_df,
              aes(x = x, y = mean),
              colour = "black", linewidth = 1.0) +
    # 1:1 reference line
    geom_abline(intercept = 0, slope = 1,
                colour = "darkred", linetype = "dashed", linewidth = 0.8) +
    scale_x_continuous(limits = c(20, 50)) +
    scale_y_continuous(limits = c(20, 50)) +
    labs(
      title    = panel_label,
      subtitle = subtitle_text,
      x        = x_label,
      y        = expression(paste("MRST (", degree, "C)"))
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title    = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      #axis.title    = element_text(face = ""bold),
      panel.grid.minor = element_blank(),
      panel.border  = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
}

# Build the four panels with RMSE values
p2 <- make_uncertainty_plot(
  model2, mos_rest, "temp_indoor",
  expression(paste("Average CIT (", degree, "C)")),
  "A: Average Community Indoor Temperature", model2$waic$waic ,vars_model2$total_sd #rmse_model2
)

p4 <- make_uncertainty_plot(
  model4, mos_rest, "satelite_outdoor_temp",
  expression(paste("Average SDT (", degree, "C)")),
  "B: Average Satellite-Derived Temperature", model4$waic$waic,vars_model4$total_sd #rmse_model4
)

p6 <- make_uncertainty_plot(
  model6, mos_rest, "sat_temp_min",
  expression(paste("Minimum SDT (", degree, "C)")),
  "C: Minimun Satellite-Derived Temperature", model6$waic$waic,vars_model6$total_sd #rmse_model6
)

p8 <- make_uncertainty_plot(
  model8, mos_rest, "sat_temp_max",
  expression(paste("Maximum SDT (", degree, "C)")),
  "D: Maximum Satellite-Derived Temperature", model8$waic$waic,vars_model8$total_sd #rmse_model8
)
#=======================================================================================
# Combine into 2x2 grid
#=======================================================================================
combined_plot <- (p2 + p4) / (p6 + p8) +
  plot_annotation(
    title    = "Mosquito Resting Site Temperature vs Temperature Predictors while accounting for random effect",
    subtitle = paste0("Best-fit line with 50% (dark) and 95% (light) credible intervals | Dashed = 1:1 line\n",
                      "Best model (lowest RMSE): ", best_rmse_model, 
                      " (RMSE = ", round(rmse_comparison$RMSE[best_rmse_idx], 3), "°C)"),
    theme = theme(
      plot.title    = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

print(combined_plot)

###########################################
# Save outputs
ggsave("results/temp_models.pdf",
       combined_plot, width = 12, height = 10, units = "in", dpi = 300)
ggsave("results/temp_models.png",
       combined_plot, width = 12, height = 10, units = "in", dpi = 300)

##########################################################################################################
### Second Method of visualization the models using the inla.sample approach ###########
#@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@@
# ============================================================
# POSTERIOR SAMPLES IN INLA for the different Models built
# ============================================================

#===========================================================
# Helper: build posterior predictive data for a straight-line INLA model
#===========================================================
build_pred_data <- function(model, predictor_col, n = 200, n_samp = 2000,
                            x_min = 20, x_max = 50) {
  
  # x-grid
  x_seq <- seq(x_min, x_max, length.out = n)
  
  # posterior samples
  post_samps <- inla.posterior.sample(n_samp, model)
  
  # latent names
  latent_names <- rownames(post_samps[[1]]$latent)
  
  # find slope index
 # beta_idx <- grep(paste0("^", predictor_col, "$"), latent_names)
  beta_idx <- grep(predictor_col, latent_names)
  
  if (length(beta_idx) == 0) {
    stop(paste("Could not find coefficient for", predictor_col, "in latent effects."))
  }
  
  # extract slope samples
  beta_samps <- sapply(post_samps, function(s) as.numeric(s$latent[beta_idx, 1]))
  
  # extract Gaussian precision samples if available
  hyper_names <- names(post_samps[[1]]$hyperpar)
  prec_idx <- grep("Precision for the Gaussian observations", hyper_names)
  
  if (length(prec_idx) == 0) {
    stop("Could not find Gaussian observation precision in posterior samples.")
  }
  
  prec_samps <- sapply(post_samps, function(s) as.numeric(s$hyperpar[prec_idx]))
  sigma_samps <- sqrt(1 / prec_samps)
  
  # posterior mean line samples: each column is one posterior sample
  mu_mat <- outer(x_seq, beta_samps)   # n x n_samp
  
  # posterior predictive samples
  yrep_mat <- matrix(
    rnorm(length(mu_mat), mean = c(mu_mat), sd = rep(sigma_samps, each = n)),
    nrow = n,
    ncol = n_samp
  )
  
  # summarize mean line (straight fitted line)
  mean_fit  <- apply(mu_mat, 1, mean)
  
  # summarize predictive intervals
  pred_lo95 <- apply(yrep_mat, 1, quantile, probs = 0.025)
  pred_hi95 <- apply(yrep_mat, 1, quantile, probs = 0.975)
  pred_lo50 <- apply(yrep_mat, 1, quantile, probs = 0.25)
  pred_hi50 <- apply(yrep_mat, 1, quantile, probs = 0.75)
  
  data.frame(
    x = x_seq,
    mean = mean_fit,
    lo95 = pred_lo95,
    hi95 = pred_hi95,
    lo50 = pred_lo50,
    hi50 = pred_hi50
  )
}

#===========================================================
# Helper: single uncertainty plot
#===========================================================
make_uncertainty_plot <- function(model, obs_data, predictor_col,
                                  x_label, panel_label, waic_val, rmse_val = NULL,
                                  n = 200, n_samp = 2000,
                                  x_min = 20, x_max = 50,
                                  y_min = 20, y_max = 50) {
  
  pred_df <- build_pred_data(
    model = model,
    predictor_col = predictor_col,
    n = n,
    n_samp = n_samp,
    x_min = x_min,
    x_max = x_max
  )
  
  obs_df <- data.frame(
    x = obs_data[[predictor_col]],
    y = obs_data$temp
  )
  obs_df <- obs_df[!is.na(obs_df$x) & !is.na(obs_df$y), ]
  
  subtitle_text <- if (!is.null(rmse_val)) {
    paste0("WAIC = ", formatC(waic_val, format = "f", digits =  2),
           " | Residual_SD = ", formatC(rmse_val, format = "f", digits = 4), "°C")
  } else {
    paste0("WAIC = ", formatC(waic_val, format = "f", digits =  2))
  }
  
  ggplot() +
    geom_ribbon(
      data = pred_df,
      aes(x = x, ymin = lo95, ymax = hi95),
      fill = "steelblue", alpha = 0.18
    ) +
    geom_ribbon(
      data = pred_df,
      aes(x = x, ymin = lo50, ymax = hi50),
      fill = "steelblue", alpha = 0.40
    ) +
    geom_point(
      data = obs_df,
      aes(x = x, y = y),
      colour = "black", alpha = 0.2, size = 2.5
    ) +
    geom_line(
      data = pred_df,
      aes(x = x, y = mean),
      colour = "black", linewidth = 1.0
    ) +
    geom_abline(
      intercept = 0, slope = 1,
      colour = "darkred", linetype = "dashed", linewidth = 0.8
    ) +
    scale_x_continuous(limits = c(x_min, x_max)) +
    scale_y_continuous(limits = c(y_min, y_max)) +
    labs(
      title = panel_label,
      subtitle = subtitle_text,
      x = x_label,
      y = expression(paste("MRST (", degree, "C)"))
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 11),
      plot.subtitle = element_text(size = 9, colour = "grey40"),
      #axis.title = element_text(face = "bold"),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(colour = "black", fill = NA, linewidth = 0.5)
    )
}

#===========================================================
# Build the four panels
#===========================================================
p2 <- make_uncertainty_plot(
  model2, mos_rest, "temp_indoor",
  expression(paste(" Average CIT (", degree, "C)")),
  "A: Average Community Indoor Temperature",
  model2$waic$waic,
  vars_model2$residual_sd
)

p4 <- make_uncertainty_plot(
  model4, mos_rest, "satelite_outdoor_temp",
  expression(paste("Average SDT (", degree, "C)")),
  "B: Average Satellite-Derived Temperature",
  model4$waic$waic,
  vars_model4$residual_sd
)

p6 <- make_uncertainty_plot(
  model6, mos_rest, "sat_temp_min",
  expression(paste("Minimum SDT (", degree, "C)")),
  "C: Minimum Satellite-Derived Temperature",
  model6$waic$waic,
  vars_model6$residual_sd
)

p8 <- make_uncertainty_plot(
  model8, mos_rest, "sat_temp_max",
  expression(paste("Maximum SDT (", degree, "C)")),
  "D: Maximum Satellite-Derived Temperature",
  model8$waic$waic,
  vars_model8$residual_sd
)

#===========================================================
# Combine into 2x2 grid
#===========================================================
combined_plt <- (p2 + p4) / (p6 + p8) +
  plot_annotation(
    title = "Mosquito Resting Site Temperature vs Temperature Predictors for random effect models",
    subtitle = paste0(
      "Best-fit line with 50% (dark) and 95% (light) posterior predictive intervals | Dashed = 1:1 line\n",
      "Best model (lowest RMSE): ", best_rmse_model,
      " (RMSE = ", round(rmse_comparison$RMSE[best_rmse_idx], 3), "°C)"
    ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "grey40")
    )
  )

print(combined_plt)

###########################################
# Save outputs
ggsave("table_results/temp_models_With_PCI_re.pdf",
       combined_plt, width = 12, height = 10, units = "in", dpi = 300)
ggsave("table_results/temp_models_with_PCI_re.png",
       combined_plt, width = 12, height = 10, units = "in", dpi = 300)


#============================================================================================#
# Save RMSE comparison to CSV
write.csv(rmse_comparison, "table_results/rmse_model_comparison_spatial.csv", row.names = FALSE)
write.csv(comprehensive_comparison, "table_results/comprehensive_model_comparison_spatial.csv", row.names = FALSE)
print("\nRMSE comparison saved to: table_results/rmse_model_comparison_spatial.csv")
print("Comprehensive comparison saved to: table_results/comprehensive_model_comparison_spatial.csv")


# Store model parameters for each model
model_parameters <- list(
  model2 = model2$summary.fixed,
  model4 = model4$summary.fixed,
  model6 = model6$summary.fixed,
  model8 = model8$summary.fixed
)

print("\nModel parameters stored for further analysis.")

# Save model results to files
write.csv(model_comparison, "results/model_comparison.csv", row.names = FALSE)

# Create a summary of the best models
best_models_summary <- data.frame(
  Model_Name = c("Model_2", "Model_4", "Model_6", "Model_8"),
  Formula = c("MRST ~ 0 + temp_indoor + (iid|days_count) + (iid|village)",
             "MRST ~ 0 + sat_temp_mean + (iid|days_count) + (iid|village)",
             "MRST ~ 0 + sat_temp_min + (iid|days_count) + (iid|village)",
             "MRST ~ 0 + sat_temp_max + (iid|days_count) + (iid|illage)"),
  WAIC = c(model2$waic$waic, model4$waic$waic, model6$waic$waic, model8$waic$waic),
  MLIK = c(model2$mlik[1,1], model4$mlik[1,1], model6$mlik[1,1], model8$mlik[1,1])
)

write.csv(best_models_summary, "results/best_models_summary.csv", row.names = FALSE)

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
  model2 = model2, model4 = model4, model6 = model6, model8 = model8
)

# Apply extraction function to all models
all_effects <- dplyr::bind_rows(lapply(names(all_models_list), function(name) {
  extract_effects_with_ci(all_models_list[[name]], name)
}))

# Write comprehensive effects to CSV
write.csv(all_effects, "table_results/all_models_effects_with_ci_full.csv", row.names = FALSE)

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
  extract_predictions_with_ci(model2, "Model_2", mos_rest),
  extract_predictions_with_ci(model4, "Model_4", mos_rest),
  extract_predictions_with_ci(model6, "Model_6", mos_rest),
  extract_predictions_with_ci(model8, "Model_8", mos_rest)
))

# Write predictions to CSV
write.csv(best_models_predictions, "table_results/best_models_predictions_with_ci.csv", row.names = FALSE)

print("\nResults saved to table_results/ directory.")
print(paste("Fixed and random effects with credible intervals saved to: table_results/all_models_effects_with_ci.csv"))
print(paste("Total effects extracted:", nrow(all_effects)))
print(paste("Best models predictions with credible intervals saved to: table_results/best_models_predictions_with_ci.csv"))
print(paste("Total predictions extracted:", nrow(best_models_predictions)))

