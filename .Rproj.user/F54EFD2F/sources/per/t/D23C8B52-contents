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
mos_rest$days_count  <- as.integer(mos_rest$days_count) # Convert days_count to numeric
mos_rest$date <- as.Date(mos_rest$date, format = "%d/%m/%Y") # Convert date to Date type
mos_rest$date_label <-format(mos_rest$date, "%d-%b-%Y") 


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
mos_rest$observation_id <- as.integer(mos_rest$observation_id)

# Define PC prior for random effects (penalized complexity prior)
pc_prior <- list(prec = list(prior = "pc.prec", param = c(2, 0.05)))

# Define priors for fixed effects (Gaussian priors for continuous, default for categorical)
# Using a general prior for all continuous predictors
fixed_prior_general <- list(mean = 0, prec = 0.1)  # General weakly informative prior

# Prior for residual error variance (control.family)
var_eps <- 0.2  # For residual error variance prior
family_prior <- list(hyper = list(prec = list(prior = "pc.prec", param = c(2, var_eps))))


mos_rest$height_from_ground_m <- mos_rest$height_from_ground/100
# Model 8: height_from_ground ~ temp_indoor + species + resting_surface + li + (1|days_count) + (1|village)
# Tells us whether mosquitoes move up or down the wall if hotter that day
model_cit <- inla(height_from_ground_m ~ 1 + temp_indoor + species + resting_surface + sp_elisa + li + rh + ws+ 
                  f(days_count_factor, model = "iid", hyper = pc_prior) +
                  f(village, model = "iid", hyper = pc_prior),
                data = mos_rest,
                family = "gaussian",
                control.fixed = fixed_prior_general,
                control.family = family_prior,
                control.predictor = list(compute = TRUE),
                control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                       return.marginals.predictor = TRUE))

# Model for satellite mean temperature
model_sat_mean <- inla(height_from_ground_m ~ 1 + satelite_outdoor_temp + species + resting_surface + sp_elisa + li + rh + ws +
                         f(days_count_factor, model = "iid", hyper = pc_prior) +
                         f(village, model = "iid", hyper = pc_prior),
                       data = mos_rest,
                       family = "gaussian",
                       control.fixed = fixed_prior_general,
                       control.family = family_prior,
                       control.predictor = list(compute = TRUE),
                       control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                              return.marginals.predictor = TRUE))

# Model for satellite minimum temperature
model_sat_min <- inla(height_from_ground_m ~ 1 + sat_temp_min + species + resting_surface +sp_elisa + li + rh + ws +
                        f(days_count_factor, model = "iid", hyper = pc_prior) +
                        f(village, model = "iid", hyper = pc_prior),
                      data = mos_rest,
                      family = "gaussian",
                      control.fixed = fixed_prior_general,
                      control.family = family_prior,
                      control.predictor = list(compute = TRUE),
                      control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                             return.marginals.predictor = TRUE))

# Model for satellite maximum temperature
model_sat_max <- inla(height_from_ground_m ~ 1 + sat_temp_max + species + resting_surface + sp_elisa + li + rh + ws +
                        f(days_count_factor, model = "iid", hyper = pc_prior) +
                        f(village, model = "iid", hyper = pc_prior),
                      data = mos_rest,
                      family = "gaussian",
                      control.fixed = fixed_prior_general,
                      control.family = family_prior,
                      control.predictor = list(compute = TRUE),
                      control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                             return.marginals.predictor = TRUE)) 



# Model for all covariates
model_h <- inla(height_from_ground_m ~ 1 +  temp_indoor + satelite_outdoor_temp + sat_temp_min + sat_temp_max +
                        species + resting_surface + sp_elisa + li + rh + ws +
                        f(days_count_factor, model = "iid", hyper = pc_prior) +
                        f(village, model = "iid", hyper = pc_prior),
                      data = mos_rest,
                      family = "gaussian",
                      control.fixed = fixed_prior_general,
                      control.family = family_prior,
                      control.predictor = list(compute = TRUE),
                      control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE, 
                                             return.marginals.predictor = TRUE)) 

# Print all model summaries
print("\n=== Model 8 (Indoor Temp) Summary ===")
print(model_cit$summary.fixed)
print(paste("WAIC:", model_cit$waic$waic))

print("\n=== Model Satellite Mean Summary ===")
print(model_sat_mean$summary.fixed)
print(paste("WAIC:", model_sat_mean$waic$waic))

print("\n=== Model Satellite Min Summary ===")
print(model_sat_min$summary.fixed)
print(paste("WAIC:", model_sat_min$waic$waic))

print("\n=== Model Satellite Max Summary ===")
print(model_sat_max$summary.fixed)
print(paste("WAIC:", model_sat_max$waic$waic))

print("\n=== Model_h Summary ===")
print(model_h$summary.fixed)
print(paste("WAIC:", model_h$waic$waic))

# Model comparison
model_comparison <- data.frame(
  Model = c("model_cit", "Model_Sat_Mean", "Model_Sat_Min", "Model_Sat_Max", "Model_h"),
  WAIC = c(model_cit$waic$waic, model_sat_mean$waic$waic, model_sat_min$waic$waic, model_sat_max$waic$waic, model_h$waic$waic),
  MLIK = c(model_cit$mlik[1,1], model_sat_mean$mlik[1,1], model_sat_min$mlik[1,1], model_sat_max$mlik[1,1], model_h$mlik[1,1]),
  Predictor = c("temp_indoor", "satelite_outdoor_temp", "sat_temp_min", "sat_temp_max", "all predictor")
)

print("\n=== Model Comparison Table ===")
print(model_comparison)

# Identify best model based on WAIC
best_waic_idx <- which.min(model_comparison$WAIC)
best_waic_model <- model_comparison$Model[best_waic_idx]
print(paste("\nBest model based on WAIC:", best_waic_model))


#=============================================================================
# DISTANCE-BASED MODEL EVALUATION (RMSE Approach)
#=============================================================================

# Generic distance measurement function (Root-Mean-Squared Deviation)
measure_distance_height <- function(model, data, response_col) {
  # Get predicted values from summary.linear.predictor
  lp <- model$summary.linear.predictor
  if (!is.null(lp) && nrow(lp) > 0) {
    pred <- lp$mean
    # Get observed values
    obs <- data[[response_col]]
    # Remove NA values
    valid_idx <- !is.na(pred) & !is.na(obs)
    pred <- pred[valid_idx]
    obs <- obs[valid_idx]
    # Compute RMSE
    diff <- obs - pred
    sqrt(mean(diff ^ 2))
  } else {
    return(NA)
  }
}

# Compute RMSE for each model
rmse_model_cit <- measure_distance_height(model_cit, mos_rest, "height_from_ground_m")
rmse_model_sat_mean <- measure_distance_height(model_sat_mean, mos_rest, "height_from_ground_m")
rmse_model_sat_min <- measure_distance_height(model_sat_min, mos_rest, "height_from_ground_m")
rmse_model_sat_max <- measure_distance_height(model_sat_max, mos_rest, "height_from_ground_m")

# Create RMSE comparison table
rmse_comparison <- data.frame(
  Model = c("Model_cit", "Model_Sat_Mean", "Model_Sat_Min", "Model_Sat_Max"),
  RMSE = c(rmse_model_cit, rmse_model_sat_mean, rmse_model_sat_min, rmse_model_sat_max),
  Predictor = c("temp_indoor", "satelite_outdoor_temp", "sat_temp_min", "sat_temp_max"),
  Description = c("Indoor temp", "Sat mean temp", "Sat min temp", "Sat max temp")
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
  Model = c("Model_cit", "Model_Sat_Mean", "Model_Sat_Min", "Model_Sat_Max"),
  Predictor = c("temp_indoor", "satelite_outdoor_temp", "sat_temp_min", "sat_temp_max"),
  Description = c("Indoor temp", "Sat mean temp", "Sat min temp", "Sat max temp"),
  WAIC = c(model_cit$waic$waic, model_sat_mean$waic$waic, model_sat_min$waic$waic, model_sat_max$waic$waic),
  RMSE = c(rmse_model_cit, rmse_model_sat_mean, rmse_model_sat_min, rmse_model_sat_max)
)

print("\n=== Comprehensive Model Comparison (WAIC vs RMSE) ===")
print(comprehensive_comparison)

# ============================================================
# VISUALIZATION: Height from Ground vs Temperature Predictors
# Best fit line + 95% and 50% credible intervals
# Y-axis: height_from_ground_m, X-axis: temperature variables
# ============================================================
#===========================================================
# Helper: extract posterior draws for fixed effects robustly
#===========================================================
get_fixed_draws_inla <- function(model, n_samp = 2000) {
  post_samps <- inla.posterior.sample(n_samp, model)
  latent_names <- rownames(post_samps[[1]]$latent)
  
  fixed_names <- rownames(model$summary.fixed)
  
  fixed_draws <- sapply(fixed_names, function(term) {
    idx <- which(latent_names %in% c(term, paste0(term, ":1")))
    
    if (length(idx) != 1) {
      stop(sprintf(
        "Could not uniquely match fixed effect '%s' in posterior samples.",
        term
      ))
    }
    
    sapply(post_samps, function(s) as.numeric(s$latent[idx, 1]))
  })
  
  fixed_draws <- as.matrix(fixed_draws)
  colnames(fixed_draws) <- fixed_names
  
  list(
    post_samps = post_samps,
    fixed_draws = fixed_draws
  )
}

#===========================================================
# Main plotting function for INLA population-level effect
#===========================================================
plot_inla_population_effect <- function(model,
                                        data,
                                        focal_var,
                                        response_var,
                                        factor_vars = c("species","resting_surface","sp_elisa"),
                                        continuous_vars = c("li", "ws"),
                                        n_grid = 200,
                                        n_samp = 2000,
                                        seed = 123,
                                        waic_value = NULL,
                                        rmse_value = NULL,
                                        x_lab = focal_var,
                                        y_lab = response_var,
                                        show_ref_line = FALSE,
                                        point_alpha = 0.20,
                                        point_size = 2) {
  
  #---------------------------------------------
  # 1. Grid for focal predictor
  #---------------------------------------------
  x_grid <- seq(
    min(data[[focal_var]], na.rm = TRUE),
    max(data[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  # remove focal_var from continuous_vars if user included it
  continuous_vars <- setdiff(continuous_vars, focal_var)
  
  #---------------------------------------------
  # 2. Posterior samples of fixed effects
  #---------------------------------------------
  samp_obj <- get_fixed_draws_inla(model, n_samp = n_samp)
  post_samps  <- samp_obj$post_samps
  fixed_draws <- samp_obj$fixed_draws
  fixed_names <- colnames(fixed_draws)
  
  #---------------------------------------------
  # 3. Intercept
  #---------------------------------------------
  if (!("(Intercept)" %in% fixed_names)) {
    stop("Model does not contain an intercept. Update function if fitting 0 + ... models.")
  }
  beta_0 <- fixed_draws[, "(Intercept)"]
  
  #---------------------------------------------
  # 4. Focal variable coefficient
  #---------------------------------------------
  if (!(focal_var %in% fixed_names)) {
    stop(sprintf("Could not find focal variable '%s' in model$summary.fixed.", focal_var))
  }
  beta_focal <- fixed_draws[, focal_var]
  
  #---------------------------------------------
  # 5. Continuous covariate offset at means
  #---------------------------------------------
  cont_offset <- rep(0, n_samp)
  
  if (length(continuous_vars) > 0) {
    cont_means <- sapply(continuous_vars, function(v) mean(data[[v]], na.rm = TRUE))
    for (v in continuous_vars) {
      if (!(v %in% fixed_names)) {
        warning(sprintf("Continuous variable '%s' not found in fixed effects; skipping.", v))
      } else {
        cont_offset <- cont_offset + fixed_draws[, v] * cont_means[v]
      }
    }
  } else {
    cont_means <- numeric(0)
  }
  
  #---------------------------------------------
  # 6. Factor offsets averaged using observed proportions
  #---------------------------------------------
  factor_offsets <- list()
  total_factor_offset <- rep(0, n_samp)
  
  for (fv in factor_vars) {
    
    if (!fv %in% names(data)) {
      warning(sprintf("Factor variable '%s' not found in data; skipping.", fv))
      next
    }
    
    xfac <- factor(data[[fv]])
    levs <- levels(xfac)
    props <- prop.table(table(xfac))
    
    # baseline level gets coefficient 0
    factor_offset_this <- rep(0, n_samp)
    
    for (lev in levs) {
      coef_name <- paste0(fv, lev)
      
      if (coef_name %in% fixed_names) {
        beta_lev <- fixed_draws[, coef_name]
      } else {
        beta_lev <- rep(0, n_samp)  # reference level
      }
      
      factor_offset_this <- factor_offset_this + as.numeric(props[lev]) * beta_lev
    }
    
    total_factor_offset <- total_factor_offset + factor_offset_this
    factor_offsets[[fv]] <- props
  }
  
  #---------------------------------------------
  # 7. Population-level posterior mean line
  #---------------------------------------------
  mu_mat <- sapply(1:n_samp, function(j) {
    beta_0[j] + total_factor_offset[j] + cont_offset[j] + beta_focal[j] * x_grid
  })
  mu_mat <- as.matrix(mu_mat)   # n_grid x n_samp
  
  #---------------------------------------------
  # 8. Posterior predictive draws
  #---------------------------------------------
  hyper_names <- names(post_samps[[1]]$hyperpar)
  prec_idx <- grep("Precision for the Gaussian observations", hyper_names)
  
  if (length(prec_idx) != 1) {
    stop("Could not uniquely match Gaussian observation precision in hyperparameters.")
  }
  
  sigma_samps <- sapply(post_samps, function(s) {
    sqrt(1 / as.numeric(s$hyperpar[prec_idx]))
  })
  
  set.seed(seed)
  yrep_mat <- sapply(1:n_samp, function(j) {
    rnorm(
      n = length(x_grid),
      mean = mu_mat[, j],
      sd = sigma_samps[j]
    )
  })
  yrep_mat <- as.matrix(yrep_mat)
  
  #---------------------------------------------
  # 9. Summarize for plotting
  #---------------------------------------------
  plot_df <- data.frame(
    x = x_grid,
    mean_fit = apply(mu_mat, 1, mean),
    lo50 = apply(yrep_mat, 1, quantile, probs = 0.25),
    hi50 = apply(yrep_mat, 1, quantile, probs = 0.75),
    lo95 = apply(yrep_mat, 1, quantile, probs = 0.025),
    hi95 = apply(yrep_mat, 1, quantile, probs = 0.975)
  )
  
  #---------------------------------------------
  # 10. Labels for WAIC / RMSE
  #---------------------------------------------
  stat_lines <- c()
  
  if (!is.null(waic_value)) {
    stat_lines <- c(stat_lines, paste0("WAIC = ", round(waic_value, 2)))
  }
  
  if (!is.null(rmse_value)) {
    stat_lines <- c(stat_lines, paste0("RMSE = ", round(rmse_value, 4)))
  }
  
  stat_label <- paste(stat_lines, collapse = "\n")
  
  x_annot <- min(plot_df$x, na.rm = TRUE)
  y_annot <- max(c(plot_df$hi95, data[[response_var]]), na.rm = TRUE)
  
  #---------------------------------------------
  # 11. Plot
  #---------------------------------------------
  p <- ggplot() +
    geom_ribbon(
      data = plot_df,
      aes(x = x, ymin = lo95, ymax = hi95),
      fill = "steelblue", alpha = 0.20
    ) +
    geom_ribbon(
      data = plot_df,
      aes(x = x, ymin = lo50, ymax = hi50),
      fill = "steelblue", alpha = 0.35
    ) +
    geom_line(
      data = plot_df,
      aes(x = x, y = mean_fit),
      colour = "black", linewidth = 1
    ) +
    geom_point(
      data = data,
      aes(x = .data[[focal_var]], y = .data[[response_var]]),
      colour = "black", alpha = point_alpha, size = point_size
    ) +
    labs(
      x = x_lab,
      y = y_lab
    ) +
    #scale_x_continuous(breaks = seq(25,42, 1), limits = c(25,42)) +
    theme_bw(base_size = 12) +
    theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
          panel.grid.major.y=element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, size = 14),
          axis.text.y = element_text(vjust = 0.5, size = 14),
          axis.title.x = element_text(size = 14),
          axis.title.y = element_text(size = 14))
  
  if (show_ref_line) {
    p <- p + geom_abline(
      intercept = 0, slope = 1,
      colour = "darkred", linetype = "dashed", linewidth = 0.8
    )
  }
  
  if (nzchar(stat_label)) {
    p <- p + annotate(
      "text",
      x = x_annot,
      y = y_annot,
      label = stat_label,
      hjust = 0,
      vjust = 1,
      size = 3.8
    )
  }
  
  return(list(
    plot_df = plot_df,
    plot = p,
    factor_offsets = factor_offsets,
    continuous_means = cont_means
  ))
}

#===========================================================
# Optional RMSE helper
#===========================================================
get_inla_rmse <- function(model, response) {
  pred_mean <- model$summary.fitted.values$mean
  keep <- is.finite(pred_mean) & is.finite(response)
  sqrt(mean((response[keep] - pred_mean[keep])^2))
}

rmse_model_cit      <- get_inla_rmse(model_cit,      mos_rest$height_from_ground_m)
rmse_model_sat_mean <- get_inla_rmse(model_sat_mean, mos_rest$height_from_ground_m)
rmse_model_sat_min  <- get_inla_rmse(model_sat_min,  mos_rest$height_from_ground_m)
rmse_model_sat_max  <- get_inla_rmse(model_sat_max,  mos_rest$height_from_ground_m)


#===========================================================
# Plot 1: Indoor temperature model
#===========================================================
res_cit <- plot_inla_population_effect(
  model = model_cit,
  data = mos_rest,
  focal_var = "temp_indoor",
  response_var = "height_from_ground_m",
  factor_vars = c("species", "resting_surface", "sp_elisa"),
  continuous_vars = c("li", "ws"),
  #waic_value = model_cit$waic$waic,
  #rmse_value = rmse_model_cit,
  x_lab = expression(paste("CIT (", degree, "C)")),
  y_lab = "Height from ground (m)",
  show_ref_line = FALSE
)

p1 <- res_cit$plot
print(p1)

###########################################
# Save outputs
ggsave("results/height_models_cit.pdf",
       p1, width = 12, height = 10, units = "in", dpi = 300)
ggsave("results/height_models_cit.png",
       p1, width = 12, height = 10, units = "in", dpi = 300)
#===========================================================
# Plot 2: Satellite mean temperature model
#===========================================================
res_sat_mean <- plot_inla_population_effect(
  model = model_sat_mean,
  data = mos_rest,
  focal_var = "satelite_outdoor_temp",
  response_var = "height_from_ground_m",
  factor_vars = c("species", "resting_surface","sp_elisa"),
  continuous_vars = c("li", "ws"),
  #waic_value = model_sat_mean$waic$waic,
 # rmse_value = rmse_model_sat_mean,
  x_lab = expression(paste("Mean SDT (", degree, "C)")),
  y_lab = "Height from ground (m)",
  show_ref_line = FALSE
)

p2 <- res_sat_mean$plot
print(p2)
#===========================================================
# Plot 3: Satellite minimum temperature model
#===========================================================
res_sat_min <- plot_inla_population_effect(
  model = model_sat_min,
  data = mos_rest,
  focal_var = "sat_temp_min",
  response_var = "height_from_ground_m",
  factor_vars = c("species", "resting_surface","sp_elisa"),
  continuous_vars = c("li", "rh", "ws"),
 # waic_value = model_sat_min$waic$waic,
  #rmse_value = rmse_model_sat_min,
  x_lab = expression(paste("Minimum SDT (", degree, "C)")),
  y_lab = "Height from ground (m)",
  show_ref_line = FALSE
)

p3 <- res_sat_min$plot
print(p3)
#===========================================================
# Plot 4: Satellite maximum temperature model
#===========================================================
res_sat_max <- plot_inla_population_effect(
  model = model_sat_max,
  data = mos_rest,
  focal_var = "sat_temp_max",
  response_var = "height_from_ground_m",
  factor_vars = c("species", "resting_surface","sp_elisa"),
  continuous_vars = c("li", "ws"),
  #waic_value = model_sat_max$waic$waic,
 # rmse_value = rmse_model_sat_max,
  x_lab = expression(paste("Maximum SDT (", degree, "C)")),
  y_lab = "Height from ground (m)",
  show_ref_line = FALSE
)

p4 <- res_sat_max$plot
print(p4)
#===========================================================
# Combine into 2x2 grid
#===========================================================
combined_plt <- (p1 + p2) / (p3 + p4) +
  plot_annotation(
    tag_levels = "I",
    caption = "Data Source: GAEC-BNARI",
    #title = "Population-level relationship between resting height and temperature metrics",
    #subtitle = "Black line = posterior mean; dark band = 50% predictive interval; light band = 95% predictive interval",
    theme = theme(
    #  plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "black")
    )
  )

print(combined_plt)

###########################################
# Save outputs
ggsave("results/height_models_With_PCI_re.pdf",
       combined_plt, width = 12, height = 10, units = "in", dpi = 300)
ggsave("results/height_models_with_PCI_re.png",
       combined_plt, width = 12, height = 10, units = "in", dpi = 300)
#============================================================================================#

sp_elisa <- ggplot(mos_rest, aes(x = factor(sp_elisa), y = height_from_ground_m, fill = sp_elisa)) +
  geom_boxplot(outlier.alpha = 0.30) +
  geom_jitter(width = 0.15, alpha = 0.25, size = 2) +
  labs(
    x = "Sporozoites-elisa status",
    y = "Height from ground (m)",
    #title = "Height from ground by sp_elisa category",
    fill = "Sporozoites-elisa"
    #shape = "Sporozoites-elisa "
  ) +
  theme_bw(base_size = 11) +
  theme(panel.grid.major = element_blank(),
        panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_blank(),
        panel.grid.minor = element_blank())


###########################################
# Save outputs
ggsave("results/sp_elisa.pdf",
       sp_elisa, width = 12, height = 10, units = "in", dpi = 300)
ggsave("results/sp_elisa.png",
       sp_elisa, width = 12, height = 10, units = "in", dpi = 300)
 #============================================================================================#
################################################################################################
# ============================================================
# VISUALIZATION: Adjusted height_from_ground_m by sp_elisa
# sp_elisa is categorical
#
# Model:
# model_h <- height_from_ground_m ~ temp_indoor + satelite_outdoor_temp +
#            sat_temp_min + sat_temp_max + species + resting_surface +
#            sp_elisa + li + rh + ws +
#            f(days_count_factor) + f(village)
#
# Points     = observed data
# Black dot  = adjusted posterior mean
# Thick line = 50% credible interval
# Thin line  = 95% credible interval
# ===========================================================

# ============================================================
# RMSE helper
# ============================================================

get_inla_rmse <- function(model, response) {
  pred_mean <- model$summary.fitted.values$mean
  keep <- is.finite(pred_mean) & is.finite(response)
  sqrt(mean((response[keep] - pred_mean[keep])^2))
}

rmse_model_h <- get_inla_rmse(
  model_h,
  mos_rest$height_from_ground_m
)

# ============================================================
# Helper: draw fixed effects from INLA marginals
# ============================================================

draw_fixed_effect <- function(model, term, n_samp = 2000) {
  if (!(term %in% names(model$marginals.fixed))) {
    stop(sprintf("Fixed effect '%s' not found in model$marginals.fixed.", term))
  }
  
  inla.rmarginal(
    n = n_samp,
    marginal = model$marginals.fixed[[term]]
  )
}

# ============================================================
# Plot adjusted categorical effect
# ============================================================

plot_inla_categorical_effect <- function(model,
                                         data,
                                         focal_var = "sp_elisa",
                                         response_var = "height_from_ground_m",
                                         continuous_vars = c(
                                           "temp_indoor",
                                           "satelite_outdoor_temp",
                                           "sat_temp_min",
                                           "sat_temp_max",
                                           "li",
                                           "rh",
                                           "ws"
                                         ),
                                         factor_vars = c(
                                           "species",
                                           "resting_surface"
                                         ),
                                         n_samp = 2000,
                                         seed = 123,
                                         waic_value = NULL,
                                         rmse_value = NULL,
                                         x_lab = "sp_elisa",
                                         y_lab = "Height from ground (m)",
                                         plot_title = "Adjusted resting height by sp_elisa",
                                         point_alpha = 0.25,
                                         point_size = 2) {
  
  set.seed(seed)
  
  fixed_names <- names(model$marginals.fixed)
  
  # Make sure focal variable is treated as a factor
  data[[focal_var]] <- factor(data[[focal_var]])
  focal_levels <- levels(data[[focal_var]])
  
  # Keep complete cases for plotting
  plot_data <- data[
    !is.na(data[[focal_var]]) &
      is.finite(data[[response_var]]),
  ]
  
  # ----------------------------------------------------------
  # 1. Draw intercept
  # ----------------------------------------------------------
  
  beta_0 <- draw_fixed_effect(
    model = model,
    term = "(Intercept)",
    n_samp = n_samp
  )
  
  # ----------------------------------------------------------
  # 2. Continuous covariate adjustment at observed means
  # ----------------------------------------------------------
  
  continuous_offset <- rep(0, n_samp)
  
  for (v in continuous_vars) {
    if (v %in% fixed_names) {
      beta_v <- draw_fixed_effect(
        model = model,
        term = v,
        n_samp = n_samp
      )
      
      continuous_offset <- continuous_offset +
        beta_v * mean(data[[v]], na.rm = TRUE)
      
    } else {
      warning(sprintf(
        "Continuous covariate '%s' not found in model; skipping.",
        v
      ))
    }
  }
  
  # ----------------------------------------------------------
  # 3. Other categorical covariates averaged by observed proportions
  # ----------------------------------------------------------
  
  factor_offset <- rep(0, n_samp)
  
  for (fv in factor_vars) {
    
    if (!(fv %in% names(data))) {
      warning(sprintf(
        "Factor variable '%s' not found in data; skipping.",
        fv
      ))
      next
    }
    
    data[[fv]] <- factor(data[[fv]])
    props <- prop.table(table(data[[fv]]))
    levs <- levels(data[[fv]])
    
    for (lev in levs) {
      
      coef_name <- paste0(fv, lev)
      
      if (coef_name %in% fixed_names) {
        beta_lev <- draw_fixed_effect(
          model = model,
          term = coef_name,
          n_samp = n_samp
        )
      } else {
        # Reference category
        beta_lev <- rep(0, n_samp)
      }
      
      factor_offset <- factor_offset +
        as.numeric(props[lev]) * beta_lev
    }
  }
  
  # ----------------------------------------------------------
  # 4. Adjusted posterior mean for each sp_elisa category
  # ----------------------------------------------------------
  
  pred_list <- list()
  
  for (lev in focal_levels) {
    
    coef_name <- paste0(focal_var, lev)
    
    if (coef_name %in% fixed_names) {
      beta_focal <- draw_fixed_effect(
        model = model,
        term = coef_name,
        n_samp = n_samp
      )
    } else {
      # Reference category of sp_elisa
      beta_focal <- rep(0, n_samp)
    }
    
    mu_draws <- beta_0 +
      continuous_offset +
      factor_offset +
      beta_focal
    
    pred_list[[lev]] <- data.frame(
      sp_elisa = lev,
      mean_fit = mean(mu_draws),
      lo50 = quantile(mu_draws, probs = 0.25),
      hi50 = quantile(mu_draws, probs = 0.75),
      lo95 = quantile(mu_draws, probs = 0.025),
      hi95 = quantile(mu_draws, probs = 0.975)
    )
  }
  
  pred_df <- do.call(rbind, pred_list)
  
  pred_df[[focal_var]] <- factor(
    pred_df$sp_elisa,
    levels = focal_levels
  )
  
  plot_data[[focal_var]] <- factor(
    plot_data[[focal_var]],
    levels = focal_levels
  )
  
  # ----------------------------------------------------------
  # 5. WAIC / RMSE label
  # ----------------------------------------------------------
  
  stat_lines <- c()
  
  if (!is.null(waic_value)) {
    stat_lines <- c(
      stat_lines,
      paste0("WAIC = ", round(waic_value, 2))
    )
  }
  
  if (!is.null(rmse_value)) {
    stat_lines <- c(
      stat_lines,
      paste0("RMSE = ", round(rmse_value, 4))
    )
  }
  
  stat_label <- paste(stat_lines, collapse = "\n")
  
  # ----------------------------------------------------------
  # 6. Plot
  # ----------------------------------------------------------
  
  p <- ggplot() +
    geom_jitter(
      data = plot_data,
      aes(
        x = .data[[focal_var]],
        y = .data[[response_var]]
      ),
      width = 0.15,
      alpha = point_alpha,
      size = point_size,
      colour = "grey40"
    ) +
    geom_errorbar(
      data = pred_df,
      aes(
        x = .data[[focal_var]],
        ymin = lo95,
        ymax = hi95
      ),
      width = 0.12,
      linewidth = 0.7,
      colour = "black"
    ) +
    geom_errorbar(
      data = pred_df,
      aes(
        x = .data[[focal_var]],
        ymin = lo50,
        ymax = hi50
      ),
      width = 0.20,
      linewidth = 1.5,
      colour = "black"
    ) +
    geom_point(
      data = pred_df,
      aes(
        x = .data[[focal_var]],
        y = mean_fit
      ),
      size = 3,
      colour = "black"
    ) +
    coord_flip() +
    labs(
      title = plot_title,
      subtitle = "Black dot = adjusted posterior mean; thick line = 50% credible interval; thin line = 95% credible interval",
      x = x_lab,
      y = y_lab
    ) +
    theme_bw(base_size = 11) +
    theme(
      plot.title = element_text(face = "bold", size = 12),
      plot.subtitle = element_text(size = 10),
      panel.grid.minor = element_blank()
    )
  
  if (nzchar(stat_label)) {
    p <- p +
      annotate(
        "text",
        x = 1,
        y = max(plot_data[[response_var]], na.rm = TRUE),
        label = stat_label,
        hjust = 0,
        vjust = 1,
        size = 3.5
      )
  }
  
  return(list(
    pred_df = pred_df,
    plot = p
  ))
}

# ============================================================
# Create plot for sp_elisa
# ============================================================

res_sp_elisa <- plot_inla_categorical_effect(
  model = model_h,
  data = mos_rest,
  focal_var = "sp_elisa",
  response_var = "height_from_ground_m",
  
  continuous_vars = c(
    "temp_indoor",
    "satelite_outdoor_temp",
    "sat_temp_min",
    "sat_temp_max",
    "li",
    "rh",
    "ws"
  ),
  
  factor_vars = c(
    "species",
    "resting_surface"
  ),
  
  waic_value = model_h$waic$waic,
  rmse_value = rmse_model_h,
  
  x_lab = "sp_elisa category",
  y_lab = "Height from ground (m)",
  plot_title = "Adjusted relationship between resting height and sp_elisa"
)

p_sp_elisa <- res_sp_elisa$plot

print(p_sp_elisa)

# View adjusted posterior estimates
print(res_sp_elisa$pred_df)

# ============================================================
# Optional: save figure
# ============================================================

ggsave(
  filename = "results\height_sp_elisa.pdf",
  plot = p_sp_elisa,
  width = 7,
  height = 5,
  dpi = 300
)
##############################################################################################
# Save RMSE comparison to CSV
write.csv(rmse_comparison, "results/rmse_model_comparison_height.csv", row.names = FALSE)
write.csv(comprehensive_comparison, "results/comprehensive_model_comparison_height.csv", row.names = FALSE)
print("\nRMSE comparison saved to: results/rmse_model_comparison_height.csv")
print("Comprehensive comparison saved to: results/comprehensive_model_comparison_height.csv")

