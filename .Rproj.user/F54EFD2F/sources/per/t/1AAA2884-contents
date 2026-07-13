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


mos_rest$dist_from_nearest_wall_m <- mos_rest$dist_from_nearest_wall/100

# Ensure days_count is treated as factor for random effect
mos_rest$days_count_factor <- as.integer(mos_rest$days_count)
mos_rest$observation_id <- as.integer(mos_rest$observation_id)

# Define PC prior for random effects (penalized complexity prior)
pc_prior <- list(prec = list(prior = "gaussian", param = c(0, 0.1)))

# Define priors for fixed effects (Gaussian priors for continuous, default for categorical)
# Using a general prior for all continuous predictors
fixed_prior_general <- list(mean = 0, prec = 0.1)  # General weakly informative prior

# Prior for residual error variance (control.family)
var_eps <- 0.2  # For residual error variance prior
family_prior <- list(hyper = list(prec = list(prior = "gaussian", param = c(0, var_eps))))

# Model 0: MRST ~ temp_indoor + species + li + dist_from_nearest_wall + (1|observation) + (1|village)

# Model 1: MRST ~ temp_indoor + species + li + dist_from_nearest_wall + (1|days_count_factor) + (1|village)
model_1 <- inla(temp ~ 0 + temp_indoor + li + dist_from_nearest_wall_m  + species + ws +
              f(days_count_factor, model = "iid", hyper = pc_prior) + 
              f(village, model ="iid", hyper = pc_prior),
              data = mos_rest,
              family = "gaussian",
              control.fixed = fixed_prior_general,
              control.family = family_prior,
              control.predictor = list(compute = TRUE, link = 1),
              control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                     return.marginals.predictor = TRUE))





# Satellite temp models with full RE (temporal AR1 + spatial IID)
model_3 <- inla(temp ~ 0 + satelite_outdoor_temp  + li + dist_from_nearest_wall_m  + species + ws +
                f(days_count_factor, model = "iid", hyper = pc_prior) + 
                f(village, model ="iid", hyper = pc_prior),
                data = mos_rest,
                family = "gaussian",
                control.fixed = fixed_prior_general,
                control.family = family_prior,
                control.predictor = list(compute = TRUE, link = 1),
                control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                       return.marginals.predictor = TRUE))

model_5 <- inla(temp ~ 0 + sat_temp_min +  li + dist_from_nearest_wall_m  + species + ws +
                f(days_count_factor, model = "iid", hyper = pc_prior) + 
                f(village, model ="iid", hyper = pc_prior),
                data = mos_rest,
                family = "gaussian",
                control.fixed = fixed_prior_general,
                control.family = family_prior,
                control.predictor = list(compute = TRUE, link = 1),
                control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                       return.marginals.predictor = TRUE))


model_7 <- inla(temp ~ 0 + sat_temp_max + li + dist_from_nearest_wall_m  + species + ws +
              f(days_count_factor, model = "iid", hyper = pc_prior) + 
              f(village, model ="iid", hyper = pc_prior),
              data = mos_rest,
              family = "gaussian",
              control.fixed = fixed_prior_general,
              control.family = family_prior,
              control.predictor = list(compute = TRUE, link = 1),
              control.compute = list(waic = TRUE, mlik = TRUE, config = TRUE,
                                     return.marginals.predictor = TRUE))

# Print model summaries


print("\nModel 1 Summary:")
print(model_1$summary.fixed)
print(paste("WAIC:", model_1$waic$waic))
print(paste("MLIK:", model_1$mlik[1,1]))



print("\nModel 3 Summary:")
print(model_3$summary.fixed)
print(paste("WAIC:", model_3$waic$waic))
print(paste("MLIK:", model_3$mlik[1,1]))



print("\nModel 5 Summary:")
print(model_5$summary.fixed)
print(paste("WAIC:", model_5$waic$waic))
print(paste("MLIK:", model_5$mlik[1,1]))


print("\nModel 7 Summary:")
print(model_7$summary.fixed)
print(paste("WAIC:", model_7$waic$waic))
print(paste("MLIK:", model_7$mlik[1,1]))

# Model comparison
model_comparison <- data.frame(
  Model = c("Model_1", "Model_3", "Model_5", "Model_7"),
  WAIC = c(model_1$waic$waic, model_3$waic$waic, model_5$waic$waic, model_7$waic$waic),
  MLIK = c( model_1$mlik[1,1], model_3$mlik[1,1], model_5$mlik[1,1], model_7$mlik[1,1])
)

print("\nModel Comparison Table:")
print(model_comparison)

# Compare best models: 1, 3, 5, 7
best_models_comparison <- data.frame(
  Model = c("Model_1", "Model_3", "Model_5", "Model_7"),
  WAIC = c(model_1$waic$waic, model_3$waic$waic, model_5$waic$waic, model_7$waic$waic),
  MLIK = c(model_1$mlik[1,1], model_3$mlik[1,1], model_5$mlik[1,1], model_7$mlik[1,1]),
  Description = c("Indoor temp + spatial RE + temporal RE",
                  "Sat mean temp + spatial RE + temporal RE",
                  "Sat min temp + spatial RE + temporal RE",
                  "Sat max temp + spatial RE + temporal RE")
)

print("\nComparison of best models (1, 3, 5, 7):")
print(best_models_comparison)

# Identify overall best model based on WAIC
overall_best_idx <- which.min(best_models_comparison$WAIC)
overall_best_model <- best_models_comparison$Model[overall_best_idx]
print(paste("\nOverall best model based on WAIC:", overall_best_model,
            "with WAIC =", best_models_comparison$WAIC[overall_best_idx]))


#########################################################################
#================== Extracting model variances ======================#
######################################################################
# Function to extract variances AND beta SD from INLA model
extract_variances <- function(model, predictor_name = NULL) {
  
  # Extract posterior mean precisions (handle different RE structures)
  hyper_names <- rownames(model$summary.hyperpar)
  
  prec_resid  <- model$summary.hyperpar["Precision for the Gaussian observations", "mean"]
  
  # Check for days_count_factor precision (may not exist in all models)
  if ("Precision for days_count_factor" %in% hyper_names) {
    prec_day <- model$summary.hyperpar["Precision for days_count_factor", "mean"]
  } else {
    prec_day <- NA
  }
  
  # Check for village precision (may not exist in all models)
  if ("Precision for village" %in% hyper_names) {
    prec_village <- model$summary.hyperpar["Precision for village", "mean"]
  } else {
    prec_village <- NA
  }
  
  # Convert to variances
  var_resid   <- 1 / prec_resid
  var_day     <- ifelse(!is.na(prec_day), 1 / prec_day, 0)
  var_village <- ifelse(!is.na(prec_village), 1 / prec_village, 0)
  
  # Create results list
  variances <- list(
    residual_variance = var_resid,
    day_variance = var_day,
    village_variance = var_village,
    total_variance = var_resid + var_day + var_village,
    
    # Standard deviations
    residual_sd = sqrt(var_resid),
    day_sd = ifelse(!is.na(prec_day), sqrt(var_day), NA),
    village_sd = ifelse(!is.na(prec_village), sqrt(var_village), NA),
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

# Usage examples for models 1, 3, 5, 7:
vars_model_1 <- extract_variances(model_1, predictor_name = "temp_indoor")
vars_model_3 <- extract_variances(model_3, predictor_name = "satelite_outdoor_temp")
vars_model_5 <- extract_variances(model_5, predictor_name = "sat_temp_min")
vars_model_7 <- extract_variances(model_7, predictor_name = "sat_temp_max")

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

# Usage for models 1, 3, 5, 7:
model_list <- list(model_1, model_3, model_5, model_7)
model_names <- c("Model_1 (Indoor, AR1 + village)",
                 "Model_3 (Sat Mean, AR1 + village)",
                 "Model_5 (Sat Min, AR1 + village)",
                 "Model_7 (Sat Max, AR1 + village)")
predictor_names <- c("temp_indoor",
                     "satelite_outdoor_temp",
                     "sat_temp_min",
                     "sat_temp_max")

variance_comparison <- compare_variances(model_list, model_names, predictor_names)
print(variance_comparison)

# ============================================================
# VISUALIZATION: Best fit line + 95% and 50% credible intervals
# Uses INLA's linear predictor which accounts for ALL predictors
# Scale: c(20, 50) as agreed
# Models: 0-7 (excluding 8-9 which have different response variables)
# ============================================================================#
###############################################################################
###############################################################################

plot_inla_population_effect <- function(model,
                                        data,
                                        focal_var = "temp_indoor",
                                        response_var = "temp",
                                        species_var = "species",
                                        continuous_vars = c("li", "dist_from_nearest_wall_m", "ws"),
                                        n_grid = 200,
                                        n_samp = 2000,
                                        seed = 123,
                                        waic_value = NULL,
                                        rmse_value = NULL,
                                        x_lab = expression(paste("Indoor Temperature (", degree, "C)")),
                                        y_lab = expression(paste("MRST (", degree, "C)")),
                                        #plot_title = "Population-level relationship",
                                        #plot_subtitle = "Line = posterior mean fit; bands = 50% and 95% posterior predictive intervals",
                                        show_ref_line = TRUE) {
  
  #---------------------------------------------------------
  # 1. Grid for focal predictor
  #---------------------------------------------------------
  x_grid <- seq(
    min(data[[focal_var]], na.rm = TRUE),
    max(data[[focal_var]], na.rm = TRUE),
    length.out = n_grid
  )
  
  #---------------------------------------------------------
  # 2. Means for continuous covariates
  #---------------------------------------------------------
  cont_means <- sapply(continuous_vars, function(v) mean(data[[v]], na.rm = TRUE))
  
  #---------------------------------------------------------
  # 3. Species proportions for population averaging
  #---------------------------------------------------------
  species_props  <- prop.table(table(data[[species_var]]))
  species_levels <- names(species_props)
  
  #---------------------------------------------------------
  # 4. Posterior samples
  #---------------------------------------------------------
  post_samps <- inla.posterior.sample(n_samp, model)
  latent_names <- rownames(post_samps[[1]]$latent)
  hyper_names  <- names(post_samps[[1]]$hyperpar)
  
  #---------------------------------------------------------
  # 5. Match coefficient indices
  #---------------------------------------------------------
  idx_focal <- grep(paste0("^", focal_var, ":1$"), latent_names)
  
  idx_cont <- sapply(continuous_vars, function(v) {
    grep(paste0("^", v, ":1$"), latent_names)
  })
  
  idx_species <- sapply(species_levels, function(sp) {
   grep(paste0("^", species_var, sp, ":1$"), latent_names)
  })
  
  stopifnot(length(idx_focal) == 1)
  stopifnot(all(idx_cont > 0))
  stopifnot(all(idx_species > 0))
  
  #---------------------------------------------------------
  # 6. Extract posterior draws of coefficients
  #---------------------------------------------------------
  beta_focal <- sapply(post_samps, function(s) as.numeric(s$latent[idx_focal, 1]))
  
  beta_cont <- sapply(seq_along(continuous_vars), function(k) {
    sapply(post_samps, function(s) as.numeric(s$latent[idx_cont[k], 1]))
  })
  beta_cont <- as.matrix(beta_cont)
  colnames(beta_cont) <- continuous_vars
  
  beta_species <- sapply(seq_along(species_levels), function(k) {
    sapply(post_samps, function(s) as.numeric(s$latent[idx_species[k], 1]))
  })
  beta_species <- as.matrix(beta_species)
  colnames(beta_species) <- species_levels
  
  #---------------------------------------------------------
  # 7. Species-weighted average offset
  #---------------------------------------------------------
  species_offset <- sapply(1:n_samp, function(j) {
    sum(beta_species[j, ] * as.numeric(species_props[colnames(beta_species)]))
  })
  # 
  #---------------------------------------------------------
  # 8. Continuous-covariate offset at means
  #---------------------------------------------------------
  cont_offset <- sapply(1:n_samp, function(j) {
    sum(beta_cont[j, ] * cont_means[colnames(beta_cont)])
  })
  
  #---------------------------------------------------------
  # 9. Population-level posterior mean line
  #    Random effects set to zero
  #---------------------------------------------------------
  mu_mat <- sapply(1:n_samp, function(j) {
    species_offset[j] + cont_offset[j] + beta_focal[j] * x_grid
  })
  mu_mat <- as.matrix(mu_mat)
  
  #---------------------------------------------------------
  # 10. Posterior predictive draws
  #---------------------------------------------------------
  prec_idx <- grep("Precision for the Gaussian observations", hyper_names)
  stopifnot(length(prec_idx) == 1)
  
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
  
  #---------------------------------------------------------
  # 11. Summarize for plotting
  #---------------------------------------------------------
  plot_df <- data.frame(
    x = x_grid,
    mean_fit = apply(mu_mat, 1, mean),
    lo50 = apply(yrep_mat, 1, quantile, probs = 0.25),
    hi50 = apply(yrep_mat, 1, quantile, probs = 0.75),
    lo95 = apply(yrep_mat, 1, quantile, probs = 0.025),
    hi95 = apply(yrep_mat, 1, quantile, probs = 0.975)
  )
  
  #---------------------------------------------------------
  # 12. Build label text for WAIC / RMSE
  #---------------------------------------------------------
  stat_lines <- c()
  
  if (!is.null(waic_value)) {
    stat_lines <- c(stat_lines, paste0("WAIC = ", round(waic_value, 2)))
  }
  
  if (!is.null(rmse_value)) {
    stat_lines <- c(stat_lines, paste0("RMSE = ", round(rmse_value, 4)))
  }
  
  stat_label <- paste(stat_lines, collapse = "\n")
  
  # annotation position
  x_annot <- min(plot_df$x, na.rm = TRUE)
  y_annot <- max(c(plot_df$hi95, data[[response_var]]), na.rm = TRUE)
  
  #---------------------------------------------------------
  # 13. Plot
  #---------------------------------------------------------
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
      colour = "black", alpha = 0.20, size = 2
    ) +
    labs(
      x = x_lab,
      y = y_lab#,
      #title = plot_title,
      #subtitle = plot_subtitle
    ) +
    scale_y_continuous(limits = c(15, 50)) +
    theme_bw(base_size = 12) +
    theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
          panel.grid.major.y=element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, size = 12),
          axis.text.y = element_text(vjust = 0.5, size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12))
  
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
    species_props = species_props,
    continuous_means = cont_means
  ))
}
#############################################################################################
#========================================================================#
# Model_1: Average Temparature in door 
#========================================================================#
res_temp_indoor <- plot_inla_population_effect(
  model = model_1,
  data = mos_rest,
  focal_var = "temp_indoor",
  response_var = "temp",
  #waic_value = model_1$waic$waic,
  #rmse_value = vars_model_1$residual_sd,
  x_lab = expression(paste("CIT (", degree, "C)")),
  y_lab = expression(paste("MRST (", degree, "C)"))#,
  #plot_title = "Population-level relationship between MRST and indoor temperature"
)

p2 <- res_temp_indoor$plot
print(p2)
#========================================================================#
# Model_3: Average Satellite Temp 
#========================================================================#
avg_sdt <- plot_inla_population_effect(
  model = model_3,
  data = mos_rest,
  focal_var = "satelite_outdoor_temp",
  response_var = "temp",
  #waic_value = model_3$waic$waic,
  #rmse_value = vars_model_3$residual_sd,
  x_lab = expression(paste("Mean SDT (", degree, "C)")),
  y_lab = expression(paste("MRST (", degree, "C)"))#,
  #plot_title = "Population-level relationship between MRST and indoor temperature"
)

p4 <- avg_sdt$plot
print(p4)

#========================================================================#
# Model_3: Minimum Satellite Temp 
#========================================================================#
min_sdt <- plot_inla_population_effect(
  model = model_5,
  data = mos_rest,
  focal_var = "sat_temp_min",
  response_var = "temp",
  #waic_value = model_5$waic$waic,
  #rmse_value = vars_model_5$residual_sd,
  x_lab = expression(paste("Minimum SDT (", degree, "C)")),
  y_lab = expression(paste("MRST (", degree, "C)"))#,
  # plot_title = "Population-level relationship between MRST and indoor temperature"
)

p6 <- min_sdt$plot
print(p6)
#========================================================================#
# Model_3: Average Satellite Temp 
#========================================================================#
max_sdt <- plot_inla_population_effect(
  model = model_7,
  data = mos_rest,
  focal_var = "sat_temp_max",
  response_var = "temp",
  #waic_value = model_7$waic$waic,
  #rmse_value = vars_model_7$residual_sd,
  x_lab = expression(paste("Maximum SDT (", degree, "C)")),
  y_lab = expression(paste("MRST (", degree, "C)"))#,
  #plot_title = "Population-level relationship between MRST and indoor temperature"
)

p8 <- max_sdt$plot
print(p8)

###########################################################
#===========================================================
# Combine into 2x2 grid
#===========================================================
############################################################
combined_plt <- (p2 + p4) / (p6 + p8) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "I",
    #title = "Mosquito Resting Site Temperature vs Predictors for random effect models",
    # subtitle = paste0(
    #   "Best-fit line with 50% (dark) and 95% (light) posterior predictive intervals | Dashed = 1:1 line\n"
    #   #,
    #   #"Best model (lowest RMSE): ", best_rmse_model,
    #   #" (RMSE = ", round(rmse_comparison$RMSE[best_rmse_idx], 3), "°C)"
    # ),
    theme = theme(
      plot.title = element_text(face = "bold", size = 13),
      plot.subtitle = element_text(size = 10, colour = "black")
    )
  )

print(combined_plt)

###########################################
# Save outputs
ggsave("table_results/full_temp_models_With_PCI_re.pdf",
       combined_plt, width = 12, height = 10, units = "in", dpi = 300)
ggsave("table_results/full_temp_models_with_PCI_re.png",
       combined_plt, width = 12, height = 10, units = "in", dpi = 300)


#============================================================================================#
###############################################################################################
# Store model parameters for each model
model_parameters <- list(
  model_1 = model_1$summary.fixed,
  model_3 = model_3$summary.fixed,
  model_5 = model_5$summary.fixed,
  model_7 = model_7$summary.fixed
)

print("\nModel parameters stored for further analysis.")

# Save model results to files
write.csv(model_comparison, "table_results/model_comparison_full.csv", row.names = FALSE)

# Create a summary of the best models
best_models_summary <- data.frame(
  Model_Name = c("Model_1", "Model_3", "Model_5", "Model_7"),
  Formula = c("MRST ~ 0 + temp_indoor + species + li + dist_from_nearest_wall + rh + ws + (ar1|days_count) + (1|observation) + (1|village)",
             "MRST ~ 0 + sat_temp_mean + species + li + dist_from_nearest_wall + rh + ws + (ar1|days_count) + (1|observation) + (1|village)",
             "MRST ~ 0 + sat_temp_min + species + li + dist_from_nearest_wall + rh + ws + (ar1|days_count) + (1|observation) + (1|village)",
             "MRST ~ 0 + sat_temp_max + species + li + dist_from_nearest_wall + rh + ws + (ar1|days_count) + (1|observation) + (1|village)"),
  WAIC = c(model_1$waic$waic, model_3$waic$waic, model_5$waic$waic, model_7$waic$waic),
  MLIK = c(model_1$mlik[1,1], model_3$mlik[1,1], model_5$mlik[1,1], model_7$mlik[1,1])
)

write.csv(best_models_summary, "table_results/best_models_summary_full.csv", row.names = FALSE)

print("\nResults saved to results/ directory.")

