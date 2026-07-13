# ============================================================================
# SECTION 2: Estimating the EIP Using Different Approaches
# ============================================================================

# ----------------------------------------------------------------------------
# MODEL A: Classical Degree-Day Model
# Detinova (1962); Macdonald (1957)
# EIP_D = D / (T - T_min);  D = 111 degree-days, T_min = 16 °C for P. falciparum
# ----------------------------------------------------------------------------
eip_degreeday <- function(T, D = 111, T_min = 16, T_max_bound = 40) {
  # T_max_bound is a sanity guard only (not biological); EIP undefined at/below T_min
  ifelse(T > T_min & T < T_max_bound, D / (T - T_min), NA_real_)
}

# ----------------------------------------------------------------------------
# MODEL B: Briere Thermal Performance Curve for parasite development rate (PDR)
# Johnson et al. (2015, Ecology) — Bayesian re-fit of Mordecai et al. (2013) data
# PDR(T) = c * T * (T - T_min) * sqrt(T_max - T)
# Parameters (P. falciparum): c = 6.65e-5, T_min = 16.72 °C, T_max = 41.49 °C
# EIP = 1 / PDR(T)  (EIP -> infinity as T -> T_max)
# ----------------------------------------------------------------------------
pdr_briere <- function(T, c = 6.65e-5, T_min = 16.72, T_max = 41.49) {
  # pmax() guards the sqrt argument: ifelse() evaluates both branches, so
  # sqrt(T_max - T) would warn ("NaNs produced") for T >= T_max even though
  # those values are masked to NA. Clamping at 0 silences the warning without
  # changing results (where T < T_max the clamp is a no-op).
  ifelse(T > T_min & T < T_max,
         c * T * (T - T_min) * sqrt(pmax(T_max - T, 0)),
         NA_real_)
}

eip_briere <- function(T, ...) {
  pdr <- pdr_briere(T, ...)
  ifelse(!is.na(pdr) & pdr > 0, 1 / pdr, NA_real_)
}

# ============================================================================
# SECTION 2b: Mosquito Mortality with temperature 
# ============================================================================

# ---- M1: Martens 2 (PRIMARY) — coefficients confirmed from figure ----------
# p(T) = exp(-1 / (-4.4 + 1.31*T - 0.03*T^2));  mu(T) = -ln(p)
p_martens2 <- function(T) {
  denom <- -4.4 + 1.31 * T - 0.03 * T^2
  p     <- exp(-1 / denom)
  ifelse(denom > 0 & !is.na(p) & p > 0 & p < 1, p, NA_real_)
}
mu_martens2 <- function(T) {
  p <- p_martens2(T)
  ifelse(!is.na(p), -log(p), NA_real_)
}

# ---- M2: Neil function (SENSITIVITY) — defaults match plotted blue curve ----
# p(T) = c*(T - T_min)^gamma * (T_max - T)^delta;  mu(T) = -ln(p)
# Illustrative parameterisation as plotted: c = 0.5, gamma = 0.1, delta = 0.1
p_neil <- function(T, c = 0.5, gamma = 0.1, delta = 0.1,
                   T_min = 4, T_max = 43) {
  ifelse(T > T_min & T < T_max,
         c * (T - T_min)^gamma * (T_max - T)^delta,
         NA_real_)
}
mu_neil <- function(T, ...) {
  p <- p_neil(T, ...)
  ifelse(!is.na(p) & p > 0 & p < 1, -log(p), NA_real_)
}

# ---- M3: Mordecai quadratic (SENSITIVITY) — confirmed --------------------
p_mordecai_surv <- function(T) {
  p <- -0.000828 * T^2 + 0.0367 * T + 0.522
  ifelse(!is.na(p) & p > 0 & p < 1, p, NA_real_)
}
mu_mordecai_surv <- function(T) {
  p <- p_mordecai_surv(T)
  ifelse(!is.na(p), -log(p), NA_real_)
}

# ---- M4 (OPTIONAL): Logistic mortality rate --------------------------------
# Monotonic heat-driven mortality saturating at mu_max (NOT U-shaped)
# mu(T) = mu_max / (1 + exp(-k*(T - T0)))
# Note: this returns the mortality RATE directly (not via -ln(p))
mu_logistic <- function(T, mu_max = 0.5, k = 0.25, T0 = 30) {
  mu_max / (1 + exp(-k * (T - T0)))
}

# ============================================================================
# SECTION 2c: PROBABILITY OF SURVIVING THE EIP
# ============================================================================

# ---- Helper: P(survive EIP) ------------------------------------------------
# Probability a mosquito lives long enough to become infectious
# Assumes constant daily mortality hazard mu over the EIP duration
# P(survive) = exp(-mu(T) * EIP(T))
p_survive_eip <- function(mu_T, eip_T) {
  ifelse(!is.na(mu_T) & !is.na(eip_T), exp(-mu_T * eip_T), NA_real_)
}


#==============================================================================
# extract_eip_comparison()
# Builds a tidy EIP comparison table from EIP_plot_df_v2.
#
# For each temperature SOURCE (satellite / simulated / observed) and each
# EIP MODEL (degree-day / Brière / mSOS), returns the range and mean EIP,
# plus the mean overestimate of satellite relative to the mosquito sources.
#==============================================================================
extract_eip_comparison <- function(df) {
  
  df <- ungroup(df)   # guard against lingering group_by(village)
  
  # ---- 1. Long format: one row per (source × model × observation) ----------
  # Map each EIP column to its source and model
  eip_long <- bind_rows(
    # Satellite source
    df %>% transmute(temp = sdt_temp, source = "Satellite (SDT)",
                     `Degree-day` = EIP_DD_sdt_temp,
                     `Brière`     = EIP_briere_sdt_temp,
                     `mSOS`       = EIP_sat_temp),
    # Simulated source
    df %>% transmute(temp = sim_temp, source = "Simulated MRST",
                     `Degree-day` = EIP_DD_sim_temp,
                     `Brière`     = EIP_briere_sim_temp,
                     `mSOS`       = EIP_sim),
    # Observed source
    df %>% transmute(temp = temp, source = "Observed MRST",
                     `Degree-day` = EIP_DD_obs_temp,
                     `Brière`     = EIP_briere_obs_temp,
                     `mSOS`       = EIP_temp)
  ) %>%
    pivot_longer(c(`Degree-day`, `Brière`, `mSOS`),
                 names_to = "eip_model", values_to = "EIP")
  
  # ---- 2. Summary per source × model ---------------------------------------
  summary_tbl <- eip_long %>%
    group_by(source, eip_model) %>%
    summarise(
      n_obs     = sum(!is.na(EIP)),
      temp_min  = min(temp, na.rm = TRUE),
      temp_max  = max(temp, na.rm = TRUE),
      EIP_min   = min(EIP,  na.rm = TRUE),
      EIP_max   = max(EIP,  na.rm = TRUE),
      EIP_mean  = mean(EIP, na.rm = TRUE),
      EIP_sd    = sd(EIP,   na.rm = TRUE),
      .groups   = "drop"
    ) %>%
    mutate(
      source    = factor(source,
                         levels = c("Satellite (SDT)", "Simulated MRST", "Observed MRST")),
      eip_model = factor(eip_model,
                         levels = c("Degree-day", "Brière", "mSOS"))
    ) %>%
    arrange(eip_model, source)
  
  # ---- 3. Headline: satellite overestimate vs each mosquito source ----------
  # Mean EIP per source/model, pivoted so sources sit side by side
  wide_means <- summary_tbl %>%
    select(eip_model, source, EIP_mean) %>%
    pivot_wider(names_from = source, values_from = EIP_mean)
  
  overestimate <- wide_means %>%
    transmute(
      eip_model,
      mean_EIP_satellite = `Satellite (SDT)`,
      EIP_simulated = `Simulated MRST`,
      EIP_observed  = `Observed MRST`,
      overest_vs_sim     = `Satellite (SDT)` - `Simulated MRST`,
      overest_vs_obs     = `Satellite (SDT)` - `Observed MRST`
    )
  
  # ---- Return both tables --------------------------------------------------
  list(
    summary      = summary_tbl,
    overestimate = overestimate
  )
}
