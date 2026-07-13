#install.packages("pacman")

# packages ####

## dply ####
#rm(list = ls()) # removing all object from the environment

#loading and reading in packages
pacman::p_load(tidyverse, haven, readr, readxl, dplyr,ggpubr, FSA, patchwork,
magrittr,ggplot2,Hmisc,purrr,DescTools,tidyr,ggthemes,validate, psych, rstan
,tibble,lme4, MASS, GGally,lattice, RColorBrewer,multcompView,merTools,cowplot,
ggalt,faraway,ResourceSelection,performance, cowplot,reshape2,fitdistrplus,cowplot,
lattice, graphics,hrbrthemes,broom, gtsummary,flextable,equatiomatic, nnet, caret,
ggeffects, emmeans, lmerTest, patchwork,effects,glmmTMB,DHARMa,corrplot, lubridate)



#sjplot, flexplot, GLMMmisc, 


nicelimits <- function(x) {
  range(scales::extended_breaks(only.loose = TRUE)(x))
}


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

# Define colors once for both fill and color
village_colors <- c(
  "Adawukwa"      = "red",
  "Atatam"        = "#4B644B",
  "Okyereko"      = "skyblue",
  "Agyenkwaso"    = "#59A77F",
  "Kusa Dinkyeae" = "#002060",
  "Osorogma"      = "black"
)

# Species fill palette — matched to your three levels
species_fills <- c(
  "coluzzii"  = "#E69F00",   # amber
  "funestus"  = "#AE2B47",   # sky blue
  "gambiae"   = "#4A2C6E"    # teal
)


#converting height in centimeters to meters
mos_rest$height_from_ground_m <- (mos_rest$height_from_ground)/100
mos_rest$dist_from_nearest_wall_m <- abs(mos_rest$dist_from_nearest_wall/100)
summary(mos_rest)

####################################################################################################
#Data Exploratory Analysis
#####################################################################################################

# plotting the average community indoor temperature (CIT) by mosquito resting-site temperature (MRST)
plt_v1 <- ggplot(mos_rest, aes(x = temp_indoor, y = temp,
                               group  = village,
                               colour = village,
                               fill   = species,
                               shape  = resting_surface)) +
  geom_jitter(width = 0.7, height = 0.5, alpha = 0.3,
              size = 2.0, stroke = 1.2) +
  
  labs(x      = "CIT (°C)",
       y      = "MRST (°C)",
       colour = "Village",
       fill   = "Species",
       shape  = "Resting surface") +
  
  scale_colour_manual(values = village_colors) +
  scale_fill_manual(values = species_fills) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  
  scale_y_continuous(breaks = seq(20, 45, 5),limits = c(20, 45)) +
  scale_x_continuous(breaks = seq(24, 43, 3),limits = c(24, 43)) +
  
  # ── Legend styling modelled on the image ──────────────────────────────────
  guides(
    # Village: show stroke colour clearly, filled circle, no fill interference
    colour = guide_legend(
      title          = "Village",
      title.position = "top",
      override.aes   = list(shape = 21, size = 5, stroke = 2.5,
                            fill  = "white"),
      order = 1
    ),
    # Species: filled circle (shape 21), thin neutral stroke
    fill = guide_legend(
      title          = "Species",
      title.position = "top",
      override.aes   = list(shape = 19, size = 4, stroke = 0.4,
                            colour = "grey40"),
      order = 2
    ),
    # Resting surface: each shape shown in neutral grey fill + dark stroke
    shape = guide_legend(
      title          = "Resting surface",
      title.position = "top",
      override.aes   = list(size = 4, fill = "grey70",
                            colour = "grey20", stroke = 0.8),
      order = 3
    )
  ) +
  
  theme_bw() +
  theme(
    panel.grid.major.x   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    
    axis.text.x          = element_text(vjust = 0.5, size = 12),
    axis.text.y          = element_text(vjust = 0.5, size = 12),
    axis.title.x         = element_text(size = 12),
    axis.title.y         = element_text(size = 12),
    
    # ── Legend box styling (matches the image's clean boxed legend) ────────
    legend.position      = "none",
    legend.direction     = "vertical",
    legend.box           = "vertical",       # stack the three legend blocks
    legend.box.spacing   = unit(0.3, "cm"),  # space between legend groups
    legend.spacing.y     = unit(0.2, "cm"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(0.5, "cm"),
    legend.text          = element_text(size = 12, face = "italic"),  # italic for species names
    legend.title         = element_text(size = 12),
    legend.background    = element_rect(fill = "white", colour = "grey80",
                                        linewidth = 0.4),  # subtle box around full legend
    legend.margin        = margin(6, 8, 6, 8)
  )

print(plt_v1)

###############################################################################################
##############################################################################################
# plotting the  relative humidity (RH) by mosquito resting-site temperature (MRST)
plt_v2 <- ggplot(mos_rest, aes(x = rh, y = temp,
                               group  = village,
                               colour = village,
                               fill   = species,
                               shape  = resting_surface)) +
  geom_jitter(width = 0.7, height = 0.5, alpha = 0.3,
              size = 2.0, stroke = 1.2) +
  
  labs(x      = "Relative humidity (%)",
       y      = " ", #"MRST (°C)",
       colour = "Village",
       fill   = "Species",
       shape  = "Resting surface") +
  
  scale_colour_manual(values = village_colors) +
  scale_fill_manual(values = species_fills) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  
  scale_y_continuous(breaks = seq(20, 45, 5),limits = c(20, 45)) +
  scale_x_continuous(breaks = seq(48, 90, 5),limits = c(48, 90)) +
  
  # ── Legend styling modelled on the image ──────────────────────────────────
  guides(
    # Village: show stroke colour clearly, filled circle, no fill interference
    colour = guide_legend(
      title          = "Village",
      title.position = "top",
      override.aes   = list(shape = 21, size = 5, stroke = 2.5,
                            fill  = "white"),
      order = 1
    ),
    # Species: filled circle (shape 21), thin neutral stroke
    fill = guide_legend(
      title          = "Species",
      title.position = "top",
      override.aes   = list(shape = 19, size = 4, stroke = 0.4,
                            colour = "grey40"),
      order = 2
    ),
    # Resting surface: each shape shown in neutral grey fill + dark stroke
    shape = guide_legend(
      title          = "Resting surface",
      title.position = "top",
      override.aes   = list(size = 4, fill = "grey70",
                            colour = "grey20", stroke = 0.8),
      order = 3
    )
  ) +
  
  theme_bw() +
  theme(
    panel.grid.major.x   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    
    axis.text.x          = element_text(vjust = 0.5, size = 12),
    axis.text.y          = element_text(vjust = 0.5, size = 12),
    axis.title.x         = element_text(size = 12),
    axis.title.y         = element_text(size = 12),
    
    # ── Legend box styling (matches the image's clean boxed legend) ────────
    legend.position      = "none",
    legend.direction     = "vertical",
    legend.box           = "vertical",       # stack the three legend blocks
    legend.box.spacing   = unit(0.3, "cm"),  # space between legend groups
    legend.spacing.y     = unit(0.2, "cm"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(0.5, "cm"),
    legend.text          = element_text(size = 12, face = "italic"),  # italic for species names
    legend.title         = element_text(size = 12),
    legend.background    = element_rect(fill = "white", colour = "grey80",
                                        linewidth = 0.4),  # subtle box around full legend
    legend.margin        = margin(6, 8, 6, 8)
  )

print(plt_v2)

##############################################################
#plotting the height from the ground in meters by mosquito resting-site temperature (MRST)
plt_v3 <- ggplot(mos_rest, aes(x = height_from_ground_m, y = temp,
                               group  = village,
                               colour = village,
                               fill   = species,
                               shape  = resting_surface)) +
  geom_jitter(width = 0.7, height = 0.5, alpha = 0.3,
              size = 2.0, stroke = 1.2) +
  
  labs(x      = "Height from the grounds (m)",
       y      = "MRST (°C)",
       colour = "Village",
       fill   = "Species",
       shape  = "Resting surface") +
  
  scale_colour_manual(values = village_colors) +
  scale_fill_manual(values = species_fills) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  
  scale_y_continuous(breaks = seq(20, 45, 5),limits = c(20, 45)) +
  scale_x_continuous(breaks = seq(0,3.5, 0.5),limits = c(0, 3.5)) +
  
  # ── Legend styling modelled on the image ──────────────────────────────────
  guides(
    # Village: show stroke colour clearly, filled circle, no fill interference
    colour = guide_legend(
      title          = "Village",
      title.position = "top",
      override.aes   = list(shape = 21, size = 5, stroke = 2.5,
                            fill  = "white"),
      order = 1
    ),
    # Species: filled circle (shape 21), thin neutral stroke
    fill = guide_legend(
      title          = "Species",
      title.position = "top",
      override.aes   = list(shape = 19, size = 4, stroke = 0.4,
                            colour = "grey40"),
      order = 2
    ),
    # Resting surface: each shape shown in neutral grey fill + dark stroke
    shape = guide_legend(
      title          = "Resting surface",
      title.position = "top",
      override.aes   = list(size = 4, fill = "grey70",
                            colour = "grey20", stroke = 0.8),
      order = 3
    )
  ) +
  
  theme_bw() +
  theme(
    panel.grid.major.x   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    
    axis.text.x          = element_text(vjust = 0.5, size = 12),
    axis.text.y          = element_text(vjust = 0.5, size = 12),
    axis.title.x         = element_text(size = 12),
    axis.title.y         = element_text(size = 12),
    
    # ── Legend box styling (matches the image's clean boxed legend) ────────
    legend.position      = "none",
    legend.direction     = "vertical",
    legend.box           = "vertical",       # stack the three legend blocks
    legend.box.spacing   = unit(0.3, "cm"),  # space between legend groups
    legend.spacing.y     = unit(0.2, "cm"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(0.5, "cm"),
    legend.text          = element_text(size = 12, face = "italic"),  # italic for species names
    legend.title         = element_text(size = 12),
    legend.background    = element_rect(fill = "white", colour = "grey80",
                                        linewidth = 0.4),  # subtle box around full legend
    legend.margin        = margin(6, 8, 6, 8)
  )

print(plt_v3)


################################################################################
## plotting the distance from the nearest wall in meters by mosquito resting-site temperature (MRST)
plt_v4 <- ggplot(mos_rest, aes(x = dist_from_nearest_wall_m, y = temp,
                               group  = village,
                               colour = village,
                               fill   = species,
                               shape  = resting_surface)) +
  geom_jitter(width = 0.7, height = 0.5, alpha = 0.3,
              size = 2.0, stroke = 1.2) +
  
  labs(#caption = "Data Source: GAEC-BNARI",
       x      = "Distance from nearest wall (m)",
       y      = " ",#"MRST (°C)",
       colour = "Village",
       fill   = "Species",
       shape  = "Resting surface") +
  
  scale_colour_manual(values = village_colors) +
  scale_fill_manual(values = species_fills) +
  scale_shape_manual(values = c(21, 22, 23, 24, 25)) +
  
  scale_y_continuous(breaks = seq(20, 45, 5),limits = c(20, 45)) +
  scale_x_continuous(breaks = seq(0, 2, 0.5),limits = c(0, 2)) +
  
  # ── Legend styling modelled on the image ──────────────────────────────────
  guides(
    # Village: show stroke colour clearly, filled circle, no fill interference
    colour = guide_legend(
      title          = "Village",
      title.position = "top",
      override.aes   = list(shape = 21, size = 5, stroke = 2,
                            fill  = "white"),
      order = 1
    ),
    # Species: filled circle (shape 21), thin neutral stroke
    fill = guide_legend(
      title          = "Species",
      title.position = "top",
      override.aes   = list(shape = 21, size = 4, stroke = 0.4,
                            colour = "grey40"),
      order = 2
    ),
    # Resting surface: each shape shown in neutral grey fill + dark stroke
    shape = guide_legend(
      title          = "Resting surface",
      title.position = "top",
      override.aes   = list(size = 4, fill = "grey70",
                            colour = "grey20", stroke = 0.8),
      order = 3
    )
  ) +
  
  theme_bw() +
  theme(
    panel.grid.major.x   = element_blank(),
    panel.grid.major.y   = element_blank(),
    panel.grid.minor     = element_blank(),
    
    axis.text.x          = element_text(vjust = 0.5, size = 12),
    axis.text.y          = element_text(vjust = 0.5, size = 12),
    axis.title.x         = element_text(size = 12),
    axis.title.y         = element_text(size = 12),
    
    # ── Legend box styling (matches the image's clean boxed legend) ────────
    legend.position      = "right",
    legend.direction     = "vertical",
    legend.box           = "vertical",       # stack the three legend blocks
    legend.box.spacing   = unit(0.3, "cm"),  # space between legend groups
    legend.spacing.y     = unit(0.2, "cm"),
    legend.key           = element_rect(fill = "white", colour = NA),
    legend.key.size      = unit(0.1, "cm"),
    legend.text          = element_text(size = 12, face = "italic"),  # italic for species names
    legend.title         = element_text(size = 12),
    legend.background    = element_rect(fill = "white", colour = "grey80",
                                        linewidth = 0.4),  # subtle box around full legend
    legend.margin        = margin(6, 8, 6, 8)
  )

print(plt_v4)

###########################################################################
#arranging all four plots together
#########################################################################
combined_plt <- (plt_v1 + plt_v2)/ ( plt_v3 + plt_v4) +
  plot_layout(guides = "collect") +
  plot_annotation(
    tag_levels = "I",
    caption = "Data Source: GAEC-BNARI", 
    #title = "Population-level relationship between resting height and temperature metrics",
    #subtitle = "Black line = posterior mean; dark band = 50% predictive interval; light band = 95% predictive interval",
    theme = theme(
      plot.title = element_text(size = 13),
      plot.subtitle = element_text(size = 10, colour = "black"),
      legend.position = "right"
    )
  )
 print(combined_plt)

 # Save 
 ggsave("figure/CIT_vs_rh_scatter_plot.pdf",height = 450/30, width = 700/30, units = "cm", dpi = 300)
 

#---------------------------------------------------------------------------------------
###################################################################################
#PART B: •	Calculate the difference between the observed temperature and the 
#community/satellite derived temperature
###################################################################################

#“Calculate the difference between the temperature at mosquitoes resting site and 
#the community indoor temperature.This represents how much cooler or warmer the mosquito’s 
 #resting spot was compared to the broader environment.
 
mos_rest <- mos_rest%>%
  mutate(temp_diff = temp - temp_indoor) # Calculate temperature difference

#Question: how much does mosquito resting temperature deviate from the 
#community indoor temperature in general (e.g across all observations)?
#we will use the MAD (Median Absolute Deviation) to describe the overall 
#variability robustly.

# Calculate the median absolute deviation (MAD)
MAD_temp_diff <- mad(mos_rest$temp_diff, na.rm = TRUE)
MAD_temp_diff
#interpretation of MAD value: This means the typical deviation of the 
#temperature difference between observed mosquito resting temperature and 
#community indoor temperature  values from the median temperature difference 
#is about 1.19°C. Implying that most mosquitoes rested in locations where the 
#temperature was about plus/minus 1.19°C (higher or less than the community 
#indoor temperature) from the typical (median) temperature difference.

#“Calculate the difference between the temperature at mosquitoes resting site and 
#the satelite community temperature.
#This represents how much cooler or warmer the mosquito’s resting spot was compared to
#the broader environment.
mos_rest <- mos_rest%>%
  mutate(temp_diff_satelite = temp - satelite_outdoor_temp) # Calculate temperature difference

#Question: how much does mosquito resting temperature deviate from the 
#community satelight temperature in general (e.g across all observations)?
#we will use the MAD (Median Absolute Deviation) to describe the overall 
#variability robustly.

# Calculate the median absolute deviation (MAD)
MAD_temp_diff_satelite <- mad(mos_rest$temp_diff_satelite, na.rm = TRUE)
MAD_temp_diff_satelite
#interpretation of MAD value: This means the typical deviation of the 
#temperature difference between observed mosquito resting temperature and 
#community satelite temperature  values from the median temperature difference 
#is about 2.15°C. Implying that most mosquitoes rested in locations where the 
#temperature was about plus/minus 2.15°C (higher or less than the community 
#satelite temperature) from the typical (median) temperature difference. 

#---------------------------------------------------------------------------------------
#let calculate the difference between the mosquito resting temperature and sat_temp_min and sat_temp_max
mos_rest <- mos_rest %>%
  mutate(temp_diff_satelite_min = temp - sat_temp_min, # Calculate temperature difference
         temp_diff_satelite_max = temp - sat_temp_max) # Calculate temperature difference 

# Calculate the median absolute deviation (MAD) for min and max satellite temperature differences
MAD_temp_diff_satelite_min <- mad(mos_rest$temp_diff_satelite_min, na.rm = TRUE)
MAD_temp_diff_satelite_min

MAD_temp_diff_satelite_max <- mad(mos_rest$temp_diff_satelite_max, na.rm = TRUE)
MAD_temp_diff_satelite_max

###############################################################################################
#Plotting the Median Absolute deviations
################################################################################################
#------------------------------------------------------------------------------------------
#plotting a histograme for mosquito resting temperature
p1 <- ggplot(mos_rest, aes(x = temp, fill = village, group = village)) +
  geom_histogram(binwidth = 1, colour = "black", alpha = 0.7, position = "dodge") +
  labs(#title = "Histogram of Mosquito Resting Temperature",
    #caption = "Data Source: GAEC-BNARI",
    x = "MRST (°C)",
    y = "Count",
    fill = "Villages",
    group = "Villages") +
  scale_y_continuous( expand = c(0,1),
                      limits = nicelimits,
                      breaks = seq(0, 140, 10)) +  # Set y-axis limits
  # scale_y_continuous(expand = c( 0, 1 ),
  #facet_wrap(~ecological_zone, ncol = 2) +  # Facet by resting surface
  #coord_flip(clip = "off") +
  # Then apply the color palette:
  scale_fill_manual(values = village_colors) +
  #scale_color_manual(values = village_colors) +
  theme_bw() +
  theme(#panel.grid.major.x = element_blank(), #element_line(color = "gray", size = 0.25),
    #panel.grid.major.y = element_line(color = "gray", size = 0.25),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.5, size = 12),
    axis.text.y = element_text(vjust = 0.5, size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "none")
print(p1)

############################################################################################################
#plot a histogram for the difference between mosquito resting temperature and community indoor temperature
p2 <- ggplot(mos_rest, aes(x = temp_diff, fill = village, group = village)) +
  geom_histogram(binwidth = 1, alpha = 0.7, colour = "black", position = "dodge") +
  labs(#title = "Histogram of Temperature Difference (MRST - CIT)",
    #caption = "Data Source: GAEC-BNARI",
    x = "CIT difference (°C)",
    y = "Count",
    fill = "Villages",
    group = "Villages") +
  scale_y_continuous( expand = c(0,1),
                      limits = nicelimits,
                      breaks = seq(0, 60, 10)) +  # Set y-axis limits
  #scale_x_continuous(expand = c( -10, 10 )) +
  #facet_wrap(~ecological_zone, ncol = 2) +  # Facet by resting surface
  scale_fill_manual(values = village_colors) +
  #coord_flip(clip = "off") +
  theme_bw() + 
  #positive bound of mad difference
  geom_vline(xintercept = MAD_temp_diff, linetype = "dashed", 
             color = "#F98B00", size = 1.1) + 
  #negative bound of mad difference
  geom_vline(xintercept = -MAD_temp_diff, linetype = "dashed", 
             color = "#F98B00", size = 1.1) + 
  # Add horizontal dashed lines at y = 0 for reference
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.2,
             color = "black") + 
  # Annotate MAD for Indoor Temp (positive and negative)
  annotate("text", x = max(mos_rest$days_count) - 3.5, 
           y = MAD_temp_diff  + 50 , 
           label = paste0("MAD CIT: ± ", round(MAD_temp_diff, 2)), 
           color = "#F98B00", hjust = 1.2, size = 3.5) + 
  theme(#panel.grid.major.x = element_blank(), #element_line(color = "gray", size = 0.25),
    #panel.grid.major.y = element_line(color = "gray", size = 0.25),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.5, size = 12),
    axis.text.y = element_text(vjust = 0.5, size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "none") 
print(p2)

##############################################################################################################
#plot a histogram for the difference between mosquito resting temperature and community satellite temperature
p3 <- ggplot(mos_rest, aes(x = temp_diff_satelite, fill = village, group = village)) +
  geom_histogram(binwidth = 1, alpha = 0.7, colour = "black", position = "dodge") +
  labs(#title = "Histogram of Temperature Difference (MRST - SDT)",
    #caption = "Data Source: GAEC-BNARI",
    x = "Mean SDT difference (°C)",
    y = "Count",
    fill = "Villages",
    group = "Villages") +
  scale_y_continuous( expand = c(0,1),
                      limits = nicelimits,
                      breaks = seq(0, 60, 10)) +  # Set y-axis limits
  #scale_x_continuous(expand = c( -10, 10 )) +
  #facet_wrap(~ecological_zone, ncol = 2) +  # Facet by resting surface
  scale_fill_manual(values = village_colors) +
  #coord_flip(clip = "off") +
  #positive bound of mad difference
  geom_vline(xintercept = MAD_temp_diff_satelite, linetype = "dashed", 
             color = "#00B0F0", size = 1.1) +
  
  #negative bound of mad difference
  geom_vline(xintercept = -MAD_temp_diff_satelite, linetype = "dashed", 
             color = "#00B0F0", size = 1.1) + 
  # Add horizontal dashed lines at y = 0 for reference
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.2,
             color = "black") + 

  # Annotate MAD for Satellite Temp (positive and negative)
  annotate("text", x = max(mos_rest$temp_diff_satelite) - 4.5, 
           y = MAD_temp_diff_satelite + 50, 
           label = paste0("MAD SDT: ± ", format(round(MAD_temp_diff_satelite, 2), nsmall=2)), 
           color = "#00B0F0", hjust = 0.8, size = 3.5) +
  
  theme_bw() +
  theme(#panel.grid.major.x = element_blank(), #element_line(color = "gray", size = 0.25),
    #panel.grid.major.y = element_line(color = "gray", size = 0.25),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.5, size = 12),
    axis.text.y = element_text(vjust = 0.5, size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "none") 
print(p3)

###################################################################################################################
#let p4_min for the difference between mosquito resting temperature and community satellite minimum temperature
p4_min <- ggplot(mos_rest, aes(x = temp_diff_satelite_min, fill = village, group = village)) +
  geom_histogram(binwidth = 1, alpha = 0.7, colour = "black", position = "dodge") +
  labs(#title = "Histogram of Temperature Difference (MRST - SDT Min)",
    #caption = "Data Source: GAEC-BNARI",
    x = " Min-SDT difference (°C)",
    y = "Count",
    fill = "Villages",
    group = "Villages") +
  scale_y_continuous( expand = c(0,1),
                      limits = nicelimits,
                      breaks = seq(0, 60, 10)) +  # Set y-axis limits
  #scale_x_continuous(expand = c( -10, 10 )) +
  #facet_wrap(~ecological_zone, ncol = 2) +  # Facet by resting surface
  scale_fill_manual(values = village_colors) +
  #coord_flip(clip = "off") +
  
  #positive bound of mad difference
  geom_vline(xintercept = MAD_temp_diff_satelite_min, linetype = "dashed", 
             color = "turquoise3", size = 1.1) +
  
  #negative bound of mad difference
  geom_vline(xintercept = -MAD_temp_diff_satelite_min, linetype = "dashed", 
             color = "turquoise3", size = 1.1) + 
  # Add horizontal dashed lines at y = 0 for reference
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.2,
             color = "black") + 
  
  # Annotate MAD for Satellite Temp (positive and negative)
  annotate("text", x = max(mos_rest$temp_diff_satelite_min) - 4.5, 
           y = MAD_temp_diff_satelite_min + 50, 
           label = paste0("MAD SDT Min: ± ", format(round(MAD_temp_diff_satelite_min, 2), nsmall = 2)), 
           color = "turquoise3", hjust = 0.8, size = 3.5) +
  
  theme_bw() +
  theme(#panel.grid.major.x = element_blank(), #element_line(color = "gray", size = 0.25),
    #panel.grid.major.y = element_line(color = "gray", size = 0.25),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.5, size = 12),
    axis.text.y = element_text(vjust = 0.5, size = 12),
    legend.text = element_text(size = 12),
    axis.title.x = element_text(size = 12),
    axis.title.y = element_text(size = 12),
    legend.title = element_text(size = 12),
    legend.position = "none") 
print(p4_min)

################################################################################################################
#let p5_max for the difference between mosquito resting temperature and community satellite maximum temperature
p5_max <- ggplot(mos_rest, aes(x = temp_diff_satelite_max, fill = village, group = village)) +
  geom_histogram(binwidth = 1, alpha = 0.7, colour = "black", position = "dodge") +
  labs(#title = "Histogram of Temperature Difference (MRST - SDT Max)",
    #caption = "Data Source: GAEC-BNARI",
    x = "Max-SDT difference (°C)",
    y = "Count",
    fill = "Villages",
    group = "Villages") +
  scale_y_continuous( expand = c(0,1),
                      limits = nicelimits,
                      breaks = seq(0, 60, 10)) +  # Set y-axis limits
  #scale_x_continuous(expand = c( -10, 10 )) +
  #facet_wrap(~ecological_zone, ncol = 2) +  # Facet by resting surface
  scale_fill_manual(values = village_colors) +
  #coord_flip(clip = "off") +
  
  #positive bound of mad difference
  geom_vline(xintercept = MAD_temp_diff_satelite_max, linetype = "dashed", 
             color = "#FF6666", size = 1.1) +
  
  #negative bound of mad difference
  geom_vline(xintercept = -MAD_temp_diff_satelite_max, linetype = "dashed", 
             color = "#FF6666", size = 1.1) + 
  # Add horizontal dashed lines at y = 0 for reference
  geom_vline(xintercept = 0, linetype = "dashed", size = 1.2,
             color = "black") + 
  
  # Annotate MAD for Satellite Temp (positive and negative)
  annotate("text", x = max(mos_rest$temp_diff_satelite_max) - 20, 
           y = MAD_temp_diff_satelite_max + 50, 
           label = paste0("MAD SDT Max: ± ", format(round(MAD_temp_diff_satelite_max, 2), nsmall = 2)), 
           color = "#FF6666", hjust = 0.4, size = 3.5) +
  
  theme_bw() +
  theme(#panel.grid.major.x = element_blank(), #element_line(color = "gray", size = 0.25),
    #panel.grid.major.y = element_line(color = "gray", size = 0.25),
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(vjust = 0.5, size = 10),
    axis.text.y = element_text(vjust = 0.5, size = 10),
    legend.text = element_text(size = 10),
    axis.title.x = element_text(size = 11),
    axis.title.y = element_text(size = 11),
    legend.title = element_text(size = 11),
    legend.position = "right") 
print(p5_max)


############################################################################################
##  putting all plots together 
############################################################################################
# Combine the plots using patchwork and add caption

plot <- (p1) | (p2 / p4_min / p3 / p5_max) 

 final_plot <- plot + plot_layout(guides = "collect") +
  plot_annotation(tag_levels = "I",
    caption = "Data Source: GAEC-BNARI"
  ) &
  theme(
    legend.position = "right",
    plot.caption = element_text(face = "italic")
  )

print(final_plot)

#save plot 
ggsave("figure/temp_difference_histograms_v1.pdf", plot = final_plot,
       height = 450/30, width = 700/30, units = "cm", dpi = 300) 


##############################################################################################
###
#### performing a boxplot and adding ANOVA/Kruskal-Wallis test results
#Question: Do resting locations(like height_from_ground, distance from the nearest wall,)
#differ by indoor temperature or (differ by species)?

#How mosquito resting height differs by community indoor temperature category
#(using a boxplot and Kruskal-Wallis test)
################################################################################

#Question: Do mosquitoes select cooler indoor spots when indoor temperatures are higher?
#Can we identify whether mosquitoes choose cooler places (collection location & 
#resting surface) based on indoor community temperature?

# We want to test whether higher indoor community temperatures are associated
# with mosquitoes selecting cooler indoor spots (i.e., larger negative temp_diff).
# 
# 🔹 Option A: Correlation
cor.test(mos_rest$temp_indoor, mos_rest$temp_diff, method = "pearson")
# The correlation coefficient is -0.72998, indicating a strong negative correlation
#t-value -22.122, df = 429, p-value is 2.2e-16

#we have a significant negative correlation between community indoor temperature 
#and the temperature difference with value -0.72998. which suggest that 
#as indoor temperature increases, mosquitoes select cooler resting spots. 


# 🔹 Option B: simple linear regression
#If we find: Negative temp_diff when community indoor temperature is high, then yes, mosquitoes 
#are selecting cooler places to rest indoors.
# a negative coefficient for indoor community temperature would suggest that
#mosquitoes select cooler resting spots when indoor temperatures are higher.
lm_diff <- lm(temp_diff ~ temp_indoor, data = mos_rest)
summary(lm_diff)
# The coefficient for temp_indoor is negative(-0.62998), indicating that as indoor temperature increases,
# the temperature difference (temp_diff) decreases. This suggests that mosquitoes are indeed
# selecting cooler resting spots when indoor temperatures are higher.



