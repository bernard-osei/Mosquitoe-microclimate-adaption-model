rm(list = ls())

#loading libraries
library(deSolve); library(tidyverse); library(zipfR); library(GoFKernel); library(ggnewscale)
library(cowplot); library(suncalc); library(chillR); library(ggpmisc)
library(lubridate); library(zoo); library(rio); library(RColorBrewer);library(foreach); library(doParallel)
library(readxl); library(patchwork); library(tidyr)

source(file = "EIP_HMTP_DTR-main/utils/model_functions.R")
source(file = "EIP_HMTP_DTR-main/utils/read_libraries_data_copy.R")
source(file = "EIP_HMTP_DTR-main/utils/read_bt_data.R")
source(file = "script/functions.R") 

#theme
theme_gh <- theme_bw() +
    theme(panel.grid.major.x=element_blank(), #(color = "gray", size=0.25),
          panel.grid.major.y=element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(vjust = 0.5, size = 12),
          axis.text.y = element_text(vjust = 0.5, size = 12),
          legend.text = element_text(size = 12),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title = element_text(size = 12),
          legend.position = "right")

# ============================================================================
# SECTION 1: LOAD DATA
# ============================================================================
# Adjust path to wherever simulated_sat_v3.csv is located
dat_raw <- read_csv("tables/simulated_sat_v3.csv", show_col_types = FALSE)

cat("Raw data dimensions:", nrow(dat_raw), "x", ncol(dat_raw), "\n")

# Identify SDT simulation columns (named SDT_xx.xx)
sdt_cols <- names(dat_raw)[str_detect(names(dat_raw), "^SDT_")]
cat("SDT columns found:", length(sdt_cols), "\n", paste(sdt_cols, collapse = ", "), "\n")

# Metadata columns to retain alongside temperatures
# NOTE: sat_temp_min and sat_temp_max are real observed satellite data and are
# NOT used for DTR estimation. DTR is derived purely from the simulated
# temperature distribution (SDT_24.11 and SDT_27.90 columns).
meta_cols <- c("nicd_no", "ecological_zone", "locality",
               "village", "longitude", "latitude", "date", "days_count",
               "temp_indoor", "sat_temp",
               "sat_temp_min", "sat_temp_max", "temp",
               "resting_site", "material_type","resting_surface", "morphological_id",
               "species", "blood_source", "point_x", "point_y",
               "observed_date_level_SDT","model_predicted_Mean_Temp","mean_simulated_MRST"
               #"sdt_temp", "mrst_model_fit","sim_mrst_mean","sim_mrst_sd", "sim_mrst_lo95",
               #"sim_mrst_hi95","sim_mrst_lo50","sim_mrst_hi50","mrst_model_fit_vs_sdt_temp_diff",
               #"sim_mrst_mean_vs_sdt_temp_diff"
               )

# Keep only metadata + SDT columns
dat_sel <- dat_raw %>%
  select(all_of(c(meta_cols))) %>% 
  mutate(#date = as.Date(date, format = "%Y/%m/%d"),  # Convert to Date
    date = as.Date(date, format="%m/%d/%Y"),#, tz = "UTC"),    # Create separate datetime column
    year = year(date),
    month = month(date),
    #hour = hour(date),
    day = day(date),
    quarter = as.yearqtr(date, format= "%m/%d/%Y"),
    f_date = floor_date(date, unit = "day"),
    location1 = "indoor",
    location2 = "outdoor",
    mean_simulated_MRST =  as.numeric(mean_simulated_MRST),
    model_predicted_Mean_Temp = as.numeric(model_predicted_Mean_Temp),
    temp = as.numeric(temp)
  )

# # Reshape to long format:
# #   One row per (mosquito x SDT_community) combination
# dat_long <- dat_sel %>%
#   pivot_longer(
#     cols      = all_of(sdt_cols),
#     names_to  = "SDT_group",
#     values_to = "simulated_MRST"
#   ) %>%
#   mutate(mean_SDT = as.numeric(str_remove(SDT_group, "SDT_")))
# 
# cat("Long format:", nrow(dat_long), "rows\n")
# cat("Temperature range:", round(min(dat_long$simulated_MRST, na.rm=TRUE), 2),
#     "to", round(max(dat_long$simulated_MRST, na.rm=TRUE), 2), "°C\n") 


#####################################################################################

DTR_data <- dat_sel %>% mutate(s_date = as.Date(f_date)) %>% 
  group_by(s_date,ecological_zone,locality,location1, village, month, day) %>% 
  summarise(DTR = format(round(max(mean_simulated_MRST) - min(mean_simulated_MRST),2),nsmall = 2))#%>% mutate(Location = ifelse(house == "ERA5", "ERA5", Location))

ggsave(file = "figure/Figure_A2.2.png", 
       ggplot(data = DTR_data %>% subset(village %in% c("Osorogma","Adawukwa","Atatam","Okyereko","Agyenkwaso","Kusa Dinkyeae") & month > 1),
              aes(x = village, y = as.numeric(DTR), fill = location1, group = interaction(village, location1))) +
         geom_boxplot(alpha = 0.75) +
         theme_bw() + theme(text = element_text(size = 18),
                            axis.text.x = element_text(angle = 45, hjust = 1)) +
         xlab("Villages") +
         ylab("Diurnal temperature range (DTR) (°C)") +
         scale_fill_manual(values = c("#56B4E9","#CC79A7","#E69F00"), name = "Temperature\ndata") +
         scale_y_continuous(breaks = seq(8, 18, 1)),
       height = 450/30, width = 700/30, units = "cm", device = "pdf"
)

##########################################################################################
# # mean monthly temp
# monthly_temp <- dat_sel %>%
#   group_by(year, month, village, location1) %>% dplyr::summarise(m_temp = round(mean(temp),2)) %>% 
#   dplyr::left_join(dat_long, by = c("year", "month", "village", "location1"))
# 
# # mean daily temp
# daily_temp <- dat_long %>%
#   group_by(year, month, date, village, location1) %>% dplyr::summarise(d_temp = round(mean(temp),2)) %>% 
#   dplyr::left_join(dat_long, by = c("year", "date", "month", "village", "location1"))
# 
# temp_plot_df <- rbind(#BF_Tiefora_data[,c("Date", "Location", "tas_simple")] %>% rename(Temp = tas_simple) %>% mutate(name = "Hourly"),
#   daily_temp[,c("date", "location1", "d_temp")] %>% rename(Temp = d_temp) %>% mutate(name = "Daily"),
#   monthly_temp[,c("date", "location1", "m_temp")] %>% rename(Temp = m_temp) %>% mutate(name = "Monthly"))
# 
# temp_plot_df$name <- factor(temp_plot_df$name, levels = c("Daily", "Monthly"))

##############################################################################################
# mean monthly temp
# monthly_temp <- dat_sel%>%
#   group_by(year, month, village, location1) %>% dplyr::summarise(m_temp = format(round(mean(mean_simulated_MRST),2), nsmall = 2))%>% 
#   dplyr::left_join(dat_sel, by = c("year", "month", "village", "location1"))

# mean daily temp
daily_temp <- dat_sel %>%
  group_by(year, month, f_date, village, location1) %>% dplyr::summarise(d_temp = format(round(mean(mean_simulated_MRST),2), nsmall =2),
                                                                         mean_mos_temp = format(round(mean(temp),2), nsmall = 2),
                                                                         sdt_temp = format(round(mean(sat_temp),2), nsmall = 2)) %>% 
  dplyr::left_join(dat_sel, by = c("year", "f_date", "month", "village", "location1")) %>% rename(mean_sim_temp = d_temp)
  
temp_plot_df <- rbind(#temp_data_plot_tiefora[,c("f_date", "Location", "Temp")] %>% mutate(name = "Hourly"),
  daily_temp[,c("f_date", "day", "location1", "village", "ecological_zone", "mean_sim_temp","mean_mos_temp", "sdt_temp")] %>%
    rename(mean_sim_temp = mean_sim_temp) %>% mutate(name = "Daily")
  #monthly_temp[,c("f_date", "location1", "m_temp")] %>% rename(Temp = m_temp) %>% mutate(name = "Monthly")
  )

temp_plot_df$name <- factor(temp_plot_df$name, levels = c("Daily")) #"Monthly")) #"Hourly", 
temp_plot_df$mean_sim_temp <- as.numeric(temp_plot_df$mean_sim_temp)
temp_plot_df$mean_mos_temp <- as.numeric(temp_plot_df$mean_mos_temp)
temp_plot_df$sdt_temp <- as.numeric(temp_plot_df$sdt_temp)

##############################################################################################
ggsave(file = "figure/Figure_A2_temp_distr.png",
#temp_plot <- 
  ggplot(data = temp_plot_df,
                    aes(x = f_date, y = mean_sim_temp, col = name, linetype = name)) +
  geom_line(linewidth = 1, alpha = 1.0) +
  #geom_line(aes(linewidth = name, linetype = name), alpha = 1) +
  scale_linetype_manual(values = c(1, 2)) +
  # geom_smooth(method = "gam", formula = y ~ s(x, bs = "tp"), se = FALSE,
  #             linewidth = 1.0) +
  #theme_gh +
  #theme_bw() + theme(text = element_text(size = 18)) +
  theme_gh + theme(text = element_text(size = 18),
                     axis.text.x = element_text(angle = 45, hjust = 1)) +
  scale_linewidth_manual(values = c(0.18, 1)) +
  #geom_smooth(formula = y ~ x, se = FALSE, method = "gam", alpha = 0.25, size = 0.75) +
  labs( x = "Date",
        y = "Temperature (°C)",
        linetype ="Temperature\ndata") +
  #facet_wrap(~Location, nrow = 3) +
  scale_y_continuous(breaks = seq(20, 40, 5), limits = c(20, 40)) +
  scale_colour_manual(values = c("#CC79A7","#56B4E9"), name = "Temperature\ndata") + # "#E69F00", "red"), 
  #scale_colour_viridis_d(option = "mako") + 
  scale_x_date(date_labels = "%d %b",
                   date_breaks = "1 month"),
  height = 450/30, width = 700/30, units = "cm", device = "pdf"

)


#print(temp_plot)

############################################################################################
# MODEL C: Suh & Stopard 2024 Mechanistic EIP (spline on tabulated posteriors)
# Source: Suh & Stopard et al. (2024) Nat. Commun. — Fig. 2a, pooled model EIP50
# EIP50 values (median posterior):
#   17°C = 49.1 d (95% CrI: 44.2–55.1);  30°C = 7.6 d (95% CrI: 7.2–8.0)
##############################################################################################
mean_EIP <- readRDS(file = "data/mean_EIP.rds")
#write.csv(mean_EIP, "data/mean_EIP.csv")
####################
### EIP function ###
####################
# assumes a linear interpolation for missing values
# separate EIP functions for the posterior quantiles - different mean values
# --- Version A: as written (floors anything > 30°C to the minimum EIP) -------
EIP_fun <- vector(mode = "list", length = 3)
EIP_fun[[1]] <- approxfun(mean_EIP$temp, mean_EIP$`2.5%`,  yleft = max(mean_EIP$`2.5%`),  yright = min(mean_EIP$`2.5%`))
EIP_fun[[2]] <- approxfun(mean_EIP$temp, mean_EIP$`50%`,   yleft = max(mean_EIP$`50%`),   yright = min(mean_EIP$`50%`))
EIP_fun[[3]] <- approxfun(mean_EIP$temp, mean_EIP$`97.5%`, yleft = max(mean_EIP$`97.5%`), yright = min(mean_EIP$`97.5%`))




# estimating the thermal performance curve values at each mean temperature
EIP_fun[[2]] <- Vectorize(EIP_fun[[2]])

#monthly_temp$EIP <- EIP_fun[[2]](monthly_temp$m_temp)
#monthly_temp$s_date <- as.Date(monthly_temp$date, format = "%Y-%m-%d")


############################################################################
###########################################################################
#EIP estimate with mean simulated mosquito-resting site temperature
daily_temp$EIP_mean_sim <- EIP_fun[[2]](daily_temp$mean_sim_temp)
daily_temp$EIP_mean_sim_lo95 <- EIP_fun[[1]](daily_temp$mean_sim_temp)
daily_temp$EIP_mean_sim_hi95 <- EIP_fun[[3]](daily_temp$mean_sim_temp)

#EIP estimate with mean observed mosquito-resting site temperature
daily_temp$EIP_mean_obs_temp <- EIP_fun[[2]](daily_temp$mean_mos_temp)
daily_temp$EIP_mean_obs_temp_lo95 <- EIP_fun[[1]](daily_temp$mean_mos_temp)
daily_temp$EIP_mean_obs_temp_hi95 <- EIP_fun[[3]](daily_temp$mean_mos_temp)

# #EIP estimate with mean observed satellite-derived temperature
daily_temp$EIP_mean_sdt_temp <- EIP_fun[[2]](daily_temp$sdt_temp)
daily_temp$EIP_mean_sdt_temp_lo95 <- EIP_fun[[1]](daily_temp$sdt_temp)
daily_temp$EIP_mean_sdt_temp_hi95 <- EIP_fun[[3]](daily_temp$sdt_temp)

#############################################################################
#############################################################################

#temp sat_temp mean_simulated_MRST
#EIP estimate with satellite-derived temperature
daily_temp$EIP_sat_temp <- EIP_fun[[2]](daily_temp$sat_temp)
daily_temp$EIP_sat_temp_lo95 <- EIP_fun[[1]](daily_temp$sat_temp)
daily_temp$EIP_sat_temp_hi95 <- EIP_fun[[3]](daily_temp$sat_temp)

#EIP estimate with observed MRST temperature
daily_temp$EIP_temp <- EIP_fun[[2]](daily_temp$temp)
daily_temp$EIP_temp_lo95 <- EIP_fun[[1]](daily_temp$temp)
daily_temp$EIP_temp_hi95 <- EIP_fun[[3]](daily_temp$temp)

#EIP estimate simulated MRST temperature
daily_temp$EIP_sim <- EIP_fun[[2]](daily_temp$mean_simulated_MRST)
daily_temp$EIP_sim_lo95 <- EIP_fun[[1]](daily_temp$mean_simulated_MRST)
daily_temp$EIP_sim_hi95 <- EIP_fun[[3]](daily_temp$mean_simulated_MRST)

daily_temp$s_date <- as.Date(daily_temp$date, format = "%Y-%m-%d")


EIP_plot_df <- rbind(#sum_out_m_m[,c("s_date", "Location", "EIP_50")] %>% rename(EIP = EIP_50) %>% mutate(model = "DTR dependent: daily"),
  daily_temp[,c("s_date","village", "ecological_zone","location1",
                "mean_sim_temp","mean_mos_temp", "sdt_temp", "temp","mean_simulated_MRST",
                "EIP_mean_sim", "EIP_mean_sim_lo95", "EIP_mean_sim_hi95",
                "EIP_mean_obs_temp", "EIP_mean_obs_temp_lo95", "EIP_mean_obs_temp_hi95",
                "EIP_mean_sdt_temp", "EIP_mean_sdt_temp_lo95", "EIP_mean_sdt_temp_hi95",
                "EIP_sat_temp", "EIP_sat_temp_lo95", "EIP_sat_temp_hi95",
                "EIP_temp", "EIP_temp_lo95", "EIP_temp_hi95",
                "EIP_sim", "EIP_sim_lo95", "EIP_sim_hi95")] %>% 
    mutate(model = "Daily")
  #monthly_temp[,c("s_date", "location1", "EIP")] %>% mutate(model = "Monthly")
)
#converting to numeric
EIP_plot_df <- EIP_plot_df %>%
  rename(sim_temp =mean_simulated_MRST) %>% 
  mutate(across(c(mean_sim_temp, mean_mos_temp, sdt_temp,temp,sim_temp,
                  EIP_mean_sim, EIP_mean_sim_lo95, EIP_mean_sim_hi95,
                  EIP_mean_obs_temp, EIP_mean_obs_temp_lo95, EIP_mean_obs_temp_hi95,
                  EIP_mean_sdt_temp,EIP_mean_sdt_temp_lo95,EIP_mean_sdt_temp_hi95,
                  EIP_temp, EIP_temp_lo95, EIP_temp_hi95,
                  EIP_sat_temp, EIP_sat_temp_lo95, EIP_sat_temp_hi95,
                  EIP_sim, EIP_sim_lo95, EIP_sim_hi95), as.numeric))

############################################################################################
#ggsave(file = "figure/Figure_A3_eip_temp.pdf",
#EIP_plot <-
  pltA <- ggplot(data = EIP_plot_df,
                   aes(x = sdt_temp,
                       fill = location1, col = model, y = EIP_mean_sdt_temp, linetype = model))+
  geom_line(aes(y = EIP_mean_sdt_temp),linewidth = 1, alpha = 1) +
  # EIP ribbon (95% intervals of simulated mosquito temps)
  geom_ribbon(aes(ymin = EIP_mean_sdt_temp_lo95, ymax = EIP_mean_sdt_temp_hi95),
              fill = "steelblue", alpha = 0.20) +
  scale_linetype_manual(values = c(1, 2)) +
  labs( x = "SDT(°C)",
        y = expression(paste("Predicted EIP (days)")),
        linetype = "Temp.\ndata",
        ) +  
  theme_gh + theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(8, 16, 2), limits = c(8, 16)) +
  scale_colour_manual(values = c("#E69F00","#56B4E9","#CC79A7"), name = "Temp.\ndata") +
  scale_x_continuous(breaks = seq(24, 28, 1), limits = c(24, 28))#, #+
  # scale_x_date(date_labels = "%d %b",
  #              date_breaks = "1 month",
  #              limits = as.Date(c("02/09/2011", "12/01/2012"), format = "%d/%m/%Y")) + 
  #ggtitle("EIP predictions based on observed temperatures")
  #height = 450/30, width = 700/30, units = "cm", device = "pdf" )
print(pltA)
########################################################################################
#ggsave(file = "figure/Figure_A3_eip_temp.pdf",
#EIP_plot <-
pltB <- ggplot(data = EIP_plot_df,
               aes(x = mean_sim_temp,
                   fill = location1, col = model, y = EIP_mean_sim, linetype = model))+
  geom_line(aes(y = EIP_mean_sim),linewidth = 1, alpha = 1) +
  # EIP ribbon (95% intervals of simulated mosquito temps)
  geom_ribbon(aes(ymin = EIP_mean_sim_lo95, ymax = EIP_mean_sim_hi95),
              fill = "forestgreen", alpha = 0.20) +
  scale_linetype_manual(values = c(1, 2)) +
  labs( x = "Simulated MRST(°C)",
        y = expression(paste(" ")),
        linetype = "Temp.\ndata",
  ) +  
  theme_gh + theme(legend.position = "none") +
  scale_y_continuous(breaks = seq(8, 16, 2), limits = c(8, 16)) +
  scale_colour_manual(values = c("#CC79A7","#E69F00","#56B4E9"), name = "Temp.\ndata") +
  scale_x_continuous(breaks = seq(27, 32, 1), limits = c(27, 32))#, #+
# scale_x_date(date_labels = "%d %b",
#              date_breaks = "1 month",
#              limits = as.Date(c("02/09/2011", "12/01/2012"), format = "%d/%m/%Y")) + 
#ggtitle("EIP predictions based on observed temperatures")
#height = 450/30, width = 700/30, units = "cm", device = "pdf" )
print(pltB)
#############################################################################################
#ggsave(file = "figure/Figure_A3_eip_temp.pdf",
#EIP_plot <-
pltC <- ggplot(data = EIP_plot_df,
               aes(x = mean_mos_temp,
                   fill = location1, col = model, y = EIP_mean_obs_temp, linetype = model))+
  geom_line(aes(y = EIP_mean_obs_temp),linewidth = 1, alpha = 1) +
  # EIP ribbon (95% intervals of simulated mosquito temps)
  geom_ribbon(aes(ymin = EIP_mean_obs_temp_lo95, ymax = EIP_mean_obs_temp_hi95),
              fill = "red", alpha = 0.20) +
  scale_linetype_manual(values = c(1, 2)) +
  labs( x = "Observed MRST (°C)",
        y = expression(paste(" ")),
        linetype = "Temp.\ndata",
  ) +  
  theme_gh +theme(legend.position = 'none') +
  scale_y_continuous(breaks = seq(8, 16, 2), limits = c(8, 16)) +
  scale_colour_manual(values = c("#56B4E9","#E69F00","#CC79A7"), name = "Temp.\ndata") +
  scale_x_continuous(breaks = seq(25, 33, 1), limits = c(25, 33))#, #+
# scale_x_date(date_labels = "%d %b",
#              date_breaks = "1 month",
#              limits = as.Date(c("02/09/2011", "12/01/2012"), format = "%d/%m/%Y")) + 
#ggtitle("EIP predictions based on observed temperatures")
#height = 450/30, width = 700/30, units = "cm", device = "pdf" )
print(pltC)
##########################################################################################
ggsave(file = "figure/eip_three_panels_v1.pdf",
       #fig_combined <- 
       pltA + pltB + pltC  +
         plot_annotation(
           tag_levels = "I",
           caption = "Data Source: GAEC-BNARI", 
           # caption = paste0(
           #   "A: satellite temp (per village)\n. B: simulated mosquito temp (per village). ",
           #   "C: mean observed mosquito temp (per days_count × village).\n",
           #   "All three use EIP computed from the group-mean temperature."
           # ),
           theme = theme(plot.caption = element_text(size = 9, colour = "grey40",
                                                     face = "italic"),
                         legend.position = "none")
           
         ),height = 450/30, width = 700/30, units = "cm", device = "pdf")


#############################################################################################
###################################################################
ggsave(file = "figure/Figure_A3_eip_date_v1.png", 
 #EIP_plot_date <- 
   ggplot(data = EIP_plot_df,
                   aes(x = s_date,
                       fill = location1, col = model, y = EIP_mean_sim, linetype = model))+
  geom_line(linewidth = 1, alpha = 1) +
  scale_linetype_manual(values = c(1, 2)) +
  labs( x = "Date",
        y = expression(paste("Predicted EIP (days)")),
        linetype = "Temperature\ndata",
  ) +  
  #theme_bw() + theme(text = element_text(size = 18)) +
  theme_gh + 
  scale_y_continuous(breaks = seq(8, 12, 1), limits = c(8, 12)) +
  scale_colour_manual(values = c("#E69F00","#56B4E9","#CC79A7",  "#E69F00"), name = "Temperature\ndata") +
  #scale_x_continuous(breaks = seq(27, 33, 1), limits = c(27, 32)) +
  scale_x_date(date_labels = "%d %b",
               date_breaks = "1 month",
               limits = as.Date(c("02/09/2011", "12/01/2012"), format = "%d/%m/%Y")), #+
  #ggtitle("EIP predictions based on observed temperatures")
  height = 450/30, width = 700/30, units = "cm", device = "pdf"
  )

#print(EIP_plot_date)
#######################################################################################
  #==============================================================================
  # SHARED SETUP
  #==============================================================================
  eip_unique <- EIP_plot_df %>%
    distinct(mean_sim_temp, mean_mos_temp, sdt_temp,temp,EIP_mean_sim, EIP_mean_sim_lo95, EIP_mean_sim_hi95,
             EIP_mean_obs_temp, EIP_mean_obs_temp_lo95, EIP_mean_obs_temp_hi95,
             EIP_mean_sdt_temp,EIP_mean_sdt_temp_lo95,EIP_mean_sdt_temp_hi95,
             EIP_temp, EIP_temp_lo95, EIP_temp_hi95,
             EIP_sat_temp, EIP_sat_temp_lo95, EIP_sat_temp_hi95,
             EIP_sim, EIP_sim_lo95, EIP_sim_hi95) %>%
    arrange(sdt_temp)
  
  col_sat <- "#C0392B"   # red  — satellite
  col_sim <- "#2C6E9E"   # blue — simulated
  col_mos <- "#1F6F54"   # green — mos_observed
  # Shared y-axis so the panels are directly comparable when combined
  y_breaks <- seq(8, 16, 2)
  y_limits <- c(8, 16)
  
  
  #==============================================================================
  # PANEL A — Satellite EIP vs satellite temperature (sdt_temp)
  #==============================================================================
  panelA <- ggplot(EIP_plot_df, aes(x = sdt_temp, y = EIP_mean_sdt_temp)) +
    geom_ribbon(aes(ymin = EIP_mean_sdt_temp_lo95, ymax = EIP_mean_sdt_temp_hi95),
                fill = col_sat, alpha = 0.15) +
    geom_line(colour = col_sat, linewidth = 1.0) +
    geom_point(colour = col_sat, shape = 15, size = 2.6) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    scale_x_continuous(breaks = seq(24, 28, 1), limits = c(24, 28)) +
    labs(
      #title = "A — Satellite temperature (SDT)",
      x     = "SDT (°C)",
      y     = "Estimated EIP (days) "
    ) +
    theme_gh
  
  print(panelA)
  #ggsave("figure/eip_sdt.pdf", panelA,  height = 450/30, width = 700/30, units = "cm", device = "pdf")
  
  #==============================================================================
  # PANEL B — Simulated mosquito EIP vs simulated mosquito temperature (mean_sim_temp)
  #==============================================================================
  panelB <- ggplot(eip_unique, aes(x = mean_sim_temp, y = EIP_mean_sim)) +
    geom_ribbon(aes(ymin = EIP_mean_sim_lo95, ymax = EIP_mean_sim_hi95),
                fill = col_sim, alpha = 0.15) +
    geom_line(colour = col_sim, linewidth = 1.0) +
    geom_point(colour = col_sim, shape = 16, size = 2.6) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    scale_x_continuous(breaks = seq(27, 32, 1), limits = c(27, 32)) +
    labs(
      #title = "B — Simulated mosquito temperature",
      x     = "Simulated MRST (°C)",
      y     = " "
    ) +
    theme_gh
    # theme(
    #   panel.grid.minor = element_blank(),
    #   plot.title       = element_text(face = "bold", size = 12),
    #   axis.text.x      = element_text(angle = 45, hjust = 1, size = 9)
    # )
  
  print(panelB)  
  #ggsave("figure/eip_mean_sim_temp.pdf", panelB,  height = 450/30, width = 700/30, units = "cm", device = "pdf")
  #==============================================================================
  # PANEL C — Observed mosquito EIP vs Oberved mosquito temperature (mean_mos_temp)
  #==============================================================================
  panelC <- ggplot(EIP_plot_df, aes(x = mean_mos_temp, y = EIP_mean_obs_temp)) +
    geom_ribbon(aes(ymin = EIP_mean_obs_temp_lo95, ymax = EIP_mean_obs_temp_hi95),
                fill = col_mos, alpha = 0.15) +
    geom_line(colour = col_mos, linewidth = 1.0) +
    geom_point(colour = col_mos, shape = 16, size = 2.6) +
    scale_y_continuous(breaks = y_breaks, limits = y_limits) +
    scale_x_continuous(breaks = seq(25, 33, 1), limits = c(25, 33)) +
    labs(
      #title = "B — Simulated mosquito temperature",
      x     = "Observed MRST(°C)",
      y     = ""
    ) +
    theme_gh
    # theme_bw(base_size = 12) +
    # theme(
    #   panel.grid.minor = element_blank(),
    #   plot.title       = element_text(face = "bold", size = 12),
    #   axis.text.x      = element_text(angle = 45, hjust = 1, size = 9)
    # )
  
  print(panelC) 
  #ggsave("figure/eip_obs_temp.pdf", panelC,  height = 450/30, width = 700/30, units = "cm", device = "pdf")
  ##############################################################
  
  ggsave(file = "figure/eip_three_panel.png",
  #fig_combined <- 
    panelA + panelB + panelC  +
    plot_annotation(
      tag_levels = "I",
      caption = "Data Source: GAEC-BNARI",
        #paste0(
      #   "A: satellite temp (per village)\n. B: simulated mosquito temp (per village). ",
      #   "C: mean observed mosquito temp (per days_count × village).\n",
      #   "All three use EIP computed from the group-mean temperature."
      # ),
      theme = theme(plot.caption = element_text(size = 9, colour = "grey40",
                                                face = "italic"))
    ),
  height = 450/30, width = 700/30, units = "cm", device = "pdf")
  
   
    
 # ggsave("results/eip_three_panels.pdf", fig_combined, width = 15, height = 5.5, dpi = 300)
  #ggsave("figure/eip_three_panels.png", fig_combined, width = 15, height = 5.5, dpi = 300)
################################################################################################################

  # ---- Step 3: Apply all EIP models to per-mosquito matched temperatures ----
  EIP_plot_df_v2 <- EIP_plot_df %>%
    mutate(
      # ── EIP: Degree-day (Model A) ────────────────────────────────────────
      EIP_DD_sim_temp     = eip_degreeday(sim_temp),
      EIP_DD_obs_temp     = eip_degreeday(temp),
      EIP_DD_sdt_temp     = eip_degreeday(sdt_temp),
      
      # ── EIP: Briere (Model B) ────────────────────────────────────────────
      EIP_briere_sim_temp = eip_briere(sim_temp),
      EIP_briere_obs_temp = eip_briere(temp),
      EIP_briere_sdt_temp = eip_briere(sdt_temp),   # was mislabelled "mean_sim_temp"
      
      # ── Mortality rate: Martens 2 (M1, primary) ──────────────────────────
      mu_martens2_sim_temp = mu_martens2(sim_temp),
      mu_martens2_obs_temp = mu_martens2(temp),
      mu_martens2_sdt_temp = mu_martens2(sdt_temp),
      
      # ── Mortality rate: Neil (M2, sensitivity) ───────────────────────────
      mu_neil_sim_temp     = mu_neil(sim_temp),
      mu_neil_obs_temp     = mu_neil(temp),
      mu_neil_sdt_temp     = mu_neil(sdt_temp),
      
      # ── Mortality rate: Mordecai quadratic (M3, sensitivity) ─────────────
      mu_mordecai_sim_temp = mu_mordecai_surv(sim_temp),
      mu_mordecai_obs_temp = mu_mordecai_surv(temp),
      mu_mordecai_sdt_temp = mu_mordecai_surv(sdt_temp),
      
      # ── Mortality rate: Logistic (M4, optional) ──────────────────────────
      mu_logistic_sim_temp = mu_logistic(sim_temp),
      mu_logistic_obs_temp = mu_logistic(temp),
      mu_logistic_sdt_temp = mu_logistic(sdt_temp),
      
      # ── P(survive EIP): Martens 2 mortality × each EIP model ─────────────
      # Pair each mortality estimate with the EIP at the SAME temperature source.
      # Using Martens 2 (primary) here; swap mu_* for sensitivity variants.
      
      # Degree-day EIP
      psurv_DD_martens2_sim_temp = p_survive_eip(mu_martens2_sim_temp, EIP_DD_sim_temp),
      psurv_DD_martens2_obs_temp = p_survive_eip(mu_martens2_obs_temp, EIP_DD_obs_temp),
      psurv_DD_martens2_sdt_temp = p_survive_eip(mu_martens2_sdt_temp, EIP_DD_sdt_temp),
      
      # Briere EIP
      psurv_briere_martens2_sim_temp = p_survive_eip(mu_martens2_sim_temp, EIP_briere_sim_temp),
      psurv_briere_martens2_obs_temp = p_survive_eip(mu_martens2_obs_temp, EIP_briere_obs_temp),
      psurv_briere_martens2_sdt_temp = p_survive_eip(mu_martens2_sdt_temp, EIP_briere_sdt_temp)
    )

########################################################################################################
  # Three EIP estimates for the satellite source, reshaped to long format
  EIP_long_sdt <- EIP_plot_df_v2 %>%
    select(village, sdt_temp, location1,
           EIP_DD_sdt_temp, EIP_briere_sdt_temp, EIP_sat_temp) %>%
    pivot_longer(
      cols      = c(EIP_DD_sdt_temp, EIP_briere_sdt_temp, EIP_sat_temp),
      names_to  = "eip_model",
      values_to = "EIP"
    ) %>%
    mutate(
      eip_model = factor(
        recode(eip_model,
               EIP_DD_sdt_temp     = "Degree-day",
               EIP_briere_sdt_temp = "Brière",
               EIP_sat_temp        = "mSOS"),
        levels = c("Degree-day", "Brière", "mSOS")
      )
    )
  
  # Separate frame for the mSOS ribbon (only mSOS has 95% intervals)
  EIP_msos_ribbon <- EIP_plot_df_v2 %>%
    select(village,sdt_temp, EIP_sat_temp_lo95, EIP_sat_temp_hi95) %>%
    distinct()
  
  p_sdt <-  ggplot() +
    # mSOS 95% credible ribbon (behind everything)
    geom_ribbon(data = EIP_msos_ribbon,
                aes(x = sdt_temp, ymin = EIP_sat_temp_lo95, ymax = EIP_sat_temp_hi95),
                fill = "grey70", alpha = 0.35) +
    # Continuous solid lines for all models
    geom_line(data = EIP_long_sdt,
              aes(x = sdt_temp, y = EIP, colour = eip_model),
              linewidth = 1) +
    # Points carry model identity via SHAPE
    geom_point(data = EIP_long_sdt,
               aes(x = sdt_temp, y = EIP,
                   colour = eip_model#, shape = eip_model
                   ),
               size = 2.0) +
    # scale_shape_manual(values = c("Degree-day" = 16,   # filled circle
    #                               "Brière"      = 4,   # cross
    #                               "mSOS"        = 1),  # open circle
    #                    name = "EIP models") +
    scale_colour_manual(values = c("Degree-day" = "#E69F00",
                                   "Brière"      = "#56B4E9",
                                   "mSOS"        = "#CC79A7"),
                        name = "EIP models") +
    labs(x = "SDT (°C)",
         y = "Predicted EIP (days)") +
    theme_gh + theme(legend.position = "none") +
    scale_x_continuous(breaks = seq(24, 28, 1), limits = c(24, 28)) +
    coord_cartesian(ylim = c(8, 22)) +
    scale_y_continuous(breaks = seq(8, 22, by = 2))  # step interval of 2
  
  print(p_sdt)
  ####################################################################################################
  #==============================================================================
  # SIMULATED MOSQUITO TEMPERATURE (mean_sim_temp)
  #==============================================================================
  EIP_long_sim <- EIP_plot_df_v2 %>%
    select(village, sim_temp, location1,
           EIP_DD_sim_temp, EIP_briere_sim_temp, EIP_sim) %>%
    pivot_longer(
      cols      = c(EIP_DD_sim_temp, EIP_briere_sim_temp, EIP_sim),
      names_to  = "eip_model",
      values_to = "EIP_val"
    ) %>%
    mutate(
      eip_model = factor(
        recode(eip_model,
               EIP_DD_sim_temp     = "Degree-day",
               EIP_briere_sim_temp = "Brière",
               EIP_sim                 = "mSOS"),
        levels = c("Degree-day", "Brière", "mSOS")
      )
    ) %>%distinct()
  
  EIP_msos_ribbon_sim <- EIP_plot_df_v2 %>%
    select(village,sim_temp, EIP_sim_lo95, EIP_sim_hi95) %>%
    distinct()
  
  p_sim <- ggplot() +
    geom_ribbon(data = EIP_msos_ribbon_sim,
                aes(x = sim_temp, ymin = EIP_sim_lo95, ymax = EIP_sim_hi95),
                fill = "grey70", alpha = 0.35) +
    geom_line(data = EIP_long_sim,
              aes(x = sim_temp, y = EIP_val, colour = eip_model),
              linewidth = 1) +
    geom_point(data = EIP_long_sim,
               aes(x = sim_temp, y = EIP_val,
                   colour = eip_model#, shape = eip_model
                   ),
               size = 2.0) +
    # scale_shape_manual(values = c("Degree-day" = 16, "Brière" = 4, "mSOS" = 1),
    #                    name = "EIP models") +
    scale_colour_manual(values = c("Degree-day" = "#E69F00",
                                   "Brière"      = "#56B4E9",
                                   "mSOS"        = "#CC79A7"),
                        name = "EIP models") +
    labs(x = "Simulated MRST (°C)",
         y = "Predicted EIP (days)") +
    theme_gh + theme(legend.position = "none") +
    coord_cartesian(ylim = c(6, 22)) +
    scale_y_continuous(breaks = seq(6, 22, by = 2)) +
    scale_x_continuous(breaks = seq(27, 32, 1), limits = c(27, 32)) 
  
  print(p_sim)
  
  
  #==============================================================================
  # OBSERVED MOSQUITO TEMPERATURE (mean_mos_temp)
  #==============================================================================
  EIP_long_mos <- EIP_plot_df_v2 %>%
    select(village,temp, location1,
           EIP_DD_obs_temp, EIP_briere_obs_temp, EIP_temp) %>%
    pivot_longer(
      cols      = c(EIP_DD_obs_temp, EIP_briere_obs_temp, EIP_temp),
      names_to  = "eip_model",
      values_to = "EIP_val"
    ) %>%
    mutate(
      eip_model = factor(
        recode(eip_model,
               EIP_DD_obs_temp     = "Degree-day",
               EIP_briere_obs_temp = "Brière",
               EIP_temp        = "mSOS"),
        levels = c("Degree-day", "Brière", "mSOS")
      )
    )
  
  EIP_msos_ribbon_mos <- EIP_plot_df_v2 %>%
    select(village,temp, EIP_temp_lo95, EIP_temp_hi95) %>%
    distinct()
  
  p_mos <- ggplot() +
    geom_ribbon(data = EIP_msos_ribbon_mos,
                aes(x = temp, ymin = EIP_temp_lo95, ymax = EIP_temp_hi95),
                fill = "grey70", alpha = 0.35) +
    geom_line(data = EIP_long_mos,
              aes(x = temp, y = EIP_val, colour = eip_model),
              linewidth = 1) +
    geom_point(data = EIP_long_mos,
               aes(x = temp, y = EIP_val,
                   colour = eip_model#, shape = eip_model
                   ),
               size = 2.0) +
    # scale_shape_manual(values = c("Degree-day" = 16, "Brière" = 4, "mSOS" = 1),
    #                    name = "EIP models") +
    scale_colour_manual(values = c("Degree-day" = "#E69F00",
                                   "Brière"      = "#56B4E9",
                                   "mSOS"        = "#CC79A7"),
                        name = "EIP models") +
    labs(x = "Observed MRST (°C)",
         y = "Predicted EIP (days) ") +
    theme_gh + 
    coord_cartesian(ylim = c(4, 40)) +
    scale_y_continuous(breaks = seq(4, 40, by = 4),limits = c(4, 40))+
    scale_x_continuous(breaks = seq(20, 40, 5), limits = c(20, 40)) 
 
  
  print(p_mos)
  #==============================================================================
  # Combine
  #==============================================================================
  ggsave(file = "figure/fig_eip1.pdf",
  #fig_eip <-
    (p_sdt / p_sim | p_mos) +
    plot_layout(guides = "collect") +
    plot_annotation(
      tag_levels = "I",
      caption = "Data Source: GAEC-BNARI",
        #"P(survive EIP) = exp(−μ(T)·EIP(T)). Colour = mortality model; line style = EIP model.",
      theme = theme(plot.caption = element_text(size = 9, colour = "grey40", face = "italic"),
                    legend.position = "right")
    ),
  
  height = 450/30, width = 700/30, units = "cm", device = "pdf")
  
  #print(fig_eip)

 ###################################################################################################
  ############## Generating EIP boxplot valuae ################################3333
  # ==============================================================================
  # EIP BOXPLOT: Three temperature sources × Three EIP models
  # Matches hand-drawn sketch: grouped boxplot with EIP on y-axis,
  # EIP model on x-axis, colour = temperature source
  # ==============================================================================
  
  # ── 1. Reshape EIP_plot_df_v2 to long format ──────────────────────────────────
  # We need one row per observation with columns:
  #   eip_value    : the estimated EIP
  #   eip_model    : "Degree-day" | "Brière" | "mSOS"
  #   temp_source  : "SDT" | "Simulated MRST" | "Observed MRST"
  
  eip_box_long <- EIP_plot_df_v2 %>%
    select(
             # Degree-day EIP columns
             EIP_DD_sdt_temp, EIP_DD_sim_temp, EIP_DD_obs_temp,
             # Brière EIP columns
             EIP_briere_sdt_temp, EIP_briere_sim_temp, EIP_briere_obs_temp,
             # mSOS EIP columns
             EIP_sat_temp, EIP_sim, EIP_temp
    ) %>%
    pivot_longer(
      cols      = c(EIP_DD_sdt_temp, EIP_DD_sim_temp, EIP_DD_obs_temp,
                    EIP_briere_sdt_temp, EIP_briere_sim_temp, EIP_briere_obs_temp,
                    EIP_sat_temp, EIP_sim, EIP_temp),
      names_to  = "variable",
      values_to = "eip_value"
    ) %>%
    mutate(
      # ── assign EIP model ──────────────────────────────────────────────────────
      eip_model = case_when(
        str_detect(variable, "^EIP_DD")     ~ "Degree-day",
        str_detect(variable, "^EIP_briere") ~ "Brière",
        TRUE                                ~ "mSOS"
      ),
      # ── assign temperature source ─────────────────────────────────────────────
      temp_source = case_when(
        str_detect(variable, "sdt_temp|sat_temp") ~ "SDT",
        str_detect(variable, "sim_temp|EIP_sim")  ~ "Sim. MRST",
        str_detect(variable, "obs_temp|EIP_temp") ~ "Obs. MRST"
      ),
      # ── ordered factors so groups appear left-to-right as in sketch ───────────
      eip_model   = factor(eip_model,   levels = c("Degree-day", "Brière", "mSOS")),
      temp_source = factor(temp_source, levels = c("SDT", "Sim. MRST", "Obs. MRST"))
    ) %>%
    filter(!is.na(eip_value), !is.na(temp_source)) # drop NAs (e.g. Brière at extreme temps)
  
  eip_box_long$eip_value <-  format(round(eip_box_long$eip_value, 2), nsmall = 2)
  eip_box_long$eip_value <-  as.numeric(eip_box_long$eip_value)
  # ── 2. Colour palette matching the sketch ─────────────────────────────────────
  source_colours <- c(
    "SDT"            = "#C0392B",   # red
    "Sim. MRST" = "#2C6E9E",   # blue
    "Obs. MRST"  = "#1F6F54"    # green
  )
  
  # ── 3. Build the grouped boxplot ──────────────────────────────────────────────
  fig_eip_boxplot <- ggplot(
    data    = eip_box_long,
    mapping = aes(
      x    = eip_model,
      y    = eip_value,
      fill = temp_source,
      colour = temp_source
    )
  ) +
    # ── boxes ──────────────────────────────────────────────────────────────────
    geom_boxplot(
      alpha        = 0.55,          # semi-transparent fill
      width        = 0.65,          # relative box width
      position     = position_dodge(width = 0.75),
      outlier.shape = 21,           # filled circle for outliers
      outlier.size  = 1.8,
      outlier.alpha = 0.6,
      linewidth    = 0.55
    ) +
    # ── colour / fill scales ───────────────────────────────────────────────────
    scale_fill_manual(
      values = source_colours,
      name   = "Temperature\nsources"
    ) +
    scale_colour_manual(
      values = source_colours,
      name   = "Temperature\nsources"
    ) +
    # ── axis labels & title ────────────────────────────────────────────────────
    labs(
      x       = "EIP models",
      y       = "Predicted EIP (days)",
      caption = "Data Source: GAEC-BNARI"
    ) +
    # ── y-axis breaks to match sketch (8, 10, 12 …) ───────────────────────────
    scale_y_continuous(
      breaks = seq(4, 40, by = 4),
      limits = c(4, 40)            # let ggplot set limits from data
    ) +
    # ── theme ──────────────────────────────────────────────────────────────────
    theme_bw() +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor   = element_blank(),
      axis.text.x        = element_text(size = 13, vjust = 0.5),
      axis.text.y        = element_text(size = 13),
      axis.title.x       = element_text(size = 13),
      axis.title.y       = element_text(size = 13),
      legend.text        = element_text(size = 12),
      legend.title       = element_text(size = 12),
      legend.position    = "right",
      plot.caption       = element_text(size = 9, colour = "grey40", face = "italic")
    )
  
  print(fig_eip_boxplot)
  
  # ── 4. Save ───────────────────────────────────────────────────────────────────
  ggsave(
    filename = "figure/fig_eip_boxplot.pdf",
    plot     = fig_eip_boxplot,
    height   = 450 / 30,
    width    = 700 / 30,
    units    = "cm",
    device   = "pdf"
  )
  
  ggsave(
    filename = "figure/fig_eip_boxplot.png",
    plot     = fig_eip_boxplot,
    height   = 450 / 30,
    width    = 700 / 30,
    units    = "cm",
    dpi      = 300
  )
  
  
  
########################################################################################################
  #==============================================================================
  # Build long-format P(survive EIP) for one temperature source
  #   temp column + its 3 EIP columns → 4 mortality × 3 EIP = 12 combos per temp
  #==============================================================================
  build_survival_long <- function(df, temp_col, eip_cols, source_label) {
    df %>%
      ungroup() %>%                                   # drop village grouping
      select(temp = all_of(temp_col), all_of(eip_cols)) %>%
      distinct() %>%
      setNames(c("temp", "Degree-day", "Brière", "mSOS")) %>%
      pivot_longer(c("Degree-day", "Brière", "mSOS"),
                   names_to = "eip_model", values_to = "EIP_val") %>%
      mutate(
        `Martens 2` = mu_martens2(temp),
        `Neil`      = mu_neil(temp),
        `Mordecai`  = mu_mordecai_surv(temp)#,
        #`Logistic`  = mu_logistic(temp)
      ) %>%
      pivot_longer(c("Martens 2", "Neil", "Mordecai"),#, "Logistic"),
                   names_to = "mort_model", values_to = "mu_val") %>%
      mutate(
        psurv  = p_survive_eip(mu_val, EIP_val),
        source = source_label,
        eip_model  = factor(eip_model,  levels = c("Degree-day", "Brière", "mSOS")),
        mort_model = factor(mort_model, levels = c("Martens 2", "Neil", "Mordecai"))#, "Logistic"))
      )
  }
  
  # EIP column names per source (mSOS column differs: EIP_sdt_temp / EIP / EIP_mean_mos_temp)
  surv_sdt <- build_survival_long(EIP_plot_df_v2, "sdt_temp",
                                  c("EIP_DD_sdt_temp", "EIP_briere_sdt_temp", "EIP_sat_temp"), "Satellite (SDT)")
  surv_sim <- build_survival_long(EIP_plot_df_v2, "sim_temp",
                                  c("EIP_DD_sim_temp", "EIP_briere_sim_temp", "EIP_sim"),          "Simulated")
  surv_mos <- build_survival_long(EIP_plot_df_v2, "temp",
                                  c("EIP_DD_obs_temp", "EIP_briere_obs_temp", "EIP_temp"), "Observed")
  

  #==============================================================================
  # Plot helper — colour = mortality model, linetype = EIP model
  #==============================================================================
  plot_surv <- function(d, xlab, ylab, title,
                        x_limits = NULL, x_step = NULL,
                        y_limits = c(0, 1), y_step = 0.2) {
    ggplot(d, aes(x = temp, y = psurv,
                  colour = mort_model, linetype = eip_model)) +
      geom_line(linewidth = 0.9, na.rm = TRUE) +
      scale_colour_manual(values = c("Martens 2" = "#0072B2",
                                     "Neil"       = "#009E73",
                                     "Mordecai"   = "#D55E00"),#,
                                     #"Logistic"   = "#CC79A7"),
                          name = "Mortality models") +
      scale_linetype_manual(values = c("Degree-day" = 1, "Brière" = 2, "mSOS" = 3),
                            name = "EIP models") +
      # y-axis breaks at the requested step
      scale_y_continuous(breaks = seq(y_limits[1], y_limits[2], by = y_step)) +
      # x-axis breaks only if a step is supplied (otherwise auto)
      {if (!is.null(x_step) && !is.null(x_limits))
        scale_x_continuous(breaks = seq(x_limits[1], x_limits[2], by = x_step))
        else NULL} +
      # limits via coord_cartesian so points aren't dropped, only zoomed
      coord_cartesian(xlim = x_limits, ylim = y_limits) +
      labs(title = title, x = xlab, y = ylab) +
      theme_gh
      # theme_bw(base_size = 11) +
      # theme(panel.grid.minor = element_blank(),
      #       panel.grid.minor.x = element_blank(),
      #       panel.grid.minor.y = element_blank(),
      #       plot.title = element_text(size = 11))
  }
  #===============================================================================
  p_sdt1 <- plot_surv(surv_sdt, "SDT (°C)",  "P(survive EIP)",      " ",
                     x_limits = c(24, 28), x_step = 1,
                     y_limits = c(0, 1),  y_step = 0.2)
  
  p_sim1 <- plot_surv(surv_sim, "Simulated MRST (°C)", " P(survive EIP)", " ",
                     x_limits = c(27, 32), x_step = 1,
                     y_limits = c(0, 1),  y_step = 0.2)
  
  p_mos1 <- plot_surv(surv_mos, "Observed MRST (°C)","P(survive EIP) "," ",
                     x_limits = c(20, 40), x_step = 2,
                     y_limits = c(0, 1),  y_step = 0.2)  

  #==============================================================================
  # Combine
  #==============================================================================
  ggsave(file = "figure/fig_survive_v1.png",
  #fig_surv <- 
    (p_sdt1 / p_sim1 | p_mos1) +
    plot_layout(guides = "collect") +
    plot_annotation(
      tag_levels = "I",
      caption = "Data Source: GAEC-BNARI",
        #"P(survive EIP) = exp(−μ(T)·EIP(T)). Colour = mortality model; line style = EIP model.",
      theme = theme(plot.caption = element_text(size = 9, colour = "grey40", face = "italic"))
    ), height = 450/30, width = 700/30, units = "cm", device = "png")
  
  
        
 # print(fig_surv)
  
  #==============================================================================
  # Usage
  #==============================================================================
  eip_comparison <- extract_eip_comparison(EIP_plot_df_v2)
  
  cat("=== EIP summary by source and model ===\n")
  print(eip_comparison$summary, n = Inf)
  
  cat("\n=== Satellite EIP overestimate (days) vs mosquito sources ===\n")
  print(eip_comparison$overestimate)
  
  # Optional: save
  write.csv(eip_comparison$summary,      "table_results/eip_comparison_summary.csv",      row.names = FALSE)
  write.csv(eip_comparison$overestimate, "table_results/eip_satellite_overestimate.csv",  row.names = FALSE)
  

