

# # # # # Model for minimum foraging requirements # # # # # 
# # # # # # # # # # C.Barry 2025 # # # # # # # # # # # # #

# 1. Calc shark mass 
# 2. Set constants
# 3. Mass specific standard metabolic rate (B) scaled by VeDBA
# 4. Mass specfic routine metabolic rate (A) scaled to SMR 
# 5. Intake rate (min prey and max prey)
# 6. Min time calc

# 1. shark mass #
tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths (tagged)
m = 12.1 * tl ^ 2.862 # Eq 1.Hsu et. al 2012 # kg
M = m * 1000 # convert to g 

# 2. constants
b = 1 # Normalisation constant # let the allometrc scaling shape the relationship # you can specify species but we don't know whale shark 
beta = 0.84 # scaling exponent SMR Barry 2023 = 0.8406 ± 0.0271 at 15 °C   
temp = 27.12 # mean temp D'antiono et. al 2024
temp_co = 2.64 # temp coefficient Q10 # Barry et. al 2023 # 2.64
mult <- 1.459854 #SMR/RMR ratio #Barry et. al 2023
jules_per_kg <- 1357 * 1000  # Convert from joules per gram to joules per kilogram (1357000 J/kg)
assim_eff <- 0.73  # assimilation efficiency correction (jules per kg)

# 3. Mass specific standard metabolic rate (B) #################################
calc_smr <- function(mass, temp, temp_co, beta) {
  B_15 <- mass^beta
  B_temp <- B_15 * temp_co^((temp - 15) / 10)
  return(B_temp)
}
B_temp <- calc_smr(m, temp, temp_co, beta) # mass and temp adjusted
B <- (B_temp * (mult - 1)) * 13.6 / (3600 * 1000)  # SMR in J/s/kg

# 4. Mass specific routine metabolic rate (A) ######################################
A <- (B_temp * mult) * 13.6 / (3600 * 1000)  # RMR in J/s/kg

# 5. Intake rates ##############################################################
calc_intake_rate <- function(num_kg,  m, jules_per_kg, assim_eff) {
  kg_h <- num_kg / 24
  kg_sec <- kg_h / 3600
  kgprey_sec_kgshark <- kg_sec / m
  I <- kgprey_sec_kgshark * jules_per_kg * assim_eff
  return(I)
}

#ranges of prey intake (from filtration model output (taken from step 9 in "filtration model loop" code))
kg_min_tot <-  c(60.91539,  75.80273,  92.17805, 110.01895, 129.30548, 150.01965, 172.14512, 195.66694, 220.57132) # total min 
kg_max_tot <- c(76.87376,  95.66123, 116.32649, 138.84129, 163.18043, 189.32121, 217.24303, 246.92700, 278.35573) # total max 
kg_mean_tot <- c(69.74258,  86.78724, 105.53549, 125.96171, 148.04303, 171.75887, 197.09053, 224.02087, 252.53413) # total mean 
kg_min_pass <- c(1.632913, 2.031987, 2.470948, 2.949196, 3.466195, 4.021465, 4.614566, 5.245097, 5.912691) # passive only min
kg_mean_pass <- c(10.46011, 13.01650, 15.82839, 18.89195, 22.20374, 25.76069, 29.55997, 33.59903, 37.87550) # passive only mean
kg_max_pass <- c(17.59129, 21.89049, 26.61939, 31.77153, 37.34114, 43.32303, 49.71247, 56.50516, 63.69710) # passive only max
kg_act <- c(59.25442,  73.73583,  89.66465, 107.01909, 125.77974, 145.92910, 167.45129, 190.33174, 214.55706) # active only 
I_tot_min <- calc_intake_rate(kg_min_tot, m, jules_per_kg, assim_eff) # tot min
I_tot_max <- calc_intake_rate(kg_max_tot, m, jules_per_kg, assim_eff) # tot max
I_tot_mean <- calc_intake_rate(kg_mean_tot, m, jules_per_kg, assim_eff) # tot mean
I_pass_min <- calc_intake_rate(kg_min_pass, m, jules_per_kg, assim_eff) # pass min 
I_pass_max <- calc_intake_rate(kg_max_pass, m, jules_per_kg, assim_eff) # pass max
I_pass_mean <- calc_intake_rate(kg_mean_pass, m, jules_per_kg, assim_eff) # pass mean
I_act <- calc_intake_rate(kg_act, m, jules_per_kg, assim_eff) # act 


# 6. Min time calcs

Pa_tot_min <- B / (B - A + I_tot_min)
Pa_tot_max <- B / (B - A + I_tot_max)
Pa_tot_mean <- B / (B - A + I_tot_mean)
Pa_pass_min <- B / (B - A + I_pass_min)
Pa_pass_max <- B / (B - A + I_pass_max)
Pa_pass_mean <- B / (B - A + I_pass_mean)
Pa_act <- B / (B - A + I_act)

dat <- data.frame(tl, Pa_tot_min, Pa_tot_max, Pa_pass_min, Pa_pass_max, Pa_act)
dat_means <- data.frame(tl, Pa_tot_min, Pa_tot_mean, Pa_tot_max, Pa_pass_min, Pa_pass_mean, Pa_pass_max, Pa_act)

# plot ##########
library(ggplot2)
library(patchwork)

p1 <- ggplot(dat, aes(x = tl)) +
  geom_line(aes(y = Pa_tot_min), colour = "#0D0887FF", size = 2) +
  geom_line(aes(y = Pa_tot_max), colour = "black", size = 2) +
  geom_line(aes(y = Pa_pass_min), colour = "#E76F5AFF", size = 2) +
  geom_line(aes(y = Pa_pass_max), colour = "#6300A7FF", size = 2) +
  geom_line(aes(y = Pa_act), colour = "#C03A83FF", size = 1) +
  geom_ribbon(aes(ymin = Pa_tot_min, ymax = Pa_tot_max), fill = "#F0F921FF", alpha = 0.2) +
  geom_ribbon(aes(ymin = Pa_pass_min, ymax = Pa_pass_max), fill ="#E76F5AFF", alpha = 0.2) +
  labs(x = "Shark total length (m)", y = "Proportion of foraging time for homeostasis") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.position = "bottom",  # Position the legend below the plot
    legend.box = "vertical"      # Allow multiple rows for the legend
  ) +
  scale_colour_manual(values = c("Total Min" = "#FDAD32FF", "Total Max" = "black", 
                                 "Passive Min" = "#E76F5AFF", "Passive Max" = "#6300A7FF", 
                                 "Active" = "#C03A83FF"), guide = "none") +  # Remove the colour label
  scale_fill_manual(values = c("Total Range" =  "#F0F921FF", "Passive Range" ="#E76F5AFF"), guide = "none") +  # Remove the fill label
  guides(colour = guide_legend(nrow = 1),  # Specify 1 row for the colour legend
         fill = guide_legend(nrow = 1)) +   # Specify 1 row for the fill legend
    coord_cartesian(ylim = c(0, 0.06))  # Focus on 0–0.07

p2 <- ggplot(dat, aes(x = tl)) +
  geom_line(aes(y = Pa_tot_min), colour = "#0D0887FF", size = 2) +
  geom_line(aes(y = Pa_tot_max), colour ="black", size = 2) +
  geom_line(aes(y = Pa_pass_min), colour = "#E76F5AFF", size = 2) +
  geom_line(aes(y = Pa_pass_max), colour = "#6300A7FF", size = 2) +
  geom_line(aes(y = Pa_act), colour = "#C03A83FF", size = 2) +
  geom_ribbon(aes(ymin = Pa_tot_min, ymax = Pa_tot_max), fill =   "#F0F921FF", alpha = 0.2) +
  geom_ribbon(aes(ymin = Pa_pass_min, ymax = Pa_pass_max), fill ="#E76F5AFF", alpha = 0.2) +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "black", size = 0.8) +  # Horizontal line at 1.0
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_text(size = 12, colour = "black")
  ) +
  coord_cartesian(ylim = c(0, 1.9))  # Focus on 0–1.3

# plot and save 
p <- p2 / p1 /p3 +  plot_layout(heights = c(1, 3, 3))  # heights
p
# etwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#ggsave("logged.png", plot = p, dpi = 300, width = 9, height = 12)  # Width and height are in inches
write.csv(dat, "prop_time.csv")


# Legend only :) crop in powerpoint later hehe. 
legend <- ggplot(dat, aes(x = tl)) +
  geom_line(aes(y = Pa_tot_min, colour = "Total Min"), size = 1) +
  geom_line(aes(y = Pa_tot_max, colour = "Total Max"), size = 1) +
  geom_line(aes(y = Pa_pass_min, colour = "Passive Min"), size = 1) +
  geom_line(aes(y = Pa_pass_max, colour = "Passive Max"), size = 1) +
  geom_line(aes(y = Pa_act, colour = "Active"), size = 1) +
  geom_ribbon(aes(ymin = Pa_tot_min, ymax = Pa_tot_max, fill = "Total Range"), alpha = 0.8) +
  geom_ribbon(aes(ymin = Pa_pass_min, ymax = Pa_pass_max, fill = "Passive Range"), alpha = 0.8) +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "black", size = 0.8) +  # Horizontal line at 1.0
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.position = "bottom",  # Position the legend below the plot
    legend.box = "vertical"      # Allow multiple rows for the legend
  ) +
  scale_colour_manual(values = c("Total Min" = "#E76F5AFF", "Total Max" ="#D5546EFF", 
                                 "Passive Min" = "#0D0887FF", "Passive Max" = "#C03A83FF", 
                                 "Active" = "#6300A7FF" ), guide = "none") +  # Remove the colour label
  scale_fill_manual(values = c("Total Range" = "#F0F921FF", "Passive Range" = "#E76F5AFF"), guide = "none") +  # Remove the fill label
  guides(colour = guide_legend(nrow = 1),  # Specify 1 row for the colour legend
         fill = guide_legend(nrow = 1)) +   # Specify 1 row for the fill legend
  coord_cartesian(ylim = c(0, 1.9))  # Focus on 0–1.9

setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#ggsave("min_time_legend.png", plot = legend, dpi = 300, width = 9, height = 12)  # Width and height are in inches

# Comparison to feeding observed in code "CH2 observed results"

#########################################################################################################################################
#########################################################################################################################################

# Updated version looking at changing the eqn 
# 5. Rizzuto 2018 Eqn needs to be adjusted for whale sharks constant movement 
# A routine (active), B standard (not doing anything but still alive somehow)
# B / I + B - A # old eqn
# (A * swimming_activity + B ) / I + ((A * feeding_activity + B) - (A * swimming_activity + B)) # whale shark 

# 1. Calc shark mass 
# 2. Set constants
# 3. Mass specific standard metabolic rate (B) 
# 4. Mass specific routine metabolic rate (A)
# 5. Adjust routine for dynamic activity (swimming, slow feeding, active feeding)
# 6. Intake rate (min prey and max prey)
# 7. Min time calc

# 1. shark mass #
tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths (tagged)
m = 12.1 * tl ^ 2.862 # Eq 1.Hsu et. al 2012 # kg
M = m * 1000 # convert to g 

# 2. constants
b = 1 # Normalisation constant # let the allometrc scaling shape the relationship # you can specify species but we don't know whale shark 
beta = 0.84 # scaling exponent SMR Barry 2023 = 0.8406 ± 0.0271 at 15 °C   
temp = 27.12 # mean temp D'antonio et. al 2024
temp_co = 2.64 # temp coefficient Q10 # Barry et. al 2023 # 2.64
mult <- 1.459854 #SMR/RMR ratio #Barry et. al 2023
jules_per_kg <- 1357 * 1000  # Convert from joules per gram to joules per kilogram (1357000 J/kg)
assim_eff <- 0.73  # assimilation efficiency correction (joules per kg)

# 3. Mass specific standard metabolic rate (B) #################################
calc_smr <- function(mass, temp, temp_co, beta) {
  B_15 <- mass^beta
  B_temp <- B_15 * temp_co^((temp - 15) / 10)
  return(B_temp)
}
B_temp <- calc_smr(m, temp, temp_co, beta) # mass and temp adjusted

B_ox <- (B_temp * (mult - 1)) # times by multiplier # mg 02 h-1    #oxygen consumption

# Change mo2 into J/s/kg
B <- B_ox * 13.6 / 3600 / 1000 # SMR in J/s/kg # takes oxygen consumption * offset value

# 4. Mass specific routine metabolic rate (A) ######################################
# calculated as two values for A_swimming and A_feeding
A_swimming <- (B_ox * mult)  # active
swim_act <- 0.3391758  # VeDBA non feeding 
feed_act <-  0.3778985 # VeDBA feeding
A_feeding <- (A_swimming / feed_act * swim_act)  # A_feeding activity #
A_s <- A_swimming * 13.6 / 3600 / 1000 # j/s/kg
A_f <- A_feeding * 13.6 / 3600 / 1000 # j/s/kg


# 5. Intake rates ##############################################################
calc_intake_rate <- function(num_kg,  m, jules_per_kg, assim_eff) {
  kg_h <- num_kg / 24
  kg_sec <- kg_h / 3600
  kgprey_sec_kgshark <- kg_sec / m
  I <- kgprey_sec_kgshark * jules_per_kg * assim_eff
  return(I)
}

#ranges of prey intake (from filtration model output (taken from step 9 in "filtration model loop" code))
kg_min_tot <-  c(60.91539,  75.80273,  92.17805, 110.01895, 129.30548, 150.01965, 172.14512, 195.66694, 220.57132) # total min 
kg_max_tot <- c(76.87376,  95.66123, 116.32649, 138.84129, 163.18043, 189.32121, 217.24303, 246.92700, 278.35573) # total max 
kg_mean_tot <- c(69.74258,  86.78724, 105.53549, 125.96171, 148.04303, 171.75887, 197.09053, 224.02087, 252.53413) # total mean 
kg_min_pass <- c(1.632913, 2.031987, 2.470948, 2.949196, 3.466195, 4.021465, 4.614566, 5.245097, 5.912691) # passive only min
kg_mean_pass <- c(10.46011, 13.01650, 15.82839, 18.89195, 22.20374, 25.76069, 29.55997, 33.59903, 37.87550) # passive only mean
kg_max_pass <- c(17.59129, 21.89049, 26.61939, 31.77153, 37.34114, 43.32303, 49.71247, 56.50516, 63.69710) # passive only max
kg_act <- c(59.25442,  73.73583,  89.66465, 107.01909, 125.77974, 145.92910, 167.45129, 190.33174, 214.55706) # active only 
I_tot_min <- calc_intake_rate(kg_min_tot, m, jules_per_kg, assim_eff) # tot min
I_tot_max <- calc_intake_rate(kg_max_tot, m, jules_per_kg, assim_eff) # tot max
I_tot_mean <- calc_intake_rate(kg_mean_tot, m, jules_per_kg, assim_eff) # tot mean
I_pass_min <- calc_intake_rate(kg_min_pass, m, jules_per_kg, assim_eff) # pass min 
I_pass_max <- calc_intake_rate(kg_max_pass, m, jules_per_kg, assim_eff) # pass max
I_pass_mean <- calc_intake_rate(kg_mean_pass, m, jules_per_kg, assim_eff) # pass mean
I_act <- calc_intake_rate(kg_act, m, jules_per_kg, assim_eff) # act 

# 6. Min time calcs ####
# A routine (active), B standard (not doing anything but still alive somehow)
# B / I + B - A
# (A * swimming_activity + B ) / I + ((A * feeding_activity + B) - (A * swimming_activity + B)) # whale shark 
# simplification 
 # Routine swimming / intake + routine swimming - feeding activity

Pa_tot_min <- A_s / (A_s - A_f + I_tot_min)
Pa_tot_max <- A_s / (A_s - A_f + I_tot_max)
Pa_tot_mean <- A_s / (A_s - A_f + I_tot_mean)
Pa_pass_min <- A_s / (A_s - A_f + I_pass_min)
Pa_pass_max <- A_s / (A_s - A_f + I_pass_max)
Pa_pass_mean <- A_s / (A_s - A_f + I_pass_mean)
Pa_act <- A_s / (I_act + A_s - A_f)

# including B 
Pa_tot_min <- A_s + B / (A_s - A_f - B + I_tot_min)
Pa_tot_max <- A_s + B / (A_s - A_f - B + I_tot_max)
Pa_tot_mean <- A_s + B / (A_s - A_f - B  + I_tot_mean)
Pa_pass_min <- A_s + B / (A_s - A_f - B + I_pass_min)
Pa_pass_max <- A_s + B / (A_s - A_f - B + I_pass_max)
Pa_pass_mean <- A_s + B / (A_s - A_f - B + I_pass_mean)
Pa_act <- A_s + B / (I_act + A_s - A_f - B)

dat <- data.frame(tl, Pa_tot_min, Pa_tot_max, Pa_tot_mean, Pa_pass_min, Pa_pass_max, Pa_pass_mean, Pa_act)

# plot ##########
library(ggplot2)

total_ribbon <- data.frame(
  tl = dat$tl,
  ymin = dat$Pa_tot_min,
  ymax = dat$Pa_tot_max
)

passive_ribbon <- data.frame(
  tl = dat$tl,
  ymin = dat$Pa_pass_min,
  ymax = dat$Pa_pass_max
)

# Combine into a long format manually
prey_line_data <- data.frame(
  tl = rep(dat$tl, 3),
  prop = c(dat$Pa_tot_mean, dat$Pa_pass_mean, dat$Pa_act),
  cat = factor(rep(c("all feeding", "slow feeding", "active feeding"),
                   each = length(dat$tl)))
)

p <- ggplot() +
  geom_ribbon(data = total_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#0D0887FF", alpha = 0.5) +
  geom_ribbon(data = passive_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#E76F5AFF", alpha = 0.5) +
  geom_line(data = prey_line_data, aes(x = tl, y = prop, colour = cat), linewidth = 1.5) +
  scale_colour_manual(values = c(
    "all feeding" = "#0D0887FF",
    "slow feeding" = "#E76F5AFF",
    "active feeding" = "#C03A83FF"
  )) +
  scale_y_continuous(
    name = "Proportion of foraging time required",
    breaks = seq(0, 1, by = 0.1)
   ) +
  scale_x_continuous(
    name = "Total length (m)",
    breaks = seq(min(dat$tl), max(dat$tl), by = 0.5)
  ) +
  labs(
    colour = NULL,
    title = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.text = element_text(size = 14, colour = "black"),
    legend.title = element_blank()
  )

p2 <- ggplot() +
  geom_ribbon(data = total_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#0D0887FF", alpha = 0.5) +
  geom_ribbon(data = passive_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#E76F5AFF", alpha = 0.5) +
  geom_line(data = prey_line_data, aes(x = tl, y = prop, colour = cat), linewidth = 1.5) +
  scale_colour_manual(values = c(
    "all feeding" = "#0D0887FF",
    "slow feeding" = "#E76F5AFF",
    "active feeding" = "#C03A83FF"
  )) +
  scale_y_continuous(
    name = "Proportion of foraging time required",
    breaks = seq(0, 0.1, by = 0.01)
  ) +
  coord_cartesian(ylim = c(0, 0.1)
  ) +
  scale_x_continuous(
    name = "Total length (m)",
    breaks = seq(min(dat$tl), max(dat$tl), by = 0.5)
  ) +
  labs(
    colour = NULL,
    title = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.text = element_text(size = 14, colour = "black"),
    legend.title = element_blank()
  )


setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
ggsave("foragingtime_all.png", plot = p, dpi = 300, width = 12, height = 9)  # Width and height are in inches
#write.csv(dat, "prop_time.csv")

=======

# # # # # Model for minimum foraging requirements # # # # # 
# # # # # # # # # # C.Barry 2025 # # # # # # # # # # # # #

# 1. Calc shark mass 
# 2. Set constants
# 3. Mass specific standard metabolic rate (B) 
# 4. Mass specfiic routine metabolic rate (A)
# 5. Intake rate (min prey and max prey)
# 6. Min time calc

# 1. shark mass #
tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths (tagged)
m = 12.1 * tl ^ 2.862 # Eq 1.Hsu et. al 2012 # kg
M = m * 1000 # convert to g 

# 2. constants
b = 1 # Normalisation constant # let the allometrc scaling shape the relationship # you can specify species but we don't know whale shark 
beta = 0.84 # scaling exponent SMR Barry 2023 = 0.8406 ± 0.0271 at 15 °C   
temp = 27.12 # mean temp D'antiono et. al 2024
temp_co = 2.64 # temp coefficient Q10 # Barry et. al 2023 # 2.64
mult <- 1.459854 #SMR/RMR ratio #Barry et. al 2023
jules_per_kg <- 1357 * 1000  # Convert from joules per gram to joules per kilogram (1357000 J/kg)
assim_eff <- 0.73  # assimilation efficiency correction (jules per kg)

# 3. Mass specific standard metabolic rate (B) #################################
calc_smr <- function(mass, temp, temp_co, beta) {
  B_15 <- mass^beta
  B_temp <- B_15 * temp_co^((temp - 15) / 10)
  return(B_temp)
}
B_temp <- calc_smr(m, temp, temp_co, beta) # mass and temp adjusted
B <- (B_temp * (mult - 1)) * 13.6 / (3600 * 1000)  # SMR in J/s/kg

# 4. Mass specific routine metabolic rate (A) ######################################
A <- (B_temp * mult) * 13.6 / (3600 * 1000)  # RMR in J/s/kg

# 5. Intake rates ##############################################################
calc_intake_rate <- function(num_kg,  m, jules_per_kg, assim_eff) {
  kg_h <- num_kg / 24
  kg_sec <- kg_h / 3600
  kgprey_sec_kgshark <- kg_sec / m
  I <- kgprey_sec_kgshark * jules_per_kg * assim_eff
  return(I)
}

#ranges of prey intake (from filtration model output (taken from step 9 in "filtration model loop" code))
kg_min_tot <-  c(60.91539,  75.80273,  92.17805, 110.01895, 129.30548, 150.01965, 172.14512, 195.66694, 220.57132) # total min 
kg_max_tot <- c(76.87376,  95.66123, 116.32649, 138.84129, 163.18043, 189.32121, 217.24303, 246.92700, 278.35573) # total max 
kg_mean_tot <- c(69.74258,  86.78724, 105.53549, 125.96171, 148.04303, 171.75887, 197.09053, 224.02087, 252.53413) # total mean 
kg_min_pass <- c(1.632913, 2.031987, 2.470948, 2.949196, 3.466195, 4.021465, 4.614566, 5.245097, 5.912691) # passive only min
kg_mean_pass <- c(10.46011, 13.01650, 15.82839, 18.89195, 22.20374, 25.76069, 29.55997, 33.59903, 37.87550) # passive only mean
kg_max_pass <- c(17.59129, 21.89049, 26.61939, 31.77153, 37.34114, 43.32303, 49.71247, 56.50516, 63.69710) # passive only max
kg_act <- c(59.25442,  73.73583,  89.66465, 107.01909, 125.77974, 145.92910, 167.45129, 190.33174, 214.55706) # active only 
I_tot_min <- calc_intake_rate(kg_min_tot, m, jules_per_kg, assim_eff) # tot min
I_tot_max <- calc_intake_rate(kg_max_tot, m, jules_per_kg, assim_eff) # tot max
I_tot_mean <- calc_intake_rate(kg_mean_tot, m, jules_per_kg, assim_eff) # tot mean
I_pass_min <- calc_intake_rate(kg_min_pass, m, jules_per_kg, assim_eff) # pass min 
I_pass_max <- calc_intake_rate(kg_max_pass, m, jules_per_kg, assim_eff) # pass max
I_pass_mean <- calc_intake_rate(kg_mean_pass, m, jules_per_kg, assim_eff) # pass mean
I_act <- calc_intake_rate(kg_act, m, jules_per_kg, assim_eff) # act 


# 6. Min time calcs

Pa_tot_min <- B / (B - A + I_tot_min)
Pa_tot_max <- B / (B - A + I_tot_max)
Pa_tot_mean <- B / (B - A + I_tot_mean)
Pa_pass_min <- B / (B - A + I_pass_min)
Pa_pass_max <- B / (B - A + I_pass_max)
Pa_pass_mean <- B / (B - A + I_pass_mean)
Pa_act <- B / (B - A + I_act)

dat <- data.frame(tl, Pa_tot_min, Pa_tot_max, Pa_pass_min, Pa_pass_max, Pa_act)
dat_means <- data.frame(tl, Pa_tot_min, Pa_tot_mean, Pa_tot_max, Pa_pass_min, Pa_pass_mean, Pa_pass_max, Pa_act)

# plot ##########
library(ggplot2)
library(patchwork)

p1 <- ggplot(dat, aes(x = tl)) +
  geom_line(aes(y = Pa_tot_min), colour = "#0D0887FF", size = 2) +
  geom_line(aes(y = Pa_tot_max), colour = "black", size = 2) +
  geom_line(aes(y = Pa_pass_min), colour = "#E76F5AFF", size = 2) +
  geom_line(aes(y = Pa_pass_max), colour = "#6300A7FF", size = 2) +
  geom_line(aes(y = Pa_act), colour = "#C03A83FF", size = 1) +
  geom_ribbon(aes(ymin = Pa_tot_min, ymax = Pa_tot_max), fill = "#F0F921FF", alpha = 0.2) +
  geom_ribbon(aes(ymin = Pa_pass_min, ymax = Pa_pass_max), fill ="#E76F5AFF", alpha = 0.2) +
  labs(x = "Shark total length (m)", y = "Proportion of foraging time for homeostasis") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.position = "bottom",  # Position the legend below the plot
    legend.box = "vertical"      # Allow multiple rows for the legend
  ) +
  scale_colour_manual(values = c("Total Min" = "#FDAD32FF", "Total Max" = "black", 
                                 "Passive Min" = "#E76F5AFF", "Passive Max" = "#6300A7FF", 
                                 "Active" = "#C03A83FF"), guide = "none") +  # Remove the colour label
  scale_fill_manual(values = c("Total Range" =  "#F0F921FF", "Passive Range" ="#E76F5AFF"), guide = "none") +  # Remove the fill label
  guides(colour = guide_legend(nrow = 1),  # Specify 1 row for the colour legend
         fill = guide_legend(nrow = 1)) +   # Specify 1 row for the fill legend
    coord_cartesian(ylim = c(0, 0.06))  # Focus on 0–0.07

p2 <- ggplot(dat, aes(x = tl)) +
  geom_line(aes(y = Pa_tot_min), colour = "#0D0887FF", size = 2) +
  geom_line(aes(y = Pa_tot_max), colour ="black", size = 2) +
  geom_line(aes(y = Pa_pass_min), colour = "#E76F5AFF", size = 2) +
  geom_line(aes(y = Pa_pass_max), colour = "#6300A7FF", size = 2) +
  geom_line(aes(y = Pa_act), colour = "#C03A83FF", size = 2) +
  geom_ribbon(aes(ymin = Pa_tot_min, ymax = Pa_tot_max), fill =   "#F0F921FF", alpha = 0.2) +
  geom_ribbon(aes(ymin = Pa_pass_min, ymax = Pa_pass_max), fill ="#E76F5AFF", alpha = 0.2) +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "black", size = 0.8) +  # Horizontal line at 1.0
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_text(size = 12, colour = "black")
  ) +
  coord_cartesian(ylim = c(0, 1.9))  # Focus on 0–1.3

# plot and save 
p <- p2 / p1 /p3 +  plot_layout(heights = c(1, 3, 3))  # heights
p
# etwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#ggsave("logged.png", plot = p, dpi = 300, width = 9, height = 12)  # Width and height are in inches
write.csv(dat, "prop_time.csv")


# Legend only :) crop in powerpoint later hehe. 
legend <- ggplot(dat, aes(x = tl)) +
  geom_line(aes(y = Pa_tot_min, colour = "Total Min"), size = 1) +
  geom_line(aes(y = Pa_tot_max, colour = "Total Max"), size = 1) +
  geom_line(aes(y = Pa_pass_min, colour = "Passive Min"), size = 1) +
  geom_line(aes(y = Pa_pass_max, colour = "Passive Max"), size = 1) +
  geom_line(aes(y = Pa_act, colour = "Active"), size = 1) +
  geom_ribbon(aes(ymin = Pa_tot_min, ymax = Pa_tot_max, fill = "Total Range"), alpha = 0.8) +
  geom_ribbon(aes(ymin = Pa_pass_min, ymax = Pa_pass_max, fill = "Passive Range"), alpha = 0.8) +
  geom_hline(yintercept = 1.0, linetype = "dashed", colour = "black", size = 0.8) +  # Horizontal line at 1.0
  labs(x = "", y = "") +
  theme_classic() +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 12, colour = "black"),
    axis.text = element_text(size = 12, colour = "black"),
    plot.title = element_text(size = 12, colour = "black"),
    legend.text = element_text(size = 12, colour = "black"),
    legend.title = element_text(size = 12, colour = "black"),
    legend.position = "bottom",  # Position the legend below the plot
    legend.box = "vertical"      # Allow multiple rows for the legend
  ) +
  scale_colour_manual(values = c("Total Min" = "#E76F5AFF", "Total Max" ="#D5546EFF", 
                                 "Passive Min" = "#0D0887FF", "Passive Max" = "#C03A83FF", 
                                 "Active" = "#6300A7FF" ), guide = "none") +  # Remove the colour label
  scale_fill_manual(values = c("Total Range" = "#F0F921FF", "Passive Range" = "#E76F5AFF"), guide = "none") +  # Remove the fill label
  guides(colour = guide_legend(nrow = 1),  # Specify 1 row for the colour legend
         fill = guide_legend(nrow = 1)) +   # Specify 1 row for the fill legend
  coord_cartesian(ylim = c(0, 1.9))  # Focus on 0–1.9

setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#ggsave("min_time_legend.png", plot = legend, dpi = 300, width = 9, height = 12)  # Width and height are in inches

# Comparison to feeding observed in code "CH2 observed results"

#########################################################################################################################################
#########################################################################################################################################

# Updated version looking at changing the eqn 
# 5. Rizzuto 2018 Eqn needs to be adjusted for whale sharks constant movement 
# A routine (active), B standard (not doing anything but still alive somehow)
# B / I + B - A # old eqn
# (A * swimming_activity + B ) / I + ((A * feeding_activity + B) - (A * swimming_activity + B)) # whale shark 

# 1. Calc shark mass 
# 2. Set constants
# 3. Mass specific standard metabolic rate (B) 
# 4. Mass specific routine metabolic rate (A)
# 5. Adjust routine for dynamic activity (swimming, slow feeding, active feeding)
# 6. Intake rate (min prey and max prey)
# 7. Min time calc

# 1. shark mass #
tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths (tagged)
m = 12.1 * tl ^ 2.862 # Eq 1.Hsu et. al 2012 # kg
M = m * 1000 # convert to g 

# 2. constants
b = 1 # Normalisation constant # let the allometrc scaling shape the relationship # you can specify species but we don't know whale shark 
beta = 0.84 # scaling exponent SMR Barry 2023 = 0.8406 ± 0.0271 at 15 °C   
temp = 27.12 # mean temp D'antonio et. al 2024
temp_co = 2.64 # temp coefficient Q10 # Barry et. al 2023 # 2.64
mult <- 1.459854 #SMR/RMR ratio #Barry et. al 2023
jules_per_kg <- 1357 * 1000  # Convert from joules per gram to joules per kilogram (1357000 J/kg)
assim_eff <- 0.73  # assimilation efficiency correction (joules per kg)

# 3. Mass specific standard metabolic rate (B) #################################
calc_smr <- function(mass, temp, temp_co, beta) {
  B_15 <- mass^beta
  B_temp <- B_15 * temp_co^((temp - 15) / 10)
  return(B_temp)
}
B_temp <- calc_smr(m, temp, temp_co, beta) # mass and temp adjusted

B_ox <- (B_temp * (mult - 1)) # times by multiplier # mg 02 h-1    #oxygen consumption

# Change mo2 into J/s/kg
B <- B_ox * 13.6 / 3600 / 1000 # SMR in J/s/kg # takes oxygen consumption * offset value

# 4. Mass specific routine metabolic rate (A) ######################################
# calculated as two values for A_swimming and A_feeding
A_swimming <- (B_ox * mult)  # active
swim_act <- 0.3391758  # VeDBA non feeding 
feed_act <-  0.3778985 # VeDBA feeding
A_feeding <- (A_swimming / feed_act * swim_act)  # A_feeding activity #
A_s <- A_swimming * 13.6 / 3600 / 1000 # j/s/kg
A_f <- A_feeding * 13.6 / 3600 / 1000 # j/s/kg


# 5. Intake rates ##############################################################
calc_intake_rate <- function(num_kg,  m, jules_per_kg, assim_eff) {
  kg_h <- num_kg / 24
  kg_sec <- kg_h / 3600
  kgprey_sec_kgshark <- kg_sec / m
  I <- kgprey_sec_kgshark * jules_per_kg * assim_eff
  return(I)
}

#ranges of prey intake (from filtration model output (taken from step 9 in "filtration model loop" code))
kg_min_tot <-  c(60.91539,  75.80273,  92.17805, 110.01895, 129.30548, 150.01965, 172.14512, 195.66694, 220.57132) # total min 
kg_max_tot <- c(76.87376,  95.66123, 116.32649, 138.84129, 163.18043, 189.32121, 217.24303, 246.92700, 278.35573) # total max 
kg_mean_tot <- c(69.74258,  86.78724, 105.53549, 125.96171, 148.04303, 171.75887, 197.09053, 224.02087, 252.53413) # total mean 
kg_min_pass <- c(1.632913, 2.031987, 2.470948, 2.949196, 3.466195, 4.021465, 4.614566, 5.245097, 5.912691) # passive only min
kg_mean_pass <- c(10.46011, 13.01650, 15.82839, 18.89195, 22.20374, 25.76069, 29.55997, 33.59903, 37.87550) # passive only mean
kg_max_pass <- c(17.59129, 21.89049, 26.61939, 31.77153, 37.34114, 43.32303, 49.71247, 56.50516, 63.69710) # passive only max
kg_act <- c(59.25442,  73.73583,  89.66465, 107.01909, 125.77974, 145.92910, 167.45129, 190.33174, 214.55706) # active only 
I_tot_min <- calc_intake_rate(kg_min_tot, m, jules_per_kg, assim_eff) # tot min
I_tot_max <- calc_intake_rate(kg_max_tot, m, jules_per_kg, assim_eff) # tot max
I_tot_mean <- calc_intake_rate(kg_mean_tot, m, jules_per_kg, assim_eff) # tot mean
I_pass_min <- calc_intake_rate(kg_min_pass, m, jules_per_kg, assim_eff) # pass min 
I_pass_max <- calc_intake_rate(kg_max_pass, m, jules_per_kg, assim_eff) # pass max
I_pass_mean <- calc_intake_rate(kg_mean_pass, m, jules_per_kg, assim_eff) # pass mean
I_act <- calc_intake_rate(kg_act, m, jules_per_kg, assim_eff) # act 

# 6. Min time calcs ####
# A routine (active), B standard (not doing anything but still alive somehow)
# B / I + B - A
# (A * swimming_activity + B ) / I + ((A * feeding_activity + B) - (A * swimming_activity + B)) # whale shark 
# simplification 
 # Routine swimming / intake + routine swimming - feeding activity

Pa_tot_min <- A_s / (A_s - A_f + I_tot_min)
Pa_tot_max <- A_s / (A_s - A_f + I_tot_max)
Pa_tot_mean <- A_s / (A_s - A_f + I_tot_mean)
Pa_pass_min <- A_s / (A_s - A_f + I_pass_min)
Pa_pass_max <- A_s / (A_s - A_f + I_pass_max)
Pa_pass_mean <- A_s / (A_s - A_f + I_pass_mean)
Pa_act <- A_s / (I_act + A_s - A_f)

# including B 
Pa_tot_min <- A_s + B / (A_s - A_f - B + I_tot_min)
Pa_tot_max <- A_s + B / (A_s - A_f - B + I_tot_max)
Pa_tot_mean <- A_s + B / (A_s - A_f - B  + I_tot_mean)
Pa_pass_min <- A_s + B / (A_s - A_f - B + I_pass_min)
Pa_pass_max <- A_s + B / (A_s - A_f - B + I_pass_max)
Pa_pass_mean <- A_s + B / (A_s - A_f - B + I_pass_mean)
Pa_act <- A_s + B / (I_act + A_s - A_f - B)

dat <- data.frame(tl, Pa_tot_min, Pa_tot_max, Pa_tot_mean, Pa_pass_min, Pa_pass_max, Pa_pass_mean, Pa_act)

# plot ##########
library(ggplot2)

total_ribbon <- data.frame(
  tl = dat$tl,
  ymin = dat$Pa_tot_min,
  ymax = dat$Pa_tot_max
)

passive_ribbon <- data.frame(
  tl = dat$tl,
  ymin = dat$Pa_pass_min,
  ymax = dat$Pa_pass_max
)

# Combine into a long format manually
prey_line_data <- data.frame(
  tl = rep(dat$tl, 3),
  prop = c(dat$Pa_tot_mean, dat$Pa_pass_mean, dat$Pa_act),
  cat = factor(rep(c("all feeding", "slow feeding", "active feeding"),
                   each = length(dat$tl)))
)

p <- ggplot() +
  geom_ribbon(data = total_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#0D0887FF", alpha = 0.5) +
  geom_ribbon(data = passive_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#E76F5AFF", alpha = 0.5) +
  geom_line(data = prey_line_data, aes(x = tl, y = prop, colour = cat), linewidth = 1.5) +
  scale_colour_manual(values = c(
    "all feeding" = "#0D0887FF",
    "slow feeding" = "#E76F5AFF",
    "active feeding" = "#C03A83FF"
  )) +
  scale_y_continuous(
    name = "Proportion of foraging time required",
    breaks = seq(0, 1, by = 0.1)
   ) +
  scale_x_continuous(
    name = "Total length (m)",
    breaks = seq(min(dat$tl), max(dat$tl), by = 0.5)
  ) +
  labs(
    colour = NULL,
    title = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.text = element_text(size = 14, colour = "black"),
    legend.title = element_blank()
  )

p2 <- ggplot() +
  geom_ribbon(data = total_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#0D0887FF", alpha = 0.5) +
  geom_ribbon(data = passive_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#E76F5AFF", alpha = 0.5) +
  geom_line(data = prey_line_data, aes(x = tl, y = prop, colour = cat), linewidth = 1.5) +
  scale_colour_manual(values = c(
    "all feeding" = "#0D0887FF",
    "slow feeding" = "#E76F5AFF",
    "active feeding" = "#C03A83FF"
  )) +
  scale_y_continuous(
    name = "Proportion of foraging time required",
    breaks = seq(0, 0.1, by = 0.01)
  ) +
  coord_cartesian(ylim = c(0, 0.1)
  ) +
  scale_x_continuous(
    name = "Total length (m)",
    breaks = seq(min(dat$tl), max(dat$tl), by = 0.5)
  ) +
  labs(
    colour = NULL,
    title = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.text = element_text(size = 14, colour = "black"),
    legend.title = element_blank()
  )


setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
ggsave("foragingtime_all.png", plot = p, dpi = 300, width = 12, height = 9)  # Width and height are in inches
#write.csv(dat, "prop_time.csv")

>>>>>>> 3d0106305464a0329dc12f58854bfdf5e04b545f
