# # # # # Model for minimum foraging requirements # # # # # 
# # # # # # # # # # C.Barry et al 2025/6 # # # # # # # # # # 

# A routine (active), B standard (not doing anything but still alive somehow)
# B / I + B - A # old eqn
# (A * swimming_activity + B ) / I + ((A * feeding_activity + B) - (A * swimming_activity + B)) # whale shark 

# 1. Calc shark mass 
# 2. Set constants
# 3. Standard metabolic rate - adjusted for mass and local temperature
# 4. Routine metabolic rate of all swimming
# 5. Feeding activity adjustment 
# 6. Intake rates
# 7. Min time calc
# 8. Plots and data comparisons :) 

# 1. shark mass ###########################################################################################################

tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths (tagged)
m <- 12.1 * tl ^ 2.862 # Eqn: Hsu et. al 2012 # kg
M <- m * 1000 # convert to g 

# 2. constants ############################################################################################################

b <- 1 # Normalisation constant # let the allometrc scaling shape the relationship # you can specify species but we don't know whale shark 
beta <- 0.84 # scaling exponent SMR Barry 2023 = 0.8406 ± 0.0271 at 15 °C   
temp <- 27.12 # mean temp D'antonio et. al 2024
temp_co <- 2.64 # temp coefficient Q10 # Barry et. al 2023 # 2.64
mult <- 1.459854 #SMR/RMR ratio #Barry et. al 2023
joules_per_kg <- 1357 * 1000  # Convert from joules per gram to joules per kilogram (1357000 J/kg)
assim_eff <- 0.73  # assimilation efficiency correction (j per kg)
cal_conv <- 13.6 # fish caloric equivalent (Kj/g) # Jobling et al. 1993 # per hour 
swim_act <- 0.3391758  # VeDBA non feeding 
feed_act <-  0.3778985 # VeDBA feeding

# 3. standard metabolic rate - adjusted for mass and local temperature #####################################################

#create function to calc SRM based on mass, temperature, temperature scaling coefficient, scaling exponent 
calc_smr <- function(mass, temp, temp_co, beta) {
  B_15 <- mass^beta # mass to the power of SMR scaling exponent = SMR at 15 deg
  B_temp <- B_15 * temp_co^((temp - 15) / 10) # SMR scaling exponent adjusted scaling temperature coefficient at 27 deg using Q10 model eqn # (temp - temp reference) / 10
  return(B_temp)
}

# run the function 
B_temp <- calc_smr(m, temp, temp_co, beta) # mass and temp adjusted

# ACG notes # 
# first part of function 
B_15 <- m^beta # mass to the power of SMR scaling exponent = SMR at 15 deg
# second part of function 
B_temp <- B_15 * temp_co^((temp - 15) / 10) # SMR scaling exponent adjusted scaling temperature coefficient at 27 deg using Q10 model eqn # (temp - temp reference) / 10
# # # # # 

# 4. RMR of all swimming #####################################################################################################

A_swimming <- B_temp * mult  # A_swimming includes SRM + routine activity cost, equivalent to RMR, in mg 02 h-1

# RMR in joules / s / kg
A_s <- A_swimming * cal_conv / 3600 / 1000  # Convert to MO2/h/mg to J/s/kg

# 5. Activity adjusted rate of feeding #######################################################################################

# feed_act / swim_act
# 1.114167 therefore feeding 11 % increase in metabolic cost when feeding 

A_feeding <- A_swimming * (feed_act / swim_act)  
A_f <- A_feeding * cal_conv / 3600 / 1000  # Convert MO2/h/g to J/s/kg

# 6. Intake rates ##############################################################################################################
calc_intake_rate <- function(num_kg,  m, joules_per_kg, assim_eff) {
  kg_h <- num_kg / 24
  kg_sec <- kg_h / 3600
  kgprey_sec_kgshark <- kg_sec / m
  I <- kgprey_sec_kgshark * joules_per_kg * assim_eff
  return(I)
}

#ranges of prey intake (from filtration model output (taken from step 9 in "filtration model" code))
kg_min_tot <-  c(60.91539,  75.80273,  92.17805, 110.01895, 129.30548, 150.01965, 172.14512, 195.66694, 220.57132) # total min 
kg_max_tot <- c(76.87376,  95.66123, 116.32649, 138.84129, 163.18043, 189.32121, 217.24303, 246.92700, 278.35573) # total max 
kg_mean_tot <- c(69.74258,  86.78724, 105.53549, 125.96171, 148.04303, 171.75887, 197.09053, 224.02087, 252.53413) # total mean 
kg_min_pass <- c(1.632913, 2.031987, 2.470948, 2.949196, 3.466195, 4.021465, 4.614566, 5.245097, 5.912691) # passive only min
kg_mean_pass <- c(10.46011, 13.01650, 15.82839, 18.89195, 22.20374, 25.76069, 29.55997, 33.59903, 37.87550) # passive only mean
kg_max_pass <- c(17.59129, 21.89049, 26.61939, 31.77153, 37.34114, 43.32303, 49.71247, 56.50516, 63.69710) # passive only max
kg_act <- c(59.25442,  73.73583,  89.66465, 107.01909, 125.77974, 145.92910, 167.45129, 190.33174, 214.55706) # active only 
I_tot_min <- calc_intake_rate(kg_min_tot, m, joules_per_kg, assim_eff) # tot min
I_tot_max <- calc_intake_rate(kg_max_tot, m, joules_per_kg, assim_eff) # tot max
I_tot_mean <- calc_intake_rate(kg_mean_tot, m, joules_per_kg, assim_eff) # tot mean
I_pass_min <- calc_intake_rate(kg_min_pass, m, joules_per_kg, assim_eff) # pass min 
I_pass_max <- calc_intake_rate(kg_max_pass, m, joules_per_kg, assim_eff) # pass max
I_pass_mean <- calc_intake_rate(kg_mean_pass, m, joules_per_kg, assim_eff) # pass mean
I_act <- calc_intake_rate(kg_act, m, joules_per_kg, assim_eff) # act 

# 7. Min time calcs ##########################################################################################################

Pa_tot_min <- A_s / (A_s - A_f + I_tot_min)
Pa_tot_max <- A_s / (A_s - A_f + I_tot_max)
Pa_tot_mean <- A_s / (A_s - A_f + I_tot_mean)
Pa_pass_min <- A_s / (A_s - A_f + I_pass_min)
Pa_pass_max <- A_s / (A_s - A_f + I_pass_max)
Pa_pass_mean <- A_s / (A_s - A_f + I_pass_mean)
Pa_act <- A_s / (I_act + A_s - A_f)

dat <- data.frame(tl, Pa_tot_min, Pa_tot_mean, Pa_tot_max, I_tot_min, I_tot_mean, I_tot_max, Pa_pass_max, Pa_pass_mean, Pa_pass_min, Pa_act)

# model complete :) ###########################################################################################################

# 8. plots and summarise ######################################################################################################
library(ggplot2)

# total feeding data 
total_ribbon <- data.frame(
  tl = dat$tl,
  ymin = dat$Pa_tot_min,
  ymax = dat$Pa_tot_max
)

# slow feeding data 
passive_ribbon <- data.frame(
  tl = dat$tl,
  ymin = dat$Pa_pass_min,
  ymax = dat$Pa_pass_max
)

# reshape data  
prey_line_data <- data.frame(
  tl = rep(dat$tl, 3),
  prop = c(dat$Pa_tot_mean, dat$Pa_pass_mean, dat$Pa_act),
  cat = factor(rep(c("all feeding", "slow feeding", "active feeding"),
                   each = length(dat$tl)))
)

# full plot 
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
p

# zoomed in y axis plot 
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
    breaks = seq(0, 0.2, by = 0.01)
  ) +
  coord_cartesian(ylim = c(0, 0.2)
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
p2

#setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#ggsave("foragingtime_all.png", plot = p, dpi = 300, width = 12, height = 9)  # Width and height are in inches


# compare to foraging efforts ##############################################################################################
df <- read.csv("foraging_efforts.csv")[,-1]

# Figure 3a  # 

# jitter total length of sharks 
set.seed(42)
df$jit_tl <- jitter(df$dep_tl, amount = 0.1)

# plot 
Figure3a <- ggplot(df, aes(x = jit_tl, y = prop_feeding)) +
  geom_point(shape = 17, colour = "#E76F5AFF", size = 4) +
  scale_x_continuous(
    name = "Shark total length (m)",
    breaks = seq(min(df$dep_tl), max(df$dep_tl), by = 0.5)
  ) +
  scale_y_continuous(
    name = "Proportion of time feeding",
    limits = c(0, max(df$prop_feeding))
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black")
)
Figure3a

# Figure 3 b #

# add foraging model results (df) to observed behaviours (dat)
df$dep_pa_act <- dat$Pa_act[match(df$dep_tl, dat$tl)] # proportion of active feeding required to meet net 0 
df$dep_pa_pass_mean <- dat$Pa_pass_mean[match(df$dep_tl, dat$tl)] # proportion of slow feeding (mean prey) required to meet net 0 
df$relative_excess <- (df$prop_act_feed / df$dep_pa_act) + (df$prop_slow_feed / df$dep_pa_pass_mean) - 1 # calculate relative excess foraging # subtract by 1 because 1 = 100%

# by shark total length 
length_sum <- aggregate(relative_excess ~ dep_tl, data = df,FUN = function(x) c(mean = mean(x), sd = sd(x), n = length(x)))
length_sum_stats <- data.frame(
  total_length_m = length_sum$dep_tl, # length
  mean_excess = length_sum$relative_excess[, "mean"], # mean 
  sd_excess = length_sum$relative_excess[, "sd"], # standard dev
  n = length_sum$relative_excess[, "n"] # no shakrs 
)
length_sum_stats$se_excess <- length_sum_stats$sd_excess / sqrt(length_sum_stats$n) # add standard error 
plot_data <- subset(length_sum_stats, n > 1) # remove samples of 1 

# plot 
Figure_3b <- ggplot(plot_data, aes(x = total_length_m, y = mean_excess)) +
  geom_ribbon(aes(ymin = mean_excess - se_excess,
                  ymax = mean_excess + se_excess,
                  group = 1),
              fill =  "#E76F5AFF", alpha = 0.2) +
  geom_line(colour =  "#E76F5AFF", size = 1.5) +
  geom_point(size = 4, colour =  "#E76F5AFF") +
  geom_hline(yintercept = 0, linetype = "dashed", colour = "#0D0887FF", linewidth = 1.5) +  # <-- added line
  labs(x = "Shark total length (m)",
       y = "Feeding duration beyond required (%)",
       title = "") +
  theme_classic(base_size = 14) +
  theme(
    axis.text = element_text(colour = "black"),
    axis.line = element_line(size = 1.2, colour = "black")
)
Figure_3b
