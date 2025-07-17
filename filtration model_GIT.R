<<<<<<< HEAD

# # # # # filtration model # # # # # # #
# C.Barry 2025 # # # # # # # # # # # # #

########### ALL SHARK SIZES ############################################

# 1. Gape calculation #
# 2. Velocity of behaviours #
# 3. Behavior gapes # 
# 4. Flow velocities # 
# 5. Prey mass # 
# 6. Count of behaviours # 
# 7. Water filtered and prey captured # 
# 8. Period summaries and plots # 

# 1. Gape calculation ########################################################## 

#train data from motta et al. 2010 
sharks_train <- c("A", "B", "C")
tl_train <- c(6.22, 5.93, 4.43) # m 
gape_sa <-c(2035, 1841, 1079) # cm2
gape_m <- gape_sa /  10000 # m2
dat_train <- data.frame(sharks_train,tl_train,gape_m)
gape_allometric <- lm(log(gape_m) ~ log(tl_train), data = dat_train) #allometric model of log gape and log total length
#summary(gape_allometric)
intercept <- exp(coef(gape_allometric)[1])
slope <- coef(gape_allometric)[2]
#slope_se <- summary(gape_allometric)$coefficients[2, 2] # standard error 

# gape tl relationship
calculate_gape <- function(tl_train) {
  slope <- slope # Scaling exponent (beta)
  gape <- intercept * tl_train^slope  # Allometric relationship: gape = a * L^beta
  return(gape)
}
# save 
save(calculate_gape, file = "calculate_gape.RData")

#tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 8.0) #shark lengths (tagged)
tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths all 
gapes <- calculate_gape(tl) # gapes 
#rm(dat_train)
#rm(gape_allometric)

########### sanity check gape variations #######
# 
# # compare prediction to train data 
# fitted_gape <- calculate_gape(tl_train)
# cbind(tl_train, gape_m, fitted_gape)
# 
# # output
# # tl_train gape_m fitted_gape
# # 6.22  0.2035   0.2024163
# # 5.93  0.1841   0.1852475
# # 4.43  0.1079   0.1078057
# 
# # 95 % confidence intervals #
# 
# # Use predict function to predict on log scale and then back transform estimates 
# new_data <- data.frame(tl_train = tl) # make df for predict functino
# pred <- predict(gape_allometric, newdata = new_data, interval = "prediction")
# summary(pred)
# pred
# 
# # Back-transform from log to natural scale (m²) to get gape upper and lower confidence 
# gapes <- exp(pred[, "fit"])
# gapes_lower <- exp(pred[, "lwr"])
# gapes_upper <- exp(pred[, "upr"])
# 
# # plot allo relationship between gape (m²) and tl (m) with 95% confidence intervals 
# library(ggplot2)
# test1 <- data.frame(tl, gapes, gapes_lower, gapes_upper)
# ggplot(test1, aes(x = tl, y = gapes)) +
#   geom_ribbon(aes(ymin = gapes_lower, ymax = gapes_upper), fill = "grey80", alpha = 0.5) +
#   geom_line(colour = "red", linewidth = 1.2) +
#   labs(
#     x = "Total length (m)",
#     y = "Predicted gape area (m²)",
#     title = ""
#   ) +
#   theme_minimal(base_size = 14)
# 
# # error increases as sharks get larger


# 2. Velocity of behaviours ####################################################

vel <- c(1, 1, 0.9, 0.9, 0.9, 0.42, 0.43, 0.33, 1.1, 1.5, 1, 0.5, 1)
behav <- c("Cruising", "Coughing", "Surface_slow_feeding" ,"Water_column_slow_feeding", "Benthic_slow_feeding", "Ascent_slow_feeding", 
           "Gliding", "Gliding_slow_feeding", "Active_surface_feeding", "Dive_feeding", "Surface_undulation", "Whale_shark_interaction", "Other") 

# 3. Behaviour gapes ###########################################################

active_gape <- gapes * 0.8457
slow_gape <- gapes * 0.6167
non_feeding_gape <- gapes * 0.175

cat <- c("non_feeding", "non_feeding", "slow", "slow", "slow", "slow", "non_feeding", "slow", "active", "active", "non_feeding", "non_feeding", "non_feeding")
gape_map <- list( "non_feeding" = non_feeding_gape, "slow" = slow_gape, "active" = active_gape)

dat <- data.frame(
  behav = rep(behav, each = length(tl)),                # Repeat behaviours for each TL
  cat = rep(cat, each = length(tl)),                    # Repeat categories for each TL
  tl = rep(tl, times = length(behav))                   # Repeat TL for each behaviour
)

dat$gapes <- NA # empty col

# Loop through each row of the data frame
for (i in seq_len(nrow(dat))) {
  current_cat <- dat$cat[i]
  current_tl <- dat$tl[i]
  gape_values <- gape_map[[current_cat]] # Get the gape vector for the category
  tl_index <- which(tl == current_tl)    # Find the index of the current total length
  dat$gapes[i] <- gape_values[tl_index]  # Assign the corresponding gape
  dat$vel <- rep(vel, each = length(tl))
}

# 4. Flow velocities ###########################################################

# Motta 2010 flow velocity is 90.8 % of swimming velocity
dat$flow_vel <- dat$vel *0.908 # m/s of flow velocity 
dat$vol_water_m3s <- dat$flow_vel * dat$gapes # volume water filtered m3/s

# 5. Prey mass #################################################################

# wet mass calculation 
a <- 4.4e-6   # a coefficient in grams
b <- 3.16     # scaling exponent
L <- 8.84     # mean length of krill in mm (sonar setting D'antonio et. al 8 - 13, wilson et. al 2001 measured mean length of female w eggs 9.59, female no eggs 8.49, male 8.44 = mean 8.84mm)

# Calculate wet mass (W) in grams using the allometric equation
W <- a * L^b
W

# delete 
# one indiv # 0.004307664 g
W * 1326 # multiple by prey in water 

active_krill
c <- 265.3521/ W # individ 1060 m 3 
# swim 0.33 - 0.99 
c * 1.5


# non - feeding 
no_krill <- ( W * 20 ) * 0.8 # 5 individuals m3 # adjusted for 80% capture efficiency 
no_krill #g/m-3
# active krill 
active_krill <- (W * 77000) * 0.8  #wilson et al. 2001 surface individuals m3 # adjusted for 80% capture efficiency 
active_krill #g/m-3
# slow feeding # min 
pass_krill_min <- (W * 207) * 0.8 # or 350 individuals m3 # adjusted for 80% capture efficiency # d'antonio et al. 2024 
pass_krill_min #g/m-3
# slow feeding # max
pass_krill_max <- (W * 2230) * 0.8 # or 350 individuals m3 # adjusted for 80% capture efficiency # d'antonio et al. 2024 
pass_krill_max #g/m-3
# slow feeding # mean
pass_krill_mean <- (W * 1326) * 0.8 # or 350 individuals m3 # adjusted for 80% capture efficiency # d'antonio et al. 2024 
pass_krill_mean #g/m-3

# connect cat and krill min 
dat$krill_min <- ifelse(dat$cat == "non_feeding", no_krill,
                    ifelse(dat$cat == "slow", pass_krill_min,
                           ifelse(dat$cat == "active", active_krill, NA)))

# connect cat and krill max
dat$krill_max <- ifelse(dat$cat == "non_feeding", no_krill,
                    ifelse(dat$cat == "slow", pass_krill_max,
                           ifelse(dat$cat == "active", active_krill, NA)))

#connect cat and krill mean
dat$krill_mean <- ifelse(dat$cat == "non_feeding", no_krill,
                         ifelse(dat$cat == "slow", pass_krill_mean,
                                ifelse(dat$cat == "active", active_krill, NA)))


# 6. Count of behaviours #######################################################

#continuous markov chain derived frequencies
dawn <- c(0.07, 0.14, 17.10, 7.63, 5.37, 15.00,29.27,24.02, 0.19, 0.88, 0.09, 0.02,0.22)
day <- c(2.5, 0.54, 12.53, 5.92, 5.48, 18.06, 20.75, 30.85, 1.40, 1.72, 0.24, 0.05, 0.25)
dusk <- c(5.35, 2.07, 14.33, 1.03, 4.32, 15.96, 14.74, 37.81, 1.49, 2.25, 0.22, 0.05, 0.38)
night <- c(2.33,0.17, 17.22, 4.67, 6.77, 11.87, 31.88, 23.53, 0.31, 0.89, 0.11, 0.01, 0.23)

behav <-c("Active_surface_feeding","Dive_feeding", "Ascent_slow_feeding", "Benthic_slow_feeding", "Gliding_slow_feeding",
          "Surface_slow_feeding", "Water_column_slow_feeding", "Cruising", "Gliding", "Surface_undulation", "Whale_shark_interaction",
          "Coughing", "Other")
percentages <- data.frame(behav, dawn, day, dusk, night)

# 1 hour has 3600 seconds 
percentages$dawn_sec <- c(percentages$dawn/ 100)*3600
percentages$day_sec <- c(percentages$day/ 100)*3600
percentages$dusk_sec <- c(percentages$dusk/ 100)*3600
percentages$night_sec <- c(percentages$night/ 100)*3600
#percentages <- percentages[, c(1,6:9)]
dat <- merge(dat, percentages, by = "behav") 

# 7. Water filtered and prey captured ##########################################

# water filtered per behaviour # m/s
# multiply vol filtered / by no of seconds of behaviour 
dat$vol_water_m3_h_dawn <- dat$vol_water_m3s * dat$dawn_sec # = m3/h
dat$vol_water_m3_h_day <- dat$vol_water_m3s * dat$day_sec # = m3/h
dat$vol_water_m3_h_dusk <- dat$vol_water_m3s * dat$dusk_sec # = m3/h
dat$vol_water_m3_h_night <- dat$vol_water_m3s * dat$night_sec # = m3/h

# prey capture min
dat$prey_g_h_dawn_min <- dat$vol_water_m3_h_dawn * dat$krill_min # = m3/h
dat$prey_g_h_day_min <- dat$vol_water_m3_h_day * dat$krill_min # = m3/h
dat$prey_g_h_dusk_min <- dat$vol_water_m3_h_dusk * dat$krill_min # = m3/h
dat$prey_g_h_night_min <- dat$vol_water_m3_h_night * dat$krill_min # = m3/h
options(scipen = 10) # piss off the scientific notation hehe 

# prey capture max
dat$prey_g_h_dawn_max <- dat$vol_water_m3_h_dawn * dat$krill_max # = m3/h
dat$prey_g_h_day_max <- dat$vol_water_m3_h_day * dat$krill_max # = m3/h
dat$prey_g_h_dusk_max <- dat$vol_water_m3_h_dusk * dat$krill_max # = m3/h
dat$prey_g_h_night_max <- dat$vol_water_m3_h_night * dat$krill_max # = m3/h

# prey capture mean
dat$prey_g_h_dawn_mean <- dat$vol_water_m3_h_dawn * dat$krill_mean # = m3/h
dat$prey_g_h_day_mean <- dat$vol_water_m3_h_day * dat$krill_mean # = m3/h
dat$prey_g_h_dusk_mean <- dat$vol_water_m3_h_dusk * dat$krill_mean # = m3/h
dat$prey_g_h_night_mean <- dat$vol_water_m3_h_night * dat$krill_mean # = m3/h

#delete
str(dat)
dat_tl6 <- dat[dat$tl == 6, ]

# 8. Adjust for all hours #################################
dat$vol_water_m3_h_dawn <- dat$vol_water_m3_h_dawn * 2  
dat$vol_water_m3_h_day <- dat$vol_water_m3_h_day * 13.5  
dat$vol_water_m3_h_dusk <- dat$vol_water_m3_h_dusk * 2  
dat$vol_water_m3_h_night <- dat$vol_water_m3_h_night * 8.5  

dat$prey_g_h_dawn_min <- dat$prey_g_h_dawn_min * 2
dat$prey_g_h_day_min <- dat$prey_g_h_day_min * 13.5
dat$prey_g_h_dusk_min <- dat$prey_g_h_dusk_min * 2
dat$prey_g_h_night_min <- dat$prey_g_h_night_min * 8.5

dat$prey_g_h_dawn_max <- dat$prey_g_h_dawn_max * 2
dat$prey_g_h_day_max <- dat$prey_g_h_day_max * 13.5
dat$prey_g_h_dusk_max <- dat$prey_g_h_dusk_max * 2
dat$prey_g_h_night_max <- dat$prey_g_h_night_max * 8.5

dat$prey_g_h_dawn_mean <- dat$prey_g_h_dawn_mean * 2
dat$prey_g_h_day_mean <- dat$prey_g_h_day_mean * 13.5
dat$prey_g_h_dusk_mean <- dat$prey_g_h_dusk_mean * 2
dat$prey_g_h_night_mean <- dat$prey_g_h_night_mean * 8.5

# 9. summarise data
#min
prey_min <- aggregate(cbind(prey_g_h_dawn_min, prey_g_h_day_min, prey_g_h_dusk_min, prey_g_h_night_min) ~ tl + cat,  data = dat, sum) # prey categories min
prey_min$cat[prey_min$cat == "slow"] <-  "slow_min"
colnames(prey_min) <-c("tl", "cat", "dawn", "day", "dusk", "night")
#max
prey_max <- aggregate(cbind(prey_g_h_dawn_max, prey_g_h_day_max, prey_g_h_dusk_max, prey_g_h_night_max) ~ tl + cat, data = dat, sum) # prey categories max
prey_max$cat[prey_max$cat == "slow"] <-  "slow_max"
colnames(prey_max) <-c("tl", "cat", "dawn", "day", "dusk", "night")
#mean
prey_mean <- aggregate(cbind(prey_g_h_dawn_mean, prey_g_h_day_mean, prey_g_h_dusk_mean, prey_g_h_night_mean) ~ tl + cat, data = dat, sum) # prey categories mean
prey_mean$cat[prey_mean$cat == "slow"] <-  "slow_mean"
colnames(prey_mean) <-c("tl", "cat", "dawn", "day", "dusk", "night")

# combine prey
data <- rbind(prey_min, prey_max[19:27, ], prey_mean[19:27, ]) # this still has the period categories 
data$prey <-  data$dawn + data$day + data$dusk + data$night

# sum water volume and repeat water volume values for each time of slow feeding (they are all the same)
water_vol <- aggregate(cbind(vol_water_m3_h_dawn, vol_water_m3_h_day, vol_water_m3_h_dusk, vol_water_m3_h_night) ~ tl + cat, data = dat, sum) # vol water 
water_vol$cat[water_vol$cat == "slow"] <-  "slow_min"
copy <- water_vol[19:27, ]
copy$cat[copy$cat == "slow_min"] <-  "slow_max"
water_vol <- rbind(water_vol, copy)
rm(copy)
copy <- water_vol[19:27, ]
copy$cat[copy$cat == "slow_min"] <-  "slow_mean"
water_vol <- rbind(water_vol, copy)
rm(copy)
water_vol$water <- water_vol$vol_water_m3_h_dawn +  water_vol$vol_water_m3_h_day + water_vol$vol_water_m3_h_dusk + water_vol$vol_water_m3_h_night
data$water <- water_vol$water
data$prey <- data$prey / 1000 # convert to kg 
data <- data[, c(1:2, 7:8) ] # clean 

# water 
total_water <- aggregate(cbind(vol_water_m3_h_dawn, vol_water_m3_h_day, vol_water_m3_h_dusk, vol_water_m3_h_night) ~ tl,  data = dat, sum)
total_water$water <- total_water$vol_water_m3_h_dawn +  total_water$vol_water_m3_h_day + total_water$vol_water_m3_h_dusk + total_water$vol_water_m3_h_night

# prey min 
total_prey <- aggregate(cbind(dawn, day, dusk, night) ~ tl, data = prey_min, FUN = sum)
total_prey$prey <- total_prey$dawn + total_prey$day + total_prey$dusk + total_prey$night
total_prey$cat <- "total_min" # make total category 
total <- cbind(total_prey, total_water) # add together 
total <- total[, c(1, 6, 7, 13)]
total$prey <- total$prey / 1000
data <- rbind(data, total)

# prey max
total_prey <- aggregate(cbind(dawn, day, dusk, night) ~ tl, data = prey_max, FUN = sum)
total_prey$prey <- total_prey$dawn + total_prey$day + total_prey$dusk + total_prey$night
total_prey$cat <- "total_max" # make total category 
total <- cbind(total_prey, total_water) # add together 
total <- total[, c(1, 6, 7, 13)]
total$prey <- total$prey / 1000
data <- rbind(data, total)

# prey mean
total_prey <- aggregate(cbind(dawn, day, dusk, night) ~ tl, data = prey_mean, FUN = sum)
total_prey$prey <- total_prey$dawn + total_prey$day + total_prey$dusk + total_prey$night
total_prey$cat <- "total_mean" # make total category 
total <- cbind(total_prey, total_water) # add together 
total <- total[, c(1, 6, 7, 13)]
total$prey <- total$prey / 1000
data <- rbind(data, total)

# extract mins and maxs for the minimum time model # and means :) 
# note: don't do this after the data has been reordered below because the tls will be in the wrong over 
min_total <- subset(data, cat == "total_min")$prey
mean_total <- subset(data, cat == "total_mean")$prey
max_total <- subset(data, cat == "total_max")$prey
min_pass <- subset(data, cat == "slow_min")$prey
mean_pass <- subset(data, cat == "slow_mean")$prey
max_pass <- subset(data, cat == "slow_max")$prey
act <- subset(data, cat == "active")$prey

# save 
#setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#write.csv(data, file = "filtration_model_results.csv")

# 10. plot :) ####
library(ggplot2)

# water filtered
water_cats <- c("total_mean", "active", "slow_mean")
water_data <- data[data$cat %in% water_cats, ]
water_data$cat[water_data$cat == "active"] <- "active feeding"
water_data$cat[water_data$cat == "slow_mean"] <- "slow feeding"
water_data$cat[water_data$cat == "total_mean"] <- "all feeding"

a <- ggplot(water_data, aes(x = tl, y = water, colour = cat)) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c("all feeding" = "#0D0887FF", "active feeding" = "#C03A83FF", "slow feeding" = "#E76F5AFF")) +
  labs(
    x = "Total length (m)",
    y = "Water filtered (m³)",
    title = "",
    colour = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
  axis.line = element_line(colour = "black", size = 1),
  axis.title = element_text(size = 14, colour = "black"),
  axis.text = element_text(size = 14, colour = "black"),
  legend.text = element_text(size = 14, colour = "black"),
  legend.title = element_text(size = 14, colour = "black")
) 

# prey plot 

# lines 
prey_line_data <- data[data$cat %in% c("total_mean", "slow_mean", "active"), ]  

# ribbons 
total_min <- data[data$cat == "total_min", c("tl", "prey")]
total_max <- data[data$cat == "total_max", c("tl", "prey")]
slow_min <- data[data$cat == "slow_min", c("tl", "prey")]
slow_max <- data[data$cat == "slow_max", c("tl", "prey")]

# total ribbon
total_ribbon <- data.frame(
  tl = total_min$tl,
  ymin = total_min$prey,
  ymax = total_max$prey
)

# slow 
slow_ribbon <- data.frame(
  tl = slow_min$tl,
  ymin = slow_min$prey,
  ymax = slow_max$prey
)

# clean 
prey_line_data$cat[prey_line_data$cat == "total_mean"] <- "all feeding"
prey_line_data$cat[prey_line_data$cat == "slow_mean"] <- "slow feeding"
prey_line_data$cat[prey_line_data$cat == "active"] <- "active feeding"

b <- ggplot() +
  geom_ribbon(data = total_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#0D0887FF", alpha = 0.5) +
  geom_ribbon(data = slow_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#E76F5AFF", alpha = 0.5) +
  geom_line(data = prey_line_data, aes(x = tl, y = prey, colour = cat), linewidth = 1.5) +
  scale_colour_manual(values = c(
    "all feeding" = "#0D0887FF",
    "slow feeding" = "#E76F5AFF",
    "active feeding" = "#C03A83FF"
  )) +
  scale_y_continuous(
    breaks = seq(0, 300, by = 25),  # adjust the upper limit as needed
    expand = expansion(mult = c(0, 0.05))  # optional: tighten the space above/below
  ) +
  labs(
    x = "Total length (m)",
    y = "Prey captured (kg)",
    title = "",
    colour = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.text = element_text(size = 14, colour = "black"),
    legend.title = element_text(size = 14, colour = "black")
  )


setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
ggsave("shark_length_x_water_filtered.png", plot = a, dpi = 300, width = 11, height = 9)  # Width and height are in inches
ggsave("shark_length_x_prey_captured.png", plot = b, dpi = 300, width = 11, height = 9)  # Width and height are in inches


=======

# # # # # filtration model # # # # # # #
# C.Barry 2025 # # # # # # # # # # # # #

########### ALL SHARK SIZES ############################################

# 1. Gape calculation #
# 2. Velocity of behaviours #
# 3. Behavior gapes # 
# 4. Flow velocities # 
# 5. Prey mass # 
# 6. Count of behaviours # 
# 7. Water filtered and prey captured # 
# 8. Period summaries and plots # 

# 1. Gape calculation ########################################################## 

#train data from motta et al. 2010 
sharks_train <- c("A", "B", "C")
tl_train <- c(6.22, 5.93, 4.43) # m 
gape_sa <-c(2035, 1841, 1079) # cm2
gape_m <- gape_sa /  10000 # m2
dat_train <- data.frame(sharks_train,tl_train,gape_m)
gape_allometric <- lm(log(gape_m) ~ log(tl_train), data = dat_train) #allometric model of log gape and log total length
#summary(gape_allometric)
intercept <- exp(coef(gape_allometric)[1])
slope <- coef(gape_allometric)[2]
#slope_se <- summary(gape_allometric)$coefficients[2, 2] # standard error 

# gape tl relationship
calculate_gape <- function(tl_train) {
  slope <- slope # Scaling exponent (beta)
  gape <- intercept * tl_train^slope  # Allometric relationship: gape = a * L^beta
  return(gape)
}
# save 
save(calculate_gape, file = "calculate_gape.RData")

#tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 7.0, 8.0) #shark lengths (tagged)
tl <- c(4.0, 4.5, 5.0, 5.5, 6.0, 6.5, 7.0, 7.5, 8.0) #shark lengths all 
gapes <- calculate_gape(tl) # gapes 
#rm(dat_train)
#rm(gape_allometric)

########### sanity check gape variations #######
# 
# # compare prediction to train data 
# fitted_gape <- calculate_gape(tl_train)
# cbind(tl_train, gape_m, fitted_gape)
# 
# # output
# # tl_train gape_m fitted_gape
# # 6.22  0.2035   0.2024163
# # 5.93  0.1841   0.1852475
# # 4.43  0.1079   0.1078057
# 
# # 95 % confidence intervals #
# 
# # Use predict function to predict on log scale and then back transform estimates 
# new_data <- data.frame(tl_train = tl) # make df for predict functino
# pred <- predict(gape_allometric, newdata = new_data, interval = "prediction")
# summary(pred)
# pred
# 
# # Back-transform from log to natural scale (m²) to get gape upper and lower confidence 
# gapes <- exp(pred[, "fit"])
# gapes_lower <- exp(pred[, "lwr"])
# gapes_upper <- exp(pred[, "upr"])
# 
# # plot allo relationship between gape (m²) and tl (m) with 95% confidence intervals 
# library(ggplot2)
# test1 <- data.frame(tl, gapes, gapes_lower, gapes_upper)
# ggplot(test1, aes(x = tl, y = gapes)) +
#   geom_ribbon(aes(ymin = gapes_lower, ymax = gapes_upper), fill = "grey80", alpha = 0.5) +
#   geom_line(colour = "red", linewidth = 1.2) +
#   labs(
#     x = "Total length (m)",
#     y = "Predicted gape area (m²)",
#     title = ""
#   ) +
#   theme_minimal(base_size = 14)
# 
# # error increases as sharks get larger


# 2. Velocity of behaviours ####################################################

vel <- c(1, 1, 0.9, 0.9, 0.9, 0.42, 0.43, 0.33, 1.1, 1.5, 1, 0.5, 1)
behav <- c("Cruising", "Coughing", "Surface_slow_feeding" ,"Water_column_slow_feeding", "Benthic_slow_feeding", "Ascent_slow_feeding", 
           "Gliding", "Gliding_slow_feeding", "Active_surface_feeding", "Dive_feeding", "Surface_undulation", "Whale_shark_interaction", "Other") 

# 3. Behaviour gapes ###########################################################

active_gape <- gapes * 0.8457
slow_gape <- gapes * 0.6167
non_feeding_gape <- gapes * 0.175

cat <- c("non_feeding", "non_feeding", "slow", "slow", "slow", "slow", "non_feeding", "slow", "active", "active", "non_feeding", "non_feeding", "non_feeding")
gape_map <- list( "non_feeding" = non_feeding_gape, "slow" = slow_gape, "active" = active_gape)

dat <- data.frame(
  behav = rep(behav, each = length(tl)),                # Repeat behaviours for each TL
  cat = rep(cat, each = length(tl)),                    # Repeat categories for each TL
  tl = rep(tl, times = length(behav))                   # Repeat TL for each behaviour
)

dat$gapes <- NA # empty col

# Loop through each row of the data frame
for (i in seq_len(nrow(dat))) {
  current_cat <- dat$cat[i]
  current_tl <- dat$tl[i]
  gape_values <- gape_map[[current_cat]] # Get the gape vector for the category
  tl_index <- which(tl == current_tl)    # Find the index of the current total length
  dat$gapes[i] <- gape_values[tl_index]  # Assign the corresponding gape
  dat$vel <- rep(vel, each = length(tl))
}

# 4. Flow velocities ###########################################################

# Motta 2010 flow velocity is 90.8 % of swimming velocity
dat$flow_vel <- dat$vel *0.908 # m/s of flow velocity 
dat$vol_water_m3s <- dat$flow_vel * dat$gapes # volume water filtered m3/s

# 5. Prey mass #################################################################

# wet mass calculation 
a <- 4.4e-6   # a coefficient in grams
b <- 3.16     # scaling exponent
L <- 8.84     # mean length of krill in mm (sonar setting D'antonio et. al 8 - 13, wilson et. al 2001 measured mean length of female w eggs 9.59, female no eggs 8.49, male 8.44 = mean 8.84mm)

# Calculate wet mass (W) in grams using the allometric equation
W <- a * L^b
W

# delete 
# one indiv # 0.004307664 g
W * 1326 # multiple by prey in water 

active_krill
c <- 265.3521/ W # individ 1060 m 3 
# swim 0.33 - 0.99 
c * 1.5


# non - feeding 
no_krill <- ( W * 20 ) * 0.8 # 5 individuals m3 # adjusted for 80% capture efficiency 
no_krill #g/m-3
# active krill 
active_krill <- (W * 77000) * 0.8  #wilson et al. 2001 surface individuals m3 # adjusted for 80% capture efficiency 
active_krill #g/m-3
# slow feeding # min 
pass_krill_min <- (W * 207) * 0.8 # or 350 individuals m3 # adjusted for 80% capture efficiency # d'antonio et al. 2024 
pass_krill_min #g/m-3
# slow feeding # max
pass_krill_max <- (W * 2230) * 0.8 # or 350 individuals m3 # adjusted for 80% capture efficiency # d'antonio et al. 2024 
pass_krill_max #g/m-3
# slow feeding # mean
pass_krill_mean <- (W * 1326) * 0.8 # or 350 individuals m3 # adjusted for 80% capture efficiency # d'antonio et al. 2024 
pass_krill_mean #g/m-3

# connect cat and krill min 
dat$krill_min <- ifelse(dat$cat == "non_feeding", no_krill,
                    ifelse(dat$cat == "slow", pass_krill_min,
                           ifelse(dat$cat == "active", active_krill, NA)))

# connect cat and krill max
dat$krill_max <- ifelse(dat$cat == "non_feeding", no_krill,
                    ifelse(dat$cat == "slow", pass_krill_max,
                           ifelse(dat$cat == "active", active_krill, NA)))

#connect cat and krill mean
dat$krill_mean <- ifelse(dat$cat == "non_feeding", no_krill,
                         ifelse(dat$cat == "slow", pass_krill_mean,
                                ifelse(dat$cat == "active", active_krill, NA)))


# 6. Count of behaviours #######################################################

#continuous markov chain derived frequencies
dawn <- c(0.07, 0.14, 17.10, 7.63, 5.37, 15.00,29.27,24.02, 0.19, 0.88, 0.09, 0.02,0.22)
day <- c(2.5, 0.54, 12.53, 5.92, 5.48, 18.06, 20.75, 30.85, 1.40, 1.72, 0.24, 0.05, 0.25)
dusk <- c(5.35, 2.07, 14.33, 1.03, 4.32, 15.96, 14.74, 37.81, 1.49, 2.25, 0.22, 0.05, 0.38)
night <- c(2.33,0.17, 17.22, 4.67, 6.77, 11.87, 31.88, 23.53, 0.31, 0.89, 0.11, 0.01, 0.23)

behav <-c("Active_surface_feeding","Dive_feeding", "Ascent_slow_feeding", "Benthic_slow_feeding", "Gliding_slow_feeding",
          "Surface_slow_feeding", "Water_column_slow_feeding", "Cruising", "Gliding", "Surface_undulation", "Whale_shark_interaction",
          "Coughing", "Other")
percentages <- data.frame(behav, dawn, day, dusk, night)

# 1 hour has 3600 seconds 
percentages$dawn_sec <- c(percentages$dawn/ 100)*3600
percentages$day_sec <- c(percentages$day/ 100)*3600
percentages$dusk_sec <- c(percentages$dusk/ 100)*3600
percentages$night_sec <- c(percentages$night/ 100)*3600
#percentages <- percentages[, c(1,6:9)]
dat <- merge(dat, percentages, by = "behav") 

# 7. Water filtered and prey captured ##########################################

# water filtered per behaviour # m/s
# multiply vol filtered / by no of seconds of behaviour 
dat$vol_water_m3_h_dawn <- dat$vol_water_m3s * dat$dawn_sec # = m3/h
dat$vol_water_m3_h_day <- dat$vol_water_m3s * dat$day_sec # = m3/h
dat$vol_water_m3_h_dusk <- dat$vol_water_m3s * dat$dusk_sec # = m3/h
dat$vol_water_m3_h_night <- dat$vol_water_m3s * dat$night_sec # = m3/h

# prey capture min
dat$prey_g_h_dawn_min <- dat$vol_water_m3_h_dawn * dat$krill_min # = m3/h
dat$prey_g_h_day_min <- dat$vol_water_m3_h_day * dat$krill_min # = m3/h
dat$prey_g_h_dusk_min <- dat$vol_water_m3_h_dusk * dat$krill_min # = m3/h
dat$prey_g_h_night_min <- dat$vol_water_m3_h_night * dat$krill_min # = m3/h
options(scipen = 10) # piss off the scientific notation hehe 

# prey capture max
dat$prey_g_h_dawn_max <- dat$vol_water_m3_h_dawn * dat$krill_max # = m3/h
dat$prey_g_h_day_max <- dat$vol_water_m3_h_day * dat$krill_max # = m3/h
dat$prey_g_h_dusk_max <- dat$vol_water_m3_h_dusk * dat$krill_max # = m3/h
dat$prey_g_h_night_max <- dat$vol_water_m3_h_night * dat$krill_max # = m3/h

# prey capture mean
dat$prey_g_h_dawn_mean <- dat$vol_water_m3_h_dawn * dat$krill_mean # = m3/h
dat$prey_g_h_day_mean <- dat$vol_water_m3_h_day * dat$krill_mean # = m3/h
dat$prey_g_h_dusk_mean <- dat$vol_water_m3_h_dusk * dat$krill_mean # = m3/h
dat$prey_g_h_night_mean <- dat$vol_water_m3_h_night * dat$krill_mean # = m3/h

#delete
str(dat)
dat_tl6 <- dat[dat$tl == 6, ]

# 8. Adjust for all hours #################################
dat$vol_water_m3_h_dawn <- dat$vol_water_m3_h_dawn * 2  
dat$vol_water_m3_h_day <- dat$vol_water_m3_h_day * 13.5  
dat$vol_water_m3_h_dusk <- dat$vol_water_m3_h_dusk * 2  
dat$vol_water_m3_h_night <- dat$vol_water_m3_h_night * 8.5  

dat$prey_g_h_dawn_min <- dat$prey_g_h_dawn_min * 2
dat$prey_g_h_day_min <- dat$prey_g_h_day_min * 13.5
dat$prey_g_h_dusk_min <- dat$prey_g_h_dusk_min * 2
dat$prey_g_h_night_min <- dat$prey_g_h_night_min * 8.5

dat$prey_g_h_dawn_max <- dat$prey_g_h_dawn_max * 2
dat$prey_g_h_day_max <- dat$prey_g_h_day_max * 13.5
dat$prey_g_h_dusk_max <- dat$prey_g_h_dusk_max * 2
dat$prey_g_h_night_max <- dat$prey_g_h_night_max * 8.5

dat$prey_g_h_dawn_mean <- dat$prey_g_h_dawn_mean * 2
dat$prey_g_h_day_mean <- dat$prey_g_h_day_mean * 13.5
dat$prey_g_h_dusk_mean <- dat$prey_g_h_dusk_mean * 2
dat$prey_g_h_night_mean <- dat$prey_g_h_night_mean * 8.5

# 9. summarise data
#min
prey_min <- aggregate(cbind(prey_g_h_dawn_min, prey_g_h_day_min, prey_g_h_dusk_min, prey_g_h_night_min) ~ tl + cat,  data = dat, sum) # prey categories min
prey_min$cat[prey_min$cat == "slow"] <-  "slow_min"
colnames(prey_min) <-c("tl", "cat", "dawn", "day", "dusk", "night")
#max
prey_max <- aggregate(cbind(prey_g_h_dawn_max, prey_g_h_day_max, prey_g_h_dusk_max, prey_g_h_night_max) ~ tl + cat, data = dat, sum) # prey categories max
prey_max$cat[prey_max$cat == "slow"] <-  "slow_max"
colnames(prey_max) <-c("tl", "cat", "dawn", "day", "dusk", "night")
#mean
prey_mean <- aggregate(cbind(prey_g_h_dawn_mean, prey_g_h_day_mean, prey_g_h_dusk_mean, prey_g_h_night_mean) ~ tl + cat, data = dat, sum) # prey categories mean
prey_mean$cat[prey_mean$cat == "slow"] <-  "slow_mean"
colnames(prey_mean) <-c("tl", "cat", "dawn", "day", "dusk", "night")

# combine prey
data <- rbind(prey_min, prey_max[19:27, ], prey_mean[19:27, ]) # this still has the period categories 
data$prey <-  data$dawn + data$day + data$dusk + data$night

# sum water volume and repeat water volume values for each time of slow feeding (they are all the same)
water_vol <- aggregate(cbind(vol_water_m3_h_dawn, vol_water_m3_h_day, vol_water_m3_h_dusk, vol_water_m3_h_night) ~ tl + cat, data = dat, sum) # vol water 
water_vol$cat[water_vol$cat == "slow"] <-  "slow_min"
copy <- water_vol[19:27, ]
copy$cat[copy$cat == "slow_min"] <-  "slow_max"
water_vol <- rbind(water_vol, copy)
rm(copy)
copy <- water_vol[19:27, ]
copy$cat[copy$cat == "slow_min"] <-  "slow_mean"
water_vol <- rbind(water_vol, copy)
rm(copy)
water_vol$water <- water_vol$vol_water_m3_h_dawn +  water_vol$vol_water_m3_h_day + water_vol$vol_water_m3_h_dusk + water_vol$vol_water_m3_h_night
data$water <- water_vol$water
data$prey <- data$prey / 1000 # convert to kg 
data <- data[, c(1:2, 7:8) ] # clean 

# water 
total_water <- aggregate(cbind(vol_water_m3_h_dawn, vol_water_m3_h_day, vol_water_m3_h_dusk, vol_water_m3_h_night) ~ tl,  data = dat, sum)
total_water$water <- total_water$vol_water_m3_h_dawn +  total_water$vol_water_m3_h_day + total_water$vol_water_m3_h_dusk + total_water$vol_water_m3_h_night

# prey min 
total_prey <- aggregate(cbind(dawn, day, dusk, night) ~ tl, data = prey_min, FUN = sum)
total_prey$prey <- total_prey$dawn + total_prey$day + total_prey$dusk + total_prey$night
total_prey$cat <- "total_min" # make total category 
total <- cbind(total_prey, total_water) # add together 
total <- total[, c(1, 6, 7, 13)]
total$prey <- total$prey / 1000
data <- rbind(data, total)

# prey max
total_prey <- aggregate(cbind(dawn, day, dusk, night) ~ tl, data = prey_max, FUN = sum)
total_prey$prey <- total_prey$dawn + total_prey$day + total_prey$dusk + total_prey$night
total_prey$cat <- "total_max" # make total category 
total <- cbind(total_prey, total_water) # add together 
total <- total[, c(1, 6, 7, 13)]
total$prey <- total$prey / 1000
data <- rbind(data, total)

# prey mean
total_prey <- aggregate(cbind(dawn, day, dusk, night) ~ tl, data = prey_mean, FUN = sum)
total_prey$prey <- total_prey$dawn + total_prey$day + total_prey$dusk + total_prey$night
total_prey$cat <- "total_mean" # make total category 
total <- cbind(total_prey, total_water) # add together 
total <- total[, c(1, 6, 7, 13)]
total$prey <- total$prey / 1000
data <- rbind(data, total)

# extract mins and maxs for the minimum time model # and means :) 
# note: don't do this after the data has been reordered below because the tls will be in the wrong over 
min_total <- subset(data, cat == "total_min")$prey
mean_total <- subset(data, cat == "total_mean")$prey
max_total <- subset(data, cat == "total_max")$prey
min_pass <- subset(data, cat == "slow_min")$prey
mean_pass <- subset(data, cat == "slow_mean")$prey
max_pass <- subset(data, cat == "slow_max")$prey
act <- subset(data, cat == "active")$prey

# save 
#setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
#write.csv(data, file = "filtration_model_results.csv")

# 10. plot :) ####
library(ggplot2)

# water filtered
water_cats <- c("total_mean", "active", "slow_mean")
water_data <- data[data$cat %in% water_cats, ]
water_data$cat[water_data$cat == "active"] <- "active feeding"
water_data$cat[water_data$cat == "slow_mean"] <- "slow feeding"
water_data$cat[water_data$cat == "total_mean"] <- "all feeding"

a <- ggplot(water_data, aes(x = tl, y = water, colour = cat)) +
  geom_line(linewidth = 1.5) +
  scale_colour_manual(values = c("all feeding" = "#0D0887FF", "active feeding" = "#C03A83FF", "slow feeding" = "#E76F5AFF")) +
  labs(
    x = "Total length (m)",
    y = "Water filtered (m³)",
    title = "",
    colour = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
  axis.line = element_line(colour = "black", size = 1),
  axis.title = element_text(size = 14, colour = "black"),
  axis.text = element_text(size = 14, colour = "black"),
  legend.text = element_text(size = 14, colour = "black"),
  legend.title = element_text(size = 14, colour = "black")
) 

# prey plot 

# lines 
prey_line_data <- data[data$cat %in% c("total_mean", "slow_mean", "active"), ]  

# ribbons 
total_min <- data[data$cat == "total_min", c("tl", "prey")]
total_max <- data[data$cat == "total_max", c("tl", "prey")]
slow_min <- data[data$cat == "slow_min", c("tl", "prey")]
slow_max <- data[data$cat == "slow_max", c("tl", "prey")]

# total ribbon
total_ribbon <- data.frame(
  tl = total_min$tl,
  ymin = total_min$prey,
  ymax = total_max$prey
)

# slow 
slow_ribbon <- data.frame(
  tl = slow_min$tl,
  ymin = slow_min$prey,
  ymax = slow_max$prey
)

# clean 
prey_line_data$cat[prey_line_data$cat == "total_mean"] <- "all feeding"
prey_line_data$cat[prey_line_data$cat == "slow_mean"] <- "slow feeding"
prey_line_data$cat[prey_line_data$cat == "active"] <- "active feeding"

b <- ggplot() +
  geom_ribbon(data = total_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#0D0887FF", alpha = 0.5) +
  geom_ribbon(data = slow_ribbon, aes(x = tl, ymin = ymin, ymax = ymax),
              fill = "#E76F5AFF", alpha = 0.5) +
  geom_line(data = prey_line_data, aes(x = tl, y = prey, colour = cat), linewidth = 1.5) +
  scale_colour_manual(values = c(
    "all feeding" = "#0D0887FF",
    "slow feeding" = "#E76F5AFF",
    "active feeding" = "#C03A83FF"
  )) +
  scale_y_continuous(
    breaks = seq(0, 300, by = 25),  # adjust the upper limit as needed
    expand = expansion(mult = c(0, 0.05))  # optional: tighten the space above/below
  ) +
  labs(
    x = "Total length (m)",
    y = "Prey captured (kg)",
    title = "",
    colour = NULL
  ) +
  theme_classic(base_size = 14) +
  theme(
    axis.line = element_line(colour = "black", size = 1),
    axis.title = element_text(size = 14, colour = "black"),
    axis.text = element_text(size = 14, colour = "black"),
    legend.text = element_text(size = 14, colour = "black"),
    legend.title = element_text(size = 14, colour = "black")
  )


setwd("C:/Users/chris/OneDrive - Murdoch University/CH2 MACHINE LEARNING/results") #setwd
ggsave("shark_length_x_water_filtered.png", plot = a, dpi = 300, width = 11, height = 9)  # Width and height are in inches
ggsave("shark_length_x_prey_captured.png", plot = b, dpi = 300, width = 11, height = 9)  # Width and height are in inches


>>>>>>> 3d0106305464a0329dc12f58854bfdf5e04b545f
