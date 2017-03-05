# Use this to run things from the top level
library(lubridate)
library(fBasics)
library(forecast)
library(lmtest)
library(fUnitRoots)
library(zoo)
library(xts)
library(ggplot2)
library(scales)
library(grid)
library(gridExtra)
library(cowplot)

source("get.R")
source("munge.R")
source("analyze.R")



# Get Data
omni_pod_raw <- get_omnipod_data()
dexcom_raw <- get_dexcom()
floors_raw <- get_floors()
distance_raw <- get_distance()
calories_raw <- get_calories()
steps_raw <- get_steps()
elevation_raw <- get_elevation()

# Mutate Data
dexcom <- munge_dexcom(dexcom_raw)
omni_pod <- munge_omni_pod(omni_pod_raw)

fitbit <- join_fitbit_data(floors_raw, distance_raw, calories_raw, steps_raw, elevation_raw)
fitbit <- munge_fitbit(fitbit)

fitbit_dexcom <- join_fitbit_with_dexcom_predictions(fitbit, dexcom)

insulin_bg_and_fitness <- munge_insulin_bg_fitness(fitbit_dexcom, omni_pod, burn_rate = 0.98)

data_plus_fields <- add_rolling_stats(insulin_bg_and_fitness) %>%
  add_categorical_fields()

data_for_ts <- select(data_plus_fields, datetime, bolus_burndown, total_insulin_burndown, acting_carbs, predicted_bg, contains("steps_sum_past"), low_bg, high_bg, dawn_phenomenon) %>%
  filter(datetime > ymd('2015-08-24'))

xts_data <- xts(select(data_for_ts, -datetime), order.by = data_for_ts$datetime)


filled_data <- na.approx(na.trim(xts_data), maxgap = 6)

hist(filled_data$predicted_bg,
     main = "Histogram of BG Levels",
     xlab = "BG Level")
hist(log(filled_data$predicted_bg),
     main = "Histogram of Log Normal BG Levels",
     xlab = "Log(BG Level)")
plot(filled_data$predicted_bg,
     main = "BG Over Time",
     ylab = "BG Level")

block_of_data <- data.frame(na.contiguous(filled_data))
block_of_data$time <- ymd_hms(rownames(block_of_data))
block_of_data$high_mid_low <- as.factor(ifelse(block_of_data$high_bg, 'High', ifelse(block_of_data$low_bg, 'Low', 'In Range')))
bg <- ggplot(aes(x = time, y = predicted_bg, color = high_mid_low), data = block_of_data) +
  geom_point() +
  ylab("BG Level") +
  xlab("Time") +
  scale_colour_manual(values = c("red", "forest green", "blue")) +
  theme(legend.position = c(0.9,0.8)) +
  scale_x_datetime(breaks = date_breaks("6 hour"),
                   labels = date_format("%b %d %I %p"))
carb <- ggplot(aes(x = time, y = acting_carbs), data = block_of_data) +
  geom_point() +
  ylab("Active Carbs") +
  xlab("Time") +
  scale_x_datetime(breaks = date_breaks("6 hour"),
                   labels = date_format("%b %d %I %p"))
insulin <- ggplot(aes(x = time, y = total_insulin_burndown), data = block_of_data) +
  geom_point() +
  ylab("Active Insulin") +
  xlab("Time") +
  scale_x_datetime(breaks = date_breaks("6 hour"),
                   labels = date_format("%b %d %I %p"))
steps <- ggplot(aes(x = time, y = steps_sum_past_1hour), data = block_of_data) +
  geom_point() +
  ylab("Hour Of Steps") +
  xlab("Time") +
  scale_x_datetime(breaks = date_breaks("6 hour"),
                   labels = date_format("%b %d %I %p"))

trends <- plot_grid(bg, carb, insulin, steps, 
          align = "v", nrow = 4, rel_heights = c(5, 3, 3, 3),
          label_size = 14)
ggsave("trends.png", trends, width = 8, height = 8)

hist(filled_data$steps_sum_past_1hour)
hist(log(filled_data$steps_sum_past_1hour))

# Help finding tranforms to make data normal
find_lambda <- MASS::boxcox(lm(predicted_bg~1,data=data.frame(filled_data)))
find_lambda <- find_lambda$x[which(find_lambda$y == max(find_lambda$y))]

find_lambda <- MASS::boxcox(lm(total_insulin_burndown~1/sqrt(predicted_bg),data=data.frame(filled_data)))
find_lambda <- find_lambda$x[which(find_lambda$y == max(find_lambda$y))]

find_lambda <- MASS::boxcox(lm(acting_carbs~1/sqrt(predicted_bg),data=data.frame(filled_data)))
find_lambda <- find_lambda$x[which(find_lambda$y == max(find_lambda$y))]

find_lambda <- MASS::boxcox(lm(steps_sum_past_1hour + 1~1/sqrt(predicted_bg),data=data.frame(filled_data)))
find_lambda <- find_lambda$x[which(find_lambda$y == max(find_lambda$y))]


filled_data$log_total_insulin_burndown <- log(filled_data$total_insulin_burndown)
filled_data$log_steps_sum_past_1hour <- log(filled_data$steps_sum_past_1hour + 1)

hist(filled_data$total_insulin_burndown,
     main = "Histogram of Insulin Burndown",
     xlab = "Insulin Burndown")
hist(filled_data$log_total_insulin_burndown,
     main = "Histogram of Log Insulin Burndown",
     xlab = "Log(Insulin Burndown)")

hist(filled_data$steps_sum_past_1hour,
     main = "Histogram of Steps Past Hour",
     xlab = "Steps Past Hour")
hist(filled_data$log_steps_sum_past_1hour,
     main = "Histogram of Log Steps Past Hour",
     xlab = "Log(Steps Past Hour)")

hist(filled_data$predicted_bg,
     main = "Histogram of BG",
     xlab = "BG")
hist(1/sqrt(filled_data$predicted_bg),
     main = "Histogram of 1 / sqrt(BG)",
     xlab = "1 / sqrt(BG)")


xts_data_15_minute_filled <- aggregate(filled_data, time(filled_data) - as.numeric(time(filled_data)) %% (15*60), mean, na.rm = T)
xts_data_30_minute_filled <- aggregate(filled_data, time(filled_data) - as.numeric(time(filled_data)) %% (30*60), mean, na.rm = T)

#write.csv(filled_data, 
#          file = "data_set.csv")



plot(xts_data_15_minute_filled$predicted_bg)
plot(xts_data_30_minute_filled$predicted_bg)



Acf(log(filled_data$predicted_bg), lag.max = 12*24, na.action = na.pass,
    main = "24 Hour ACF of BG",
    xlab = "5 Minute Lags")
Pacf(log(filled_data$predicted_bg), lag.max = 24, na.action = na.pass) # suggestive of AR(5)
Acf(diff(log(filled_data$predicted_bg)), lag.max = 12*24, plot = T, na.action = na.pass) #first difference helps keep the acf from running away
Pacf(diff(log(filled_data$predicted_bg)), lag.max = 24, plot = T, na.action = na.pass) # suggestive of AR(3)

# continue investigation in scott-scratch-pad.R

Acf(filled_data$predicted_bg)
Acf(diff(filled_data$predicted_bg))
plot(diff(filled_data$predicted_bg))
hist(diff(filled_data$predicted_bg))
m1 <- auto.arima(filled_data$predicted_bg, ic = 'bic', trace = T, seasonal = F, stationary = F, xreg=filled_data[,c('acting_carbs', 'total_insulin_burndown')])
Acf(m1$residuals)
