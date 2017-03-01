# Use this to run things from the top level
library(lubridate)
library(fBasics)
library(forecast)
library(lmtest)
library(fUnitRoots)
library(zoo)
library(xts)

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

data_for_ts <- select(data_plus_fields, datetime, bolus_burndown, total_insulin_burndown, acting_carbs, predicted_bg, contains("steps_sum_past"), low_bg, high_bg, dawn_phenomenon)

xts_data <- xts(select(data_for_ts, -datetime), order.by = data_for_ts$datetime)
filled_data <- na.approx(xts_data)

write.csv(filled_data, 
          file = "data_set.csv")

plot(filled_data$predicted_bg)

Acf(filled_data$predicted_bg)
Acf(diff(filled_data$predicted_bg))
plot(diff(filled_data$predicted_bg))
hist(diff(filled_data$predicted_bg))
m1 <- auto.arima(filled_data$predicted_bg, ic = 'bic', trace = T)
Acf(m1$residuals)
