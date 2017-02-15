# Use this to run things from the top level
source("get.R")
source("munge.R")
source("analyze.R")

library(dplyr)

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

write.csv(data_plus_fields, file = "data_set.csv")

# Analyse Data

plot_omnipod_type_frequency(omni_pod)
hist_bolus(omni_pod)
hist_fitbit(fitbit)
corrplot_fitbit(fitbit)

dd <- lm(predicted_bg_lead_1hour ~ predicted_bg + acting_carbs + total_insulin_burndown +  steps_sum_next_1hour, data = data_plus_fields)
summary(dd)


