#Use this to do data cleaning and manipulation
library(dplyr)
library(tidyr)
library(lubridate)
library(RcppRoll)

column_names_to_lower_case <- function(data) {
  colnames(data) <- tolower(colnames(data))
  
  return(data)
}

split_units <- function(data){
  data <- data %>%
    mutate(value = ifelse(value == "", " ", value)) %>%
    separate(value, c("value", "units"), sep = " ") %>%
    mutate(units = gsub("\\(|\\)", "", units))
  
  return(data)
}

aggregate_omni_pod <- function(omni_pod){
  omni_pod1 <- omni_pod %>%
    filter(!(type %in% c("Bolus Extended", "Basal Insulin"))) %>%
    group_by(datetime, type) %>%
    summarize(value = as.character(sum(as.numeric(value))))
  
  omni_pod2 <- omni_pod %>%
    filter(type %in% c("Bolus Extended", "Basal Insulin")) %>%
    group_by(datetime, type) %>%
    summarize(value = last(value))
  
  return(rbind(omni_pod1, omni_pod2))
}

flatten_omni_pod <- function(omni_pod){
  omni_pod <- omni_pod %>%
    spread(type, value)
  
  return(omni_pod)
}

munge_omni_pod <- function(omni_pod_raw){
  Sys.setlocale('LC_ALL','C')
  omp <- omni_pod_raw %>%
    column_names_to_lower_case() %>%
    select(-hidden) %>%
    filter(!(type %in% c("Pump Alarm", "State Of Health", "Notes"))) %>%
    mutate(datetime = parse_date_time(paste(date, time), c("mdy hm", "mdy")) + ifelse(grepl("PM", time), dhours(12), dhours(0))) %>%
    split_units() %>%
    mutate(value = as.numeric(value)) %>%
    mutate(extend_amount = ifelse(grepl("Bolus-Extended Meal Bolus - ", description, fixed = T), value, 0),
           extend_time = ifelse(grepl("Bolus-Extended Meal Bolus - ", description, fixed = T), gsub(" minutes.", "", gsub("Bolus-Extended Meal Bolus - ", "", description)), 0),
           value = ifelse(extend_time != 0, paste(extend_amount, extend_time, sep = "~"), value),
           type = ifelse(extend_time != 0, "Bolus Extended", type)) %>%
    aggregate_omni_pod() %>%
    flatten_omni_pod() %>%
    column_names_to_lower_case()
  
  colnames(omp) <- gsub(" |\\(|\\)", "_", colnames(omp))
  
  omp <- omp %>%
    mutate(bolus_extended = gsub(" Reverse Corrected.", "", bolus_extended),
           bolus_extended = ifelse(is.na(bolus_extended), "~", bolus_extended)) %>%
    separate(bolus_extended, c("bolus_extended_units", "bolus_extended_for_minutes"), sep = "~") %>%
    mutate(bolus_extended_units = ifelse(bolus_extended_units == "", NA, bolus_extended_units),
           bolus_extended_for_minutes = ifelse(bolus_extended_for_minutes == "", NA, bolus_extended_for_minutes)) %>%
    rename(basal_insulin_hourly_rate = basal_insulin,
           bolus_insulin_units = bolus_insulin,
           meal_approx_carbs = meal,
           daily_insulin_summary_units = insulin_summary) %>%
    mutate_each(funs(as.numeric), -datetime) %>%
    mutate(bolus_extended_for_minutes = dminutes(bolus_extended_for_minutes)) %>%
    select(-glucose__control_)
    
  return(omp)
}

join_fitbit_data <- function(floors, distance, calories, steps, elevation){
  data <- floors %>%
    full_join(distance, by = c("date" = "date", "time" = "time")) %>%
    full_join(calories, by = c("date" = "date", "time" = "time")) %>%
    full_join(steps, by = c("date" = "date", "time" = "time")) %>%
    full_join(elevation, by = c("date" = "date", "time" = "time"))
  
  return(data)
}

munge_fitbit <- function(fitbit){
  fitbit <- fitbit %>%
    mutate(datetime = ymd_hms(paste(date, time)),
           date = NULL,
           time = NULL)
  
  return(fitbit)
}

munge_dexcom <- function(dexcom_raw){
  dexcom <- dexcom_raw %>%
    column_names_to_lower_case() %>%
    select(-contains("internaltime"))
  
  predicted_bg <- select(dexcom, contains("glucose")) %>%
    filter(!is.na(glucosevalue))
  colnames(predicted_bg) <- gsub("glucose", "", colnames(predicted_bg))
  predicted_bg <- predicted_bg %>%
    rename(datetime = displaytime, predicted_bg = value) %>%
    mutate(datetime = mdy_hm(datetime),
           predicted_bg = as.numeric(ifelse(predicted_bg == "Low", 40, predicted_bg)))
  
  calibrated_bg <- select(dexcom, contains("meter")) %>%
    filter(!is.na(metervalue))
  colnames(calibrated_bg) <- gsub("meter", "", colnames(calibrated_bg))
  calibrated_bg <- calibrated_bg %>% 
    rename(datetime = displaytime, calibrated_bg = value) %>%
    mutate(datetime = mdy_hm(datetime),
           calibrated_bg = as.numeric(calibrated_bg))
  
  return(list(predicted_bg = predicted_bg,
              calibrated_bg = calibrated_bg))
}

join_fitbit_with_dexcom_predictions <- function(fitbit, dexcom) {
  predicted_bg <- dexcom$predicted_bg 
  predicted_bg <- predicted_bg %>%
    mutate(datetime = as.POSIXct(floor(as.numeric(datetime) / (5 * 60)) * 5 * 60, origin = "1970-01-01",tz = "GMT")) %>%
    group_by(datetime) %>%
    summarize(predicted_bg = mean(predicted_bg))
  
  fitbit_aggregate <- fitbit %>%
    mutate(datetime = as.POSIXct(ceiling(as.numeric(datetime) / (5 * 60)) * 5 * 60, origin = "1970-01-01",tz = "GMT")) %>%
    group_by(datetime) %>%
    summarize_each(funs(sum))
  
  data <- predicted_bg %>%
    inner_join(fitbit_aggregate, by = c("datetime" = "datetime"))
  
  return(data)
}

munge_insulin_bg_fitness <- function(fitbit_dexcom, omni_pod, burn_rate = 0.98){
  lol <- omni_pod %>%
    mutate(extended_till = datetime + bolus_extended_for_minutes) %>%
    select(extended_till, bolus_extended_units) %>%
    filter(!is.na(bolus_extended_units)) %>%
    rename(datetime = extended_till,
           bolus_insulin_units = bolus_extended_units)
  
  boom <- rbind_all(list(omni_pod, lol)) %>%
    select(-contains("extended"), -contains("summary")) %>%
    filter(!is.na(basal_insulin_hourly_rate) |
           !is.na(bolus_insulin_units) |
           !is.na(glucose) |
           !is.na(meal_approx_carbs))
  
  ddd <- boom %>%
    mutate(datetime = as.POSIXct(floor(as.numeric(datetime) / (5 * 60)) * 5 * 60, origin = "1970-01-01",tz = "GMT")) %>%
    arrange(datetime) %>%
    group_by(datetime) %>%
    summarize(basal_insulin_hourly_rate = last(basal_insulin_hourly_rate), #Note: might introduce null
              bolus_insulin_units = sum(bolus_insulin_units, na.rm = T),
              glucose = mean(glucose, na.rm = T),
              meal_approx_carbs = sum(meal_approx_carbs, na.rm = T))
    
  time_series <- data.frame(datetime = seq(min(ddd$datetime), max(ddd$datetime), by = "5 mins"))
  
  data_spread <- time_series %>%
    left_join(ddd, by = c("datetime" = "datetime")) %>%
    arrange(datetime)
  
  last_value <- 0
  data_spread$basal_delievery <- sapply(data_spread$basal_insulin_hourly_rate, function(x){
    if(!is.na(x)){
      last_value <<- x / 12 # hourly rate breaks up into 12 delieveries per hour = 5 minute amount
    }
    
    return(last_value)
  })
  
  last_value <- 0
  data_spread$bolus_burndown <- sapply(data_spread$bolus_insulin_units, function(x){
    last_value <<- last_value * burn_rate
    
    if(!is.na(x)){
      last_value <<- last_value + x
    }
    
    return(last_value)
  })
  
  last_value <- 0
  data_spread$total_insulin_burndown <- mapply(function(x, y){
    last_value <<- last_value * burn_rate
    
    if(!is.na(x)){
      last_value <<- last_value + x
    }
    
    if(!is.na(y)){
      last_value <<- last_value + y
    }
    
    return(last_value)
  }, data_spread$bolus_insulin_units, data_spread$basal_delievery)
  
  last_value <- 0
  data_spread$acting_carbs <- sapply(data_spread$meal_approx_carbs, function(x){
    last_value <<- last_value * burn_rate
    
    if(!is.na(x)){
      last_value <<- last_value + x
    }
    
    return(last_value)
  })
  
  data_spread <- data_spread %>%
    filter(between(datetime, min(fitbit_dexcom$datetime), max(fitbit_dexcom$datetime))) %>%
    left_join(fitbit_dexcom, by = c("datetime" = "datetime"))
  
  return(data_spread)
}

add_rolling_stats <- function(insulin_bg_and_fitness) {
  rolling_stats <- insulin_bg_and_fitness %>%
    mutate(predicted_bg_lag_1hour = lag(predicted_bg, n = 12),
           predicted_bg_lag_2hour = lag(predicted_bg, n = 24),
           predicted_bg_lag_3hour = lag(predicted_bg, n = 36),
           predicted_bg_lead_1hour = lead(predicted_bg, n = 12),
           predicted_bg_lead_2hour = lead(predicted_bg, n = 24),
           predicted_bg_lead_3hour = lead(predicted_bg, n = 36),
           predicted_bg_lead_4hour = lead(predicted_bg, n = 48),
           predicted_bg_lead_5hour = lead(predicted_bg, n = 60)
           ) %>%
    mutate(total_insulin_burndown_lag_1hour = lag(total_insulin_burndown, n = 12),
           total_insulin_burndown_lag_2hour = lag(total_insulin_burndown, n = 24),
           total_insulin_burndown_lag_3hour = lag(total_insulin_burndown, n = 36),
           total_insulin_burndown_lead_1hour = lead(total_insulin_burndown, n = 12),
           total_insulin_burndown_lead_2hour = lead(total_insulin_burndown, n = 24),
           total_insulin_burndown_lead_3hour = lead(total_insulin_burndown, n = 36),
           total_insulin_burndown_lead_4hour = lead(total_insulin_burndown, n = 48),
           total_insulin_burndown_lead_5hour = lead(total_insulin_burndown, n = 60)
           ) %>%
    mutate(acting_carbs_lag_1hour = lag(acting_carbs, n = 12),
           acting_carbs_lag_2hour = lag(acting_carbs, n = 24),
           acting_carbs_lag_3hour = lag(acting_carbs, n = 36),
           acting_carbs_lead_1hour = lead(acting_carbs, n = 12),
           acting_carbs_lead_2hour = lead(acting_carbs, n = 24),
           acting_carbs_lead_3hour = lead(acting_carbs, n = 36),
           acting_carbs_lead_4hour = lead(acting_carbs, n = 48),
           acting_carbs_lead_5hour = lead(acting_carbs, n = 60)
           ) %>%
    mutate(calories_sum_past_1hour = roll_sum(calories, n = 12, align = "right", fill = NA),
           calories_sum_past_2hour = roll_sum(calories, n = 24, align = "right", fill = NA),
           calories_sum_past_3hour = roll_sum(calories, n = 36, align = "right", fill = NA),
           calories_sum_next_1hour = roll_sum(calories, n = 12, align = "left", fill = NA),
           calories_sum_next_2hour = roll_sum(calories, n = 24, align = "left", fill = NA),
           calories_sum_next_3hour = roll_sum(calories, n = 36, align = "left", fill = NA),
           calories_sum_next_4hour = roll_sum(calories, n = 48, align = "left", fill = NA),
           calories_sum_next_5hour = roll_sum(calories, n = 60, align = "left", fill = NA)
           ) %>%
    mutate(steps_sum_past_1hour = roll_sum(steps, n = 12, align = "right", fill = NA),
           steps_sum_past_2hour = roll_sum(steps, n = 24, align = "right", fill = NA),
           steps_sum_past_3hour = roll_sum(steps, n = 36, align = "right", fill = NA),
           steps_sum_next_1hour = roll_sum(steps, n = 12, align = "left", fill = NA),
           steps_sum_next_2hour = roll_sum(steps, n = 24, align = "left", fill = NA),
           steps_sum_next_3hour = roll_sum(steps, n = 36, align = "left", fill = NA),
           steps_sum_next_4hour = roll_sum(steps, n = 48, align = "left", fill = NA),
           steps_sum_next_5hour = roll_sum(steps, n = 60, align = "left", fill = NA)
           ) %>%
    mutate(floors_sum_past_1hour = roll_sum(floors, n = 12, align = "right", fill = NA),
           floors_sum_past_2hour = roll_sum(floors, n = 24, align = "right", fill = NA),
           floors_sum_past_3hour = roll_sum(floors, n = 36, align = "right", fill = NA),
           floors_sum_next_1hour = roll_sum(floors, n = 12, align = "left", fill = NA),
           floors_sum_next_2hour = roll_sum(floors, n = 24, align = "left", fill = NA),
           floors_sum_next_3hour = roll_sum(floors, n = 36, align = "left", fill = NA),
           floors_sum_next_4hour = roll_sum(floors, n = 48, align = "left", fill = NA),
           floors_sum_next_5hour = roll_sum(floors, n = 60, align = "left", fill = NA))
  
  return(rolling_stats)
}

add_categorical_fields <- function(data_plus_fields) {
  data_plus_fields <- data_plus_fields %>%
    mutate(high_bg = predicted_bg > 180,
           low_bg = predicted_bg < 70,
           in_range_bg = !(high_bg | low_bg),
           after_noon = hour(datetime) > 12,
           dawn_phenomenon = hour(datetime) > 2 & hour(datetime) < 8) # http://www.mayoclinic.org/diseases-conditions/diabetes/expert-answers/dawn-effect/faq-20057937
  
  return(data_plus_fields)
}