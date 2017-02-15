#Use this to source the data
library(dplyr)

get_omnipod_data <- function(){
  data <- read.csv("Data/OmniPod.csv", stringsAsFactors = F)
  
  return(data)
}

get_dexcom <- function(){
  data1 <- read.table("Data/Dexcom 1.Export.txt", header = TRUE, sep="\t", stringsAsFactors = F)
  data2 <- read.table("Data/Dexcom 2.Export.txt", header = TRUE, sep = "\t", stringsAsFactors = F)
  
  dexcom_total <- rbind(data1, data2)
  
  return(dexcom_total)
}

read_fitbit_folder <- function(folder_name){
  file_names <- list.files(folder_name)
  data <- lapply(paste(folder_name, file_names, sep = "/"), read.csv, stringsAsFactors = F) %>%
    do.call("rbind", .)
  
 return(data)
}

get_floors <- function(){
  data <- read_fitbit_folder("Data/floors")
  data <- select(data, date, time, value) %>%
    rename(floors=value)
  
  return(data)
}

get_distance <- function(){
  data <- read_fitbit_folder("Data/distance")
  data <- select(data, date, time, value) %>%
    rename(distance = value)
  
    return(data)
}

get_calories <- function(){
  data <- read_fitbit_folder("Data/calories")
  data <- select(data, date, time, value) %>%
    rename(calories = value)
  
  return(data)
}

get_steps <- function(){
  data <- read_fitbit_folder("Data/steps")
  data <- select(data, date, time, value) %>%
    rename(steps = value)
  
  return(data)
}

get_elevation <- function(){
  data <- read_fitbit_folder("Data/elevation")
  data <- select(data, date, time, value) %>%
    rename(elevation = value)
  
  return(data)
}
