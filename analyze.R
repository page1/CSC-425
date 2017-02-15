#Use this to plot and analyze 
library(ggplot2)
library(corrplot)

plot_omnipod_type_frequency <- function(data){
  type_frequency <- data %>%
    group_by(type) %>%
    summarize(type_count = n())
  
  plot <- ggplot(aes(x = type, y = type_count), data = type_frequency) +
    geom_bar(stat = "identity") +
    theme(axis.text.x=element_text(angle = -45, hjust = 0)) +
    xlab("Record Type") +
    ylab("Record Type Frequency") +
    ggtitle("Omnipod Record Type Frequency")
  
  print(plot)
}

hist_bolus <- function(omni_pod){
  data <- filter(omni_pod,
                 type == "Bolus Insulin")
  
  hist(data$value, xlab = "Bolus Size (units)", ylab = "Frequency", main = "Hist of Bolus sizes")
}

hist_fitbit <- function(fitbit){
  hist(fitbit$floors, xlab = "Floors", main = "Hist of Fitbit Floors")
  hist(fitbit$distance, xlab = "Distance", main = "Hist of Fitbit Distance")
  hist(fitbit$calories, xlab = "Calories", main = "Hist of Fitbit Calories")
  hist(fitbit$steps, xlab = "Steps", main = "Hist of Fitbit Steps")
  hist(fitbit$elevation, xlab = "Elevation", main = "Hist of Fitbit Elevation")
}

corrplot_fitbit <- function(fitbit){
  old <- par()
  corrplot(cor(select(fitbit, -date, -time)))
  par(old)
}
