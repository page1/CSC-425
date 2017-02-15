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

#LDA on Glucose levels in comparission to true/false
library(MASS)
library(lubridate)
library(dplyr)
data_set=read.csv("data_set.csv", stringsAsFactors = F)%>%
  select(-X) %>%
  mutate(datetime = ymd_hms(datetime)) %>%
  rename(basal_delivery = basal_delievery)
newdata= data_set %>% select(-datetime, -basal_insulin_hourly_rate, -bolus_insulin_units, -glucose, -meal_approx_carbs, -low_bg, -in_range_bg, -after_noon, -dawn_phenomenon)
newdata = newdata[complete.cases(newdata),]
head(newdata)

#head(newdata)
try1= lda(high_bg ~ ., data = newdata)
pred = predict(try1,newdata)
names(pred)
table(newdata$high_bg,pred$class)
vlda = function(v,formula,data,cl){
  require(MASS)
  grps = cut(1:nrow(data),v,labels=FALSE)[sample(1:nrow(data))]
  pred = lapply(1:v,function(i,formula,data){
    omit = which(grps == i)
    z = lda(formula,data=data[-omit,])
    predict(z,data[omit,])
  },formula,data)
  
  wh = unlist(lapply(pred,function(pp)pp$class))
  table(wh,cl[order(grps)])
}
tt = vlda(5, high_bg~., newdata, newdata$high_bg)
tt
error = sum(tt[row(tt) != col(tt)])/ sum(tt)
error
library(rpart)
glucose.rpart = rpart(high_bg~., data = data_set)
glucose.rpart
plot(glucose.rpart)
text(glucose.rpart, use.n=TRUE,xpd=TRUE)
plot(try1)
