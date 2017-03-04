

diff_acf <- Acf(diff(log(filled_data$predicted_bg)), lag.max = 100, plot = T)
possible_period1 <- which(diff_acf$acf == min(diff_acf$acf[40:70])) 

Pacf(diff(diff(log(filled_data$predicted_bg)), possible_period1), lag.max = 100, plot = T) 

m1 <- Arima(filled_data$predicted_bg, order = c(1,1,1), seasonal = list(order = c(1, 1, 1), period = possible_period1), 
            xreg=filled_data[,c('acting_carbs', 'total_insulin_burndown', 'steps_sum_past_1hour')],
            lambda = 0)

Acf(m1$residuals, lag.max = 12*24)
Box.test(m1$resid, lag=100, type='Ljung', fitdf=2)
coeftest(m1)
Acf(m1$residuals, lag.max = 100)
Pacf(m1$residuals, lag.max = 24)
View(Acf(m1$residuals, lag.max = 12*24)$acf)
coeftest(m1)
