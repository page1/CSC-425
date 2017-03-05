source("backtest.R")

train <- head(xts_data_30_minute_filled, nrow(xts_data_30_minute_filled) * .95)
test <- tail(xts_data_30_minute_filled, nrow(xts_data_30_minute_filled) * .05)
adfTest(1/sqrt(train$predicted_bg), lags=24*2, type=c("c"))

Acf(1/sqrt(train$predicted_bg), lag.max = 48, plot = T, na.action = na.pass)
Pacf(1/sqrt(train$predicted_bg), lag.max = 48, plot = T, na.action = na.pass)

Acf(diff(1/sqrt(train$predicted_bg), 9), lag.max = 48, plot = T, na.action = na.pass)
Pacf(diff(1/sqrt(train$predicted_bg), 9), lag.max = 48, plot = T, na.action = na.pass)

find_lambda <- MASS::boxcox(lm(predicted_bg~acting_carbs + log_total_insulin_burndown + log_steps_sum_past_1hour,data=data.frame(train)))
find_lambda <- find_lambda$x[which(find_lambda$y == max(find_lambda$y))]
m3 <- Arima(train$predicted_bg, order = c(3,0,1), seasonal = list(order = c(1, 0, 0), period = 9),
      xreg=train[,c('acting_carbs', 'log_total_insulin_burndown', 'log_steps_sum_past_1hour')],
      lambda = find_lambda)
m3
Acf(m3$residuals, na.action = na.pass, lag.max = 24)
Pacf(m3$residuals, na.action = na.pass, lag.max = 24)
coeftest(m3)
Box.test(m3$residuals, lag=24, type='Ljung', fitdf=2)

f_cast <- forecast(m3, xreg = head(na.approx(test)[,c('acting_carbs', 'log_total_insulin_burndown', 'log_steps_sum_past_1hour')], 48))
plot(f_cast, include = 400, xaxt="n", 
     ylab = "BG Level",
     main = "Forecast of BG ARIMA(3,0,1)(1,0,0)[9]")
axis.POSIXct(1, x=c(tail(index(train), 400), head(index(test), 48)), format="%b %d")

full_model <- Arima(xts_data_30_minute_filled$predicted_bg, order = c(3,0,1), seasonal = list(order = c(1, 0, 0), period = 9),
            xreg=xts_data_30_minute_filled[,c('acting_carbs', 'log_total_insulin_burndown', 'log_steps_sum_past_1hour')],
            lambda = find_lambda)

Acf(full_model$residuals, na.action = na.pass, lag.max = 24)
Pacf(full_model$residuals, na.action = na.pass, lag.max = 24)
coeftest(full_model)
Box.test(full_model$residuals, lag=24, type='Ljung', fitdf=2)

backtest_results <- backtest(full_model, xts_data_30_minute_filled$predicted_bg, round(nrow(xts_data_30_minute_filled) * .7), 1)

quantile(filter(backtest_results, ahead_n == 1)$err, c(.1,.9), na.rm = T)
hist(filter(backtest_results, ahead_n == 1)$err, 
     breaks = 30,
     main = "30 Minute Ahead Error",
     xlab = "Error")

one_ahead <- filter(backtest_results, ahead_n == 1)
one_ahead$wide_error <- as.factor(ifelse(abs(one_ahead$err) > 20, "red", "forest green"))
ggplot(aes(x = obs, y = err), data = one_ahead) +
  geom_point() +
  geom_hline(yintercept = 0) + 
  geom_hline(yintercept = quantile(one_ahead$err, .05, na.rm = T), color = 'red') +
  geom_hline(yintercept = quantile(one_ahead$err, .95, na.rm = T), color = 'red') +
  xlab("Observed BG") +
  ylab("Prediction Error") +
  ggtitle("Observed BG vs Error")

true_line <- ggplot(aes(x = time, y = obs), data = one_ahead) +
  geom_line(colour = one_ahead$wide_error) +
  ylab("Observed BG") +
  ggtitle("Observed BG and Error over Time")

error_line <- ggplot(aes(x = time, y = err), data = one_ahead) +
  geom_line(color = 'black') +
  geom_hline(yintercept = 0) +
  ylab("Prediction Error")

plot_grid(true_line, error_line, 
                    align = "v", nrow = 2, rel_heights = c(5, 3),
                    label_size = 14)

Acf(one_ahead$err, lag.max = 48, na.action = na.pass)


