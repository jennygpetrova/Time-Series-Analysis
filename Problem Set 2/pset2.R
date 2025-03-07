library(forecast)
library(quantmod)
library(tseries)
library(ggplot2)

gdp.full <- getSymbols("A191RL1Q225SBEA", src = "FRED", auto.assign = FALSE)
gdp <- gdp.full["/2019"]
(start(gdp))
(end(gdp))
gdp <- ts(gdp, frequency=4, start=c(1947,1))  
(start(gdp))
(end(gdp))

# (a)
# Plot time series
plot.ts(gdp, main="Time Series of GDP Growth Rates")

# Plot ACF
acf(gdp, main="ACF of GDP Growth Rates")


# (b)
# Estimate AR(1)
fit_ar1 <- arima(gdp, order=c(1,0,0))
(fit_ar1)
checkresiduals(fit_ar1)

# Estimate MA(1)
fit_ma1 <- arima(gdp, order=c(0,0,1))
(fit_ma1)
checkresiduals(fit_ma1)

# Estimate ARMA(1,1)
fit_arma11 <- arima(gdp, order=c(1,0,1))
(fit_arma11)
checkresiduals(fit_arma11)

# (c) Find best AR model
# Best AR model
best_ar = auto.arima(gdp, max.p=4, max.q=0, seasonal=FALSE)
best_ar$coef
checkresiduals(best_ar)

# Checking best model - AR(4)
fit_ar4 <- arima(gdp, order=c(4,0,0))
checkresiduals(fit_ar4)

# AIC
AIC(fit_ar1) 
AIC(fit_ar4)

# BIC
BIC(fit_ar1)
BIC(fit_ar4)

# PACF
(pacf(gdp, main="PACF of GDP Growth Rates"))

# (d)
# Find polynomial zeros
ar_coeffs <- coef(best_ar)[grep("ar", names(coef(best_ar)))]
roots <- polyroot(c(1, -ar_coeffs))  
print(roots)

# Compute business cycle length 
if (any(Im(roots) != 0)) {
  cycle_length <- 2 * pi / abs(Arg(roots[Im(roots) != 0]))
  print(cycle_length)
} else {
  print("No business cycles detected.")
}

autoplot(fit_ar4)

# (e)
# 4-step ahead forecast (time origin 2019Q4)
gdp_training <- window(gdp, end = c(2018,3))
gdp_test <- window(gdp, start = c(2018,4))  
ar4 <- arima(gdp_training, order=c(4,0,0))
autoplot(ar4)
auto <-auto.arima(gdp_training)
auto
fcast_ar4 <- forecast(ar4, h = 4)
autoplot(fcast_ar4) +
  autolayer(gdp, color = 1, lwd = 0.8) +
  autolayer(fcast_ar4$mean, series = "Forecasts", lwd = 0.8) + 
  theme_bw()
accuracy(fcast_ar4, gdp_test)

# 95% confidence interval bounds
lower_95 <- fcast_ar4$lower[,2]
upper_95 <- fcast_ar4$upper[,2]
print(lower_95)
print(upper_95)

# (f)
# 4-step ahead forecast (time origin 2018Q4)
gdp_training <- window(gdp, end = c(2017,4))
gdp_test <- window(gdp, start = c(2018,1))  
ar4_2 <- arima(gdp_training, order=c(4,0,0))
autoplot(ar4_2)
auto2 <-auto.arima(gdp_training)
auto2
fcast_ar4_2 <- forecast(ar4_2, h = 4)
autoplot(fcast_ar4_2) +
  autolayer(gdp, color = 1, lwd = 0.8) +
  autolayer(fcast_ar4_2$mean, series = "Forecasts", lwd = 0.8) + 
  theme_bw()
accuracy(fcast_ar4_2, gdp_test)

# 95% confidence interval bounds
lower_95_2 <- fcast_ar4_2$lower[,2]
upper_95_2 <- fcast_ar4_2$upper[,2]
print(lower_95_2)
print(upper_95_2)

