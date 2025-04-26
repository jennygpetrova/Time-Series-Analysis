library(readxl)
library(tseries)
library(quantmod) 
library(vars)
library(urca)
library(tsDyn)
library(forecast)

# Problem 5
data <- read_excel("/Users/jennypetrova/Desktop/Time Series Analysis/Codes/quarterly.xls")
head(data)

# 5(a)
rgdp_ts  <- ts(data$RGDP,  start = c(1960, 1), frequency = 4)
unemp_ts <- ts(data$Unemp, start = c(1960, 1), frequency = 4)

plot(rgdp_ts,  main = "Real GDP (Levels)")
plot(unemp_ts, main = "Unemployment (Levels)")

lgdp <- diff(log(rgdp_ts)) * 100
ur <- diff(unemp_ts)

plot(lgdp,  main = "Log-Differenced RGDP (in %)")
plot(ur,    main = "Differenced Unemployment")

acf(lgdp, main = "ACF of LGDP")
acf(ur,   main = "ACF of UR")

# 5 (b)
# Merge data
merged_data <- cbind(lgdp, ur)
colnames(merged_data) <- c("lgdp", "ur")
head(merged_data)
plot(merged_data, main = "LGDP and UR Combined", plot.type = "single", col = 1:2)
legend("topright", legend = c("LGDP","UR"), col = 1:2, lty = 1)

# VAR
y <- window(merged_data, start = c(1973, 1), end = c(1999, 4))
plot(y)
VARselect(y, lag.max = 8, type = "const")
VAR_fit <- VAR(ts(y), p=2, type="const")
summary(VAR_fit)
roots(VAR_fit, modulus=FALSE)
roots(VAR_fit) 

res <- serial.test(VAR_fit, lags.pt=8, type="PT.adjusted")
res
plot(res)
plot(res, names="lgdp")
plot(res, names="ur")

# 5 (c)
# Forecasting
(fcast <- predict(VAR_fit, n.ahead=8))
plot(fcast)
fanchart(fcast)

# 5 (d)
# Granger causality
causality(VAR_fit, cause="lgdp")
causality(VAR_fit, cause="ur")

# 5 (e)
# Impulse response function
irf_gdp <- irf(VAR_fit, impulse="ur", response="lgdp", n.ahead=20)
plot(irf_gdp, ylab="lgdp")
irf_spread <- irf(VAR_fit, impulse="lgdp", response="ur", n.ahead=20)
plot(irf_spread, ylab="ur")
plot(irf(VAR_fit)) 

# 5(f)
# FEVD
fevd_result <- fevd(VAR_fit, n.ahead=12)
fevd_result
plot(fevd_result)

# Problem 6
# Define symbols
start_date <- "1981-01-01"
end_date   <- "2024-12-31"
symbols <- c("TB3MS", "TB6MS", "GS1", "GS5", "GS10")

# Retrieve data from FRED
getSymbols(symbols, src = "FRED", from = start_date, to = end_date)

# Merge data
rates <- merge(TB3MS, TB6MS, GS1, GS5, GS10)
colnames(rates) <- c("TB3MS", "TB6MS", "GS1", "GS5", "GS10")
head(rates)
tail(rates)

# Optimal Lag
lag_sel <- VARselect(rates, lag.max = 12, type = "const")
lag_sel$selection  

# Johansen Trace Test
ctest_tr <- ca.jo(rates, type  = "trace", ecdet = "none", K = 2, 
                      spec = "transitory", season = 4)
summary(ctest_tr)

# Johansen Maximum Eigenvalue Test
ctest_eig <- ca.jo(rates, type  = "eigen", ecdet = "none", K = 2, 
                      spec = "transitory", season = 4)
summary(ctest_eig)

VECM_model <- VECM(rates, lag=1, r=3, estim = "ML", LRinclude = "none")
summary(VECM_model)


