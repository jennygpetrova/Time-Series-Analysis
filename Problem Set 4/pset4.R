library(quantmod)
library(forecast)
library(rugarch)
library(FinTS)

jpm_full <- getSymbols("JPM", src="yahoo", auto.assign = F)
jpm <- jpm_full["2016/2019"]

# 5(a)
r <- diff(log(jpm$JPM.Close))[-1]
length(r)
plot(r, main = "Daily Return")
plot(r^2, main = "Daily Squared Return")

# 5(b)
(acf(r, main = "ACF of Return"))
(pacf(r, main = "PACF of Return"))
tsdisplay(r)
Box.test(r, lag=10, type = "Ljung-Box")
Box.test(r, lag=10, type = "Box-Pierce")
ArchTest(r, 10)

# 5(c)
# Fit a GARCH(1,1) model
spec_garch11 <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                           mean.model = list(armaOrder=c(0,0)))
fit_garch11 <- ugarchfit(spec=spec_garch11, data=r)
fit_garch11
plot(fit_garch11) 

10# 5(d)
spec_garch11std <- ugarchspec(variance.model = list(garchOrder=c(1,1)), 
                              mean.model = list(armaOrder=c(0,0)),
                              distribution.model = "std")
fit_garch11std <- ugarchfit(spec=spec_garch11std, data=r)
fit_garch11std
plot(fit_garch11std)

# 5(e)
(fcst_garch11std <- ugarchforecast(fit_garch11std, n.ahead=10))
plot(fcst_garch11std, which=1)
plot(fcst_garch11std, which=3)
