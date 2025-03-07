install.packages('quantmod')
library(quantmod)
library(tseries)
library(urca)

getSymbols('GDP', src = 'FRED')

# 5(a)
# (i)
lgdp <- log(GDP)['/2019']
plot.ts(lgdp, main='Log U.S. Quarterly GDP (1947-2019)')
acf(lgdp, main='ACF of Log GDP')    

# (ii)
# ADF test
result_lgdp <- ur.df(lgdp, type = "trend", selectlags = "AIC")  
summary(result_lgdp)
plot(result_lgdp) 


# 5(b)
# (i)
gdpgr <- 100 * diff(lgdp)[-1]
plot.ts(gdpgr, main='U.S. Quarterly GDP Growth Rate') 
acf(gdpgr, main='ACF of GDP Growth Rate')     

# (ii)
# ADF test
result_gdpgr <- ur.df(gdpgr, type = "none", selectlags = "AIC")
summary(result_gdpgr)
plot(result_gdpgr) 


# 5(c)
# (i)
cpi <- getSymbols("CPALTT01USQ661S", src = "FRED", auto.assign = FALSE)
inf <- 100 * diff(log(cpi))
inf <- na.omit(inf)
plot.ts(inf, main = "U.S. Quarterly Inflation Rate")
acf(inf, main = "ACF of Inflation Rate")

# (ii)
# ADF test
adf_inf <- ur.df(inf, type = "drift", selectlags = "AIC")
summary(adf_inf)
plot(adf_inf)

# KPSS test
?kpss.test
kpss.test(inf, "Level")
?ur.kpss
kpss_inf <- ur.kpss(inf, type="mu", lags="short")
summary(kpss_inf)

# (iii)
# ADF test - 1st difference
dinf <- diff(inf)
dinf <- na.omit(dinf)
adf_dinf <- ur.df(dinf, type = "drift", selectlags = "AIC")
summary(adf_dinf)
plot(adf_dinf)

# KPSS test - 1st difference
?kpss.test
kpss.test(dinf, "Level")
?ur.kpss
kpss_dinf <- ur.kpss(dinf, type="mu", lags="short")
summary(kpss_dinf)
