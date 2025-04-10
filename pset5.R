library(quantmod)
library(tseries)
library(readxl)

# Problem 4
data <- read_excel('/Users/jennypetrova/Desktop/Time Series Analysis/Codes/quarterly.xls') 
rgdp_full <- ts(data$RGDP, frequency=4, start=c(1960,1))
rgdp <- window(rgdp_full, end=c(2011,4))
plot(rgdp)
unemp <- ts(data$Unemp)
plot(unemp)

# 4(a)
lgdp = diff(log(rgdp)) * 100
plot(lgdp)
ur = diff(unemp)
plot(ur)
(acf(lgdp))
(acf(ur))

# 4(b)
y <- merge(lgdp, ur)
plot(y)
VARselect(y, type="const")
VAR_fit <- VAR(ts(y), p=2, type="const") # can choose lag order by "ic=" (but  
# must set "lag.max" simultaneously)
summary(VAR_fit)
roots(VAR_fit, modulus=FALSE) # eigenvalues of the companion coefficient matrix
roots(VAR_fit) # modulus defaults to TRUE