#Loading Libraries

library(quantmod)
library(xts)
library(stats)
library(forecast)
library(moments)
library(nortest)
library(fitdistrplus)
library(tseries)

#Loading Data

getSymbols("^GSPC", from = "2009-01-01", to = "2020-12-31", 
           warnings = FALSE, auto.assign = TRUE)

class(GSPC)

p = GSPC$GSPC.Adjusted #Assigning adjusted close price to p

dt = index(GSPC, format = "%Y/%m/%d") #Assigning dates to dt

plot(dt,p)

#constructing our time series

ts = xts(p, dt)

main_title = "SP500: Historical Data"

plot(ts, main = main_title, lty = "solid")

#Constructing Log return to make data stationary

ret = quantmod::periodReturn(ts, period = "daily", type = "log")
head(ret)
main_title = "SP500: Log Returns"
plot(ret, main = main_title, lty = "solid")

summary(ret)

#Mean and Standard deviation

ret_mean <- zoo::rollmean(ret, 21, align = "right")
ret_sds <- rollapply(ret, width = 21, FUN = sd, align = "right")
lines(ret_mean, col= "red")
lines(ret_sds, col= "green")

#Histogram

hist(ret, breaks = 78, probability = T)
lines(density(ret), col=1, lwd=1.5)
curve(dnorm(x, mean(ret),sd(ret)), col="blue", lwd=3, add=T)

#Some checks on our log returns

print(paste0("Mean: ", mean(ret)))
print(paste0("Std. Dev: ", sd(ret)))
print(paste0("Skewness: ", moments::skewness(ret)))
print(paste0("Kurtosis: ", moments::kurtosis(ret)))

#Annual mean and std. Dev.
#252 working days in a year, not 365

ann_mean = mean(ret)*252
ann_sd = sd(ret)*sqrt(252)

print(paste0("Annual Mean: ", ann_mean*100, " %"))
print(paste0("Annual Std. Dev: ", ann_sd*100, " %"))

##QQ Plot for normality of data
#Solid black line shows normality, tangent line to data points means it is normal
#but start and end of the data points are not normal, except the middle area

qqnorm(ret,main = "QQ plot", pch=19)
qqline(ret)

#Some normality tests on returns
lillie.test(coredata(ret))
ad.test(as.numeric(coredata(ret)))
sf.test(ret[1:3000])

plotdist(as.numeric(coredata(ret)), histo = TRUE, breaks = 40, demp = TRUE)

fitdistrplus::descdist(as.numeric(coredata(ret)), discrete = FALSE, boot = 500, graph = FALSE)

fit_norm <- fitdist(as.numeric(coredata(ret)), "norm")
denscomp(fit_norm, breaks = 1001)

cdfcomp(fit_norm)
qqcomp(fit_norm)

plot(fit_norm)

# ACF and PACF examination

acf(ret)
pacf(ret)

# Ljung-Box test

Box.test(ret, lag = 21, type = "Ljung-Box")

acf(ret^2)

# Stationarity check 

tseries::adf.test(ret)
tseries::kpss.test(ret, null = "Trend")



# checking best order for ARIMA model

auto.arima(ret)

# Fitting ARIMA model

arimaModel <- arima(ret, order = c(2,0,0))
print(arimaModel)

# Residual examination for our model
checkresiduals(arimaModel)
