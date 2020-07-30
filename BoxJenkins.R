### References: 
### Example 2.3, Tsay's book, pp. 56-58 (approx.)
### Downloaded GNP data from https://fred.stlouisfed.org/series/GNP
### using the library quantmod

library(tseries)
library(forecast)
library(quantmod)

## Outline
## 1. Load the dataset
## 2. Knowing your data
## 3. Define the appropriate time series
## 4. Find the appropriate model
## 5. Fit the model and perform a diagnostic checking
## 6. Forecast

## 1. Download the dataset, GNP.csv, from Blackboard and load it in RStudio
getSymbols('GNP', src='FRED')

## 2. Know your data: 
### Units: Billions of USD, seasonally adjusted
### Source: FRED website

### Frequency: quarterly (frequency = 4)
### start: Jan 1, 1947
head(GNP)
tail(GNP) #latest observations
### number of observations = length of the series
length(GNP)
### The dataset is a time series, xts() class: 
class(GNP)

## 3. Define the associated time series in logarithmic form
gnp.log <- ts(log(GNP$GNP), frequency = 4, start = c(1947,1))
plot.ts(gnp.log)
## and then compute the associated returns
gnp.rate <- diff(gnp.log)
length(gnp.rate)
tdx <- c(1:length(gnp.rate))/4+1947
plot(tdx,gnp.rate, type = "l")
plot.ts(gnp.rate)
## Add a horizontal line, at level = 0
abline(a=0, b=0, col = "blue")

## 4. Find the appropriate model
## 4.1 Determine the appropriate model: AR or MA (for the time being)
acf(gnp.rate, lag.max = 12)
## ACF shows a decreasing, possibly oscillating, pattern, 
# which is compatible with AR
pacf(gnp.rate, lag.max = 12)
## Find the order of AR(p): we may use the function stats::ar()
mm1 <- ar(gnp.rate[,1], method = "mle")
mm1$aic
## The best order, according to AIC, is p=10

## 5 Fit the chosen model
## 5.1 Fit an AR(3)
m1 <- arima(gnp.rate, order = c(3,0,0))
m1

## 5.1.1 Diagnositc checking (Adequacy of the model)
## If the model is adequate, then the residual series should 
## look like white noise.
acf(m1$residuals) # there are some autocorrelation coefficients different from zero
Box.test(m1$residuals, type = "Ljung", lag = 12,  fitdf = 3) 
#reject the null hypothsis of no joint autocorrelation
## The model does not seem adequate

## 5.2 Fit an AR(10)
m2 <- arima(gnp.rate, order = c(10,0,0))
m2

## 5.3 Diagnostic checking
acf(m2$residuals, lag.max = 25) # compatible with alpha = .05
Box.test(m2$residuals, type = "Ljung", lag = 12, fitdf = 10)
# The residuals look like white noise

## 6. Forecast
## 287*.8 approx 230
train.size <- length(gnp.rate)*.8
m.train <- Arima(gnp.rate[1:train.size], order = c(10,0,0))
in.sample <- forecast(m.train, h =length(gnp.rate)-train.size)
m.test = window(ts(gnp.rate), start = train.size+1)
plot(in.sample, type = "l")
lines(m.test, col="red")

length(gnp.rate)
## RMSE = sqrt((1/T) sum((forecast.value - actual.value))^2)
## in.sample$mean extracts the point estimates
## It gives error percentage
(1/length(gnp.rate))*sqrt((1/T)*sum((in.sample$mean - m.test))^2)


## MAPE = (1/T) sum abs[(forecast.value - actual.value)]/actual.value
## in.sample$mean extracts the point estimates
## It gives error percentage
(1/length(gnp.rate))*sum(abs((in.sample$mean - m.test)/m.test))

## MAE = (1/T) sum(abs(forecast.value - actual.value))
(1/length(gnp.rate))*sum(abs(in.sample$mean - m.test))
