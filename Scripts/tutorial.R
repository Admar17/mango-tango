
# https://www.analyticsvidhya.com/blog/2015/12/complete-tutorial-time-series-modeling/
# <- Link for Time Series Tutorial
data(AirPassengers)
class(AirPassengers)
start(AirPassengers)
end(AirPassengers)
frequency(AirPassengers)
summary(AirPassengers)

## Create a Plot With A fitted line
plot(AirPassengers)
abline(reg = lm(AirPassengers ~ time(AirPassengers)))


## Print the Cycle Across Years
cycle(AirPassengers)
plot(aggregate(AirPassengers, FUN = mean))
boxplot(AirPassengers~cycle(AirPassengers))

# Where do we start?
# In the AirPassengers data we have a trend compnent which grows year by year
# The seasonal component has a cycle less than 12 months.
# The variance in the data keeps in increasing with time.

# Checking for the best lag and difference for the time series.
# Will help us determine what is the best fit/assumptions for the model.

adf.test(diff(log(AirPassengers)))
acf(AirPassengers)
acf(log(AirPassengers))
acf(diff(log(AirPassengers)))
pacf(diff(log(AirPassengers)))

## Fitting the Model

(fit <- arima(log(AirPassengers), c(0,1,1), seasonal = list(order = c(0,1,1), period = 12)))
  