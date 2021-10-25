library(tidyverse)
library(forecast)

costs <- Construction$Costs

cost_ts <- ts(costs, start = c(1990,1), frequency = 12)

cost_ts_decompose <- decompose(cost_ts)

plot(cost_ts_decompose)


## Linear Model

cost.lm <- tslm(cost_ts ~ trend)
plot(cost_ts)
lines(cost.lm$fitted.values)
summary(cost.lm)

cost.lm.expo.trend <- tslm(cost_ts ~ trend, lambda = 0)
cost.lm.expo.trend.pred <- forecast(cost.lm.expo.trend, level = 0)

cost.lm.linear.trend <- tslm(cost_ts ~ trend, lambda = 1)
cost.lm.linear.trend.pred <- forecast(cost_ts, level = 0)

plot(cost.lm.expo.trend.pred)
# axis(1)
plot(cost.lm.expo.trend.pred$fitted)

## Polyplot

cost_ts_polytrend <- tslm(cost_ts ~ trend + I(trend^2))
summary(cost_ts_polytrend)

#### Forecasting with Seasonality

## Linear Model

cost.lm <- tslm(cost_ts ~ season)
plot(cost_ts)
lines(cost.lm$fitted.values)
summary(cost.lm)

cost.lm.expo.trend <- tslm(cost_ts ~ trend, lambda = 0)
cost.lm.expo.trend.pred <- forecast(cost.lm.expo.trend, level = 0)

cost.lm.linear.trend <- tslm(cost_ts ~ trend, lambda = 1)
cost.lm.linear.trend.pred <- forecast(cost_ts, level = 0)

plot(cost.lm.expo.trend.pred)
# axis(1)
plot(cost.lm.expo.trend.pred$fitted)

## Polyplot

cost_ts_polytrend <- tslm(cost_ts ~ season + I(season^2)) # No Squared Seasonality
summary(cost_ts_polytrend)

### Seasonlity and Trend
cost_ts_polytrend_2 <- tslm(cost_ts ~ trend + season)
summary(cost_ts_polytrend_2)

#### Construction lm

c.lm <- lm(Costs ~ Lumber.price + Year + Contracts:as.factor(Year) + as.factor(Month), data = Construction)

summary(c.lm)

pairs(Construction[c(4,5:7)], panel = panel.smooth)


boxCox(c.lm)

plot(c.lm)


## 75% of the sample size
smp_size <- floor(0.75 * nrow(Construction))

## set the seed to make your partition reproducible
set.seed(123)
train_ind <- sample(seq_len(nrow(Construction)), size = smp_size)

train <- Construction[train_ind, ]
test <- Construction[-train_ind, ]

c.lm.train <- lm(Costs ~ Lumber.price + Employees  , data = train)
summary(c.lm.train)


## Correlations

c.lm <- lm(Costs ~ Lumber.price, data = Construction)

summary(c.lm)

