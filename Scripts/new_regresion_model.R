library(ggplot2)
library(dplyr)
library(tidyverse)
library(lubridate)
library(forecast)
library(tseries)

# Note: The Tutorial Uses Different Test Data

all_vists <- read.csv('../Data/DentalIntelFiles/DentalIntelFiles_all_vists.csv')

all_vists_monthly <- all_vists %>% 
  mutate(
    startOfMonth = floor_date(as_date(mdy_hm(AppointmentDate, tz = NULL)), 'month') 
    # Floors each date to the beginning of the month and adds a new column.
  ) %>% 
  group_by(
    startOfMonth # Groups each entity by the start date of the month
  ) %>% 
  summarise(Appointments = n()) # Aggregates all appointments

# all_vists_monthly$month <- month(all_vists_monthly$startOfMonth, label = TRUE) 

all_vists_monthly$COVID_month <- if_else(all_vists_monthly$startOfMonth >= ymd('2020-02-01') & all_vists_monthly$startOfMonth <= ymd('2020-05-01'),1,0)
all_vists_monthly$IsWinter <- if_else(month(all_vists_monthly$startOfMonth) %in% c(12,1,2),1,0)
all_vists_monthly$IsDec <- if_else(month(all_vists_monthly$startOfMonth) %in% c(12),1,0) 
all_vists_monthly$IsJan <- if_else(month(all_vists_monthly$startOfMonth) %in% c(1),1,0) 
all_vists_monthly$IsFeb <- if_else(month(all_vists_monthly$startOfMonth) %in% c(2),1,0) 

all_vists_monthly$IsSpring <- if_else(month(all_vists_monthly$startOfMonth) %in% c(3,4,5),1,0)
all_vists_monthly$IsMar <- if_else(month(all_vists_monthly$startOfMonth) %in% c(3),1,0) 
all_vists_monthly$IsApr <- if_else(month(all_vists_monthly$startOfMonth) %in% c(4),1,0) 
all_vists_monthly$IsMay <- if_else(month(all_vists_monthly$startOfMonth) %in% c(5),1,0) 

all_vists_monthly$IsSummer <- if_else(month(all_vists_monthly$startOfMonth) %in% c(6,7,8),1,0)
all_vists_monthly$IsJun <- if_else(month(all_vists_monthly$startOfMonth) %in% c(6),1,0) 
all_vists_monthly$IsJul <- if_else(month(all_vists_monthly$startOfMonth) %in% c(7),1,0) 
all_vists_monthly$IsAug <- if_else(month(all_vists_monthly$startOfMonth) %in% c(8),1,0) 

all_vists_monthly$IsFall <- if_else(month(all_vists_monthly$startOfMonth) %in% c(9,10,11),1,0)
all_vists_monthly$IsSep <- if_else(month(all_vists_monthly$startOfMonth) %in% c(9),1,0) 
all_vists_monthly$IsOct <- if_else(month(all_vists_monthly$startOfMonth) %in% c(10),1,0) 
all_vists_monthly$IsNov <- if_else(month(all_vists_monthly$startOfMonth) %in% c(11),1,0) 


all_vists_monthly$IsSchoolSession <- if_else(month(all_vists_monthly$startOfMonth) %in% c(1:5,9:12),1,0)
all_vists_monthly$IsNotSchoolSession <- if_else(month(all_vists_monthly$startOfMonth) %in% c(1:5,9:12),0,1) 



COVID_month_dummies <- model.matrix(~0 + COVID_month, data = all_vists_monthly)

### Creating Test Train Split

x <- all_vists_monthly[-2]
y <- all_vists_monthly$Appointments

nTotal <- length(y)
nValid <- floor(.20*nTotal)
nTrain <- nTotal - nValid

xTrain <- x[1:nTrain, ]
yTrain <- y[1:nTrain]
xValid <- x[(nTrain+1):nTotal, ]
yValid <- y[(nTrain+1):nTotal]

yTrain.ts <- ts(yTrain, start = c(2017,6), frequency = 12)



vists.tslm.2 <-tslm(yTrain.ts ~ I(trend**2) + COVID_month + IsWinter, data = xTrain)
summary(vists.tslm.2)
vists.tslm.2.pred <- forecast(vists.tslm.2, newdata = xValid)

plot(vists.tslm.2.pred)
lines(fitted(vists.tslm.2))
