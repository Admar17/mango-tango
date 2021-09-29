library(tidyverse)
library(lubridate)
all_vists <- read.csv('Data/DentalIntelFiles/DentalIntelFiles_all_vists.csv')

all_vists_dated <- all_vists %>% 
  mutate(
    AppointmentDate2 = as_date(mdy_hm(AppointmentDate, tz = NULL)),
    DayOfWeek = wday(AppointmentDate2, label = TRUE),
    StartOfMonth = floor_date(AppointmentDate2, unit = 'month'),
    Day = day(AppointmentDate2),
    Month = month(AppointmentDate2, label = TRUE),
    Year = year(AppointmentDate2) 
  )

all_vists_dated %>% 
  group_by(Year) %>% 
  summarise(total = n()) %>% 
  ggplot()+
  geom_histogram(aes(x = Year, y = total), stat = 'identity')


all_vists_dated %>% 
  group_by(DayOfWeek) %>% 
  summarise(total = n()) %>% 
  ggplot()+
  geom_bar(aes(x = DayOfWeek, y = total), stat = 'identity')

all_vists_dated %>% 
  group_by(StartOfMonth) %>% 
  summarise(total = n()) %>% 
  ggplot()+
  geom_line(aes(x = StartOfMonth, y = total), stat = 'identity')+
  geom_line(data = all_vists_dated %>% 
              group_by(StartOfMonth) %>% 
              summarise(unique_patients = length(unique(PatientId))), 
            aes( x =StartOfMonth, y = unique_patients), color = 'red')


all_vists_dated %>% 
  group_by(StartOfMonth) %>% 
  summarise(unique_patients = length(unique(PatientId))) %>% 
  ggplot()+
  geom_line(aes(x = StartOfMonth, y = unique_patients))


### All Visits Time Series Analysis 

all_vists_ts <- all_vists_dated %>% 
  group_by(StartOfMonth) %>% 
  summarise(count = (n())) %>% 
  pull(count) %>% 
  ts(start = c(2017,6), frequency = 12)

# all_vists_ts[35] <- mean(all_vists_ts[c(33:34,36:37)])

all_vists_ts_decompsed <- decompose(all_vists_ts)
plot(all_vists_ts_decompsed)

arima(all_vists_ts)

Box.test(all_vists_ts, lag=10, type = 'Ljung-Box')

#### Creating ACF plot

acf(x= all_vists_ts, lag.max = 10)
