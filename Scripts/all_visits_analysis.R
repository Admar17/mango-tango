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

#### Original Model

all_vists_ts <- all_vists_dated %>% 
  group_by(StartOfMonth) %>% 
  summarise(count = (n())) %>% 
  pull(count) %>% 
  ts(start = c(2017,6), frequency = 12)


all_vists_ts_decompsed <- decompose(all_vists_ts)
plot(all_vists_ts_decompsed)



#### New Model with Outlier Fix t_diff = 1
all_vists_ts_fix_1 <- all_vists_ts
all_vists_ts_fix_1[35] <- mean(all_vists_ts_fix_1[c(34,35:36)])

all_vists_ts_1_decompsed <- decompose(all_vists_ts_fix_1)
plot(all_vists_ts_1_decompsed)


#### New Model with Outlier Fix t_diff = 2
all_vists_ts_fix_2 <- all_vists_ts
all_vists_ts_fix_2[35] <- mean(all_vists_ts_fix_2[c(33:34,36:37)])

all_vists_ts_2_decompsed <- decompose(all_vists_ts_fix_2)
plot(all_vists_ts_2_decompsed)

#### New Model with Outlier Fix t_diff = 3
all_vists_ts_fix_3 <- all_vists_ts
all_vists_ts_fix_3[35] <- mean(all_vists_ts_fix_3[c(32:34,36:38)])

all_vists_ts_3_decompsed <- decompose(all_vists_ts_fix_3)
plot(all_vists_ts_3_decompsed)

#### New Model with Outlier Fix t_diff = 4
all_vists_ts_fix_4 <- all_vists_ts
all_vists_ts_fix_4[35] <- mean(all_vists_ts_fix_4[c(31:34,36:39)])

all_vists_ts_4_decompsed <- decompose(all_vists_ts_fix_4)
plot(all_vists_ts_4_decompsed)

#### New Model with Outlier Fix t_diff = 4
all_vists_ts_fix_4 <- all_vists_ts
all_vists_ts_fix_4[35] <- mean(all_vists_ts_fix_4[c(31:34,36:39)])

all_vists_ts_4_decompsed <- decompose(all_vists_ts_fix_4)
plot(all_vists_ts_4_decompsed)

#### New Model with Outlier Fix t_diff = 12
all_vists_ts_fix_12 <- all_vists_ts
all_vists_ts_fix_12[35] <- mean(all_vists_ts_fix_12[c(21:34,36:47)])

all_vists_ts_12_decompsed <- decompose(all_vists_ts_fix_12)
plot(all_vists_ts_12_decompsed)



