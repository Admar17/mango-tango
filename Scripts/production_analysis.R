library(tidyverse)
library(lubridate)
production <- read.csv('Data/DentalIntelFiles/production.csv')
production %>% head()

top_80_patients <- production %>% 
  filter(Status == 'Completed') %>% 
  group_by(Patient) %>% 
  summarise(
    totalDollarsCompleted = sum(DollarsCompleted),
    n(),
    ARPP = totalDollarsCompleted/n()
    
  ) %>% 
  arrange(desc(totalDollarsCompleted)) %>% 
  head(80)

production %>% mutate(
  top_80 = if_else(Patient %in% top_80_patients$Patient, 1,0)
) %>% 
  group_by(top_80) %>% 
  summarise(
    sum(DollarsCompleted)/sum(production$DollarsCompleted)
    )

### Production Timeseries Analysis

production_by_month <- production %>% 
  mutate(BeginningOfMonth = floor_date(mdy(AppointmentDate),'month')) %>% 
  group_by(BeginningOfMonth) %>% 
  summarise(totalProcedures = n()) 

ggplot(production_by_month)+
  geom_line(aes(x = BeginningOfMonth, y=totalProcedures))

production_by_month_ts <- ts(production_by_month$totalProcedures,start = c(2017,6), frequency = 12)
production_by_month_ts_decompose <- decompose(production_by_month_ts)
plot(production_by_month_ts_decompose)
