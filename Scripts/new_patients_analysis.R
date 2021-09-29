library(tidyverse)
library(lubridate)
new_patients <- read.csv('Data/DentalIntelFiles/new_patients.csv')

new_patients %>% head()

new_patients$IsInsured <- if_else(str_length(str_trim(new_patients$Insurance)) <= 3 ,0,1 )

new_patients_dated <- new_patients %>% 
  mutate(
    AppointmentDate2 = as_date(mdy_hm(AppointmentDate, tz = NULL)),
    DayOfWeek = wday(AppointmentDate2, label = TRUE),
    StartOfMonth = floor_date(AppointmentDate2, unit = 'month'),
    Day = day(AppointmentDate2),
    Month = month(AppointmentDate2, label = TRUE),
    Year = year(AppointmentDate2),
    HasZimbiDentalPlan = str_detect(Insurance, "Zimbi")
  )
sum(new_patients_dated$HasZimbiDentalPlan)

# new_patients_dated$HasZimbiDentalPlan

new_patients_dated %>% 
  ggplot()+
  geom_line(aes(x = StartOfMonth, y = sum(count),group = HasZimbiDentalPlan, color = as.factor(HasZimbiDentalPlan)))

new_patients_dated %>% 
  ggplot()+
  geom_line(aes(x = StartOfMonth, group = Source, color = as.factor(Source)), stat = 'count')

new_patients_dated$Source <- if_else(str_length(str_trim(new_patients_dated$Source)) >= 2, new_patients_dated$Source,NULL)
new_patients_dated$PersonToPersonSource <- str_detect(new_patients_dated$Source, ",")

new_patients_dated %>% 
  # filter(!is.na(PersonToPersonSource)) %>% 
  ggplot()+
  geom_line(aes(x = StartOfMonth, group = PersonToPersonSource, color = PersonToPersonSource), stat = 'count')+
  geom_point(aes(x = StartOfMonth, group = PersonToPersonSource, color = PersonToPersonSource), stat = 'count')


### Time Series Formatting

new_patients_dated %>% 
  group_by(StartOfMonth) %>% 
  summarise(new_patients_in_month = n()) %>% 
  ggplot()+
  geom_line(aes(x = StartOfMonth, y = cumsum(new_patients_in_month)))

ts_data_new_patients <- new_patients_dated %>% 
  group_by(StartOfMonth) %>% 
  summarise(new_patients_in_month = n())


ts_data_new_patients_2 <- ts((ts_data_new_patients$new_patients_in_month), start =c(2017,6), frequency = 12)

components_ts <- decompose(ts_data_new_patients_2)

plot(components_ts)

acf(log(ts_data_new_patients_2), type = 'correlation')
boxCox(ts_data_new_patients_2)
