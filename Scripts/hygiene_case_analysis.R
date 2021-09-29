library(tidyverse)
library(lubridate)

hygiene_case <- read.csv('Data/DentalIntelFiles/hygiene_case.csv',na.strings=c(""," ","NA"))

hygiene_case <- hygiene_case %>% 
  mutate(
    Fee = as.numeric(str_remove(str_trim(Fee),'\\$'))
  ) 




4## Basic Time Series Analysis

monthly_hygiene_case <- hygiene_case %>% 
  mutate(BeginningOfMonth = floor_date(mdy(Presented),'month')) %>% 
  group_by(BeginningOfMonth) %>% 
  summarise(Monthy = length(unique(Patient))) 


monthly_hygiene_case_ts <- ts(monthly_hygiene_case$Monthy, start = c(2017,6), frequency = 12)  

monthly_hygiene_case_ts_decompose <- decompose(monthly_hygiene_case_ts)
plot(monthly_hygiene_case_ts_decompose)


## Average Revenue Per User
hygiene_case_base_metrics <- hygiene_case %>% 
  group_by(Patient) %>% 
  summarise(TotalFees = sum(Fee, na.rm = TRUE),
            AverageFee = mean(Fee, na.rm = TRUE),
            TotalProdcedures = n(),
            FirstProcedure = min(mdy(Presented)),
            MostRecentProcedure = max(mdy(Presented)),
            DateDiff = (MostRecentProcedure - FirstProcedure),
            AverageDaysBetweenAppointment = (MostRecentProcedure - FirstProcedure)/TotalProdcedures,
            logTotalFee = log(TotalFees),
            logAvgFee = log(AverageFee)
            
            )

## 
hist(hygiene_case_base_metrics$AverageFee)


hygiene_case_base_metrics2 <- hygiene_case_base_metrics %>% 
  filter(!is.infinite(logTotalFee))

Fees.lm <- lm(logTotalFee ~ DateDiff, data = hygiene_case_base_metrics2)

summary(Fees.lm)

plot(log(TotalFees) ~ DateDiff, data = hygiene_case_base_metrics2)

## Find Top 10 Procedures And Fees

hygiene_case_cost_df <- hygiene_case %>% 
  group_by(Procedures) %>% 
  summarise(
    avg_cost = mean(Fee, na.rm = TRUE),
    max_cost = max(Fee, na.rm = TRUE),
    min_cost = min(Fee, na.rm = TRUE),
    count = sum(if_else(!is.na(Fee),1,0))
    ) %>% 
  arrange(desc(count)) %>% 
  filter(!is.na(Procedures))

summary(hygiene_case$Fee)

hygiene_case %>% 
  filter(Procedures %in% head(hygiene_case_cost_df$Procedures,5)) %>% 
  # sample_n(size = 30) %>% 
  ggplot()+
  geom_boxplot(aes(x = Procedures, y = log(Fee), color = Procedures),outlier.shape=NA, fill =NA)+
  geom_jitter(aes(x = Procedures, y = log(Fee), color = Procedures), alpha = 0.15,width = 2.0)
