library(dplyr)
library(readr)
library(lubridate)

sensor <- read_csv("~/Downloads/SensorOutput/data.csv")
sensor %>%
  rename("temp_c" = "http://mmisw.org/ont/cf/parameter/air_temperature (degree_Celsius)") %>% 
  mutate(month = month(date_time),
         day = day(date_time)) %>% 
  filter(month == 4) %>% 
  group_by(day) %>% 
  summarize(meandaily = mean(temp_c)) %>%
  summarize(mean(meandaily))
