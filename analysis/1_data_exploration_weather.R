library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(ggplot2)
library(patchwork)

weather_sum_all <- data.frame()
weather_all <- data.frame()

files <- list.files("./data/weather", pattern = ".csv")

for (file in files) {
  weather <- read_csv(paste0("./data/weather/", file)) %>%
    select('Date & Time', 'Temp - _', 'Rain - mm') %>%
    rename(DateTime = 'Date & Time', Temp ='Temp - _', Rain = 'Rain - mm') %>%
    mutate(DateTime = mdy_hm(DateTime, tz = "Africa/Nairobi"),
           Yr = year(DateTime), Mo = month(DateTime), da = day(DateTime),
           Temp = as.numeric(Temp)) %>%
    filter(DateTime >= mdy_hm("05-01-2018 00:00", tz = "Africa/Nairobi") & 
             DateTime < mdy_hm("09-01-2020 00:00", tz = "Africa/Nairobi")   )

  weather_all <- rbind(weather, weather_all)
    
  weather_sum <- weather %>% 
    group_by(Yr, Mo) %>%
    summarise(Days_Recorded = length(unique(da)),
              Total_records = n(), 
              Total_Rain = sum(Rain, na.rm = T), 
              Ave_Rain = mean(Rain, na.rm = T),
              Ave_Temp = mean(Temp, na.rm = T))
  
  weather_sum_all <- rbind(weather_sum_all, weather_sum)
}

weather_sum_all

p1 <-  weather_sum_all  %>% mutate(Date = paste0(Yr,  "-", Mo, "-01"), 
                           Date = ymd(Date)) %>% 
  ggplot(aes(x = as.factor(Mo), y = Total_Rain, group = Yr)) +
  geom_line(aes(color = as.factor(Yr))) +
  geom_point() +
  theme_ipsum()

p2 <- weather_sum_all %>% mutate(Date = paste0(Yr,  "-", Mo, "-01"), 
                           Date = ymd(Date)) %>% 
  ggplot(aes(x = Date, y = Total_records)) +
  geom_line() +
  geom_point() +
  theme_ipsum()

p1/p2

weather_sum_all %>% mutate(Date = paste0(Yr,  "-", Mo, "-01"), 
                           Date = ymd(Date)) %>% 
  ggplot(aes(x = Date, y = Ave_Temp)) +
  geom_line() +
  geom_point() +
  theme_ipsum()


## need to calculate N for each month so know whether the data is reliable. e.g. 2020 October only 1 record and the temp is abnormal.