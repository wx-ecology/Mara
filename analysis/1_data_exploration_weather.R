library(tidyverse)
library(hrbrthemes)
library(lubridate)
library(ggplot2)

weather_sum_all <- data.frame()
weather_all <- data.frame()

files <- list.files("./data/weather", pattern = ".csv")

for (file in files) {
  weather <- read_csv(paste0("./data/weather/", file)) %>%
    select('Date & Time', 'Temp - _', 'Rain - mm') %>%
    rename(DateTime = 'Date & Time', Temp ='Temp - _', Rain = 'Rain - mm') %>%
    mutate(DateTime = mdy_hm(DateTime, tz = "Africa/Nairobi"),
           Yr = year(DateTime), Mo = month(DateTime),
           Temp = as.numeric(Temp)) 

  weather_all <- rbind(weather, weather_all)
    
  weather_sum <- weather %>% 
    group_by(Yr, Mo) %>%
    summarise(Total_Rain = sum(Rain, na.rm = T), Ave_Temp = mean(Temp, na.rm = T))
  
  weather_sum_all <- rbind(weather_sum_all, weather_sum)
}

weather_sum_all %>% mutate(Date = paste0(Yr,  "-", Mo, "-01"), 
                           Date = ymd(Date)) %>% 
  ggplot(aes(x = Date, y = Total_Rain)) +
  geom_point() +
  theme_ipsum()

weather_sum_all %>% mutate(Date = paste0(Yr,  "-", Mo, "-01"), 
                           Date = ymd(Date)) %>% 
  ggplot(aes(x = Date, y = Ave_Temp)) +
  geom_line() +
  geom_point() +
  theme_ipsum()


## need to calculate N for each month so know whether the data is reliable. e.g. 2020 October only 1 record and the temp is abnormal.