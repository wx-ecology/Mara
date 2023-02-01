library(tidyverse)
library(ggplot2)
library(hrbrthemes)
library(lubridate)
library(GGally)
# compare the remote sensing vegetation index and the ground measured biomass
gee.data <- read_csv( "./data/mara_gee_VI_rad.csv") 
ground.data <- read_csv("./data/cleaned_grass_data.csv")

# merge dataset 
df <- ground.data %>% left_join(gee.data) %>% mutate(date = ymd(paste0(Year, "-", Month, "-01")))

df %>%
  filter(Biomass < 5000) %>%
  select(NDVI, EVI, Avg_Height, Frame_Height, DM, Biomass, 
         Percent_Grazed, E, Protein, Fibre, Ash, ADF, NDF, Total_Gap_Size, Avg_Gaps) %>%
  ggpairs()

df %>%
  ggplot(aes( x = NDVI, y = pr)) +
  geom_point()

p1 <- df %>% 
  group_by(date, Site) %>%
  summarise(NDVI = mean(NDVI)) %>%
 ggplot() +
  geom_line(aes(x = date, y = NDVI, group = Site, color = Site)) +
  theme_ipsum()

p2 <- df %>% 
  group_by(date, Site) %>%
  summarise(Avg_Height = mean(Avg_Height)) %>%
  ggplot() +
  geom_line(aes(x = date, y = Avg_Height, group = Site, color = Site)) +
  theme_ipsum()

p3 <- df %>% 
  ggplot() +
  geom_line(aes(x = date, y = pr), size = 2) +
  theme_ipsum()

p4 <-  df %>% 
  filter(Biomass < 5000) %>%
  group_by(date, Site) %>%
  summarise(Biomass = mean(Biomass)) %>%
  ggplot() +
  geom_line(aes(x = date, y = Biomass, group = Site, color = Site)) +
  theme_ipsum()
