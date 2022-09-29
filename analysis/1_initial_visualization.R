# now making some initial visualization!
library(tidyverse)
library(sf)
library(hrbrthemes)
# ----- basic info -------
grass <- read_csv("./data/cleaned_grass__data.csv")

grass.sf <- grass %>% filter(!is.na(Easting), !is.na(Northing)) %>% # 59 points filtered 
  filter((Easting > 720000) & (Easting < 780000) & (Northing > 9820000) & (Northing < 9860000)) %>% # based on an envelope around Mara 
  st_as_sf(., coords = c("Easting", "Northing"))

# figures to visualize transects 
grass.sf %>% filter(Year == 2018) %>% ggplot() +
  geom_sf() +
  facet_wrap(vars(Year, Month), nrow = 2) +
  theme_minimal()

# does biomass change by transects? ------------------------- biomass needs to be fixed 
grass %>% ggplot(aes(x = Date, y = DM)) + geom_point() 

# average frame height -----
grass %>% ggplot(aes( x = Date, y = Avg_Height)) + geom_point()
grass %>% ggplot(aes( x = Date, y = Stdev_Height)) + geom_point()

grass %>% mutate(Month = as.factor(Month)) %>% 
  ggplot(aes( x = Month, y = Avg_Height)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
  mutate(Year = as.factor(Year)) %>% 
  ggplot(aes( x = Year, y = Avg_Height)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
#  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Transect, y = Avg_Height)) + 
  geom_boxplot() + 
  facet_wrap("Year") +
  theme_ipsum()

grass %>% 
  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Site, y = Avg_Height)) + 
  geom_boxplot() + 
  facet_wrap("Year") +
  theme_ipsum()

# lab frame height --------------
grass %>% mutate(Month = as.factor(Month)) %>% 
  ggplot(aes( x = Month, y = Frame_Height)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
  mutate(Year = as.factor(Year)) %>% 
  ggplot(aes( x = Year, y = Frame_Height)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
#  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Transect, y = Frame_Height)) + 
  geom_boxplot() + 
  facet_wrap("Year") +
  theme_ipsum()

grass %>% 
  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Site, y = Frame_Height)) + 
  geom_boxplot() + 
#  facet_wrap("Year") +
  theme_ipsum()



# Energy --------------
grass %>% mutate(Month = as.factor(Month)) %>% 
  ggplot(aes( x = Month, y = E)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
  mutate(Year = as.factor(Year)) %>% 
  ggplot(aes( x = Year, y = E)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
  #  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Transect, y = E)) + 
  geom_boxplot() + 
  facet_wrap("Year") +
  theme_ipsum()

grass %>% 
  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Site, y = E)) + 
  geom_boxplot() + 
 # facet_wrap("Year") +
  theme_ipsum()

# protein -------------
grass %>% mutate(Month = as.factor(Month)) %>% 
  ggplot(aes( x = Month, y = Protein)) + 
  geom_boxplot() + 
  theme_ipsum()

grass %>% 
  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Site, y = Protein)) + 
  geom_boxplot() + 
  # facet_wrap("Year") +
  theme_ipsum()
