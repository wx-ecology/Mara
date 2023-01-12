
############################################
########## explore grass sheet #############
############################################
grass <- read_csv("./data/cleaned_grass_data.csv")

# grass %>% mutate(Month = as.factor(Month)) %>%
#   ggplot(aes( x = Month, y = Avg_Height)) +
#   geom_boxplot() +
#   theme_ipsum() +
#   facet_wrap("Year")
# 
# grass %>% 
#   mutate(Year = as.factor(Year)) %>% 
#   ggplot(aes( x = Year, y = Avg_Height)) + 
#   geom_boxplot() + 
#   theme_ipsum()
#
# grass %>% 
# #  mutate(Site = as.factor(Site)) %>% 
#   ggplot(aes( x = Transect, y = Avg_Height)) + 
#   geom_boxplot() + 
#   facet_wrap("Year") +
#   theme_ipsum()
# 
# grass %>% 
#   mutate(Site = as.factor(Site)) %>% 
#   ggplot(aes( x = Site, y = Avg_Height)) + 
#   geom_boxplot() + 
#   facet_wrap("Year") +
#   theme_ipsum()

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
  geom_violin() + 
  facet_wrap("Year") +
  theme_ipsum()

grass %>% 
  mutate(Site = as.factor(Site)) %>% 
  ggplot(aes( x = Site, y = Frame_Height)) + 
  geom_violin() + 
  facet_wrap("Year") +
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
