# now making some initial visualization!
library(tidyverse)
library(sf)
library(hrbrthemes)
library(lubridate)
# ----- basic info -------
master.df <- read_csv( "./data/cleaned_master_data.csv")

############################################
## check every column by plottng against date
############################################
# does biomass change by transects? 
master.df %>% ggplot(aes(x = Date, y = DM)) + geom_point()  
master.df %>% ggplot(aes( x = Date, y = Avg_Height)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Stdev_Height)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Frame_Height)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = N_Species)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Most_Common_Grass_Spp)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Most_Common_Grass_Color)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Percent_Grazed)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = N_Different_Colors)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = N_Gaps)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Avg_Gaps)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Stdev_Gaps)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Total_Gap_Size)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Cattle)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Sheep_Goats)) + geom_point()  # almost never. maybe should remove
master.df %>% ggplot(aes( x = Date, y = Wildebeest)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Zebra)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Thompsons_Gazelle)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Impala)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Topi)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Eland)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Buffalo)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Grants_Gazelle)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Waterbuck)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Dikdik)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Elephant)) + geom_point()
master.df %>% ggplot(aes( x = Date, y = Giraffe)) + geom_point() # <- alsmost never. maybe should remove
master.df %>% ggplot(aes( x = Date, y = Ostrich)) + geom_point() # should remove
master.df %>% ggplot(aes( x = Date, y = Total_N)) + geom_point() 
master.df %>% filter(Biomass < 10000) %>% ggplot(aes( x = Date, y = Biomass)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = E)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = Protein)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = Fibre)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = Fat)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = Starch)) + geom_point() # <- does not seem to be very informative
master.df %>% ggplot(aes( x = Date, y = ADF)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = NDF)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = Sugar)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = NCGD)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = total_strikes)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = max_strikes)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = ave_strikes)) + geom_point() 
master.df %>% ggplot(aes( x = Date, y = Nitrate_N)) + geom_point()  # seems more variations then Ammonium
master.df %>% ggplot(aes( x = Date, y = Ammonium)) + geom_point() 
master.df %>% ggplot(aes( x = Site, y = pH)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = EC_Salts)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = Phosphorus)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = Potassium)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = Calcium)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = Magnesium)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = Sodium)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = C.E.C)) + geom_point()
master.df %>% ggplot(aes( x = Site, y = OB)) + geom_point() # should remove.

############################################
########## explore animal sheet ############
############################################
animal <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = format_ISO8601(Date, precision = "ym"),
          Site = as.factor(Site), 
          All = Cattle + Sheep_Goats +	Wildebeest +	Zebra +	Thompsons_Gazelle +	Impala +	Topi +	Eland +
            Buffalo +	Grants_Gazelle +	Waterbuck +	Dikdik + Elephant +	Giraffe +	Ostrich)

animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(All))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  facet_grid(facets = "Transect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = Percent_Grazed)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "orange") +
  facet_grid(facets = "Transect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Cattle))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  facet_grid(facets = "Transect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Wildebeest))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  facet_grid(facets = "Transect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Zebra))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  facet_grid(facets = "Transect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

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
