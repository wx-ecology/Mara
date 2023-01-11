# now making some initial visualization!
library(tidyverse)
library(sf)
library(hrbrthemes)
library(lubridate)
library(viridis)
library(cowplot)
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

# which animals are the most prevalent? ----- 
animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Yr_Mo, Species) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ggplot(aes(x = Yr_Mo, y = 1, size = sqrt(Count))) +
  geom_point(color = "orange", alpha = 0.6) +
  facet_grid(facets = "Species") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_blank())

animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Yr_Mo, Species) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ungroup() %>% 
  ggplot(aes(x = ym(Yr_Mo), y = sqrt(Count))) +
  geom_line(alpha = 0.6) +
  geom_point(color = "orange", alpha = 0.8, size = 2) +
  facet_grid(facets = "Species") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(axis.text.y = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x = element_blank())

animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Species) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ggplot(aes(x = as.factor(Species), y = Count)) +  
  geom_segment( aes(x=Species, xend=Species, y=0, yend=Count), color="grey") +
  geom_point( color="orange", size=4) +
  theme_classic(base_size = 16) +
  coord_flip() +
  scale_x_discrete(limits=rev) +
  theme(
    panel.grid.major.y = element_blank(),
    panel.border = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)
  )

# spatial temporal trend of animal occupancy and percentage grazed ----- 
p1 <- animal  %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>% 
  group_by(Site, Yr_Mo) %>%
  summarise(Count = sum(Count, na.rm = T)) %>%   ### <------ sum or mean show very similar pattern
  ggplot(aes(x = Yr_Mo, y = Site, fill = Count)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")  +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  ggtitle ("total dung count")

p2 <- animal  %>%
  group_by(Site, Yr_Mo) %>%
  summarise(Mean_Percent_Grazed = mean(Percent_Grazed, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = Mean_Percent_Grazed)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "orange") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  ggtitle ("avergae percentage grazed")

plot_grid(p1, p2)

# spatial temporal trend of animal occupancy by species ----------
animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Yr_Mo, Site, Species) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%   ### <------ sum and mean shows very similar patterns
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Count))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")  +
  facet_wrap(facets = "Species", nrow = 4) +
  theme_ipsum() +
  theme(axis.text.x = element_blank(),
        legend.position="bottom") 

# spatial temporal trend of animal occupancy by animal groups ----------


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

animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Thompsons_Gazelle))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red") +
  facet_grid(facets = "Transect") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

animal  %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Topi))) +
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
