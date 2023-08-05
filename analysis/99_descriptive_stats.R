## descriptive stats discussed in the manuscript 

library(tidyverse)
library(lubridate)
library(gridExtra)

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% select(Year, Month, Transect, Site, pr)
grass <- read_csv("./data/cleaned_grass_data.csv") %>% select(Year, Month, Transect, Site, Avg_Height)
data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date)) %>%
  filter( Date < "2019-12-01") %>%
  mutate(Yr_Mo = as.factor(format_ISO8601(Date, precision = "ym"))) %>%
  arrange(Year, Month) %>%
  left_join(gee_data, by = c("Year", "Month", "Transect", "Site")) %>%
  left_join(grass, by = c("Year", "Month", "Transect", "Site")) 

#####################################################################
### table S1. All 1140 events. 
tableS1 <- data %>% select (Yr_Mo, Transect, Site, Cattle, 
                            Buffalo, Dikdik, Eland, Elephant, Grants_Gazelle, Impala,
                              Thompsons_Gazelle, Topi, Waterbuck, Wildebeest, Zebra)


#####################################################################
# events where cattle were present 
a <-data %>% filter(Cattle > 0)
# how many uniqur months cattle were inside of MMNR
unique(a$Yr_Mo) 
# what the max # of cattle presence 
a %>% filter(Cattle == max(Cattle, na.rm = T))
# max total cattle presence in MMNR in one month
a %>% group_by(Yr_Mo) %>% summarise(Cattle = sum(Cattle, na.rm = T))

# cattle cooccurence with other speices 
a1 <- a %>% mutate (wildlife = Buffalo + Dikdik + Eland + Elephant + Grants_Gazelle + Impala + 
                      Thompsons_Gazelle + Topi + Waterbuck + Wildebeest + Zebra) %>% 
  filter(wildlife > 0)
nrow(a1)/nrow(a) # % time cattle were present, other species were also found at the same site

# summarizing of all species occurence in sample events where cattle were present
a2 <- a %>% 
  group_by(Yr_Mo) %>% 
  pivot_longer(cols = c(9, 11:21), names_to = "spp", values_to = "count") %>%
  select(Yr_Mo, Transect, Site, spp, count) 

#### table S2. summary of each species that have coexisted with cattle. 
tableS2 <- a2 %>% filter(spp != "Cattle") %>%
  mutate(count = case_when(count != 0 ~ 1, 
                           TRUE ~ 0)) %>%
  group_by(spp) %>%
  summarise ( n = sum(count)) %>%
  mutate (prec_cooccur = n/nrow(a1))
# write_csv(tableS2, "./results/tables/supp_tableS2_cattle_wild_cooccur.csv")

#####################################################################
############################# figure S1 #############################
### species richness at each sampling events
richness1 <- data %>%
  group_by(Yr_Mo) %>% 
  pivot_longer(cols = c(9, 11:21), names_to = "spp", values_to = "count") %>%
  select(Yr_Mo, Transect, Site, spp, count) %>%
  mutate(count = case_when(count != 0 ~ 1, 
                           TRUE ~ 0)) %>%
  filter(count > 0) %>% # filter to species that were present 
  group_by(Yr_Mo, Transect, Site) %>%
  summarise(n = sum(count)) # calculate # of species. i.e. species richness

richness1 <- data %>% select(Yr_Mo, Transect, Site) %>% 
  distinct() %>%
  left_join(richness1) %>%
  mutate (n = case_when( is.na(n) ~ 0 , TRUE ~ n))  # nrow = 1140

#### fig S1 b, number of wildlife species at each sampling events when cattle was present 
richness2 <- a2 %>% filter(spp != "Cattle") %>%
  mutate(count = case_when(count != 0 ~ 1, 
                           TRUE ~ 0)) %>%
  filter(count> 0) %>% 
  group_by(Yr_Mo, Transect, Site) %>%
  summarise(n = sum(count))

richness2 <- a %>% select(Yr_Mo, Transect, Site) %>% 
  distinct() %>%
  left_join(richness2) %>%
  mutate (n = case_when( is.na(n) ~ 0 , TRUE ~ n)) # n = 67
summary(richness2$n)

a <- ggplot() +
  geom_density(data = richness1, aes(x = n), fill = "#e8b874",alpha = 0.4) +
  theme_minimal() +
  labs(title = "A. Wild ungulate species richness \nin all sampling events (n = 1,140)") +
  ylim (0, 0.3) +
  xlim (0, 10)
b <- ggplot() +
  geom_density(data = richness2, aes(x = n), adjust = 2, fill = "#448700", alpha = 0.4) +
  theme_minimal() +
  labs(title = "B. wild ungulate species richness \nin presence of cattle (n = 67)")+
  ylim (0, 0.3) +
  xlim (0, 10)
grid.arrange(a, b, nrow =1)

ggsave("./figures/materials/supp_fig_spp_richness.png", grid.arrange(a, b, nrow = 1),
       width = 11, height = 5, device = ragg::agg_png)


#####################################################################
# months wildebeest were present 
w <-data %>% filter(Wildebeest > 0)
# how many uniqur months wildebeest were inside of MMNR
uniquea(w$Yr_Mo) 
# what the max # of cattle presence 
w %>% filter(Wildebeest == max(Wildebeest, na.rm = T))
# max total cattle presence in MMNR in one month
w %>% group_by(Yr_Mo) %>% summarise(Wildebeest = sum(Wildebeest, na.rm = T))