library(tidyverse)
library(sf)
library(hrbrthemes)
library(lubridate)
library(viridis)
library(cowplot)
library(patchwork)
library(GGally)

############################################
########## explore grass sheet #############
############################################
grass <- read_csv("./data/cleaned__grass__data.csv")

# corregram
 grass %>%
  mutate( 
    Date = ymd(Date), 
    Yr_Mo = format_ISO8601(Date, precision = "ym"),
    Site = as.factor(Site),
    Fat = as.numeric(replace (Fat, Fat == "< 0.10", 0.001)),
    Starch = as.numeric(replace (Starch, Starch %in% c("< 0.10", "< 0.1" ) , 0.001)),
    Sugar = as.numeric(replace (Sugar, Sugar == "< 0.50", 0.01)),
    Avg_Gaps = Avg_Gaps*100 
  ) %>% 
  select(Biomass, Percent_Grazed, E, Protein, Fibre, Ash, ADF, NDF, NCGD, Avg_Gaps, Avg_Height) %>%
   filter(Biomass < 1000) %>%
  ggpairs()

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



# ---- grass height heat map --------------------
grass.height <- grass %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = format_ISO8601(Date, precision = "ym"),
          Site = as.factor(Site)) %>%
  pivot_longer(Avg_Height:Frame_Height, names_to = "feature", values_to = "value") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) 

p <- grass.height %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  facet_grid(feature~.) +
  theme (strip.text.y = element_text((size = 16)),
         axis.title.y = element_blank())

# ----- grass lab -------------------
grass.lab <- grass %>% 
  mutate( 
    Date = ymd(Date), 
    Yr_Mo = format_ISO8601(Date, precision = "ym"),
    Site = as.factor(Site),
    Fat = as.numeric(replace (Fat, Fat == "< 0.10", 0.001)),
    Starch = as.numeric(replace (Starch, Starch %in% c("< 0.10", "< 0.1" ) , 0.001)),
    Sugar = as.numeric(replace (Sugar, Sugar == "< 0.50", 0.01)),
   ) %>%
  pivot_longer(DM:NCGD, names_to = "feature", values_to = "value") 


p1 <- grass.lab %>% filter(feature == "ADF") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle("ADF")

p2 <- grass.lab %>% filter(feature == "Ash") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Ash")

p3 <- grass.lab %>% filter(feature == "Biomass") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  mutate (mean_value = replace(mean_value, mean_value > 300, NA)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Biomass")

p4 <- grass.lab %>% filter(feature == "DM") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("DM")

p5 <- grass.lab %>% filter(feature == "E") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("E")

p6 <- grass.lab %>% filter(feature == "Fat") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Fat")

p7 <- grass.lab %>% filter(feature == "Fibre") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Fibre")

p8 <- grass.lab %>% filter(feature == "NCGD") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("NCGD")

p9 <- grass.lab %>% filter(feature == "NDF") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("NDF")

p10 <- grass.lab %>% filter(feature == "Percent_Grazed") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("% grazed")

p11 <- grass.lab %>% filter(feature == "Protein") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Protein")

p12 <- grass.lab %>% filter(feature == "Starch") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Starch")

p13 <- grass.lab %>% filter(feature == "Sugar") %>%
  group_by (feature, Yr_Mo, Site) %>%
  summarise (mean_value = mean(value, na.rm = T)) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = mean_value)) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "forestgreen") +
  theme_ipsum() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
        legend.position="bottom") +
  theme (axis.title.y = element_blank(), axis.text.x = element_blank()) +
  ggtitle ("Sugar")

(p1 | p2 | p3 | p5) / (p8 | p6 | p7 | p9) / (p10 | p11 | p12 | p13)
