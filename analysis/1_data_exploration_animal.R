# now making some initial visualization!
library(tidyverse)
library(sf)
library(hrbrthemes)
library(lubridate)
library(viridis)
library(cowplot)
library(patchwork)
library(GGally)
# ----- basic info -------
master.df <- read_csv( "./data/cleaned_master_data.csv")

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
## dung count bubble graph -----
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

## dung count trend graph -----
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

## dung count stream chart ----- 
theme_update(
  plot.title = element_text(
    size = 25,
    face = "bold",
    hjust = .5,
    margin = margin(10, 0, 30, 0)
  ),
  plot.caption = element_text(
    size = 9,
    color = "grey40",
    hjust = .5,
    margin = margin(20, 0, 5, 0)
  ),
  axis.text.y = element_blank(),
  axis.title = element_blank(),
  #plot.background = element_rect(fill = "grey88", color = NA),
  panel.background = element_rect(fill = NA, color = NA),
  #panel.grid = element_blank(),
  panel.spacing.y = unit(0, "lines"),
  strip.text.y = element_blank(),
  legend.position = "bottom",
  legend.text = element_text(size = 9, color = "grey40"),
  legend.box.margin = margin(t = 30), 
  legend.background = element_rect(
    color = "grey40", 
    #fill = "grey95",
    size = .3
  ),
  legend.key.height = unit(.25, "lines"),
  legend.key.width = unit(2.5, "lines"),
  plot.margin = margin(rep(20, 4))
)


animal.stream <- animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Yr_Mo, Species) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ungroup() %>%
  mutate(
    Yr_Mo_i = match(Yr_Mo, time)
  )

g <- animal.stream %>%
  ggplot(
    aes(
      Yr_Mo_i,
      Count,
      color = Species,
      fill = Species,
    )
  )  +
  geom_hline(
    data = tibble(y = c(2043/2, 3043, 3043*1.5, 6086)),
    aes(yintercept = y),
    color = "grey22", 
    size = .5,
    linetype = "dotted"
  ) +
  geom_stream(
    geom = "polygon",
    type = "ridge",
    bw = 0.55,
    size = 0,
    extra_span = .2
  ) +
  scale_fill_manual(
    expand = c(0, 0),
    values = lighten(paletteer_d("rcartocolor::Prism"), .15, space = "HLS")
  )  +
  theme (axis.text.x = element_blank())

# ggsave("./figures/total-count-streamplot1.png", g, 
#        width = 16, height = 10, device = ragg::agg_png)

animal.stream2 <- animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Yr_Mo, Species, Site) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ungroup() %>%
  mutate(
    Yr_Mo_i = match(Yr_Mo, time)
  )

animal.stream2 %>%
  ggplot(
    aes(
      Yr_Mo_i,
      Count,
      color = Species,
      fill = Species,
    )
  ) +
  geom_stream(
    geom = "polygon",
    type = "ridge",
    bw = 0.55,
    size = 0,
    extra_span = .2
  ) +
  scale_fill_manual(
    expand = c(0, 0),
    values = lighten(paletteer_d("rcartocolor::Prism"), .15, space = "HLS")
  )  +
  facet_grid(Site ~ ., 
             scales = "free_y", 
             space = "free") + 
  theme (axis.text.x = element_blank())

# total dung by species lollipop -------
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

p1/p2

# spatial temporal trend of animal occupancy by species ----------
animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  group_by(Yr_Mo, Site, Species) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%   ### <------ sum and mean shows very similar patterns
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Count))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")  +
  facet_wrap(facets = "Species", ncol = 3) +
  theme_ipsum() +
  theme(axis.text.x = element_blank(),
        legend.position="bottom") 

# spatial temporal trend of animal occupancy by animal groups ----------
animal %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  mutate(Species_group = case_when(Species == "Elephant" ~ "Mega",
                                    Species == "Cattle" ~ "Livestock",
                                    Species %in% c("Dikdik", "Thompsons_Gazelle") ~ "Small", 
                                    Species %in% c("Buffalo", "Eland", "Grants_Gazelle", "Impala", "Topi", "Waterbuck", "Wildebeest", "Zebra") ~ "Medium"
                                    )) %>%
  group_by(Yr_Mo, Site, Species_group) %>% 
  summarise(Count = sum(Count, na.rm = T)) %>%   ### <------ sum and mean shows very similar patterns
  ggplot(aes(x = Yr_Mo, y = Site, fill = sqrt(Count))) +
  geom_tile() +
  scale_fill_gradient(low = "white", high = "red")  +
  facet_wrap(facets = "Species_group", nrow = 2) +
  theme_ipsum() +
  theme(axis.text.x = element_blank(),
        legend.position="bottom")    ## < --- lost variations among different "medium-sized" animals


# simple correlation among animal species ----------
animal %>% select(Percent_Grazed:Ostrich) %>%
  select(-Giraffe, -Sheep_Goats, -Ostrich) %>%
  ggcorr(., method = c("complete.obs", "spearman"))

# # try out overlap package ---- 
# library(overlap)
# wildebeest<- animal %>% select(Transect, Site, Year, Month, Wildebeest) %>%
#   mutate(Time = Month/12 * 2 * pi) %>% 
#   filter(Wildebeest != 0,
#          Year == 2019)
# w.2019 <- wildebeest[rep(seq_len(dim(wildebeest)[1]), wildebeest$Wildebeest), 6]
# p.2019 <- densityPlot(w.2019, xscale = 12)
# 
# cattle <- animal %>% select(Transect, Site, Year, Month, Cattle) %>%
#   mutate(Time = Month/12 * 2 * pi) %>% 
#   filter(Cattle != 0,
#          Year == 2019)
# c.2019 <- cattle[rep(seq_len(dim(cattle)[1]), cattle$Cattle), 6]$Time
# p.2019 <- densityPlot(c.2019, xscale = 12, add = T)   # <--- did not work well because of the disceret nature of our data structure


# Gaussian copula graphical models to quantify conditional species association ----------
library("ecoCopula")
