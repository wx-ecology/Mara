pal12 <- c( "#fdcce5", "#bd7ebe",  "#BF7E7E", "#ffb55a",     "#ffee65",     "#beb9db",   "#b2e061",   "#FEFEE3",    "#E9C09B",   "#7eb0d5",   "#8bd3c7" , "#fd7f6f" )

#         "Buffalo" ,  "Dikdik"  ,"Eland"  ,"Elephant", "Grants_Gazelle",  "Impala",  "Thompsons_Gazelle", "Topi",   "Waterbuck", "Wildebeest", "Zebra"  ,  "Cattle" 

library(tidyverse)
library(ggplot2)
library(lubridate)
library(waffle)
library(bbplot)
### figure - waffle plot showing animal composition over time overlapped with precipitation ###

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% select(Year, Month, Transect, Site, pr)

grass <- read_csv("./data/cleaned__grass__data.csv") %>% select(Year, Month, Transect, Site, Avg_Height)

data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = as.factor(format_ISO8601(Date, precision = "ym"))) %>%
  arrange(Year, Month) %>%
  left_join(gee_data, by = c("Year", "Month", "Transect", "Site")) %>%
  left_join(grass, by = c("Year", "Month", "Transect", "Site"))

data.waffle <- data %>% 
  pivot_longer(cols = Cattle:Ostrich, names_to = "Species", values_to = "Count") %>%
  filter(!Species %in% c("Giraffe", "Sheep_Goats", "Ostrich")) %>%
  group_by(Yr_Mo, Species) %>%
  summarise(Count = sum(Count, na.rm = T)) %>%
  mutate(
    Species = factor(Species, levels = c( "Buffalo" ,  "Dikdik"  ,"Eland"  ,
                                               "Elephant", "Grants_Gazelle",  "Impala",  "Thompsons_Gazelle", 
                                               "Topi",   "Waterbuck", "Wildebeest", "Zebra",  "Cattle" )))


##########################################################################################
############# waffle chart ######################################################
##########################################################################################
# p.waffle <- data.waffle %>%
#   ggplot(aes(fill = Species, values = Count)) +
#   geom_waffle(color = "white", size = 0.03, nrow = 50, flip = TRUE) +
#   facet_wrap(~ Yr_Mo, nrow = 1, strip.position = "bottom") + 
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   guides(col = guide_legend(nrow = 2, byrow = TRUE)) +
#   theme (axis.text.x = element_blank(),
#          axis.text = element_text(size = 15),
#          legend.text = element_text(size = 15, color = "grey40"),
#          legend.box.margin = margin(t = 30), 
#          strip.text = element_blank())
## the waffle chart looks very similar to the stack plot so decided just use the stack plot  


##########################################################################################
############# stack bar plot + precipitation ######################################################
##########################################################################################
data.pr <- data %>% select(Yr_Mo, pr) %>% distinct()
coeff = 1/24
p_count_pr <-  ggplot() +
  geom_bar(data = data.waffle %>% mutate(Yr_Mo = ym (Yr_Mo)), aes(fill = Species, y = Count, x = Yr_Mo),
           color = "white", size = 0.1, position = "stack", stat="identity") +
    geom_line(data = data.pr %>% mutate(Yr_Mo = ym (Yr_Mo)), aes(x = Yr_Mo, y = pr / coeff), color = "#6B8BA4", size = 1.5, alpha = 0.7) +
    scale_y_continuous(
      name = "Dung count",
      sec.axis = sec_axis(~.*coeff, name="Precipitation (mm)")
    ) +
  bbplot::bbc_style()  +
  scale_fill_manual(values = pal12) +
  theme (axis.text.x = element_blank(),
         axis.text = element_text(size = 14),
         axis.title.y = element_text(size=16,
                                   color="#222222"),
         legend.text = element_text(size = 14, color = "grey40"),
         legend.box.margin = margin(t = 10),
         legend.background = element_rect(
           color = "grey40", 
           fill = "grey95",
           size = .3
         ),
         strip.text = element_blank()) +
  guides(fill = guide_legend(ncol = 6))
# ggsave("./figures/count_precipitation.png", p_count_pr, 
#         width = 20, height = 8, device = ragg::agg_png)


##########################################################################################
############# stack bar plot + biomass ######################################################
##########################################################################################
data.grass <- data %>% select(Yr_Mo, Transect, Site, Avg_Height) %>%
  group_by(Yr_Mo)
