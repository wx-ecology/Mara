#  ----------  read library ----------# 
library(ggplot2)
library(tidyverse)
library(Hmsc)
library(bbplot)
library(cowplot)

prediction <- readRDS("./results/Hmsc_prediction.RDS")
pred_dry <- prediction$predY_dry
pred_wet <- prediction$predY_wet

## --- function turn HMSC prediction result list to data frame ---- ###
predlist.to.df <- function(pred.list) {
  pred.list <- lapply(pred.list, as.data.frame)
  pred.df <- lapply(pred.list, function(x) {x <- x %>% mutate (dist = row_number())})   # add a column to be distance
  predY.df <-   dplyr::bind_rows(pred.df, .id = "index")
  rownames(predY.df) <- NULL
  return(predY.df)
}

p <- c(.05,.5,.95) 
p_names <- paste0(p*100)
p_funs <- map(p, ~partial(quantile, probs = .x, na.rm = TRUE)) %>%
  set_names(nm = p_names)

# obtain distance gradient # 
xx = seq(from = 1, to = 12, length = 20)
xx.df <- data.frame(xx = xx) %>% 
  mutate(dist = row_number())

# create prediction dataframe 
pred.df <- predlist.to.df(pred_dry) %>%  left_join(., xx.df)

## ------- create ribbom plots of all speices abundace in wet time ------ ##
# get CIs
pred.CI <- pred.df %>%
  pivot_longer(2:13, "species", "value") %>%
  select(-dist, -index) %>%
  mutate(across(everything(),type.convert)) %>% 
  group_by(xx, species) %>%
  summarise(across(everything(),p_funs)) 

pred.CI %>% ggplot (aes (x = xx, y = value_50, group = species, color = species, fill = species)) +
  geom_ribbon(aes( ymin = value_5, ymax = value_95), alpha = 0.2)

# pred.CI %>%
#   #filter(species != "Wildebeest") %>% 
#   ggplot (aes (x = xx, y = value_50, group = species)) +
#   geom_ribbon(aes( ymin = value_5, ymax = value_95, fill = species), alpha = 0.2) 

## ------- create line plots of catle, wildebeest, and zebra in dry time ------ ##
#  5% - 95% quantiles of draws 
good.index.cattle <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Cattle, index) %>%
  filter(Cattle >=quantile(Cattle, 0.05) & Cattle <= quantile(Cattle, 0.95)) %>%
  pull(index)

good.index.wildebeest <-   pred.df %>% 
  filter(dist == 1) %>%
  select(Wildebeest, index) %>%
  filter(Wildebeest >= quantile(Wildebeest, 0.05) & Wildebeest <= quantile(Wildebeest, 0.95)) %>%
  pull(index)

good.index.zebra <-   pred.df %>% 
  filter(dist == 1) %>%
  select(Zebra, index) %>%
  filter(Zebra >= quantile(Zebra, 0.05) & Zebra <= quantile(Zebra, 0.95)) %>%
  pull(index)

p.c <- pred.df %>% 
  select(Cattle, index, xx) %>%
  filter(index %in% good.index.cattle) %>%
  ggplot(aes(x = xx, y = Cattle, group = index)) +
  geom_line(alpha = 0.1, color = "#fd7f6f")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Cattle") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))
  
p.w <- pred.df %>% 
  select(Wildebeest, index, xx) %>%
  filter(index %in% good.index.wildebeest) %>%
  ggplot(aes(x = xx, y = Wildebeest, group = index)) +
  geom_line(alpha = 0.1, color = "#7eb0d5")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Wildebeest") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                      color="#222222"),
         plot.subtitle = element_text(size = 18))
 
p.z <- pred.df %>% 
  select(Zebra, index, xx) %>%
  filter(index %in% good.index.zebra) %>%
  ggplot(aes(x = xx, y = Zebra, group = index)) +
  geom_line(alpha = 0.1, color =  "#8bd3c7") +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Zebra") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p <- plot_grid(p.c, p.z, p.w, nrow = 1)

ggsave("./figures/materials/HMSC_prediction_3spp.png", p, 
       width = 20, height = 4, device = ragg::agg_png)


### ==================== other species ================= ##
#  5% - 95% quantiles of draws 
good.index.tgazelle <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Thompsons_Gazelle, index) %>%
  filter(Thompsons_Gazelle >=quantile(Thompsons_Gazelle, 0.05) & Thompsons_Gazelle <= quantile(Thompsons_Gazelle, 0.95)) %>%
  pull(index)

good.index.topi <-   pred.df %>% 
  filter(dist == 1) %>%
  select(Topi, index) %>%
  filter(Topi >= quantile(Topi, 0.05) & Topi <= quantile(Topi, 0.95)) %>%
  pull(index)

good.index.eland <-   pred.df %>% 
  filter(dist == 1) %>%
  select(Eland, index) %>%
  filter(Eland >= quantile(Eland, 0.05) & Eland <= quantile(Eland, 0.95)) %>%
  pull(index)

good.index.ggazelle <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Grants_Gazelle, index) %>%
  filter(Grants_Gazelle >= quantile(Grants_Gazelle, 0.05) & Grants_Gazelle <= quantile(Grants_Gazelle, 0.95)) %>%
  pull(index)

good.index.buffalo <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Buffalo, index) %>%
  filter(Buffalo >=quantile(Buffalo, 0.05) & Buffalo <= quantile(Buffalo, 0.95)) %>%
  pull(index)

good.index.waterbuck <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Waterbuck, index) %>%
  filter(Waterbuck >=quantile(Waterbuck, 0.05) & Waterbuck <= quantile(Waterbuck, 0.95)) %>%
  pull(index)

good.index.dikdik <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Dikdik, index) %>%
  filter(Dikdik >=quantile(Dikdik, 0.05) & Dikdik <= quantile(Dikdik, 0.95)) %>%
  pull(index)

good.index.elephant <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Elephant, index) %>%
  filter(Elephant >=quantile(Elephant, 0.05) & Elephant <= quantile(Elephant, 0.95)) %>%
  pull(index)

good.index.impala <-  pred.df %>% 
  filter(dist == 1) %>%
  select(Impala, index) %>%
  filter(Impala >=quantile(Impala, 0.05) & Impala <= quantile(Impala, 0.95)) %>%
  pull(index)

p.tg <- pred.df %>% 
  select(Thompsons_Gazelle, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Thompsons_Gazelle, group = index)) +
  geom_line(alpha = 0.1, color = "#b2e061")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Thompsons's gazelle") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p.tp <- pred.df %>% 
  select(Topi, index, xx) %>%
  filter(index %in% good.index.topi) %>%
  ggplot(aes(x = xx, y = Topi, group = index)) +
  geom_line(alpha = 0.1, color = "#7eb0d5")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Topi") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))


p.e <- pred.df %>% 
  select(Eland, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Eland, group = index)) +
  geom_line(alpha = 0.1, color = "#BF7E7E")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Eland") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p.gg <- pred.df %>% 
  select(Grants_Gazelle, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Grants_Gazelle, group = index)) +
  geom_line(alpha = 0.1, color = "#ffee65")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Grant's gazelle") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))


p.buffalo <- pred.df %>% 
  select(Buffalo, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Buffalo, group = index)) +
  geom_line(alpha = 0.1, color = "#fdcce5")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Buffalo") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p.waterbuck <- pred.df %>% 
  select(Waterbuck, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Waterbuck, group = index)) +
  geom_line(alpha = 0.1, color = "#E9C09B")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Waterbuck") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p.dikdik <- pred.df %>% 
  select(Dikdik, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Dikdik, group = index)) +
  geom_line(alpha = 0.1, color = "#bd7ebe")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Dik dik") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p.elephant <- pred.df %>% 
  select(Elephant, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Elephant, group = index)) +
  geom_line(alpha = 0.1, color = "#ffb55a")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Elephant") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p.impala <- pred.df %>% 
  select(Impala, index, xx) %>%
  filter(index %in% good.index.tgazelle) %>%
  ggplot(aes(x = xx, y = Impala, group = index)) +
  geom_line(alpha = 0.1, color = "#beb9db")  +
  xlab("Distance to boundary (km)") +
  scale_x_continuous(breaks = c(3,6,9,12), labels = c(3,6,9,12)) +
  labs(subtitle = "Impala") + 
  bbplot::bbc_style() +
  theme (axis.text.x =  element_text(size=14),
         axis.text = element_text(size = 14),
         axis.title.x = element_text(size=16,
                                     color="#222222"),
         plot.subtitle = element_text(size = 18))

p2 <- plot_grid(p.buffalo, p.dikdik, p.e, p.elephant, p.gg, p.impala,
                p.tg, p.tp, p.waterbuck, ncol = 3)

ggsave("./figures/materials/supp_spp_prediction_9spp.png", p2, 
       width = 16, height = 16, device = ragg::agg_png) 
