## this script create a stacked bar figure of HMSC variance partitioning 
pal8 = c("#827580", "#b5aab3", "#e6dae4" , "#9FE7F5", "#429EBD", "#053F5C", "#F7AD19", "#ff7700")
pal9 = c("#827580", "#b5aab3", "#e6dae4" , "#74e3c4", "#74c7e3","#429EBD", "#053F5C", "#F7AD19", "#ff7700")

#  ----------  read library ----------# 
library(ggplot2)
library(tidyverse)

df <- readRDS("./results/Hmsc_VP.RDS")
df <- df$vals
df <- data.frame(variable = row.names(df), df) %>%
  pivot_longer(cols = 2:13, names_to = "species", values_to = "value") %>%
  mutate(variable = factor(variable, levels = c("Random: sample", "Random: month", "Random: plot",
                                                "Forage quality","Forage quantity (% grazed)", "Forage quantity (grass height)",  "Precipitation", "Season", "Distance to border"
                                  #   "Forage-quality", "Forage-quantity", "Precipitation", "Season","Distance-to-border"
                                  )),
         species = factor(species, levels = colnames(df)))


p <- ggplot(df, aes(fill = variable, y = value, x = species )) +
  geom_bar(position="fill", stat="identity") +
  bbplot::bbc_style()  + 
  scale_fill_manual(values = pal9) +
  theme (axis.text.x = element_text(size = 14),
         axis.text = element_text(size = 14),
         axis.title.y = element_blank(),
         legend.text = element_text(size = 14, color = "grey40"),
         legend.box.margin = margin(t = 10),
         legend.background = element_rect(
           color = "grey40", 
           fill = "grey95",
           size = .3
         ),
         strip.text = element_blank()) +
  guides(fill = guide_legend(nrow = 1, reverse = T)) +
  coord_flip()
ggsave("./figures/hmsc_vp.png", p,
        width = 20, height = 8, device = ragg::agg_png)
