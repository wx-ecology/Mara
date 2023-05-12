## make a table about cattle effect on eco function 

# ----- set up ----- # 
library(tidyverse)
library(ggplot2)
library(bbplot)
pal3 <- c( "#fd7f6f", "#7eb0d5", "#8bd3c7" )
effect <- read_csv("./results/ecosystem_effects.csv") %>%
  mutate(effect = case_when(
    effect == "cattle_use" ~ "cattle use",
    effect == "total_use" ~ "total ungulate use",
    TRUE ~ "distance to boundary"
  ), 
  effect = factor(effect, levels = c("cattle use", "total ungulate use", "distance to boundary")) ,
  response_variable = case_when(
    response_variable == "grass_height" ~ "vegetation quantity (grass height)",
    response_variable == "NDVI" ~ "vegetation quantity (NDVI)",
    response_variable ==  "crude_protein" ~ "vegetation quality (crude protein)", 
    response_variable == "soil_N" ~ "soil quality (soil N)"
  ),
  response_variable = factor(response_variable, levels = c("vegetation quantity (grass height)", 
                                                           "vegetation quantity (NDVI)",
                                                           "vegetation quality (crude protein)", 
                                                           "soil quality (soil N)"))
  )


p_effect <- 
  ggplot(effect, aes(effect, estimate, color = effect)) +
  geom_hline( yintercept = 0, color = "grey40") +
  geom_pointrange(aes(ymin = lower, ymax = upper), size = 1, linewidth = 1.5) +
  coord_flip() +
  facet_wrap("response_variable", nrow = 2) +
  labs(y = "standardized coefficient") +
  bbplot::bbc_style()  +
  scale_color_manual(values = pal3) +
  theme (axis.text.x = element_text(size=14,
                                    color="#222222"),
         axis.text = element_text(size = 14),
         axis.text.y = element_blank(),
         axis.title.x = element_text(size = 15, color="#222222"),
         legend.position = "bottom",
         legend.text = element_text(size = 15, color = "grey40"),
         legend.box.margin = margin(t = 10),
         legend.background = element_rect(
           color = "grey40", 
           fill = "grey95",
           size = .3
         ),
         strip.text = element_text(size=15,
                                   color="#222222"),
         panel.grid.major.y = ggplot2::element_line(color = "#cbcbcb", linetype = "dashed"),
         panel.spacing = unit(2.5, "lines"), 
         ) +
  guides(fill = guide_legend(ncol = 6)
         )

ggsave("./figures/materials/ecosystem_effect.png", p_effect,
       width = 12, height = 6, device = ragg::agg_png)
