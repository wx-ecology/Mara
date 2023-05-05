## make a table about cattle effect on eco function 

# ----- set up ----- # 
library(tidyverse)

cattle_effect <- read_csv("./results/cattle_inst_eco_effects.csv")
dist_effect <- read_csv( "./results/cattle_longterm_effects.csv")

effect.df <- rbind(cattle_effect, dist_effect)


ggplot(effect.df, aes(response_variable, estimate, color = response_variable)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) +
  facet_wrap("cattle_effect") +
  coord_flip()
