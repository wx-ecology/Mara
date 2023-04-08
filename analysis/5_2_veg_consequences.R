## this script apply the HMSC framework to 
## examine relationships between vegetation properties to grazing intensity 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(GGally)

## get soil data ## 
vegetation <- read_csv("./data/cleaned_grass_data.csv")
ggpairs(vegetation [,c(4,8:10, 13:18, 20, 22, 23, 25:29, 33:36)]) 

vegetation.dynamics <- vegetation %>% select(Year, Month, Transect, Site, Protein, N_Gaps, Total_Gap_Size)
ggpairs(vegetation.dynamics[,c(4:7)]) 

# ----- question 2: does the presence of cattle lead to lower forage quality (forage N)? ------ # 

# hypotheses 2.1 - lower vegetation quality near park edge 1) if domestic cattle negatively influence soil N. 
# hypotheses 2.2 - lower vegetation quality after cattle use

# ----- question 4: which species influence forage quality? -------- #
# model selection on vege quality PC over all species. 

# ----- caveates ------ #
# we did not measure all N pools (i.e. grass, soil, tree)