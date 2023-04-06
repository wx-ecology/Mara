## this script apply the HMSC framework to 
## examine relationships between vegetation properties to grazing intensity 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(GGally)

## get soil data ## 
vegetation <- read_csv("./data/cleaned_soil_data.csv")
ggpairs(vegetation [,c(4,8:10, 12:21)]) 

vegetation.baseline <- vegetation  %>% filter(!is.na(Phosphorus))
ggpairs(vegetation.baseline[,c(4,8:10, 12:21)]) 

vegetation.dynamics <- vegetation %>% select(Year, Month, Transect, Site, Nitrate_N, Ammonium)
ggpairs(soil.dynamics[,c(4:6)]) 

# ----- overview of 28 months animal usage to estimate overall "grazing pressure" ------- # 

# ----- question 0: does soil N positively related to grass N? ------ # 
# total soil N = (nitrate + ammonium) * 4

# ----- question 1: does the presence of cattle lead to lower soil quality (soil N)? ------ # 
## the negative relationship was shown by Sitters 2020; 

# hypotheses 1.1 - domestic cattle negatively influence soil N over long term. we should see poorer soil N near park edge 
# hypotheses 1.2 - sudden intense cattle use will lead to soil N responses in the following months 

# ----- question 2: does the presence of cattle lead to lower forage quality (forage N)? ------ # 

# hypotheses 2.1 - lower vegetation quality near park edge 1) if domestic cattle negatively influence soil N. 
# hypotheses 2.2 - lower vegetation quality after cattle use


# ----- question 3: which species influence soil quality? -------- #
# model selection on soil PC over all species.

# ----- question 4: which species influence forage quality? -------- #
# model selection on vege quality PC over all species. 


# ----- caveates ------ #
# we did not measure all N pools (i.e. grass, soil, tree)