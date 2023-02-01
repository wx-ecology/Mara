library(tidyverse)
library(ecoCopula)
library(lubridate)
#Copulas are a way to construct a multivariate distribution, 
# can be used as an alternative to hierarchical models 
# and generalised estimating equations (GEE; as in mvabund).

#######################################################
################## prep df ############################
# environmental variables that likely contribute to species useage 
# current month: precipitation, 
# previous month: ave_grass_height, forage quality (Protein)
# distance to border (aka site) 
# previous month protein 
#######################################################
#######################################################

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number())  # add unique id for each month

protein <- read_csv("./data/cleaned__grass__data.csv") %>%  arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number() + 5 ) %>%  # add unique id for each month. The starting month should be 5 (May 2018) but now it is 6 because we want to 
                                                                         # match the animal abundance with quality measure of the month prior
  select(Transect, Site, month_id, Protein, Avg_Height, Percent_Grazed)

data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = format_ISO8601(Date, precision = "ym")) %>%
  arrange(Year, Month) %>%
  left_join(gee_data)  %>%  # this step to get NDVI of the same month 
  left_join(
    (gee_data %>% mutate(month_id = month_id + 1) %>%  
      select(Transect, Site, NDVI, month_id, pr)), by = c("Transect", "Site", "month_id") # match NDVI from the prior month
  ) %>%
    left_join(
      protein, by = c("Transect", "Site", "month_id")
    ) %>%
  rename (NDVI = NDVI.x, 
          NDVI_lag1 = NDVI.y,
          Precip = pr.x,
          Precip_lag1 = pr.y,
          Protein_lag1 = Protein,
          Height_lag1 = Avg_Height,
          Pgrazed = Percent_Grazed.x,
          Pgrazed_lag1 = Percent_Grazed.y)  # NDVI_lag1 represents the NDVI value from the previous month at the same site

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

ENV <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  select(Transect, Site, Pgrazed_lag1, Precip, 
        Protein_lag1, Height_lag1) %>%
  mutate(Site = as.numeric(Site), 
         Protein_lag1 = scale_this(Protein_lag1),
         Height_lag1 = scale_this(Height_lag1),
         Precip = scale_this(Precip),
         Pgrazed_lag1 = scale_this(Pgrazed_lag1))
# ggpairs(ENV[,2:6]) 

COUNT <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
         Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)

test <-  stackedsdm(COUNT,~., data = ENV, family="negative.binomial", ncores = 2)
