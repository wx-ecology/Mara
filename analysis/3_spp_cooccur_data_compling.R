library(tidyverse)
library(lubridate)

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number())  # add unique id for each month

protein <- read_csv("./data/cleaned_grass_data.csv") %>%  arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number() + 5 ) %>%  # add unique id for each month. The starting month should be 5 (May 2018) but now it is 6 because we want to 
  # match the animal abundance with quality measure of the month prior
  dplyr::select(Transect, Site, month_id, Protein, Avg_Height, Percent_Grazed)

sample_sites <- read_csv("./data/Sampling_site.csv") %>% select(-Name)  # for getting coordinates

data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = format_ISO8601(Date, precision = "ym")) %>%
  arrange(Year, Month) %>%
  left_join(gee_data,  by = c("Year", "Month", "Transect", "Site"))  %>%  # this step to get NDVI of the same month 
  left_join(
    (gee_data %>% mutate(month_id = month_id + 1) %>%  
       dplyr::select(Transect, Site, NDVI, month_id, pr)), by = c("Transect", "Site", "month_id") # match NDVI from the prior month
  ) %>%
  left_join(
    protein, by = c("Transect", "Site", "month_id")
  ) %>%
  left_join(., sample_sites, by = c("Transect", "Site")) %>%
  rename (NDVI = NDVI.x, 
          NDVI_lag1 = NDVI.y,
          Precip = pr.x,
          Precip_lag1 = pr.y,
          Protein_lag1 = Protein,
          Height_lag1 = Avg_Height,
          Pgrazed = Percent_Grazed.x,
          Pgrazed_lag1 = Percent_Grazed.y)  # xxx_lag1 represents the NDVI value from the previous month at the same site
# 1,680 × 39
write_csv(data, "./data/mara-cooccurence-compiled.csv") 


# ---- subsetting the data ------- #
data_subset <- data %>% filter(month_id <= 23)
write_csv(data_subset, "./data/mara-cooccurence-compiled-subset.csv") 
