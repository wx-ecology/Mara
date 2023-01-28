library(rgee)
library(sf)
library(tidyverse)
library(ggspatial)
library(patchwork)

ee_Initialize()

sample_sites <- read_csv("./data/Sampling_site.csv") %>% 
  filter(Location == "Start") %>%
  select(-Location) %>%
  mutate(Name = paste0(Transect,Site)) %>% 
  st_as_sf(., coords = c("X", "Y"), crs = st_crs(21036))

study_site_box <- read_sf("./data/Mara_Geo/Study_area_bbox_3kmbuffer.shp")

ggplot() +
#  annotation_map_tile(zoomin = 1) +   # <- this line will take a while to run in download the tiles but looks nice
  geom_sf(data = study_site_box) +
  geom_sf(data = sample_sites, aes(color = Transect)) 

### -------------------- extract precipitation data from the Terraclimate dataset -----------
terraclimate <- ee$ImageCollection("IDAHO_EPSCOR/TERRACLIMATE") %>%
  ee$ImageCollection$filterDate("2018-01-01", "2021-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("pr")) %>% # Select only precipitation bands
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("PP_%02d",1:36)) # rename the bands of an image

ee_mara_rain <- ee_extract(x = terraclimate, y = study_site_box["Id"], fun = ee$Reducer$mean(), sf = FALSE) %>%
  pivot_longer(-Id, names_to = "month", values_to = "pr") %>%
  mutate(month = as.numeric(gsub("PP_", "", month)),
         year = case_when(month >=1 & month <= 12 ~ "2018",
                          month >12 & month <= 24 ~ "2019",
                          month >24 & month <= 36 ~ "2020"),
         month = rep(1:12, 3))

# ee_mara_rain %>%
#   ggplot(aes(x = month, y = pr, color = year)) +
#   geom_line(size = 1, alpha = 0.8) +
#   xlab("Month") +
#   ylab("Precipitation (mm)") +
#   theme_minimal() +
#   theme(legend.position = "bottom")


### -------------------- extract NDVI data from MODIS -----------
NDVI.ic <- ee$ImageCollection('MODIS/006/MOD13Q1') %>%  #Vegetation Indices 16-Day L3 Global 250m
  ee$ImageCollection$filterDate("2018-01-01", "2021-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("NDVI")) # Select only precipitation bands
# ee_print(NDVI.ic)  #get to know that we have 69 images 

# extract image dates from the image collection to later combine with NDVI 
date.df <- data.frame( date = ee_get_date_ic(NDVI.ic)$time_start , serial = seq(1:69))

# image collection to image - prep for extraction 
NDVI <- NDVI.ic %>% 
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("NDVI_%02d",1:69)) # rename the bands of an image

# extract NDVI values for each site 
ee_mara_NDVI <- ee_extract(x = NDVI, y = sample_sites["Name"], sf = FALSE) %>%
  pivot_longer(-Name, names_to = "serial", values_to = "NDVI") %>%
  mutate(serial = as.numeric(gsub("NDVI_", "", serial)))

# join NDVI and date 
ee_mara_NDVI <- ee_mara_NDVI %>% left_join(date.df, by = "serial") %>%
  mutate (Transect = str_sub(Name, 1,1),
          Site = str_sub(Name, 2)) %>%
  select(-serial)

# ee_mara_NDVI %>%
#   ggplot(aes(x = date, y = NDVI, group = Name, color = Name)) +
#   geom_line(size = 1, alpha = 0.8) +
#   xlab("Date") +
#   ylab("NDVI") +
#   theme_minimal() +
#   theme(legend.position = "none")


### -------------------- extract EVI data from MODIS -----------
EVI.ic <- ee$ImageCollection('MODIS/006/MOD13Q1') %>%  #Vegetation Indices 16-Day L3 Global 250m
  ee$ImageCollection$filterDate("2018-01-01", "2021-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("EVI")) # Select only precipitation bands
# ee_print(EVI.ic)  #get to know that we have 69 images 

# extract image dates from the image collection to later combine with EVI 
date.df <- data.frame( date = ee_get_date_ic(EVI.ic)$time_start , serial = seq(1:69))

# image collection to image - prep for extraction 
EVI <- EVI.ic %>% 
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("EVI_%02d",1:69)) # rename the bands of an image

# extract EVI values for each site 
ee_mara_EVI <- ee_extract(x = EVI, y = sample_sites["Name"], sf = FALSE) %>%
  pivot_longer(-Name, names_to = "serial", values_to = "EVI") %>%
  mutate(serial = as.numeric(gsub("EVI_", "", serial)))

# join EVI and date 
ee_mara_EVI <- ee_mara_EVI %>% left_join(date.df, by = "serial") %>%
  mutate (Transect = str_sub(Name, 1,1),
          Site = str_sub(Name, 2)) %>%
  select(-serial)

# ee_mara_EVI %>%
#   ggplot(aes(x = date, y = EVI, group = Name, color = Name)) +
#   geom_line(size = 1, alpha = 0.8) +
#   xlab("Date") +
#   ylab("EVI") +
#   theme_minimal() +
#   theme(legend.position = "none")


### -------------------- extract nightlight data  -----------
rad.ic <- ee$ImageCollection('NOAA/VIIRS/DNB/MONTHLY_V1/VCMSLCFG') %>%  #Vegetation Indices 16-Day L3 Global 250m
  ee$ImageCollection$filterDate("2018-01-01", "2021-01-01") %>%
  ee$ImageCollection$map(function(x) x$select("avg_rad")) # Select only precipitation bands
#ee_print(rad.ic)  #get to know that we have 36 images 

# extract image dates from the image collection to later combine with EVI 
date.df <- data.frame( date = ee_get_date_ic(rad.ic)$time_start , serial = seq(1:36))

# image collection to image - prep for extraction 
rad <- rad.ic %>% 
  ee$ImageCollection$toBands() %>% # from imagecollection to image
  ee$Image$rename(sprintf("rad_%02d",1:36)) # rename the bands of an image

# extract rad values for each site 
ee_mara_rad <- ee_extract(x = rad, y = sample_sites["Name"], sf = FALSE) %>%
  pivot_longer(-Name, names_to = "serial", values_to = "rad") %>%
  mutate(serial = as.numeric(gsub("rad_", "", serial)))

# join EVI and date 
ee_mara_rad <- ee_mara_rad %>% left_join(date.df, by = "serial") %>%
  mutate (Transect = str_sub(Name, 1,1),
          Site = str_sub(Name, 2)) %>%
  select(-serial)

# ee_mara_rad %>%
#   ggplot(aes(x = as.numeric(Site), y = rad, group = Transect, color = Transect)) +
#   geom_violin( alpha = 0.8) +
#   xlab("Site") +
#   ylab("rad") +
#   theme_minimal() +
#   theme(legend.position = "none")

### ------------------------ organize dataframe ----------------------------
ee_mara_VI <- ee_mara_NDVI %>% 
  left_join(ee_mara_EVI) %>%
  mutate(month = month(date),
         year = year(date)) %>%
  group_by( Name, Transect, Site, month, year) %>%
  summarise(NDVI = mean(NDVI),
            EVI = mean(EVI)) %>%
  mutate(date = ymd(paste0(year,"-",month,"-01")))

ee_mara <- ee_mara_VI %>%
  left_join(ee_mara_rad) %>%
  left_join(
    ee_mara_rain %>% 
      mutate(date = ymd(paste0(year,"-",month,"-01"))) %>%
      select(-Id, -month, -year)
            ) %>%
  select(-date) %>%
  rename(Year = year, Month = month)

# write_csv(ee_mara, "./data/mara_gee_VI_rad.csv")

### ---------------------- simple visualization ------------
# Simple RGB Vizualization -Landsat
study_area <- sf_as_ee(study_site_box)
sampling_site <- sf_as_ee(sample_sites)
Map$setCenter(35.178043,-1.451947, 10)
Map$addLayer(sampling_site)

### ----------------------- END ----------------------------
