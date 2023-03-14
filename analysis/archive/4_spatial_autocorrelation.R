library(tidyverse)
library(lubridate)
library(sf)
library(GGally)

## this script examine spatial autocorrelation


# - make a df with x and y and lat and long. 
# sample_sites <- read_csv("./data/Sampling_site.csv") %>% 
#   filter(Location == "Start") %>%
#   dplyr::select(-Location) %>%
#   mutate(Name = paste0(Transect,Site)) %>% 
#   st_as_sf(., coords = c("X", "Y"), crs = st_crs(21036))
# 
# sample_sites_lat_long <- sample_sites %>%
#   st_transform(., crs = 4326 ) %>%
#   mutate( lon= st_coordinates(.)[,1],
#           lat = st_coordinates(.)[,2])
# 
# sample_sites_df <-  sample_sites %>%
#   mutate( x = st_coordinates(.)[,1],
#           y = st_coordinates(.)[,2]) %>%
#   st_drop_geometry() %>%
#   left_join(sample_sites_lat_long %>% st_drop_geometry()) 
# write_csv(sample_sites_df, "./data/Sampling_site.csv") 


sample_sites <- read_csv("./data/Sampling_site.csv")
data.sf <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = format_ISO8601(Date, precision = "ym")) %>%
  left_join(., sample_sites_df, by = c("Transect", "Site")) %>%
  st_as_sf(., coords = c("x", "y"), crs = st_crs(21036))

# plot semi-variogram for all species 



v = variogram(Cattle ~ 1, data.sf %>% drop_na())
m = fit.variogram(v, fit.method = 1, vgm("Exp"))
plot(v, model = m)

v = variogram(Wildebeest ~ 1, data.sf %>% drop_na())
m = fit.variogram(v, fit.method = 1, vgm("Exp"))
plot(v, model = m)

v = variogram(Zebra ~ 1, data.sf %>% drop_na())
m = fit.variogram(v, fit.method = 1, vgm("Exp"))
plot(v, model = m)









library(gstat)
library(sp)
data(meuse)
coordinates(meuse) = ~x+y
data(meuse.grid)
gridded(meuse.grid) = ~x+y

# convert to sf/stars -----------------------------------------------------
library(sf)
#> Linking to GEOS 3.6.1, GDAL 2.3.2, PROJ 4.9.3
library(stars)
#> Loading required package: abind
meuse = st_as_sf(meuse)
meuse.grid = st_as_stars(meuse.grid)

# ordinary kriging --------------------------------------------------------
v = variogram(log(zinc)~1, meuse)
m = fit.variogram(v, vgm(1, "Sph", 300, 1))
plot(v, model = m)
