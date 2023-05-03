# this script examine relationships between long term effect of cattle use (using distance to boundary as a proxy) in ecosystem function (vegetation quantity, quality, and soil N). 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(DHARMa)
library(spaMM)
library(gridExtra)

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, NDVI, pr) %>%
  rename(Precip = pr)

veg_data <- read_csv("./data/cleaned_grass_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Protein, Avg_Height)

soil_data <- read_csv("./data/cleaned_soil_data.csv") %>% 
  select(Transect, Site, Year, Month, Nitrate_N, Ammonium) %>%
  mutate(soil_N = (Nitrate_N + Ammonium) * 4) %>%
  select(-Nitrate_N, -Ammonium)

sample_sites <- read_csv("./data/Sampling_site.csv")

animal_data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Cattle)

full.dat <- animal_data %>% left_join(sample_sites) %>% left_join(veg_data) %>% left_join(soil_data) %>% left_join(gee_data) %>% 
  mutate(sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%
  filter(!is.na(Protein),
         !is.na(Avg_Height),
         !is.na(NDVI))  %>% 
  arrange(Year, Month) %>%
  group_by(Transect, Site) 

#####      cattle use on vegetation quantity (height)      #####
## ---- first use grass height ---- ## 
spamm.height <- fitme(Avg_Height ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
# model summary
summary(spamm.height) # rho and nu values under "correlation parameter" the scale parameter and the smoothness parameter of the martern model. lamda is random effect variance parameter

# get confidence interval of Site estimate
# confint(spamm.height, "Site")
# lower Site upper Site 
# 0.01263429 0.04568487    ## positive?

# # visualize spatial autocorrelation
# dd <- dist(full.dat[,c("x","y")])
# mm <- MaternCorr(dd, nu = 1.9438, rho = 0.000591)
# # #Then the correlation parameter nu and rho which represent the strength and the speed of decay in the spatial effect, which we can turn into the actual spatial correlation effect 
# # # by plotting the estimated correlation between two locations against their distance
# plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]")

# DHARMa diagnostics
sim_res <- DHARMa::simulateResiduals(spamm.height, 250)
plot(sim_res)  ## hm

#####      cattle use on vegetation quantity (NDVI)      #####
spamm.NDVI <- fitme(NDVI ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = gaussian()) # this take a bit of time
summary(spamm.NDVI)

# confint(spamm.NDVI, "Site")
# lower Site upper Site 
#  -6.134287  35.920273     ## not significant!

sim_res <- DHARMa::simulateResiduals(spamm.NDVI, 250)
plot(sim_res)  ## looks good

#####      cattle use on vegetation quality       #####
spamm.protein <- fitme(Protein ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
summary(spamm.protein)

# confint(spamm.protein, "Site")
# lower Site   upper Site 
# -0.010111785  0.004280205   # not significant!

sim_res <- DHARMa::simulateResiduals(spamm.protein, 250)
plot(sim_res)  


#####      cattle use on soil N    #####
spamm.soilN <- fitme(soil_N ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
summary(spamm.soilN)

 # confint(spamm.soilN, "Site")
 # lower Site   upper Site 
 # -0.007101439  0.018741301   # not significant

sim_res <- DHARMa::simulateResiduals(spamm.soilN, 250)
plot(sim_res)  ## looks fine