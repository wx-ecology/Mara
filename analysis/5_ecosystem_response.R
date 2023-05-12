
# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(DHARMa)
library(spaMM)
library(gridExtra)

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, pr, NDVI) %>%
  rename(Precip = pr)

veg_data <- read_csv("./data/cleaned_grass_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Protein, Avg_Height)

soil_data <- read_csv("./data/cleaned_soil_data.csv") %>% 
  select(Transect, Site, Year, Month, Nitrate_N, Ammonium) %>%
  mutate(Soil_N = (Nitrate_N + Ammonium) * 4) %>%
  select(-Nitrate_N, -Ammonium)

sample_sites <- read_csv("./data/Sampling_site.csv")

animal_data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate(Total = Cattle + Wildebeest + Zebra + Thompsons_Gazelle + Impala + Topi + Eland + Buffalo + Grants_Gazelle + Waterbuck + Dikdik + Elephant ) %>%
  dplyr::select(Transect, Site, Year, Month, Cattle, Total)

full.dat <- animal_data %>% left_join(sample_sites) %>% left_join(veg_data) %>% left_join(soil_data) %>% left_join(gee_data) %>% 
  mutate(sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%
  filter(!is.na(Protein),
         !is.na(Avg_Height),
         Soil_N != 1865.12)  %>% # get rid of this obvious outlier 
  arrange(Year, Month) %>%
  group_by(Transect, Site) 

full.dat.trans <- full.dat %>% mutate(Avg_Height = scale(log(Avg_Height)),
                                      NDVI = scale(log(NDVI)),
                                      Protein = scale(log(Protein)),
                                      Soil_N = scale(log(Soil_N)))
full.dat.trans$Total <- scale(full.dat$Total)
full.dat.trans$Cattle <- scale(full.dat$Cattle)
full.dat.trans$Site <- scale(full.dat$Site)

spamm.height.dist <- fitme(Avg_Height ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.protein.dist <- fitme(Protein ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.NDVI.dist <- fitme(NDVI ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.soilN.dist <- fitme(Soil_N ~ Site + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)

spamm.height.cattle <- fitme(Avg_Height ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans) 
spamm.protein.cattle <- fitme(Protein ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.NDVI.cattle <- fitme(NDVI ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.soilN.cattle <- fitme(Soil_N ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)

spamm.height.total <- fitme(Avg_Height ~ Total + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.protein.total <- fitme(Protein ~ Total + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.NDVI.total <- fitme(NDVI ~ Total + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)
spamm.soilN.total <- fitme(Soil_N ~ Total + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat.trans)

distance_effect <- 
  data.frame(
    effect = rep("dist_to_bound", 4),
    response_variable = c("grass_height", "crude_protein", "soil_N", "NDVI"), 
    estimate = c(spamm.height.dist$fixef[[2]],
                 spamm.protein.dist$fixef[[2]],
                 spamm.soilN.dist$fixef[[2]],
                 spamm.NDVI.dist$fixef[[2]]
    ),
    lower = c(confint(spamm.height.dist, "Site")$interval[[1]],
              confint(spamm.protein.dist, "Site")$interval[[1]],
              confint(spamm.soilN.dist, "Site")$interval[[1]],
              confint(spamm.NDVI.dist, "Site")$interval[[1]]
    ), 
    upper = c(confint(spamm.height.dist, "Site")$interval[[2]],
              confint(spamm.protein.dist, "Site")$interval[[2]],
              confint(spamm.soilN.dist, "Site")$interval[[2]],
              confint(spamm.NDVI.dist, "Site")$interval[[2]]
    ),
    mAIC =  c(AIC(spamm.height.dist)[[1]],
              AIC(spamm.protein.dist)[[1]],
              AIC(spamm.soilN.dist)[[1]],
              AIC(spamm.NDVI.dist)[[1]])
  )
# ggplot(distance_effect, aes(response_variable, estimate, color = response_variable)) +
#   geom_pointrange(aes(ymin = lower, ymax = upper)) 

cattle_effect <- 
  data.frame(
    effect = rep("cattle_use", 4),
    response_variable = c("grass_height", "crude_protein", "soil_N", "NDVI"), 
    estimate = c(spamm.height.cattle$fixef[[2]],
                 spamm.protein.cattle$fixef[[2]],
                 spamm.soilN.cattle$fixef[[2]],
                 spamm.NDVI.cattle$fixef[[2]]
    ),
    lower = c(confint(spamm.height.cattle, "Cattle")$interval[[1]],
              confint(spamm.protein.cattle, "Cattle")$interval[[1]],
              confint(spamm.soilN.cattle, "Cattle")$interval[[1]],
              confint(spamm.NDVI.cattle, "Cattle")$interval[[1]]
    ), 
    upper = c(confint(spamm.height.cattle, "Cattle")$interval[[2]],
              confint(spamm.protein.cattle, "Cattle")$interval[[2]],
              confint(spamm.soilN.cattle, "Cattle")$interval[[2]],
              confint(spamm.NDVI.cattle, "Cattle")$interval[[2]]
    ),
    mAIC =  c(AIC(spamm.height.cattle)[[1]],
                     AIC(spamm.protein.cattle)[[1]],
                     AIC(spamm.soilN.cattle)[[1]],
                     AIC(spamm.NDVI.cattle)[[1]]
  )
  )
# ggplot(cattle_effect, aes(response_variable, estimate, color = response_variable)) +
#   geom_pointrange(aes(ymin = lower, ymax = upper)) 

total_effect <- 
  data.frame(
    effect = rep("total_use", 4),
    response_variable = c("grass_height", "crude_protein", "soil_N", "NDVI"), 
    estimate = c(spamm.height.total$fixef[[2]],
                 spamm.protein.total$fixef[[2]],
                 spamm.soilN.total$fixef[[2]],
                 spamm.NDVI.total$fixef[[2]]
    ),
    lower = c(confint(spamm.height.total, "Total")$interval[[1]],
              confint(spamm.protein.total, "Total")$interval[[1]],
              confint(spamm.soilN.total,"Total")$interval[[1]],
              confint(spamm.NDVI.total, "Total")$interval[[1]]
    ), 
    upper = c(confint(spamm.height.total, "Total")$interval[[2]],
              confint(spamm.protein.total, "Total")$interval[[2]],
              confint(spamm.soilN.total, "Total")$interval[[2]],
              confint(spamm.NDVI.total, "Total")$interval[[2]]
    ),
    mAIC =  c(AIC(spamm.height.total)[[1]],
              AIC(spamm.protein.total)[[1]],
              AIC(spamm.soilN.total)[[1]],
              AIC(spamm.NDVI.total)[[1]]
    )
  )
# ggplot(cattle_effect, aes(response_variable, estimate, color = response_variable)) +
#   geom_pointrange(aes(ymin = lower, ymax = upper)) 

response <- rbind(total_effect, cattle_effect, distance_effect)
write_csv(response, "results/ecosystem_effects.csv")

### ---- diagnostics ------ #
# # visualize spatial autocorrelation
# dd <- dist(full.dat[,c("x","y")])
# mm <- MaternCorr(dd, nu = 1.9438, rho = 0.000591)
# # #Then the correlation parameter nu and rho which represent the strength and the speed of decay in the spatial effect, which we can turn into the actual spatial correlation effect 
# # # by plotting the estimated correlation between two locations against their distance
# plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]")

# # DHARMa diagnostics
# sim_res <- DHARMa::simulateResiduals(spamm.height, 250)
# plot(sim_res)  ## ahh pretty good