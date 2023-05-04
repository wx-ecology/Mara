# this script examine relationships between instantaneous effect of cattle use in ecosystem function (vegetation quantity, quality, and soil N). 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(DHARMa)
library(spaMM)
library(gridExtra)
library(effectsize)

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, pr) %>%
  rename(Precip = pr)

veg_data <- read_csv("./data/cleaned_grass_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Protein, Avg_Height)

soil_data <- read_csv("./data/cleaned_soil_data.csv") %>% 
  select(Transect, Site, Year, Month, Nitrate_N, Ammonium) %>%
  mutate(Soil_N = (Nitrate_N + Ammonium) * 4) %>%
  select(-Nitrate_N, -Ammonium)

sample_sites <- read_csv("./data/Sampling_site.csv")

animal_data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Cattle)

full.dat <- animal_data %>% left_join(sample_sites) %>% left_join(veg_data) %>% left_join(soil_data) %>% left_join(gee_data) %>% 
  mutate(sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%
  filter(!is.na(Protein),
         !is.na(Avg_Height),
         Soil_N != 1865.12)  %>% # get rid of this obvious outlier 
  arrange(Year, Month) %>%
  group_by(Transect, Site) 

#####      cattle use on vegetation quantity (height)      #####
## ---- first use grass height ---- ## 
spamm.height <- fitme(Avg_Height ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
# model summary
#summary(spamm.height) # rho and nu values under "correlation parameter" the scale parameter and the smoothness parameter of the martern model. lamda is random effect variance parameter

# # visualize spatial autocorrelation
# dd <- dist(full.dat[,c("x","y")])
# mm <- MaternCorr(dd, nu = 1.9438, rho = 0.000591)
# # #Then the correlation parameter nu and rho which represent the strength and the speed of decay in the spatial effect, which we can turn into the actual spatial correlation effect 
# # # by plotting the estimated correlation between two locations against their distance
# plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]")

# # DHARMa diagnostics
# sim_res <- DHARMa::simulateResiduals(spamm.height, 250)
# plot(sim_res)  ## ahh pretty good

#####      cattle use on vegetation quality       #####
spamm.protein <- fitme(Protein ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
# summary(spamm.protein)

# sim_res <- DHARMa::simulateResiduals(spamm.protein, 250)
# plot(sim_res)  ## ok


#####      cattle use on soil N    #####
spamm.soilN <- fitme(Soil_N ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
# summary(spamm.soilN)

# sim_res <- DHARMa::simulateResiduals(spamm.soilN, 250)
# plot(sim_res)

######### ----- plot cattle effect --------- ############# 
cattle_effect <- 
  data.frame(
             cattle_effect = rep("cattle_use", 3),
             response_variable = c("grass_height", "crude_protein", "soil_N"), 
             estimate = c(spamm.height$fixef[[2]],
                          spamm.protein$fixef[[2]],
                          spamm.soilN$fixef[[2]]
             ),
             lower = c(confint(spamm.height, "Cattle")$interval[[1]],
                       confint(spamm.protein, "Cattle")$interval[[1]],
                       confint(spamm.soilN, "Cattle")$interval[[1]]
             ), 
             upper = c(confint(spamm.height, "Cattle")$interval[[2]],
                       confint(spamm.protein, "Cattle")$interval[[2]],
                       confint(spamm.soilN, "Cattle")$interval[[2]]
             ),
             marginalAIC = c(AIC(spamm.height)[[1]],
                             AIC(spamm.protein)[[1]],
                             AIC(spamm.soilN)[[1]]
             )
  )
ggplot(cattle_effect, aes(response_variable, estimate, color = response_variable)) +
  geom_pointrange(aes(ymin = lower, ymax = upper)) 

write_csv(cattle_effect, "./results/cattle_inst_eco_effects.csv")


# ######### ----- prediction and plot --------- ############# 
# # ---- predict cattle effect on Grass height and protein ----- #
# # visualize effect of cattle
# newdat <- expand.grid(x = 743430, y = 9835271,
#                       Cattle = seq(0, 30, length.out = 100),
#                       Precip = 46.67278,
#                       sin_month = sin(2*pi*5/12),
#                       cos_month = cos(2*pi*5/12))
# 
# newdat$Avg_Height <- as.numeric(predict(spamm.height, newdat, re.form = NA)) # re.form = NA used to remove spatial effects
# # get 95% confidence intervals around predictions
# newdat <- cbind(newdat, get_intervals(spamm.height, newdata = newdat, intervals = "fixefVar", re.form = NA))
# gg_height <- ggplot(full.dat, aes(x = Cattle, y = Avg_Height)) +
#   geom_point() +
#   geom_path(data = newdat) +
#   geom_ribbon(data = newdat, aes(ymin = fixefVar_0.025, ymax = fixefVar_0.975), alpha = 0.2)
# 
# newdat <- expand.grid(x = 743430, y = 9835271,
#                       Cattle = seq(0, 30, length.out = 100),
#                       Precip = 46.67278,
#                       sin_month = sin(2*pi*5/12),
#                       cos_month = cos(2*pi*5/12))
# newdat$Protein <- as.numeric(predict(spamm.protein, newdat, re.form = NA)) # re.form = NA used to remove spatial effects
# # get 95% confidence intervals around predictions
# newdat <- cbind(newdat, get_intervals(spamm.protein, newdata = newdat, intervals = "fixefVar", re.form = NA))
# gg_protein <- ggplot(full.dat, aes(x = Cattle, y = Protein)) +
#   geom_point() +
#   geom_path(data = newdat) +
#   geom_ribbon(data = newdat, aes(ymin = fixefVar_0.025, ymax = fixefVar_0.975), alpha = 0.2)
# 
# grid.arrange(gg_height, gg_protein, ncol = 3)
# # This plot is not very informative. In the manuscript will show a result table.