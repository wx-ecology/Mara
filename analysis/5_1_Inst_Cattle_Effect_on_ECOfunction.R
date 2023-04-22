# this script examine relationships between instantaneous effect of cattle use in ecosystem function (vegetation quantity, quality, and soil N). 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(brms)
library(DHARMa)
library(spaMM)

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, NDVI, pr) %>%
  rename(Precip = pr)

veg_data <- read_csv("./data/cleaned_grass_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Protein, Avg_Height)

sample_sites <- read_csv("./data/Sampling_site.csv")

animal_data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  dplyr::select(Transect, Site, Year, Month, Cattle)

full.dat <- animal_data %>% left_join(sample_sites) %>% left_join(veg_data) %>% left_join(gee_data) %>%
  mutate(sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%
  filter(!is.na(Protein),
         !is.na(Avg_Height),
         !is.na(NDVI))  %>% 
  arrange(Year, Month) %>%
  group_by(Transect, Site) 

# ----- cattle use on vegetation quantity -----# 
## ---- first use grass height ---- ## 
spamm.hight <- fitme(Avg_Height ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
# model summary
summary(spamm.height)

# # visualize spatial autocorrelation
# dd <- dist(full.dat[,c("x","y")])
# mm <- MaternCorr(dd, nu = 1.9438, rho = 0.000591)
# #Then the correlation parameter nu and rho which represent the strength and the speed of decay in the spatial effect, which we can turn into the actual spatial correlation effect 
# # by plotting the estimated correlation between two locations against their distance
# plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]")

sim_res <- DHARMa::simulateResiduals(spamm.height, 250)
plot(sim_res)  ## ahh pretty good



## ---- then use NDVI ---- ## 
m.NDVI.1 <- glm(NDVI ~ Cattle + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
m.NDVI.2 <- glm(NDVI ~ Cattle + I(Cattle^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))


# ---- predict cattle effect on Grass height, NDVI, and protein ----- #
# visualize effect of cattle
newdat <- expand.grid(x = 743430, y = 9835271,
                      Cattle = seq(0, 30, length.out = 100),
                      Precip = 46.67278,
                      sin_month = sin(2*pi*5/12),
                      cos_month = cos(2*pi*5/12))
newdat$Avg_Height <- as.numeric(predict(spamm.height, newdat, re.form = NA)) # re.form = NA used to remove spatial effects
# newdat$Avg_Height <- newdat$Ave_Height + mean(c(0,fixef(spamm.height)[3:5])) # to remove region effect (??)
# get 95% confidence intervals around predictions
#newdat <- cbind(newdat, get_intervals(spamm.height, newdata = newdat, intervals = "fixefVar", re.form = NA) + mean(c(0,fixef(m_spamm.height)[3:4])))
newdat <- cbind(newdat, get_intervals(spamm.height, newdata = newdat, intervals = "fixefVar", re.form = NA))

ggplot(full.dat, aes(x = Cattle, y = Avg_Height)) +
  geom_point() +
  geom_path(data = newdat) +
  geom_ribbon(data = newdat, aes(ymin = fixefVar_0.025, ymax = fixefVar_0.975), alpha = 0.2)