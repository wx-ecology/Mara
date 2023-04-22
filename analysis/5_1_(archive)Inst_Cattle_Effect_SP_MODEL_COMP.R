# this script examine relationships between instantaneous effect of cattle use in ecosystem function (vegetation quantity, quality, and soil N). 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(brms)
library(DHARMa)

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
  group_by(Transect, Site) %>% 
  mutate(time = row_number()) 

# ----- cattle use on vegetation quantity -----# 

## ---- first use grass height ---- ## 
# glm
m.veg.height.glm <- glm(Avg_Height ~ Cattle + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
# test spatial autocorrelation 
sims.veg.height.glm  <- recalculateResiduals(m.veg.height.glm)
plot(sims.veg.height.glm)

# dist <- as.matrix(dist(cbind(full.dat$x, full.dat$y)))
# inv.dist <- 1/dist
# inv.dist[inv.dist == Inf] <- 0
# diag(inv.dist) <- 0
# Moran.I(recalculateResiduals$residuals, inv.dist) # there are so much spatial autocorrlation!

## ----  glmmfields with stan ---- ##  modeling spatial deviations in GLMs as random effects 
library(glmmfields)
options(mc.cores = parallel::detectCores()) 

m.veg.height.gfields <- glmmfields(Avg_Height ~ Cattle + Precip,
                                   data = data.frame(full.dat), time = "time",
                                   lat = "lat", lon = "lon",
                                   nknots = 6, estimate_df = TRUE, iter = 2000, seed = 1, 
                                   control = list(adapt_delta = 0.95), save_log_lik = T)
plot(m.veg.height.gfields, type = "residual-vs-fitted")

# 1: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 2: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 

m.veg.height.gfields.gamma <- glmmfields(Avg_Height ~ Cattle + Precip,
                                         data = data.frame(full.dat), 
                                         family = Gamma(link = "log"),
                                         time = "time",
                                         lat = "lat", lon = "lon",
                                         nknots = 6, estimate_df = TRUE, iter = 2000, seed = 1, 
                                         control = list(adapt_delta = 0.95),  save_log_lik = T)
plot(m.veg.height.gfields.gamma, type = "residual-vs-fitted")

# Warning messages:
#   1: There were 1000 transitions after warmup that exceeded the maximum treedepth. Increase max_treedepth above 10. See
# https://mc-stan.org/misc/warnings.html#maximum-treedepth-exceeded 
# 2: There were 1 chains where the estimated Bayesian Fraction of Missing Information was low. See
# https://mc-stan.org/misc/warnings.html#bfmi-low 
# 3: Examine the pairs() plot to diagnose sampling problems
# 
# 4: The largest R-hat is 1.63, indicating chains have not mixed.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#r-hat 
# 5: Bulk Effective Samples Size (ESS) is too low, indicating posterior means and medians may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#bulk-ess 
# 6: Tail Effective Samples Size (ESS) is too low, indicating posterior variances and tail quantiles may be unreliable.
# Running the chains for more iterations may help. See
# https://mc-stan.org/misc/warnings.html#tail-ess 


m.veg.height.gfields.gamma2 <- glmmfields(Avg_Height ~ Cattle + Precip + sin_month + cos_month,
                                          data = data.frame(full.dat), 
                                          family = Gamma(link = "log"),
                                          lat = "lat", lon = "lon",
                                          nknots = 6, estimate_df = TRUE, iter = 2000, seed = 1,  
                                          control = list(adapt_delta = 0.95),  save_log_lik = T)
plot(m.veg.height.gfields.gamma2, type = "residual-vs-fitted")


## ----  spaMM ---- ##  
library(spaMM)
spamm <- fitme(Avg_Height ~ Cattle + Precip + sin_month + cos_month + Matern(1 | x + y), data = full.dat, family = Gamma(log)) # this take a bit of time
# model summary
summary(spamm)

dd <- dist(full.dat[,c("x","y")])
mm <- MaternCorr(dd, nu = 1.9438, rho = 0.000591)
#Then the correlation parameter nu and rho which represent the strength and the speed of decay in the spatial effect, which we can turn into the actual spatial correlation effect 
# by plotting the estimated correlation between two locations against their distance
plot(as.numeric(dd), as.numeric(mm), xlab = "Distance between pairs of location [in m]")

sim_res <- DHARMa::simulateResiduals(spamm, 250)
plot(sim_res)  ## ahh pretty good

# effect of cattle 
newdat <- expand.grid(x = 743430, y = 9835271, 
                      Cattle = seq(0, 30, length.out = 100),
                      Precip = 46.67278,
                      sin_month = sin(2*pi*5/12),
                      cos_month = cos(2*pi*5/12))
newdat$Avg_Height <- as.numeric(predict(spamm, newdat, re.form = NA)) # re.form = NA used to remove spatial effects
# newdat$Avg_Height <- newdat$Ave_Height + mean(c(0,fixef(spamm)[3:5])) # to remove region effect (??)
# get 95% confidence intervals around predictions
#newdat <- cbind(newdat, get_intervals(spamm, newdata = newdat, intervals = "fixefVar", re.form = NA) + mean(c(0,fixef(m_spamm)[3:4])))
newdat <- cbind(newdat, get_intervals(spamm, newdata = newdat, intervals = "fixefVar", re.form = NA))

ggplot(full.dat, aes(x = Cattle, y = Avg_Height)) +
  geom_point() +
  geom_path(data = newdat) +
  geom_ribbon(data = newdat, aes(ymin = fixefVar_0.025, ymax = fixefVar_0.975), alpha = 0.2)


## ----  glmmTMB ---- ##  
library(glmmTMB)
# fitst we need to create a numeric factor recording the coordinates of the sampled locations
full.dat$pos <- numFactor(scale(full.dat$x), scale(full.dat$y))
# then create a dummy group factor to be used as a random term
full.dat$ID <- factor(rep(1, nrow(full.dat)))

# fit the model
tmb <- glmmTMB(Avg_Height ~ Cattle + Precip + sin_month + cos_month + mat(pos + 0 | ID), full.dat) # take some time to fit
# model summary of fixed effects
summary(tmb)

sims <- simulateResiduals(tmb)
plot(sims)  # not as well as spaMM

# # extract and re-order the estimated correlation between pairs of locations
# fit_cor <- matrix(as.numeric(attr(VarCorr(tmb)$cond$ID, "correlation")), nrow = 60, ncol = 60, byrow = FALSE, 
#                   dimnames = attr(attr(VarCorr(tmb)$cond$ID, "correlation"),"dimnames"))
# ff <- dimnames(fit_cor)[[1]]
# ff <- gsub("pos","",ff)
# fit_cor2 <- fit_cor[order(match(ff, full.dat$pos)), order(match(ff, full.dat$pos))]
# # plot
# plot(as.numeric(dd), fit_cor2[lower.tri(fit_cor2)],
#      xlab = "Distance between pairs of location [m]",
#      ylab = "Estimated correlation")
# we don't seem to have direct estimation of the Matern parameters that were readily available in spaMM. 
# glmmTMB for spatial data are that model fitting is particularly slower than spaMM

## ---- then use NDVI ---- ## 
m.NDVI.1 <- glm(NDVI ~ Cattle + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
m.NDVI.2 <- glm(NDVI ~ Cattle + I(Cattle^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))

# compare two models 
AIC(m.NDVI.1, m.NDVI.2)  
# df      AIC
# m.NDVI.1  6 27569.08
# m.NDVI.2  7 27570.74
# less than 2, select the more parcimonious model. 
best.m.NDVI <- m.NDVI.1
summary(best.m.NDVI)  # cattle use does not significantly affect NDVI after controlling temporal and precipitation effects
