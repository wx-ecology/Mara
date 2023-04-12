## this script apply the HMSC framework to 
## examine relationships between soil properties to grazing intensity 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(GGally)
library(stringr)
library(nlme)
library(pgirmess)

## get soil data ## 
soil <- read_csv("./data/cleaned_soil_data.csv")
#ggpairs(soil[,c(4,8:10, 12:21)]) 

soil.baseline <- soil %>% filter(!is.na(Phosphorus))
#ggpairs(soil.baseline[,c(4,8:10, 12:21)]) 

soil.dynamics <- soil %>% select(Year, Month, Transect, Site, Nitrate_N, Ammonium)
#ggpairs(soil.dynamics[,c(4:6)]) 

# ----- question 0: does soil N positively related to grass N? ------ # 
# yes according to Sitters 2020. According to Augustine et al 2003, N is the primary limiting nutrient on nutrient rich glades
# total soil N = (nitrate + ammonium) * 4
soil.vegetation <- read_csv("./data/cleaned_grass_data.csv") %>%
  select(Year, Month, Transect, Site, E, Protein, Fibre, Biomass, Avg_Height, Percent_Grazed, Total_Gap_Size) %>%
  left_join(soil.dynamics) %>%
  mutate(total_soil_N = (Nitrate_N + Ammonium) * 4)

summary(lm(Protein ~ log(total_soil_N), data = soil.vegetation)) # - nope # soil N alone does not correlated with grass N. 
summary(lm(Biomass ~ log(total_soil_N), data = soil.vegetation)) # - nope 
summary(lm(Avg_Height ~ log(total_soil_N), data = soil.vegetation)) # - negatively; which should mostly related to grazing 

# ----- overview of 28 months animal usage to estimate overall "grazing pressure" ------- # 
full.dat <- read_csv("./data/for-spp-relationship/mara-cooccurence-compiled.csv") %>% 
  select(-Northing, - Easting) %>%
  left_join(soil.vegetation) %>% 
  mutate(total_dung = Cattle + Wildebeest + Zebra + Thompsons_Gazelle + Impala + Topi + Eland + Buffalo +
           Grants_Gazelle + Waterbuck + Dikdik + Elephant, 
         livestock_dung = Cattle, 
         mega_dung = Elephant, 
         meso_dung = Wildebeest + Zebra + Thompsons_Gazelle + Impala + Topi + Eland + Buffalo +
           Grants_Gazelle + Waterbuck + Dikdik,
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%
  filter(total_soil_N < 1500,
         !is.na(total_dung)) 

###  can # of animal dung predict forage quantity  ---- 
summary(lm(Total_Gap_Size ~ total_dung, data = full.dat)) # - NAH
# summary(lm(Total_Gap_Size ~ livestock_dung, data = full.dat))
# summary(lm(Total_Gap_Size ~ mega_dung, data = full.dat))
# summary(lm(Total_Gap_Size ~ meso_dung + mega_dung + livestock_dung, data = full.dat))

summary(lm(Biomass ~ total_dung, data = full.dat)) # - nah 

summary(lm(Avg_Height ~ total_dung, data = full.dat)) # - significant, NEGATIVE

summary(lm(NDVI ~ total_dung, data = full.dat))  # -- significant, negative 

summary(lm(total_soil_N ~ total_dung, data = full.dat))  # --- significant, but negative.  shouldn;t it be positive? maybe there is a lag...

###  can # of animal dung predict forage quality  ---- 
summary(lm(Protein ~ total_dung, data = full.dat))  # -- significant, positive
summary(lm(Fibre~ total_dung, data = full.dat))  # significant, negative
summary(lm(E ~ total_dung, data = full.dat))  # significant, positive
# summary(lm(Protein ~ meso_dung + mega_dung + livestock_dung, data = full.dat))


# ----- question 1: does the presence of cattle lead to lower soil quality (soil N)? ------ # 
## the negative relationship was shown by Sitters 2020; 

#######
# hypotheses 1.1 - sudden/instantaneous intense cattle use will lead to soil N responses in the following months 
#######

hist(full.dat$total_soil_N)  # should use gaussian with log link, which means the underlying model is ln(Y) ~ A + B + ...

# Collinearity does not violate any assumptions of GLMs (unless there is perfect collinearity). so it is ok to use 

# model 1, regular glm without coordinates 
glmGammaLog.1 <- glm(total_soil_N ~ livestock_dung + Precip , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1)$aic #[1] 18467.89

# model 2, add time 
glmGammaLog.2 <- glm(total_soil_N ~ livestock_dung + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2)$aic # [1] 18366.77  # better when months is controlled

# now testing whether adding a quadratic term is better
glmGammaLog.3 <- glm(total_soil_N ~ livestock_dung + I(livestock_dung^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.3)$aic # [1] 18367.7  # hmm very similar AIC. choose the more parsimonious model

### now testing whether there's spatial autocorrelation 
#visualize spatial autocorrelation in the resituals 
nbc <- 20 # so that each step is 1km
cor_r <- pgirmess::correlog(coords=full.dat[,c("x", "y")],
                            z=glmGammaLog.2$residuals,
                            method="Moran", nbclass=nbc)

cor_r #<<<--- no signigicant spatial autocurrelation in the residuals 
correlograms <- as.data.frame(cor_r)
correlograms$variable <- "residuals_glm"   
# Plot correlogram
p <- ggplot(subset(correlograms, variable=="residuals_glm"), aes(dist.class, coef)) + 
  geom_hline(yintercept = 0, col="grey") +
  geom_line(col="steelblue") + 
  geom_point(col="steelblue") +
  xlab("distance") + 
  ylab("Moran's coefficient")+
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# ggsave("./figures/supp_fig_glm_correlograms.png", p, 
#        width = 10, height = 4, device = ragg::agg_png)   # <- no spatial autocorrelation, good. 

best.model.1.1 <- glmGammaLog.2
summary(best.model.1.1)  # livestock dung and precipitation negatively influence soil N

######
# below code is useful if there are spatial autocorrelation in residual variance and needs to be taken care of 
#####
# model 3, spatial autocorrelation in residual structure 
# using glmmfields to incorperate spatial autocorrelation in residual structure   ## <<< --- no need to this now! 
# another method is spaMM package 
# library(glmmfields)
# d.test <- data.frame(total_soil_N = full.dat$total_soil_N, total_dung = full.dat$total_dung, Precip = full.dat$Precip,
#                      lat = full.dat$lat, lon = full.dat$lon)
# glmGammalog.4.2 <-  glmmfields(total_soil_N ~ total_dung + Precip,
#                              data = d.test, family = Gamma(link = "log"),
#                              lat = "lat", lon = "lon", nknots = 12, iter = 10000, chains = 4,  # chains are still not mixed.. 
#                              prior_intercept = student_t(3, 0, 10),
#                              prior_beta = student_t(3, 0, 3),
#                              prior_sigma = half_t(3, 0, 3),
#                              prior_gp_theta = half_t(3, 0, 10),
#                              prior_gp_sigma = half_t(3, 0, 3),
#                              seed = 1222 # passed to rstan::sampling()
# )

#######
# hypotheses 1.2 - domestic cattle negatively influence soil N over long term. we should see poorer soil N near park edge 
#######

glmGammaLog.1.2.1 <- glm(total_soil_N ~ Site + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.2)$aic

glmGammaLog.1.2.2 <- glm(total_soil_N ~ Site + I(Site^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.2.2)$aic 

best.model.1.2 <- glmGammaLog.1.2.1 
summary(best.model.1.2 )  # distance to border does not significantly predict soil N

# ----- question 2: how does total space use intensity influence soil quality? --- #
# -- hypothesis - the influence is not linear 
glmGammaLog.2.1 <- glm(total_soil_N ~ total_dung  + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2.1)$aic

glmGammaLog.2.2 <- glm(total_soil_N ~ total_dung + I(total_dung^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2.2)$aic  # <- quadratic is better! makes sense

best.model.2 <- glmGammaLog.2.2
summary(best.model.2)

### --- visualize the model prediction ----- ###
new.dat.dry <- data.frame(total_dung = seq(0,300,1),
                      Precip = rep(46.67278, 301),
                      sin_month = rep(sin(2*pi*5/12), 301),
                      cos_month = rep(cos(2*pi*5/12), 301))
pred <- predict(best.model.2, new.dat.dry, type = "response", se.fit = TRUE)
new.dat.dry$total_soil_N <- pred$fit
new.dat.dry$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.dry$lwr <- pred$fit - 1.96 * pred$se.fit

new.dat.wet <- data.frame(total_dung = seq(0,300,1),
                          Precip = rep(103.28157, 301),
                          sin_month = rep(sin(2*pi*12/12), 301),
                          cos_month = rep(cos(2*pi*12/12), 301))
pred <- predict(best.model.2, new.dat.wet, type = "response", se.fit = TRUE)
new.dat.wet$total_soil_N <- pred$fit
new.dat.wet$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.wet$lwr <- pred$fit - 1.96 * pred$se.fit

pred.dat <- rbind(new.dat.wet %>% mutate (season = "wet"), 
                  new.dat.dry %>% mutate (season = "dry"))

pred.dat %>% ggplot(aes(fill = season)) +
  geom_ribbon(aes( x = total_dung,  ymin = lwr, ymax = upr, alpha = 0.2)) +
  geom_line(aes(x = total_dung, y = total_soil_N))  ## << --- will make it look better using ggdist::stat_linearibbon() 


# ----- question 3: which species influence soil quality?  -------- #
# model selection on soil PC over all species.
glmGammaLog.3 <- glm(total_soil_N ~ Cattle + Wildebeest + Zebra + Thompsons_Gazelle + Impala + 
                       Topi + Eland + Buffalo + Grants_Gazelle + Waterbuck +
                       Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))

summary(glmGammaLog.3)  ## <- the issue of this model is that it does not consider all the interactions among different species. the abundance of dung is not independent. so probably not appropriate

library(broom)
model.estimate <- tidy(glmGammaLog.3, conf.int = T) %>%
  filter(!term %in% c("(Intercept)", "Precip", "sin_month", "cos_month"))

ggplot(model.estimate, aes(x = term, y = estimate)) +
  geom_hline(yintercept = 0, color = "grey") +
  geom_errorbar(aes(ymin = conf.low, ymax = conf.high), width = 0.2, size = 1, position = "dodge") +
  geom_point() +
  coord_flip()

# ----- caveates ------ #
## cattle dung could be an underestiamte of actual use becasuse they can poop most inside of corrals. 
## heavy rain could wash away some dungs and lead to lower dung detection in rain season. But we already know lower use of Mara for all species during rain season. 

a <- soil.vegetation %>% mutate(
  Month = as.character(Month),
  Month = str_pad(Month, width=2, side="left", pad="0"),
  Yr_Month = as.numeric(paste0(Year, Month))) %>%
  group_by(Transect, Site) %>%
  mutate(time.stamp = row_number(Yr_Month))

# we did not measure all N pools (i.e. grass, soil, tree)