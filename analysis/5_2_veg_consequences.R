## this script apply the HMSC framework to 
## examine relationships between vegetation properties to grazing intensity 

# ----- set up ----- # 
library(tidyverse)
library(lme4)
library(GGally)

## get soil data ## 
vegetation <- read_csv("./data/cleaned_grass_data.csv")
#ggpairs(vegetation [,c(4,8:10, 13:18, 20, 22, 23, 25:29, 33:36)]) 

full.dat <- read_csv("./data/for-spp-relationship/mara-cooccurence-compiled.csv") %>% 
  select(-Northing, - Easting) %>%
  left_join(vegetation) %>% 
  mutate(total_dung = Cattle + Wildebeest + Zebra + Thompsons_Gazelle + Impala + Topi + Eland + Buffalo +
           Grants_Gazelle + Waterbuck + Dikdik + Elephant, 
         livestock_dung = Cattle, 
         mega_dung = Elephant, 
         meso_dung = Wildebeest + Zebra + Thompsons_Gazelle + Impala + Topi + Eland + Buffalo +
           Grants_Gazelle + Waterbuck + Dikdik,
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%
  filter(!is.na(Protein),
    !is.na(total_dung)) 

###  exploration - can # of animal dung predict forage quantity  ---- 
#summary(lm(Total_Gap_Size ~ total_dung, data = full.dat)) # - NAH
#summary(lm(Total_Gap_Size ~ livestock_dung, data = full.dat))
#summary(lm(Total_Gap_Size ~ mega_dung, data = full.dat))
#summary(lm(Total_Gap_Size ~ meso_dung + mega_dung + livestock_dung, data = full.dat))
# summary(lm(Biomass ~ total_dung, data = full.dat)) # - nah 

summary(lm(Avg_Height ~ total_dung, data = full.dat)) # - significant, NEGATIVE

summary(lm(NDVI ~ total_dung, data = full.dat))  # -- significant, negative 

###  can # of animal dung predict forage quality  ---- 
summary(lm(Protein ~ total_dung, data = full.dat))  # -- significant, positive
#summary(lm(Fibre~ total_dung, data = full.dat))  # significant, negative
#summary(lm(E ~ total_dung, data = full.dat))  # significant, positive

# ----- question 1: does the presence of cattle lead to lower forage quality (forage N)? ------ # 
# hypotheses 1.1 - lower vegetation quality after cattle use
hist(full.dat$Protein)  # should use gaussian with log link, which means the underlying model is ln(Y) ~ A + B + ...

# Collinearity does not violate any assumptions of GLMs (unless there is perfect collinearity). so it is ok to use 

# model 1, regular glm without coordinates 
glmGammaLog.1.1 <- glm(Protein ~ livestock_dung + Precip , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.1)$aic #[1] 7533.085

# model 2, add time 
glmGammaLog.1.2 <- glm(Protein ~ livestock_dung + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.2)$aic # [1] 7413.761

# now testing whether adding a quadratic term is better
glmGammaLog.1.3 <- glm(Protein ~ livestock_dung + I(livestock_dung^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.3)$aic # [1]7680.427  # hmm very similar AIC. choose the more parsimonious model

glmGammaLog.1.4 <- glm(Protein ~ livestock_dung + Precip + sin_month + cos_month + x + y, data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.4)$aic #7680.648

### now testing whether there's spatial autocorrelation 
#visualize spatial autocorrelation in the resituals 
nbc <- 20 # so that each step is 1km
cor_r <- pgirmess::correlog(coords=full.dat[,c("x", "y")],
                            z=glmGammaLog.1.2$residuals,
                            method="Moran", nbclass=nbc)

# cor_r #<<<--- no signigicant spatial autocurrelation in the residuals 
correlograms <- as.data.frame(cor_r)
correlograms$variable <- "residuals_glm"   
# Plot correlogram
p <- ggplot(subset(correlograms, variable == "residuals_glm"), aes(dist.class, coef)) + 
  geom_hline(yintercept = 0, col="grey") +
  geom_line(col="steelblue") + 
  geom_point(col="steelblue") +
  ylim(-0.023, 0.007) +
  xlab("distance") + 
  ylab("Moran's coefficient") +
  theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.background = element_blank(), axis.line = element_line(colour = "black"))
# ggsave("./figures/supp_fig_glm_vegprotein_correlograms.png", p,
#        width = 6, height = 4, device = ragg::agg_png)

best.model.1.1 <- glmGammaLog.1.2
summary(best.model.1.1)  # positively correlated. 

# what about total dung 
glmGammaLog.1.5 <- glm(Protein ~ total_dung + Precip + sin_month + cos_month, data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.5)$aic #7246.203
glmGammaLog.1.6 <- glm(Protein ~ total_dung + I(total_dung^2) + Precip + sin_month + cos_month, data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.6)$aic # 7121.833
glmGammaLog.1.7 <- glm(Protein ~ total_dung + I(total_dung^2) + Precip + sin_month + cos_month + x + y, data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.7)$aic # 7117.374

summary(glmGammaLog.1.7)

best.model.total <- glmGammaLog.1.7

### --- visualize the model prediction ----- ###
new.dat.dry <- data.frame(total_dung = seq(0,300,1),
                          Precip = rep(46.67278, 301),
                          sin_month = rep(sin(2*pi*5/12), 301),
                          cos_month = rep(cos(2*pi*5/12), 301), 
                          x = mean(full.dat$x),
                          y = mean(full.dat$y))
pred <- predict(best.model.total, new.dat.dry, type = "response", se.fit = TRUE)
new.dat.dry$Protein <- pred$fit
new.dat.dry$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.dry$lwr <- pred$fit - 1.96 * pred$se.fit

new.dat.wet <- data.frame(total_dung = seq(0,300,1),
                          Precip = rep(103.28157, 301),
                          sin_month = rep(sin(2*pi*12/12), 301),
                          cos_month = rep(cos(2*pi*12/12), 301), 
                          x = mean(full.dat$x),
                          y = mean(full.dat$y))
pred <- predict(best.model.total, new.dat.wet, type = "response", se.fit = TRUE)
new.dat.wet$Protein <- pred$fit
new.dat.wet$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.wet$lwr <- pred$fit - 1.96 * pred$se.fit

pred.dat <- rbind(new.dat.wet %>% mutate (season = "wet"), 
                  new.dat.dry %>% mutate (season = "dry"))

pred.dat %>% ggplot(aes(fill = season)) +
  geom_ribbon(aes( x = total_dung,  ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(x = total_dung, y = Protein))  ## << --- will make it look better using ggdist::stat_linearibbon() 


# hypotheses 1.2 - lower vegetation quality near park edge 1) if domestic cattle negatively influence soil N. 

glmGammaLog.1.2.1 <- glm(Protein ~ Site + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.2.1)$aic #7693.691
glmGammaLog.1.2.2 <- glm(Protein ~ Site + I(Site^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.1.2.2)$aic #7682.327

# nbc <- 20 # so that each step is 1km
# cor_r <- pgirmess::correlog(coords=full.dat[,c("x", "y")],
#                             z=glmGammaLog.1.2.2$residuals,
#                             method="Moran", nbclass=nbc)

best.model.1.2 <- glmGammaLog.1.2.2
summary(best.model.1.2 )  # distance to border significantly for protein



### --- visualize the model prediction ----- ###
new.dat.dry <- data.frame(Site = seq(0,12,0.1),
                          Precip = rep(46.67278, 121),
                          sin_month = rep(sin(2*pi*5/12), 121),
                          cos_month = rep(cos(2*pi*5/12), 121))
pred <- predict(best.model.1.2, new.dat.dry, type = "response", se.fit = TRUE)
new.dat.dry$Protein <- pred$fit
new.dat.dry$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.dry$lwr <- pred$fit - 1.96 * pred$se.fit

new.dat.wet <- data.frame(Site = seq(0,12,0.1),
                          Precip = rep(103.28157, 121),
                          sin_month = rep(sin(2*pi*12/12), 121),
                          cos_month = rep(cos(2*pi*12/12), 121))
pred <- predict(best.model.1.2, new.dat.wet, type = "response", se.fit = TRUE)
new.dat.wet$Protein <- pred$fit
new.dat.wet$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.wet$lwr <- pred$fit - 1.96 * pred$se.fit

pred.dat <- rbind(new.dat.wet %>% mutate (season = "wet"), 
                  new.dat.dry %>% mutate (season = "dry"))

pred.dat %>% ggplot(aes(fill = season)) +
  geom_ribbon(aes( x = Site,  ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(x = Site, y = Protein))  ## << --- will make it look better using ggdist::stat_linearibbon() 
# so protein is actually the lowest ~ 6 km

# ----- question 2: does the presence of cattle lead to lower forage quantity (Ave_Height)? ------ # 
# hypotheses 2.1 - lower vegetation quality after cattle use
glmGammaLog.2.1.1 <- glm(Avg_Height ~ livestock_dung + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2.1.1)$aic # [1] 11491.9

# now testing whether adding a quadratic term is better
glmGammaLog.2.1.2 <- glm(Avg_Height ~ livestock_dung + I(livestock_dung^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2.1.2)$aic # [1] 11493.9  # hmm very similar AIC. choose the more parsimonious model

# nbc <- 20 # so that each step is 1km 
# cor_r <- pgirmess::correlog(coords=full.dat[,c("x", "y")],
#                             z=glmGammaLog.2.1.1$residuals,
#                             method="Moran", nbclass=nbc)   # bad selection of class break

best.model.2.1 <- glmGammaLog.2.1.1
summary(best.model.2.1)

# glmGammaLog.2.2.3 <- glm(Avg_Height ~ livestock_dung + Precip + sin_month + cos_month + x + y, data = full.dat, family = Gamma(link = "log"))
# summary(glmGammaLog.1.4)$aic #7680.648

# hypotheses 2.2- lower vegetation quantity near park edge 1) if domestic cattle negatively influence soil N. 
glmGammaLog.2.2.1 <- glm(Avg_Height ~ Site + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2.2.1)$aic #11464.8
glmGammaLog.2.2.2 <- glm(Avg_Height ~ Site + I(Site^2) + Precip + sin_month + cos_month , data = full.dat, family = Gamma(link = "log"))
summary(glmGammaLog.2.2.2)$aic #11446.65

# nbc <- 20 # so that each step is 1km
# cor_r <- pgirmess::correlog(coords=full.dat[,c("x", "y")],
#                             z=glmGammaLog.2.2.2$residuals,
#                             method="Moran", nbclass=nbc)

best.model.2.2 <- glmGammaLog.2.2.2 
summary(best.model.2.2)

### --- visualize the model prediction ----- ###
new.dat.dry <- data.frame(Site = seq(0,12,0.1),
                          Precip = rep(46.67278, 121),
                          sin_month = rep(sin(2*pi*5/12), 121),
                          cos_month = rep(cos(2*pi*5/12), 121))
pred <- predict(best.model.2.2, new.dat.dry, type = "response", se.fit = TRUE)
new.dat.dry$Avg_Height <- pred$fit
new.dat.dry$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.dry$lwr <- pred$fit - 1.96 * pred$se.fit

new.dat.wet <- data.frame(Site = seq(0,12,0.1),
                          Precip = rep(103.28157, 121),
                          sin_month = rep(sin(2*pi*12/12), 121),
                          cos_month = rep(cos(2*pi*12/12), 121))
pred <- predict(best.model.2.2, new.dat.wet, type = "response", se.fit = TRUE)
new.dat.wet$Avg_Height <- pred$fit
new.dat.wet$upr <- pred$fit + 1.96 * pred$se.fit
new.dat.wet$lwr <- pred$fit - 1.96 * pred$se.fit

pred.dat <- rbind(new.dat.wet %>% mutate (season = "wet"), 
                  new.dat.dry %>% mutate (season = "dry"))

pred.dat %>% ggplot(aes(fill = season)) +
  geom_ribbon(aes( x = Site,  ymin = lwr, ymax = upr), alpha = 0.2) +
  geom_line(aes(x = Site, y = Avg_Height))  ## << --- will make it look better using ggdist::stat_linearibbon() 
# distance to boundary for forage quantity - first increase than decrease, threshold ~ 8km

# ----- question 2: which species influence forage quality? -------- #
# model selection on vege quality PC over all species. 

# ----- caveates ------ #
# we did not measure all N pools (i.e. grass, soil, tree)