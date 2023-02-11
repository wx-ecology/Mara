# this script fit multivariate model to examine whether community structure is influenced by env covariates, and how animal composition change in response to distance to boundary

pal12 <- c( "#fdcce5", "#bd7ebe",  "#BF7E7E", "#ffb55a",     "#ffee65",     "#beb9db",   "#b2e061",   "#baba97",    "#E9C09B",   "#7eb0d5",   "#8bd3c7" , "#fd7f6f" )
#         "Buffalo" ,  "Dikdik"  ,"Eland"  ,"Elephant", "Grants_Gazelle",  "Impala",  "Thompsons_Gazelle", "Topi",   "Waterbuck", "Wildebeest", "Zebra"  ,  "Cattle" 

library(tidyverse)
library(lubridate)
library(GGally)
library(tidygraph)
library(ggraph)
library(gridExtra)
library(mvabund)

################### define function ##########################
# define standardization (and centering) function 
scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

# define function, return a prediction table varying with a target variable 
get_prediction <- function(model, new_data, target_variable) {
  pred.dist <- predict(model, new_data, se.fit = T, type =  "response")
  fit.low <- data.frame((pred.dist$fit - 1.96*pred.dist$se.fit), 
                        new_data[,which(colnames(new_data) == target_variable)],
                        Transect = new_data$Transect) %>% 
    pivot_longer(1:12, names_to = "species", values_to = "fit_low")  #1.96 is to turn se into 95CI
  fit.high <- data.frame((pred.dist$fit + 1.96*pred.dist$se.fit), 
                         new_data[,which(colnames(new_data) == target_variable)],
                         Transect = new_data$Transect) %>% 
    pivot_longer(1:12, names_to = "species", values_to = "fit_high")
  fit <- data.frame(pred.dist$fit, new_data[,which(colnames(new_data) == target_variable)], Transect = new_data$Transect) %>% 
    pivot_longer(1:12, names_to = "species", values_to = "fit") %>%
    left_join(fit.low, by = c(target_variable, "species", "Transect")) %>%
    left_join(fit.high, by = c(target_variable, "species", "Transect")) %>%
    mutate( species = factor(species, levels = c( "Buffalo" ,  "Dikdik"  ,"Eland"  ,
                                                  "Elephant", "Grants_Gazelle",  "Impala",  "Thompsons_Gazelle",
                                                  "Topi",   "Waterbuck", "Wildebeest", "Zebra",  "Cattle" )))
  return(fit)
}

#extract legend
#https://github.com/hadley/ggplot2/wiki/Share-a-legend-between-two-ggplot2-graphs
g_legend<-function(a.gplot){
  tmp <- ggplot_gtable(ggplot_build(a.gplot))
  leg <- which(sapply(tmp$grobs, function(x) x$name) == "guide-box")
  legend <- tmp$grobs[[leg]]
  return(legend)}

############################## covariates note ################################
# environmental variables that likely contribute to species usage 
# current month: precipitation, 
# previous month: forage quantity (ave_height), forage quality (Protein), percentage grazed
# animals do not necessarily graze grass to the root, 
# so % graze and standing biomass might have different info.
# distance to border (aka site) 
# time (month) stamp 

############################## prep df ########################################

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number())  # add unique id for each month

protein <- read_csv("./data/cleaned_grass_data.csv") %>%  arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number() + 5 ) %>%  # add unique id for each month. The starting month should be 5 (May 2018) but now it is 6 because we want to 
  # match the animal abundance with quality measure of the month prior
  dplyr::select(Transect, Site, month_id, Protein, Avg_Height, Percent_Grazed)

sample_sites <- read_csv("./data/Sampling_site.csv") %>%  # for getting coordinates
  filter(Location == "Start") %>%
  dplyr::select(-Location) %>%
  mutate(Name = paste0(Transect,Site))

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
          Pgrazed_lag1 = Percent_Grazed.y)  # NDVI_lag1 represents the NDVI value from the previous month at the same site
# 1,680 × 35
data <- data %>% drop_na()  # 1,455 x 35. Some months/sites missing values. Dropped. 

#### organize all environmental covariates and standardize coefficients ####
# # For comparing coefficients for different predictors within a model, standardizing gets the nod. 
# # https://statmodeling.stat.columbia.edu/2009/07/11/when_to_standar/ 
ENV <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select(Transect, Site, Pgrazed_lag1, Precip, 
                Protein_lag1, Height_lag1, Month) %>%
  mutate(Site = as.numeric(Site),  # distance to boundary 
         ## -- covariates in similar magnitude, no nend to scale (and also easier for interpretation)
         #Protein_lag1 = scale_this(Protein_lag1), # forage quality of the previous month 
         #Height_lag1 = scale_this(Height_lag1), # forage quantity of the previous months
         #Precip = scale_this(Precip),  # precipitation of the current months
         #Pgrazed_lag1 = scale_this(Pgrazed_lag1), 
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%  # including sin and cos month transformation resolves the circular nature of month
  dplyr::select(-Month) # use as time stamp
#ggpairs(ENV[,2:8])   # Collinearity does not violate any assumptions of GLMs (unless there is perfect collinearity).

#### organize counts ####
COUNT <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
                Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)

# # visualize mean-variance relationships 
meanvar.plot(COUNT) # we see that the species with high means (on the x axis) also have high variances (y axis).
# # mvabund uses negative binomial which assumes a quadratic mean-variance relationship 
# # and a log-linear relationship between the response variables and any continuous variables.

#########################################################################################
#### ---------- community and species environmental response with mvabund -------- -#####
#########################################################################################
# manyglm first fit a single generalized linear model (GLM) to each response variable with a common set of predictor variables.

mara.m <- manyglm(COUNT ~ Transect + Site + Pgrazed_lag1 + 
                    Precip + Protein_lag1 + Height_lag1 + 
                    sin_month + cos_month, data = ENV, family="negative.binomial")
# equivalent to mara.m <- manyglm(COUNT ~., data = ENV, family="negative.binomial"); but the formula needs to be spell out for correctly run prediction

mara.m2 <- manyglm(COUNT ~ Site + Pgrazed_lag1 + 
                     Precip + Protein_lag1 + Height_lag1 + 
                     sin_month + cos_month, data = ENV, family="negative.binomial")

# test <- anova(mara.m, mara.m2)
# writeRDS(test, "./results/manyglm_allspp_model_compare.RDS")
test <- readRDS("./results/manyglm_allspp_model_compare.RDS")
test  ## mara.m is a better fit. adding transect does improve the fit.

mara.m3 <- manyglm(COUNT ~ Transect + Site +  
                    Precip + Protein_lag1 + Height_lag1 + 
                    sin_month + cos_month, data = ENV, family="negative.binomial")

# test2 <- anova(mara.m, mara.m3)
# writeRDS(test2, "./results/manyglm_allspp_model_compare2.RDS")
test2 <- readRDS("./results/manyglm_allspp_model_compare2.RDS")
test2 ## still shows mara.m is a better fit.

# plot of residuals show that the model assumption is met and the model is a good fit.
plot(mara.m) 

# visually examine spatial autocorrelation in residuals. 
# examine whether residuals are spatially autocorrelated 
df.cor <- data.frame(mara.m$residuals, x = data$X, y = data$Y) %>%
  pivot_longer(1:12, names_to = "species", values_to = "residual") %>%
  mutate(color = case_when(residual >=0 ~ "red", TRUE ~ "blue"))

df.cor %>% 
#  filter(species == "Buffalo") %>%
  ggplot(aes(x = x, y = y, size = residual, color = color)) +
  geom_point(alpha = 0.3) +
  facet_wrap("species") +
  theme_minimal()

##################### ------------------- hypothesis testing --------------- ###############
# use resampling to test for significant community level or species level responses to our predictors.
# cor.type I should suffice, according to the package recommendation.
# still ran all three types of cor types to make sure results are consistent.
# # ---- already ran, read RSD for further analysis 

# # run all three types of correlation structure to make sure the results are stable
#  anova.adj.I <- anova(mara.m, p.uni = "adjusted", show.time = "all", rep.seed = T,  cor.type = "I")
# #           # The “adjusted” part of the argument refers to the resampling method used to compute the p values, taking into account the correlation between the response variables.
# #           # the default bootstrappingmethod is "PIT-trap", bootstraps probability integral transform residuals, which we have found to give the most reliable Type I error rates.
# #           # Time elapsed: 0 hr 31 min 55 sec
#  saveRDS(anova.adj.I, file = "./results/manyglm_allspp_anova_I.rds")

# # The cor.type="shrink" option applies ridge regularisation (Warton 2008), shrinking the sample
# # correlation matrix towards the identity, which improves its stability when p is not small compared
# # to N. This provides a compromise between "R" and "I", allowing us to account for correlation
# # between variables, while using a numerically stable test statistic that has good properties.
# anova.adj.S <- anova(mara.m, p.uni = "adjusted", show.time = "all", rep.seed = T, cor.type = "shrink")
# saveRDS(anova.adj.S, file = "./results/manyglm_allspp_anova_S.rds")

# # The cor.type="R" option uses the unstructured correlation
# # matrix (only possible when N>p), such that the standard classical multivariate test statistics are
# # obtained. Note however that such statistics are typically numerically unstable and have low power
# # when p is not small compared to N.
# anova.adj.R <- anova(mara.m, p.uni = "adjusted", show.time = "all", rep.seed = T, cor.type = "R")
# saveRDS(anova.adj.R, file = "./results/manyglm_allspp_anova_R.rds")

anova.adj.I <- readRDS("./results/manyglm_allspp_anova_I.rds")
anova.adj.S <- readRDS("./results/manyglm_allspp_anova_S.rds")
anova.adj.R <- readRDS("./results/manyglm_allspp_anova_R.rds")

# multivariate (community composition) results
data.frame(covariate = row.names(anova.adj.I$table),
           I = anova.adj.I$table$`Pr(>Dev)`,
           S = anova.adj.S$table$`Pr(>wald)`,
           R = anova.adj.R$table$`Pr(>wald)`)  # <- nearly identical multivariate result

# univariate (single species response) results
list( anova.adj.I$uni.p,  anova.adj.S$uni.p,  anova.adj.R$uni.p)
# only two values differ (topi - precip and waterbuck-precip), I is not significant while S and R are significant. 
# because S considers the correlation between response variables, decided to go with the S result. 

remove(anova.adj.I, anova.adj.R)

#######################################################
#######################################################
## coefficient interval plots 
# coefplot.manyglm(mara.m, which.Xcoef=6:10)
### <<--- figure out what is the difference between this and the anova results
# this coefficient result is not after controllong for species interaction. Just for exploring.

anova.adj.S 

# the multivariate results show that all environmental covariates sinificantlty influence community conposition. (Wald test score 14.05, P = 0.001)
# the univariate result show that, after adjusting for multiple testing, there is an effect of distance to boundary to cattle, Thompsons gazelle, topi, and buffalo.

# significance table from anova #########
# # this is derived after resampling. 
# uni.p.df <- data.frame(anova.adj.S$uni.p) %>%
#   mutate(covariate = row.names(.)) %>%
#   filter(covariate != "(Intercept)") %>%
#   pivot_longer(1:12, names_to = "species", values_to = "p")

#########################################################################################
#### ----------------------------- prediction and plot   ------------------------- -#####
#########################################################################################

############--- animal use as a function of distance to boundary---######################
#### a 2-panel figure showing prediction with varing distance in ########################

target_variable = "Site"

##  1. dry month prediction ##
# set month consistent as the driest month and precipitation as the lower 25 quantile 
low.Pre = quantile(data$Precip, 0.25)
gee_data %>% ungroup() %>% dplyr::select(Year, Month, pr) %>% filter (pr <= low.Pre) %>% distinct() # select July, which has the low prep in all three years

# create a new covairate dataset, only target variable varies
ENV.dry <- tibble(
  Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
  Site = rep(seq(from = 1, to = 12, length.out = 100),5),
  Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1),500),
  Precip =   rep(low.Pre,500),
  Protein_lag1 =   rep(mean(ENV$Protein_lag1),500),
  Height_lag1 =   rep(mean(ENV$Height_lag1),500),
  sin_month =   rep(sin(2*pi*7/12), 500),# set to July, which has the low prep in all three years
  cos_month =   rep(cos(2*pi*7/12), 500)
)

predict.dist.dry <- get_prediction(model = mara.m, new_data = ENV.dry, target_variable) 

#species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
p.dist.dry <- predict.dist.dry %>% 
  group_by(species, Site) %>%
  summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
  ggplot(aes(x = Site)) + 
  geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
  geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
  xlim(1, 12) +
  ylim(0,37) +
  bbplot::bbc_style()  +
  scale_fill_manual(values = pal12) +
  scale_color_manual(values = pal12) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16, color = "grey40"),
        legend.box.margin = margin(t = 10),
        legend.background = element_rect(
          color = "grey40", 
          fill = "grey95",
          linewidth = .3)) +
  xlab("Distance to boundary (km)") +
  ylab("Predicted Utilization (Dry Season)")
#p.dist.dry

##  1. wet month prediction ##
high.Pre = quantile(data$Precip, 0.75)
gee_data %>% ungroup() %>% dplyr::select(Year, Month, pr) %>% filter (pr >= high.Pre) %>% distinct() # select Apr, which has the high prep in all three years


# create a new covairate dataset, only target variable varies
ENV.wet <- tibble(
  Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
  Site = rep(seq(from = 1, to = 12, length.out = 100),5),
  Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1),500),
  Precip =   rep(high.Pre,500),
  Protein_lag1 =   rep(mean(ENV$Protein_lag1),500),
  Height_lag1 =   rep(mean(ENV$Height_lag1),500),
  sin_month =   rep(sin(2*pi*4/12), 500),# set to Apr, which has the high prep in all three years
  cos_month =   rep(cos(2*pi*4/12), 500)
)

predict.dist.wet <- get_prediction(model = mara.m, new_data = ENV.wet, target_variable) 

#species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
p.dist.wet <- predict.dist.wet %>% 
  #  filter(species %in% species.list) %>%
  group_by(species, Site) %>%
  summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
  ggplot(aes(x = Site)) + 
  geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
  #  facet_wrap("species") + 
  geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
  xlim(1, 12) +
  ylim(0,37) +
  bbplot::bbc_style()  +
  scale_fill_manual(values = pal12) +
  scale_color_manual(values = pal12) +
  theme(axis.title = element_text(size = 16),
        axis.text = element_text(size = 14),
        legend.text = element_text(size = 16, color = "grey40"),
        legend.box.margin = margin(t = 10),
        legend.background = element_rect(
          color = "grey40", 
          fill = "grey95",
          linewidth = .3)) +
xlab("Distance to boundary (km)") +
  ylab("Predicted Utilization (Wet Season)")
p.dist.wet

mylegend<-g_legend(p.dist.dry)
p.dist.combo <- grid.arrange(arrangeGrob(p.dist.dry + theme(legend.position="none"),
                                         p.dist.wet + theme(legend.position="none"),
                               nrow=1),
                   mylegend, nrow=2,heights=c(20, 6))

ggsave("./figures/materials/species-abundance-along-dist2bound-combo.png", p.dist.combo , 
        width = 13, height = 8, device = ragg::agg_png)

















# ## ------ next make prediction of usage as a function of precipitation, forage quantity, quality at the boudnary ---- ###
# ##### -------------- target varaible 2 - Precip at the border --- #####
# target_variable = "Precip"
# summary(ENV$Precip) # 15.32 - 191.49
# 
# # create a new covairate dataset, only target variable varies
# ENV.new.2 <- tibble(
#   Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
#   Site = rep(1, 500), # at the border
#   Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1), 500),
#   Precip =   rep(seq(0, 200, length.out = 100),5),
#   Protein_lag1 =   rep(mean(ENV$Protein_lag1),500),
#   Height_lag1 =   rep(mean(ENV$Height_lag1),500),
#   sin_month =   rep(sin(2*pi*7/12), 500),# set toJuly, the month with the most counts of cattle dung 
#   cos_month =   rep(cos(2*pi*7/12), 500)
# )
# 
# predict.precip <- get_prediction(model = mara.m, new_data = ENV.new.2, target_variable) 
# 
# 
# #species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
# p.precip <- predict.precip  %>%
#   group_by(species, Precip) %>%
#   summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
#   ggplot(aes(x = Precip)) +
#   geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
#   geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   scale_color_manual(values = pal12) +
#   theme(axis.title = element_text(size = 18),
#         legend.position = "none") +
#   #  ylab("Relative usage prediction") +
#   xlab("Precipitation (mm)") 
# #p.precip
# 
# #########################################################################################
# ##########################--- DRY MONTH PREDICTION ---###################################
# #########################################################################################
# ##### -------------- target varaible 1 - Site (aka distance to border) --- #####
# 
# 
# 
# 
# ##### -------------- target varaible 3 - forage quantity (average frame height) at the border--- #####
# target_variable = "Height_lag1"
# summary(ENV$Height_lag1)  # 3.3 - 87.4 
# 
# # create a new covairate dataset, only target variable varies
# ENV.new.3 <- tibble(
#   Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
#   Site = rep(1, 500), # at the border
#   Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1), 500),
#   Precip =   rep(mean(ENV$Precip), 500),
#   Protein_lag1 =   rep(mean(ENV$Protein_lag1),500),
#   Height_lag1 =   rep(seq(0, 90, length.out = 100),5),
#   sin_month =   rep(sin(2*pi*7/12), 500),# set toJuly, the month with the most counts of cattle dung 
#   cos_month =   rep(cos(2*pi*7/12), 500)
# )
# 
# predict.quant <- get_prediction(model = mara.m, new_data = ENV.new.3, target_variable) 
# 
# #species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
# p.quant <- predict.quant %>%
#   group_by(species, Height_lag1) %>%
#   summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
#   ggplot(aes(x = Height_lag1)) +
#   geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
#   #  facet_wrap("species") +
#   geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   scale_color_manual(values = pal12) +
#   theme(axis.title = element_text(size = 18),
#         legend.position = "none") +
#   xlab("Forage quantity") 
# #ylab("Relative usage prediction")
# #p.quant 
# 
# 
# ##### -------------- target varaible 4 - forage quality (protein) at the border  --- #####
# target_variable = "Protein_lag1"
# summary(ENV$Protein_lag1)  # 6.5 - 26.4
# 
# # create a new covairate dataset, only target variable varies
# ENV.new.4 <- tibble(
#   Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
#   Site = rep(1, 500), # at the border
#   Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1), 500),
#   Precip =   rep(mean(ENV$Precip), 500),
#   Protein_lag1 =   rep(seq(0, 30, length.out = 100),5),
#   Height_lag1 =   rep(mean(ENV$Height_lag1), 500),
#   sin_month =   rep(sin(2*pi*7/12), 500),  # set toJuly, the month with the most counts of cattle dung 
#   cos_month =   rep(cos(2*pi*7/12), 500)
# )
# 
# predict.qual <- get_prediction(model = mara.m, new_data = ENV.new.4, target_variable) 
# 
# #species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
# p.qual <- predict.qual  %>%
#   group_by(species, Protein_lag1) %>%
#   summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
#   ggplot(aes(x = Protein_lag1)) +
#   geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
#   #  facet_wrap("species") +
#   geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   scale_color_manual(values = pal12) +
#   theme(axis.title = element_text(size = 18),
#         legend.position = "none") +
#   xlab("Forage quality") 
# # ylab("Relative usage prediction") 
# #p.qual 
# 
# dry <- p.precip/p.quant/p.qual
# 
# #########################################################################################
# ##########################--- WET MONTH PREDICTION ---###################################
# #########################################################################################
# 
# 
# # ggsave("./figures/species-abundance-along-dist2bound.png", p.dist, 
# #        width = 13, height = 8, device = ragg::agg_png)
# 
# ## ------ next make prediction of usage as a function of precipitation, forage quantity, quality at the boudnary ---- ###
# ##### -------------- target varaible 2 - Precip at the border --- #####
# target_variable = "Precip"
# summary(ENV$Precip) # 15.32 - 191.49
# 
# # create a new covairate dataset, only target variable varies
# ENV.new.2 <- tibble(
#   Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
#   Site = rep(1, 500), # at the border
#   Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1), 500),
#   Precip =   rep(seq(0, 200, length.out = 100),5),
#   Protein_lag1 =   rep(mean(ENV$Protein_lag1),500),
#   Height_lag1 =   rep(mean(ENV$Height_lag1),500),
#   sin_month =   rep(sin(2*pi*2/12), 500),# set toJuly, the month with the most counts of cattle dung 
#   cos_month =   rep(cos(2*pi*2/12), 500)
# )
# 
# predict.precip2 <- get_prediction(model = mara.m, new_data = ENV.new.2, target_variable) 
# 
# 
# #species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
# p.precip2 <- predict.precip2  %>%
#   group_by(species, Precip) %>%
#   summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
#   ggplot(aes(x = Precip)) +
#   geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
#   #  facet_wrap("species") +
#   geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   scale_color_manual(values = pal12) +
#   theme(axis.title = element_text(size = 18),
#         legend.position = "none") +
#   #ylab("Relative usage prediction") +
#   xlab("Precipitation (mm)")
# #p.precip
# 
# 
# ##### -------------- target varaible 3 - forage quantity (average frame height) at the border--- #####
# target_variable = "Height_lag1"
# summary(ENV$Height_lag1)  # 3.3 - 87.4 
# 
# # create a new covairate dataset, only target variable varies
# ENV.new.3 <- tibble(
#   Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
#   Site = rep(1, 500), # at the border
#   Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1), 500),
#   Precip =   rep(mean(ENV$Precip), 500),
#   Protein_lag1 =   rep(mean(ENV$Protein_lag1),500),
#   Height_lag1 =   rep(seq(0, 90, length.out = 100),5),
#   sin_month =   rep(sin(2*pi*2/12), 500),# set toJuly, the month with the most counts of cattle dung 
#   cos_month =   rep(cos(2*pi*2/12), 500)
# )
# 
# predict.quant2 <- get_prediction(model = mara.m, new_data = ENV.new.3, target_variable) 
# 
# #species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
# p.quant2 <- predict.quant2 %>%
#   group_by(species, Height_lag1) %>%
#   summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
#   ggplot(aes(x = Height_lag1)) +
#   geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
#   #  facet_wrap("species") +
#   geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   scale_color_manual(values = pal12) +
#   theme(axis.title = element_text(size = 18),
#         legend.position = "none") +
#   xlab("Forage quantity") 
# #ylab("Relative usage prediction")
# #p.quant2
# 
# 
# ##### -------------- target varaible 4 - forage quality (protein) at the border  --- #####
# target_variable = "Protein_lag1"
# summary(ENV$Protein_lag1)  # 6.5 - 26.4
# 
# # create a new covairate dataset, only target variable varies
# ENV.new.4 <- tibble(
#   Transect = rep(c("C", "D", "E", "F", "G"), each = 100),
#   Site = rep(1, 500), # at the border
#   Pgrazed_lag1 =  rep(mean(ENV$Pgrazed_lag1), 500),
#   Precip =   rep(mean(ENV$Precip), 500),
#   Protein_lag1 =   rep(seq(0, 30, length.out = 100),5),
#   Height_lag1 =   rep(mean(ENV$Height_lag1), 500),
#   sin_month =   rep(sin(2*pi*2/12), 500),  # set toJuly, the month with the most counts of cattle dung 
#   cos_month =   rep(cos(2*pi*2/12), 500)
# )
# 
# predict.qual2 <- get_prediction(model = mara.m, new_data = ENV.new.4, target_variable) 
# 
# #species.list <- pull(uni.p.df[which(uni.p.df[uni.p.df$covariate == target_variable,]$p <= 0.05),2])
# p.qual2 <- predict.qual2  %>%
#   group_by(species, Protein_lag1) %>%
#   summarise(fit = mean(fit), fit_high = mean(fit_high), fit_low = mean(fit_low)) %>%
#   ggplot(aes(x = Protein_lag1)) +
#   geom_line(aes(y = fit, group = species, color = species), size = 1.5) +
#   #  facet_wrap("species") +
#   geom_ribbon(aes(y = fit, ymin = fit_low, ymax = fit_high, group = species, fill = species), alpha = 0.3) +
#   bbplot::bbc_style()  +
#   scale_fill_manual(values = pal12) +
#   scale_color_manual(values = pal12) +
#   theme(axis.title = element_text(size = 18),
#         legend.position = "none") +
#   xlab("Forage quality") 
# #ylab("Relative usage prediction") 
# # p.qual2
# 
# wet <- p.precip2/p.quant2/p.qual2
# 
# (p.precip + p.precip2) / ( p.quant + p.quant2) / (p.qual + p.qual2)