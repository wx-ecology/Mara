# this script fit multivariate model to examine how wildlife composition respond to presence/absence of cattle after controlling other environmental covariates

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

# cattle influence: cattle dung counts of the previous month, cattle dung counts of the same month

############################## prep df ########################################

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number())  # add unique id for each month

protein <- read_csv("./data/cleaned_grass_data.csv") %>%  arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number() + 5 ) %>%  # add unique id for each month. The starting month should be 5 (May 2018) but now it is 6 because we want to 
  # match the animal abundance with quality measure of the month prior
  dplyr::select(Transect, Site, month_id, Protein, Avg_Height, Percent_Grazed)

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
  rename (NDVI = NDVI.x, 
          NDVI_lag1 = NDVI.y,
          Precip = pr.x,
          Precip_lag1 = pr.y,
          Protein_lag1 = Protein,
          Height_lag1 = Avg_Height,
          Pgrazed = Percent_Grazed.x,
          Pgrazed_lag1 = Percent_Grazed.y)  # NDVI_lag1 represents the NDVI value from the previous month at the same site
# 1,680 × 35

cattle <- data %>% dplyr::select(Transect, Site, Cattle, month_id) %>%
  mutate(month_id = month_id + 1) %>% # so that cattle from a month will be matched with the next month row
  rename(Cattle_lag1 = Cattle)

data <- data %>% left_join(cattle,  by = c("Transect", "Site", "month_id")) 

data <- data %>% drop_na()  # 1,455 x 35. Some months/sites missing values. Dropped. 

#### organize all environmental covariates and standardize coefficients ####
ENV <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select(Transect, Site, Pgrazed_lag1, Precip, 
                Protein_lag1, Height_lag1, Month, Cattle_lag1, Cattle) %>%
  mutate(Site = as.numeric(Site),  # distance to boundary 
         ## -- covariates in similar magnitude, no nend to scale (and also easier for interpretation)
         #Protein_lag1 = scale_this(Protein_lag1), # forage quality of the previous month 
         #Height_lag1 = scale_this(Height_lag1), # forage quantity of the previous months
         #Precip = scale_this(Precip),  # precipitation of the current months
         #Pgrazed_lag1 = scale_this(Pgrazed_lag1), 
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%  # including sin and cos month transformation resolves the circular nature of month
  dplyr::select(-Month) # use as time stamp
#ggpairs(ENV[,2:10])   # Collinearity does not violate any assumptions of GLMs (unless there is perfect collinearity).

#### organize counts ####
COUNT <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select(Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
                Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)

# # visualize mean-variance relationships 
meanvar.plot(COUNT) # we see that the species with high means (on the x axis) also have high variances (y axis).
# # mvabund uses negative binomial which assumes a quadratic mean-variance relationship 
# # and a log-linear relationship between the response variables and any continuous variables.

#####################################################################
#### ---------- wildlife community response to cattle -------- -#####
#####################################################################
wildlife.m <- manyglm(COUNT ~ Transect + Site + Precip + 
                        Cattle + Cattle_lag1 + 
                        Pgrazed_lag1 + Height_lag1 + Protein_lag1 + 
                        sin_month + cos_month, data = ENV, family="negative.binomial")

plot(wildlife.m) 

##################### ------------------- hypothesis testing --------------- ###############
# use resampling to test for significant community level or species level responses to our predictors.
# cor.type I should suffice, according to the package recommendation.
# still ran all three types of cor types to make sure results are consistent.
# # ---- already ran, read RSD for further analysis

# run all three types of correlation structure to make sure the results are stable

# anova.adj.I <- anova(wildlife.m, p.uni = "adjusted", show.time = "all", rep.seed = T,  cor.type = "I")
#           # The “adjusted” part of the argument refers to the resampling method used to compute the p values, taking into account the correlation between the response variables.
#           # the default bootstrappingmethod is "PIT-trap", bootstraps probability integral transform residuals, which we have found to give the most reliable Type I error rates.
#           # Time elapsed: 0 hr 31 min 55 sec
# saveRDS(anova.adj.I, file = "./results/manyglm_wildlife_anova_I.rds")

# The cor.type="shrink" option applies ridge regularisation (Warton 2008), shrinking the sample
# correlation matrix towards the identity, which improves its stability when p is not small compared
# to N. This provides a compromise between "R" and "I", allowing us to account for correlation
# between variables, while using a numerically stable test statistic that has good properties.

#anova.adj.S <- anova(wildlife.m, p.uni = "adjusted", show.time = "all", rep.seed = T, cor.type = "shrink")
#saveRDS(anova.adj.S, file = "./results/manyglm_wildlife_anova_S.rds")

# The cor.type="R" option uses the unstructured correlation
# matrix (only possible when N>p), such that the standard classical multivariate test statistics are
# obtained. Note however that such statistics are typically numerically unstable and have low power
# when p is not small compared to N.

#anova.adj.R <- anova(wildlife.m, p.uni = "adjusted", show.time = "all", rep.seed = T, cor.type = "R")
#saveRDS(anova.adj.R, file = "./results/manyglm_wildlife_anova_R.rds")

anova.adj.I <- readRDS("./results/manyglm_wildlife_anova_I.rds")
anova.adj.S <- readRDS("./results/manyglm_wildlife_anova_S.rds")
anova.adj.R <- readRDS("./results/manyglm_wildlife_anova_R.rds")


# multivariate (community composition) results
data.frame(covariate = row.names(anova.adj.I$table),
           I = anova.adj.I$table$`Pr(>Dev)`,
           S = anova.adj.S$table$`Pr(>wald)`,
           R = anova.adj.R$table$`Pr(>wald)`)  # <- cattle become not significant if consider S or R covariate structure. Go with S acrroding to package recommendation.

# univariate (single species response) results
list( anova.adj.I$uni.p,  anova.adj.S$uni.p,  anova.adj.R$uni.p)
# only two values differ (topi - precip and waterbuck-precip), I is not significant while S and R are significant. 
# because S considers the correlation between response variables, decided to go with the S result. 


## issue with this method - cattle abundance is influenced by other environmental variables. By putting cattle as a
#@ covairate, we did not consider this relationships. ecoCopula, on the other hand, solves this issue but looking at 
## species interactions after taking out environmental influences.