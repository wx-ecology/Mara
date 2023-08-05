pal12 <- c( "#fdcce5", "#bd7ebe",  "#BF7E7E", "#ffb55a",     "#ffee65",     "#beb9db",   "#b2e061",   "#baba97",    "#E9C09B",   "#7eb0d5",   "#8bd3c7" , "#fd7f6f" )
#         "Buffalo" ,  "Dikdik"  ,"Eland"  ,"Elephant", "Grants_Gazelle",  "Impala",  "Thompsons_Gazelle", "Topi",   "Waterbuck", "Wildebeest", "Zebra"  ,  "Cattle" 

library(tidyverse)
library(ecoCopula)
library(lubridate)
library(GGally)
library(tidygraph)
library(ggraph)
library(corrplot)
library(igraph)
#Copulas are a way to construct a multivariate distribution, 
# can be used as an alternative to hierarchical models 
# and generalised estimating equations (GEE; as in mvabund).
# 
# ################### define function ##########################
# # define standardization (and centering) function 
# 
# scale_this <- function(x){
#   (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
# }


############################## covariates note ################################
# environmental variables that likely contribute to species usage 
# current month: precipitation, 
# previous month: forage quantity (ave_height), forage quality (Protein), percentage grazed
                       # animals do not necessarily graze grass to the root, 
                       # so % graze and standing biomass might have different info.
# distance to border (aka site) 
# time (month) stamp 

############################## prep df ########################################
data <- read_csv("./data/mara-cooccurence-compiled-subset.csv") # 1,680 × 40
data <- data %>% drop_na()  # 1,455 x 35. Some months/sites missing values. Dropped. 

#### organize all environmental covariates and standardize coefficients ####
# # For comparing coefficients for different predictors within a model, standardizing gets the nod. 
# # https://statmodeling.stat.columbia.edu/2009/07/11/when_to_standar/ 
ENV <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select( 
        Transect, Site, Pgrazed_lag1, Precip, 
        Protein_lag1, Height_lag1, Month) %>%
  mutate(Site = as.numeric(Site),  # distance to boundary 
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%  # including sin and cos month transformation resolves the circular nature of month
  dplyr::select(-Month) # use as time stamp
# ggpairs(ENV[,2:8])   # Collinearity does not violate any assumptions of GLMs (unless there is perfect collinearity).

#### organize counts ####
COUNT <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  dplyr::select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
         Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)

###############################################################################################
###### ---------  community composition in response to environment using ecoCopula ------- ####
###############################################################################################
# now run the graphic model
mara_nb <- stackedsdm(COUNT,~., data = ENV, family="negative.binomial")
mara_gr <- cgr(mara_nb, seed=5)  #seed for demonstration
# plot(mara_gr, pad=0.15)  # the final model you plot is the best model chosen by BIC.


valid.m <- manyglm(COUNT ~ ., data = ENV, family="negative.binomial")
plot(valid.m) 

#########################################################################
############ ------- calculating graph metrics --------- ################
#########################################################################
igraph_out <- mara_gr$best_graph$igraph_out

betweenness(igraph_out)
degree(igraph_out)
#######################################################
############--------  visualization  ------- ##########
#######################################################
corrplot(mara_gr$best_graph$cov,  
         method = "circle", type = "lower",tl.col = "black",
         col = colorRampPalette(c("#ff5e1f","#ffffff","#389bd9"))(200))

cov_out <- mara_gr$best_graph$part
saveRDS(cov_out, "./results/ecoCoupla_partcor_network.RDS")
