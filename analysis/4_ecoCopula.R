library(tidyverse)
library(ecoCopula)
library(lubridate)
library(GGally)
library(tidygraph)
library(ggraph)
#Copulas are a way to construct a multivariate distribution, 
# can be used as an alternative to hierarchical models 
# and generalised estimating equations (GEE; as in mvabund).

#######################################################
################## prep df ############################
# environmental variables that likely contribute to species usage 
# current month: precipitation, 
# previous month: forage quantity (ave_height), forage quality (Protein), percentage grazed
                       # animals do not necessarily graze grass to the root, 
                       # so % graze and standing biomass might have different info.
# distance to border (aka site) 
# time (month) stamp 
#######################################################
#######################################################
# define standardization (and centering) function 

scale_this <- function(x){
  (x - mean(x, na.rm=TRUE)) / sd(x, na.rm=TRUE)
}

#######################################################
#######################################################

gee_data <- read_csv("./data/mara_gee_VI_rad.csv") %>% arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number())  # add unique id for each month

protein <- read_csv("./data/cleaned_grass_data.csv") %>%  arrange(Year, Month) %>%
  group_by(Transect, Site) %>% mutate(month_id = row_number() + 5 ) %>%  # add unique id for each month. The starting month should be 5 (May 2018) but now it is 6 because we want to 
                                                                         # match the animal abundance with quality measure of the month prior
  select(Transect, Site, month_id, Protein, Avg_Height, Percent_Grazed)

data <- read_csv("./data/cleaned_animal_data.csv") %>% 
  mutate( Date = ymd(Date), 
          Yr_Mo = format_ISO8601(Date, precision = "ym")) %>%
  arrange(Year, Month) %>%
  left_join(gee_data,  by = c("Year", "Month", "Transect", "Site"))  %>%  # this step to get NDVI of the same month 
  left_join(
    (gee_data %>% mutate(month_id = month_id + 1) %>%  
      select(Transect, Site, NDVI, month_id, pr)), by = c("Transect", "Site", "month_id") # match NDVI from the prior month
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
data <- data %>% drop_na()  # 1,455 x 35. Some months/sites missing values. Dropped. 

#### organize all environmental covariates and standardize coefficients ####
# # For comparing coefficients for different predictors within a model, standardizing gets the nod. 
# # https://statmodeling.stat.columbia.edu/2009/07/11/when_to_standar/ 
ENV <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  select(Transect, Site, Pgrazed_lag1, Precip, 
        Protein_lag1, Height_lag1, month_id) %>%
  mutate(Site = scale_this(as.numeric(Site)),  # distance to boundary 
         Protein_lag1 = scale_this(Protein_lag1), # forage quality of the previous month 
         Height_lag1 = scale_this(Height_lag1), # forage quantity of the previous months
         Precip = scale_this(Precip),  # precipitation of the current months
         Pgrazed_lag1 = scale_this(Pgrazed_lag1), 
         month_id = scale_this(month_id - 5)) # use as time stamp
 # ggpairs(ENV[,2:7]) 

#### organize counts ####
COUNT <- data %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%   # the first month does not have previous month measurement
  select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
         Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)
 
 #visualize mean-variance relationships 
 meanvar.plot(COUNT) # we see that the species with high means (on the x axis) also have high variances (y axis).

 ## mvabund uses negative binomial which assumes a quadratic mean-variance relationship 
 ## and a log-linear relationship between the response variables and any continuous variables.
 
 #######################################################
 #######################################################
 # now run the graphic model
mara_nb <- stackedsdm(COUNT,~., data = ENV, family="negative.binomial")
mara_gr <- cgr(mara_nb, seed=6)  #seed for demonstration
#plot(mara_gr, pad=1)

#######################################################
#######################################################
# visualization 
igraph_out <- mara_gr$best_graph$igraph_out

igraph_out %>% ggraph(circular = T) + # see ?layout_tbl_graph_igraph
  geom_edge_fan0(aes( colour = partcor, width=partcor)) +
  scale_edge_width(range = c(1, 3))+
  scale_edge_color_gradient2(low="#b2182b",mid="white",high="#2166ac")+
  geom_node_label(aes(label=name), repel = TRUE)+
  geom_node_point(size=2)+
  theme_void() +
  theme(legend.position = 'none') 
