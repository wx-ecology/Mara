## this script apply the HMSC framework to 
## 1. understand the relative contributions of environmental variables and biotic interactions to species occurrences
## 2. predict cattle and other species spatial use in wetter and dryer months to be used as input later soil and vegetation condition analyses

library(tidyverse)
library(Hmsc)
library(corrplot)

# -----data prep ---
data <- read_csv("./data/for-spp-relationship/mara-cooccurence-compiled.csv") %>%
  dplyr::select(-Date, -Northing, -Easting) %>% # this northing and easting is measured by GPS in the field with lots of noises. using site GPS locations instead.
  mutate(Transect = factor(Transect, levels = c("C", "D", "E", "F", "G")),
         Site = as.numeric(Site),  # distance to boundary 
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>% 
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%  # the first month does not have previous month measurement
  # resulting in 1520 * 38 valid sampling units
  mutate(sample.id = as.factor(seq(1:nrow(data))))

# community data 
Y = data %>% 
  dplyr::select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
                Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)

dim(Y) # 12 species in 1520 sampling units
#S = rowSums(Y)
#P = colMeans(Y)

# environmental data 
XData <- data %>%
  dplyr::select(Name, Pgrazed_lag1, Precip, 
              Protein_lag1, Height_lag1, Month)   ## <<------------------ two options. including month as random variable or as fixed effect with circular transformation 

# spatial data 
xy <- data %>%
  dplyr::select(x, y) %>% as.matrix(.)
rownames(xy) = data$sample.id
colnames(xy) = c("x-coordinate","y-coordinate") 

# temporal data 
month <- data %>%
  dplyr::select(Month) %>% as.matrix(.)
rownames(month) = data$sample.id
colnames(month) = "month"  ???
# trait data 
## ..... TrData 



studyDesign = data.frame(sample = as.factor(data$sample.id),
                         plot = as.factor(data$Name),
                         month = as.factor()) 
rL = HmscRandomLevel(sData = xy)  # sData defines the spatial structure 



month <- as.matrix(data.frame(studyDesign$month))  ## as in serial number OR real month??
studyDesign$month <- as.factor(sprintf('month_%.3d', studyDesign$month))
studyDesign$Sample <- NULL
rownames(time) <- studyDesign$month
plot <- as.matrix(data.frame(studyDesign$Plot))
rownames(plot) <- studyDesign$Year

# random effect 
# include a random effect at the level of sampling unit to estimate species-to-species residual associations
rL1 = HmscRandomLevel(units = studyDesign$sample)
rL2 = HmscRandomLevel(sData = xy) # spatial structure
rL3 = HmscRadomLevel(sData = studyDesign$month) # temporal structure 
ranLevels = list(rL1, rL2, rL3)

rnd_eff = list("sampling_sites" =HmscRandomLevel(sData = xycoords),   # account for spatial autocorrelation
               "month" = HmscRandomLevel(sData = study_design$year))   # account for temporal autocorrelation. (Temporal data can be modelled the same way as spatial data, but with only one dimension.


XFormula = ~x1+x2







#  ----- model construction -----
dristr = "lognormal poisson"
m = Hmsc(Y = Y, XData = XData, XFormula = ~x1+x2)

# To fit the HMSC model with Bayesian inference
# Set parameter 
nChains = 2
test.run = FALSE  # <- only small amount of MCMC sampling
if (test.run){
  #with this option, the vignette runs fast but results are not reliable
  thin = 1
  samples = 10
  transient = 5
  verbose = 0
} else {
  #with this option, the vignette evaluates slow but it reproduces the results of the #.pdf version
  thin = 10
  samples = 1000
  transient = 500*thin
  verbose = 0 }

# call sampleMcmc and thus estimate the model parameters.
m = sampleMcmc(m, thin = thin, samples = samples, transient = transient, nChains = nChains, nParallel = nChains, verbose = verbose)

# ------ examining MCMC convergence -------

###
# In a model with random effects, it is important to look at the convergence diagnostics not only for the
# β parameters, but also for the Ω parameters. The matrix Ω is the matrix of species-to-species residual
# covariances.
## see tutorial P7 https://cran.r-project.org/web/packages/Hmsc/vignettes/vignette_2_multivariate_low.pdf 


# ------ predictive power ------


# ------ explanatory power -----


# ------ exploring parameter estimates ------- 


# ----- making predictions ---------