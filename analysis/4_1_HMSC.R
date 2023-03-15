## this script apply the HMSC framework to 
## 1. understand the relative contributions of environmental variables and biotic interactions to species occurrences
## 2. predict cattle and other species spatial use in wetter and dryer months to be used as input later soil and vegetation condition analyses

library(tidyverse)
library(Hmsc)
library(corrplot)

# -----data prep ---
data <- read_csv("./data/for-spp-relationship/mara-cooccurence-compiled.csv") %>%
  #dplyr::select(-Northing, -Easting) %>% # this northing and easting is measured by GPS in the field with lots of noises. using site GPS locations instead.
  mutate(Site = as.numeric(Site),  # distance to boundary 
         sin_month = sin(2*pi*Month/12),
         cos_month = cos(2*pi*Month/12)) %>%   # ). To account for the periodic nature of months over time, we included the mean timing of each event as the linear effect of its cosine and sine transformations
  filter(Yr_Mo != "2018-05",
         !is.na(Protein_lag1),
         !is.na(Height_lag1)) %>%  # the first month does not have previous month measurement
  dplyr::select(
    month_id, Name, x, y,  # study design info
    Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,  # species counts
    Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant, # species counts
    Site, Name, Pgrazed_lag1, Precip, Protein_lag1, Height_lag1, sin_month, cos_month # environmental data
  ) %>%
  drop_na(.) # 1516 * 23 

# community data 
Y = data %>% 
  dplyr::select(Cattle, Wildebeest, Zebra, Thompsons_Gazelle, Impala, Topi,
                Eland, Buffalo, Grants_Gazelle, Waterbuck, Dikdik, Elephant) %>%
  as.matrix(.)

dim(Y) # 12 species in 1516 sampling units
#S = rowSums(Y)
#P = colMeans(Y)

# environmental data 
XData <- data %>%
  dplyr::select(Site, Pgrazed_lag1, Precip, Protein_lag1, Height_lag1, sin_month, cos_month) 

XFormula = ~ Site + Pgrazed_lag1 + Precip + Protein_lag1 + Height_lag1 + sin_month + cos_month

## random effect ##
# To account for the nature of the study design and to evaluate co-variation among xxxx, 
# we included three random effects for the site, the year and the sampling unit (that is, year–site pairs)
# include a random effect at the level of sampling unit to estimate species-to-species residual associations

# study design 
studyDesign = data.frame(sample = as.factor(seq(1:nrow(data))), 
                         plot = as.factor(data$Name),
                         month = as.factor(data$month_id))  # account for repeated measurement in different sampling seasons, the month should be a serial number month to indicate different survey season

# spatial data 
xy <- data %>%
  dplyr::select(x, y) %>% as.matrix(.)
rownames(xy) = studyDesign$plot
colnames(xy) = c("x-coordinate","y-coordinate") 

# temporal data 
month <- data %>%
  dplyr::select(month_id) %>% as.matrix(.)
rownames(month) <- studyDesign$month
colnames(month) <- "month"

rL.sample = HmscRandomLevel(units = levels(studyDesign$sample)) # to obtain residual correlations; units = ... degines an unstructured random effect; 
rL.temporal = HmscRandomLevel(sData = unique(month))  # sData defines the autocorrelation structure, temporal autocorrelation
rL.spatial = HmscRandomLevel(sData = unique(xy))  # spatial autocorrelation; also used to account for site effects

ranLevels = list(sample = rL.sample, month = rL.temporal, plot = rL.spatial)

# trait data 
# to be comparable with ecoCopula, not including body mass here but should include in the consequence analysis 

#  ----- model construction and fitting with Bayesian inference -----
# Fitting the four alternative HMSC models with increasing thinning
samples = 25
nChains = 4
ModelDir = "./results/Hmsc_model/"
for (thin in c(1,10,100,1000)){
  m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
           #  TrData = TrData,TrFormula = TrFormula,
           distr = "lognormal poisson",
           studyDesign = studyDesign, 
           ranLevels = ranLevels)
  m = sampleMcmc(m, samples = samples, thin=thin,
                 adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                 transient = ceiling(0.5*samples*thin),
                 nChains = nChains)
  filename = file.path(ModelDir, paste("model_thin_", as.character(thin),
                                       "_samples_", as.character(samples),
                                       "_chains_",as.character(nChains),
                                       ".Rdata",sep = ""))
  save(m,file=filename)
}


# ------ evaluating model convergence -------- #
# In a model with random effects, it is important to look at the convergence diagnostics not only for the
# β parameters, but also for the Ω parameters. The matrix Ω is the matrix of species-to-species residual
# covariances.
## see tutorial P7 https://cran.r-project.org/web/packages/Hmsc/vignettes/vignette_2_multivariate_low.pdf 


# ------ predictive power ------


# ------ explanatory power -----


# ------ exploring parameter estimates ------- 


# ----- making predictions ---------