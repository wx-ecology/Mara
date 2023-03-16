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

#  ----------------------------------- model construction and fitting with Bayesian inference ---------------------------------- #
# Fitting the four alternative HMSC models with increasing thinning
samples = 1000
nChains = 4
thin = 1000
ModelDir = file.path(getwd(), "results/Hmsc_model")

run.model = TRUE 
if(run.model){
  for (thin in c(1,10,100,1000)){
    m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
             distr = "lognormal poisson",
             studyDesign = studyDesign, 
             ranLevels = ranLevels)
    
    m = sampleMcmc(m, samples = samples, thin=thin,
                   adaptNf=rep(ceiling(0.4*samples*thin),m$nr), 
                   transient = ceiling(0.5*samples*thin),
                   nChains = nChains, initPar = "fixed effects",
                   nParallel = nParallel)
    # MCMC convergence can be difficult to achieve especially in those models that are not based on normal distribution.
    # For this reason, in the script above we initialize model with
    # initPar="fixed effects", with which option the MCMC chains are not started from locations randomized from the prior
    # but from a maximum likelihood solution to the fixed-effects part of the model
    
    filename = file.path(ModelDir, paste("model_thin_", as.character(thin),
                                         "_samples_", as.character(samples),
                                         "_chains_",as.character(nChains),
                                         ".Rdata",sep = ""))
    save(m,file=filename)
  }
} else {
  load(file.path(ModelDir, paste0("model_thin_", thin, "_samples_", samples, "_chains_4.Rdata")))
}

# ------------------------------------ evaluating model convergence --------------------------------- #
# In a model with random effects, it is important to look at the convergence diagnostics not only for the
# β parameters, but also for the Ω parameters. The matrix Ω is the matrix of species-to-species residual
# covariances.
## see tutorial P7 https://cran.r-project.org/web/packages/Hmsc/vignettes/vignette_2_multivariate_low.pdf 

# check MCMC convergence diagnostics
mpost = convertToCodaObject(m)

## trace plots for the beta parameters 
# starting point is determined by transient, total iteration is 40,000, recorded every m-th (thin = m) step of the interation
# a good plot should show that different chains look identical and the chains mix very well, and they seem to have reached 
# a stationary distribution (e.g. the first half of the recorded interations looks statistically identical to the second half of the recorded iterations).
plot(mpost$Beta)

## alternatively, we evaluate MCMC convergence in a quantitative way in terms of effective sample size and potential scale reduction factors.
# we want to see the effective sample sizes are very close to the theoretical value of the actual number of
# samples, which should be samples*nChains. If true, it indicates very litte autocorrelation among consecutive samples.
# we also want to see the scale reduction factors very close to 1, which indicates the different chains gave consistent results.
par(mfrow=c(2,2))
hist(effectiveSize(mpost$Beta), main="ess(beta)", breaks = 20)
hist(gelman.diag(mpost$Beta, multivariate=FALSE)$psrf, main="psrf(beta)", breaks = 20)
hist(effectiveSize(mpost$Omega[[1]]), main="ess(omega)", breaks = 20)
hist(gelman.diag(mpost$Omega[[1]], multivariate=FALSE)$psrf, main="psrf(omega)", breaks = 20)

# ------------------------------------ explanatory power -------------------------------------------------- #
#To assess model fit 
preds = computePredictedValues(m) # get posterior predictive distribution
evaluateModelFit(hM=m, predY=preds) # explanatory power varies for different species 

# ------ predictive power ------
# through two-fold cross validation 
# takes a lot of time, as the model needs to be re-fitted twice. 
# Set run.cross.validation = TRUE to perform the cross-validation and to save the results.
# Set run.cross.validation = FALSE to read in results from cross-validation that you have run previously.

nParallel = 2
run.cross.validation = TRUE # start with TRUE when you introduce the script
filename=file.path(ModelDir, paste0("CV_thin_", as.character(m$thin), "_samples_", samples,"_chains_", nChains))

if(run.cross.validation){
  partition = createPartition(m, nfolds = 2)
  preds = computePredictedValues(m,partition=partition, nParallel = nParallel)
  MFCV = evaluateModelFit(hM=m, predY=preds)
  save(partition,MFCV,file=filename)
} else {
  load(filename)
}

if(run.cross.validation){
  partition = createPartition(m, nfolds = 2, column = "route")
  preds = computePredictedValues(m,partition=partition, nParallel = nParallel)
  MFCV = evaluateModelFit(hM=m, predY=preds)
  save(partition,MFCV,file=filename)
} else {
  load(filename)
}

# While the code above yields the average results over the species, we may also look at the species-specific results.
# For example, let us plot the explanatory and predictive AUC measures with respect to each other.

plot(MF$AUC,MFCV$AUC)
abline(0,1)
# The call to abline(0,1) adds the identity line (y=x) to the plot. For points (=species) below the line,
# explanatory power is greater than predictive power.

# ---------------------------------------------- exploring parameter estimates ------------------------------------------ # 
## variance partitioning 
groupnames = c("Distance-to-border","Forage-quality", "Forage-quantity", "Precipitation", "Season")
group = c(1,1,3,4,2,3,5) # # Arbitrarily, we include the intercept in the habitat variables
VP = computeVariancePartitioning(m, group = group, groupnames = groupnames)
plotVariancePartitioning(m,VP, las = 2)

## beta estiamtes
postBeta = getPostEstimate(m, parName = "Beta")
plotBeta(m, post = postBeta, param = "Support", supportLevel = 0.95) # red are parameters significantly positive and blue are ones significantly negative. 

## omega estimates (species-to-species associations), estimated through a latent factor approach by introducing a random effect at the level of the sampling unit. 
OmegaCor = computeAssociations(m) # converts the covariances to the more convenient scale of correlation (ranging from -1 to +1)
supportLevel = 0.95 #plot only those associations for which the posterior probability for being negative or positive is at least 0.95. 
toPlot = ((OmegaCor[[1]]$support>supportLevel)
          + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean
corrplot(toPlot, method = "color",
         col = colorRampPalette(c("red","white","blue"))(200),
         title = paste("random effect level:", m$rLNames[1]), mar=c(0,0,1,0))

# ------------------------------ making predictions using Gaussian Predictive Process ---------------------------------- #
# We start by making gradient plots that visualise how the communities vary among the 
# environmental variables.
Gradient = constructGradient(m,focalVariable = "Site")
predY = predict(m, Gradient=Gradient, expected = TRUE)

# Occurrence probability of Corvus monedula
# sppecies 1 - cattle, 2 - widebeest, 3 - zebra, 4 - T gazelle, 5 - impala, 6 - topi, 7 - eland, 
# 8 - buffalo, 9 - G gazelle, 10 - waterbuck, 11 - dikdik, 12 - elephant
plotGradient(m, Gradient, pred=predY, measure="Y", index = 1, showData = TRUE)
plotGradient(m, Gradient, pred=predY, measure="Y", index = 2, showData = TRUE)
plotGradient(m, Gradient, pred=predY, measure="Y", index = 3, showData = TRUE)



# conditional prediction for focal species 
# If the focal species and the other species show residual associations, knowing the observed data for the other
# species can help to make improved predictions for the focal species.

# setting the knots
Knots = constructKnots(sData = xy, knotDist = 250, minKnotDist = 1000)
plot(xy[,1],xy[,2],pch=18, asp=1)
points(Knots[,1],Knots[,2],col='red',pch=18)