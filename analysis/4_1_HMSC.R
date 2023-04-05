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
# To account for the nature of the study design and to evaluate co-variation among samples, 
# we included three random effects for the site, the year and the sampling unit (that is, year–site pairs)
# include a random effect at the level of sampling unit to estimate species-to-species residual associations

# study design 
studyDesign = data.frame(sample = as.factor(seq(1:nrow(data))), 
                         plot = as.factor(data$Name),
                         month = as.factor(data$month_id))  # account for repeated measurement in different sampling seasons, the month should be a serial number month to indicate different survey season
                          # including months in both the random effects is for controling for temporal autocorrelation. 
                          # including months in the fix effects is for controlling other potential seasonal effects (???) 
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
nParallel = 2
ModelDir = file.path(getwd(), "results/Hmsc_model")

run.model = TRUE
if(run.model){ 
  for (thin in c(1, 10, 100,1000)){  # thin = 1000 took 107 hours
    m = Hmsc(Y=Y, XData = XData,  XFormula = XFormula,
             distr = "lognormal poisson",
             studyDesign = studyDesign, 
             ranLevels = ranLevels)
    
    m = sampleMcmc(m, samples = samples, thin=thin,
                   transient = 50*thin,
                   nChains = nChains, initPar = "fixed effects",
                   nParallel = nParallel)
    # MCMC convergence can be difficult to achieve especially in those models that are not based on normal distribution.
    # For this reason, in the script above we initialize model with
    # initPar="fixed effects", with which option the MCMC chains are not started from locations randomized from the prior
    # but from a maximum likelihood solution to the fixed-effects part of the model
    
    filename = file.path(ModelDir, paste("model_thin_", as.character(thin),
                                         "_samples_", as.character(samples),
                                         "_chains_",as.character(nChains),
                                         "newTransient.Rdata",sep = ""))
    save(m,file=filename)
  }
} else {
  load(file.path(ModelDir, paste0("model_thin_", thin, "_samples_", samples, "_chains_", nChains,"newTransient.Rdata")))
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
#plot(mpost$Beta[,1])

## alternatively, we evaluate MCMC convergence in a quantitative way in terms of effective sample size and potential scale reduction factors.
# we want to see the effective sample sizes are very close to the theoretical value of the actual number of
# samples, which should be samples*nChains. If true, it indicates very litte autocorrelation among consecutive samples.
# we also want to see the scale reduction factors very close to 1, which indicates the different chains gave consistent results.
par(mfrow=c(2,2))
# beta is the species niches 
hist(effectiveSize(mpost$Beta), main="ess(beta)", breaks = 20)
hist(gelman.diag(mpost$Beta, multivariate=FALSE)$psrf, main="psrf(beta)", breaks = 20)
# omega is the residual species associations 
hist(effectiveSize(mpost$Omega[[1]]), main="ess(omega)", breaks = 20)  # ess omega is not the best. some of them have low effective sample saize.
hist(gelman.diag(mpost$Omega[[1]], multivariate=FALSE)$psrf, main="psrf(omega)", breaks = 20)

# ------------------------------------ explanatory power -------------------------------------------------- #
#To assess model fit 
preds = computePredictedValues(m) # get posterior predictive distribution
MF = evaluateModelFit(hM=m, predY=preds) # explanatory power varies for different species 
# For Poisson models, the observed and predicted data are also truncated to occurrences (presence-absences), for which the same measures are given as for the probit models (O.RMSE, O.AUC and O.TjurR2). 
# For Poisson models, the observed and predicted data are also subsetted to conditional on presence, for which the root-mean-square error and pseudo-R2 based on squared spearman correlation are computed (C.RMSE, C.SR2).
# we stick with the psudo R2 here. 
# hist(MF$SR2, xlim = c(0,1), main=paste0("Mean = ", round(mean(MF$SR2),2)), breaks = 10) # for poisson models, pseudo-R2 is computated as squared spearman correlation between observed and predicted values, times the sign of the correlation (SR2)
round(mean(MF$SR2),2)
round(sd(MF$SR2),2)
# some species are high some are low. The mean = 0.43, sd = 0.25 across all species. 

# ------ predictive power ------
# through two-fold cross validation 
# takes a lot of time, as the model needs to be re-fitted twice. 
# Set run.cross.validation = TRUE to perform the cross-validation and to save the results.
# Set run.cross.validation = FALSE to read in results from cross-validation that you have run previously.

run.cross.validation = TRUE # start with TRUE when you introduce the script
filename=file.path(ModelDir, paste0("CV_thin_", as.character(m$thin), "_samples_", samples,"_chains_", nChains, "newTransient"))

if(run.cross.validation){
  partition = createPartition(m, nfolds = 2, column = "sample")
  # Increasing the number of folds means that more data are available for fitting the model, which can be
  # expected to lead to greater predictive performance. For this reason, the predictive power estimated by
  # two-fold cross-validation is likely to underestimate the true predictive power of the full model fitted to all
  # data.  "coluum = plot" means when making a prediction for a particular sampling unit, the model was fitted without 
  # training data from any other sampling units in the same plot, thus the model has no possibility to estimate the actual random
  # effect for the focal plot and predictive power is solely on the fixed effect.
  preds = computePredictedValues(m,partition=partition, nParallel = nParallel)
  MFCV = evaluateModelFit(hM=m, predY=preds)
  save(partition,MFCV,file=filename)
} else {
  load(filename)
}

# While the code above yields the average results over the species, we may also look at the species-specific results.
# For example, let us plot the explanatory and predictive AUC measures with respect to each other.

plot(MF$SR2,MFCV$SR2)
abline(0,1)
# The call to abline(0,1) adds the identity line (y=x) to the plot. All points (=species) are below the line,
# suggesting explanatory power is greater than predictive power - as expected

round(mean(MFCV$SR2),2) #0.32
round(sd(MFCV$SR2),2) #0.2

# ---------------------------------------------- exploring parameter estimates ------------------------------------------ # 
## variance partitioning 
dev.off()
# group the fixed effects in any way that works the best for presenting the resutls.  
# variance component accounts also for co-variation within the group whereas co-variation among groups is ignored 
#  The intercept does not have any variation, and thus it will not contribute to the variance partitioning 

# groupnames = c("Distance-to-border","Forage-quality", "Forage-quantity", "Precipitation", "Season")
# group = c(1,1,3,4,2,3,5,5) 
# VP = computeVariancePartitioning(m, group = group, groupnames = groupnames)
# # plotVariancePartitioning(m,VP, las = 2)
# saveRDS(VP, file = file.path("./results/Hmsc_VP.RDS"))

## forage quantity, measured as persent grazed and height from last month, plays a more important role than quality, measured as protein from the past month. 

groupnames = c("Distance to border", "Forage quantity (% grazed)", "Forage quantity (grass height)", "Forage quality", "Precipitation", "Season")
group = c(1,1,2,5,4,3,6,6)
VP = computeVariancePartitioning(m, group = group, groupnames = groupnames)
# plotVariancePartitioning(m,VP, las = 2)
# saveRDS(VP, file = file.path("./results/Hmsc_VP.RDS")) ## <--- continue 99_HMSC_Variance_Partitioning.R for making the plot

## beta estimates
postBeta = getPostEstimate(m, parName = "Beta")
#saveRDS(list(m, postBeta), file = file.path("./results/Hmsc_beta_estimates.RDS"))
par(mar = c(8, 8, 5, 5))
plotBeta(m, post = postBeta, param = "Support", supportLevel = 0.95, mar = c(8,1,2,1)) # red are parameters significantly positive and blue are ones significantly negative. 

## omega estimates (species-to-species associations), estimated through a latent factor approach by introducing a random effect at the level of the sampling unit. 
OmegaCor = computeAssociations(m) # converts the covariances to the more convenient scale of correlation (ranging from -1 to +1)
supportLevel = 0.95 #plot only those associations for which the posterior probability for being negative or positive is at least 0.95. 
toPlot = ((OmegaCor[[1]]$support>supportLevel)
          + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean

saveRDS(toPlot, "./results/Hmsc_network_95.RDS")

corrplot(toPlot, method = "circle", type = "lower", tl.col = "black",
         col = colorRampPalette(c("#ff5e1f","#ffffff","#389bd9"))(200))
# The red and blue colours indicate those species pairs for which the support for
# either a negative or positive association is at least 0.95.

# when trying lower supportive level, we see the weak positive correlation between cattle and other species, and weak negative correlation between elephant and other species
supportLevel = 0.5
toPlot = ((OmegaCor[[1]]$support>supportLevel)
          + (OmegaCor[[1]]$support<(1-supportLevel))>0)*OmegaCor[[1]]$mean

saveRDS(toPlot, "./results/Hmsc_network_50.RDS")

corrplot(toPlot, method = "circle", type = "lower",tl.col = "black",
         col = colorRampPalette(c("#ff5e1f","#ffffff","#389bd9"))(200))

# The red and blue colours indicate those species pairs for which the support for
# either a negative or positive association is at least 0.95.
# Note that we have ordered the species (with the function corrMatOrder) so that the cluster of
# co-occurring species is most easily seen. To keep the original species order, write e.g. plotOrder = 1:m$ns

# We next examine at which spatial scale the variation captured by the random effect occurs.

mpost = convertToCodaObject(m) # mpost was calculated above
summary(mpost$Alpha[[1]], quantiles = c(0.025, 0.5, 0.975))

# There is no support for spatial signal in the residual, as the posterior distribution
# of the alphas overlap with zero. Thus, the variation is independent among the survey routes.
# Note that in case of the model without environmental covariates (not shown here but see the book),
# the variation in the leading factor occurs at the scale of ca. 150 km, reflecting the scale
# at which the relevant environmental conditions vary.



# ------------------------------ making predictions  ---------------------------------- #
# We start by making gradient plots that visualise how the communities vary among the
# environmental variables.

# making two predictions. One during dry season, one during wet season. Using data from last 20 years average calculated from GEE_data_extraction.R

# read the 20 year rain info 
rain_history <- read_csv("./data/mara_gee_rain_20yr.csv")
# rain_history %>% ggplot( aes(x = month, y = pr, group = year, color = year)) +
#   geom_line()  # the 20 year rain is very variable and hard to differentiate dry/wet 
# rain_history %>% ggplot( aes(x = as.factor(month), y = pr)) +
#   geom_boxplot()
rain_sum <- rain_history %>% group_by(month) %>% summarise(mean = mean(pr), sd = sd(pr))
## driest, May, pr = 46.67278; wettest, Dec, pr = 103.28157

### ------ prediction in a dry time ------- ### 
Gradient_dry = constructGradient(m,focalVariable = "Site", 
                             non.focalVariables = list( "Precip" = list(3, 46.67278),"sin_month" = list(3, sin(2*pi*5/12)), "cos_month" = list(3, cos(2*pi*5/12)))) 
predY_dry = predict(m, Gradient=Gradient_dry, expected = TRUE) # made 1000 prediction

# Occurrence probability of Corvus monedula
# sppecies 1 - cattle, 2 - widebeest, 3 - zebra, 4 - T gazelle, 5 - impala, 6 - topi, 7 - eland,
# 8 - buffalo, 9 - G gazelle, 10 - waterbuck, 11 - dikdik, 12 - elephant
plotGradient(m, Gradient_dry, pred=predY_dry, measure="Y", index = 1, showData = TRUE) # cattle
plotGradient(m, Gradient_dry, pred=predY_dry, measure="Y", index = 2, showData = TRUE) # wildebeest
plotGradient(m, Gradient_dry, pred=predY_dry, measure="Y", index = 3, showData = TRUE) # zebra
# Species richness (meansure = "S" means summed abundance over all species)
plotGradient(m, Gradient_dry, pred=predY_dry, measure="S", showData = TRUE)

### ------ prediction in a wet time ------- ### 
Gradient_wet = constructGradient(m,focalVariable = "Site", 
                                 non.focalVariables = list( "Precip" = list(3, 103.28157),"sin_month" = list(3, sin(2*pi*12/12)), "cos_month" = list(3, cos(2*pi*12/12)))) 
predY_wet = predict(m, Gradient=Gradient_wet, expected = TRUE) # made 1000 prediction

# Occurrence probability of Corvus monedula
# sppecies 1 - cattle, 2 - widebeest, 3 - zebra, 4 - T gazelle, 5 - impala, 6 - topi, 7 - eland,
# 8 - buffalo, 9 - G gazelle, 10 - waterbuck, 11 - dikdik, 12 - elephant
plotGradient(m, Gradient_wet, pred=predY_wet, measure="Y", index = 1, showData = TRUE) # cattle
plotGradient(m, Gradient_wet, pred=predY_wet, measure="Y", index = 2, showData = TRUE) # wildebeest
plotGradient(m, Gradient_wet, pred=predY_wet, measure="Y", index = 3, showData = TRUE) # zebra
# Species richness (meansure = "S" means summed abundance over all species)
plotGradient(m, Gradient, pred=predY, measure="S", showData = TRUE)

saveRDS(list(predY_dry = predY_dry, predY_wet = predY_wet), "./results/Hmsc_prediction.RDS")
