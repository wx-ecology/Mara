## this script apply the HMSC framework to 
## 1. understand the relative contributions of environmental variables and biotic interactions to species occurrences
## 2. predict cattle and other species spatial use in wetter and dryer months to be used as input later soil and vegetation condition analyses

library(Hmsc)
library(corrplot)

# -----data prep ---

# notes - study desigh - lognormal poisson"; 
dristr = "lognormal poisson"

sample.id = as.factor(1:n)
studyDesign = dataframe(sample = sample.id,
                        plot = as.factor(), # c1 c2 etc. but this column is not used as random effect as in ranLevels because X and Ys are used
                         )  

rownames(xycoords) = sample.id


month <- as.matrix(data.frame(studyDesign$month))  ## as in serial number OR real month??
studyDesign$month <- as.factor(sprintf('month_%.3d', studyDesign$month))
studyDesign$Sample <- NULL
rownames(time) <- studyDesign$month
plot <- as.matrix(data.frame(studyDesign$Plot))
rownames(plot) <- studyDesign$Year

# random effect 
# include a random effect at the level of sampling unit to estimate species-to-species residual associations
rL1 = HmscRandomLevel(units = studyDesign$sample)
rL2 = HmscRandomLevel(sData = xycoords) # spatial structure
rL3 = HmscRadomLevel(sData = studyDesign$month) # temporal structure 
ranLevels = list(rL1, rL2, rL3)

rnd_eff = list("sampling_plot" =HmscRandomLevel(sData = xycoords),   # account for spatial autocorrelation
               "year" = HmscRandomLevel(sData = study_design$year))   # account for temporal autocorrelation. (Temporal data can be modelled the same way as spatial data, but with only one dimension.










#  ----- model construction -----
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