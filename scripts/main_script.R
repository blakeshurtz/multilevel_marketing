library(rstan)
library(rstanarm)
library(loo)
library(tidyverse)
library(bayesplot)
library(shinystan)
library(tictoc)
library(readr)

rstan_options(auto_write = TRUE) #cache model
options(mc.cores = parallel::detectCores()) #multiple cores for multiple chains
#save.image(file="multilevel marketing.RData") 

###import data
dat <- read.csv("data/simulated_project_data.csv")

###run models
source('scripts/rstanarm-models.r')

###loo
source('scripts/loo.r')

###prior predictive simulation
source('scripts/prior_predictive.r')

###ShinyStan
source('scripts/shinystan.r')

###bayesplot
source('scripts/bayesplot.r')

###model summary
print(mlm_sim)
summary(mlm_sim)
coef(mlm_sim)
ranef(mlm_sim)
fixef(mlm_sim)

###posterior intervals
posterior_interval(mlm_sim)
posterior_vs_prior(mlm_sim)

###residuals
summary(residuals(mlm_sim)) # not deviance residuals

