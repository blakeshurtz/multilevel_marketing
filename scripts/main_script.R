library(rstan)
library(rstanarm)
library(loo)
library(tidyverse)
library(bayesplot)
library(shinystan)
library(lme4)
library(readr)
library(tictoc)

rstan_options(auto_write = TRUE) #cache model
options(mc.cores = parallel::detectCores()) #multiple cores for multiple chains

save.image(file="multilevel marketing.RData") 

###set wd
setwd("D:/Google Drive/R/RStan/sales_marketing_pipeline_revisited")

###import data
source('data/prep_data.r')
d <- read_csv("data/pipelinedata.csv")

###run models
source('scripts/rstanarm-models.r')

###loo
source('scripts/loo.r')

###prior predictive simulation
#source('scripts/prior_predictive.r')

###ShinyStan
y <- as.numeric(d$close) #for posterior pred check
y_rep <- posterior_predict(mlm) # for posterior pred check
launch_shinystan(mlm) #launch it
###to save to shinyapps.io
mlm_sso <- as.shinystan(mlm) #convert to shiny object
deploy_shinystan(sso, "ShinyStan_Marketing_Model", account = 'blakeobeans') #deploy

###NUTS diagnostics
source('scripts/stan_utility_rstanarm.R')
check_div(mlm)
check_rhat(mlm)
check_n_eff(mlm) 
check_energy(mlm)
check_treedepth(mlm)

l_post <- log_posterior(mlm)
nuts_p <- nuts_params(mlm)
pars = names(mlm$coefficients)

color_scheme_set("darkgray")
mcmc_parcoord(mlm, np = nuts_p, pars = pars) + 
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank()) + 
    theme(axis.text.x = element_text(angle = 90, hjust = 1))

color_scheme_set("red")
mcmc_nuts_divergence(nuts_p, l_post, chain = 2)

###Energy
color_scheme_set("red")
mcmc_nuts_energy(nuts_p)

###MCMC diagnostics

###R-hat
m_summary <- summary(mlm) %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_tibble()

m_summary %>% 
  ggplot(aes(Rhat)) + 
  geom_histogram() 

color_scheme_set("brightblue") # see help("color_scheme_set")
rhats <- rhat(mlm, pars = pars) #works as a CDF too
mcmc_rhat(rhats) + yaxis_text(hjust = 1)

###Effective Sample Sie
ratios_cp <- neff_ratio(mlm, pars = pars)
#print(ratios_cp)
mcmc_neff(ratios_cp, size = 2)

iterations = mlm[["stanfit"]]@sim[["iter"]]
m_summary %>% 
  ggplot(aes(n_eff/iterations)) + 
  geom_histogram()

###autocorrelation
pars = names(mlm$coefficients[1:3])
mcmc_acf(mlm, pars = pars, lags = 10) 

###model summary
print(mlm)
print(m_lme)

unique(d$source_type)
###posterior intervals
coef(mlm)$source_type
posterior_interval(mlm, pars = unique(d$source_type))

###residuals
summary(residuals(mlm)) # not deviance residuals
summary(residuals(m_lme))

###posterior predictive check
pp_check(mlm)
