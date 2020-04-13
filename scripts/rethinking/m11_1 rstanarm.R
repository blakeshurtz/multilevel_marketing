library(rethinking)
library(rstanarm)

data("chimpanzees")
d <- chimpanzees

#create index variable
d$treatment <- 1 + d$prosoc_left + 2*d$condition

mlm_sim <- stan_glm(formula = pulled_left ~ 1,
                      prior_PD = TRUE,
                      prior = normal(location = 0, scale = 10, autoscale = FALSE),
                      data = d,
                      family = binomial,
                      cores = 3,
                      chains = 4,
                      iter = 1000,
                      warmup = 150,
                      seed = 1, 
                      refresh = 50)

###construct prior predictive distribution 
temp <- posterior_predict(mlm_sim)
table(temp)
pp_check(mlm_sim)
