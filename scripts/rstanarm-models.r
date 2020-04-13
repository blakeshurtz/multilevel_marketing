###mlm
mlm_sim <- stan_glmer(formula = close ~ (1|lead_source) +
                                (1|year) +
                                (1|city) +
                        (1|sales_professional) +
                        product +
                        n_service +
                        prior_revenue +
                        call_to_estimate - 1, 
                      prior = normal(location = 0, scale = 1, autoscale = FALSE),
                      prior_intercept = cauchy(location = 0, scale = 2.5, autoscale = FALSE),
                      prior_aux = cauchy(location = 0, scale = 25, autoscale = FALSE),
                      data = dat,
                      family = binomial,
                      cores = 3,
                      chains = 4,
                      iter = 1000,
                      warmup = 150,
                      seed = 1, 
                      refresh = 50)

glm_sim <- stan_glm(formula = close ~ lead_source +
                                year +
                                city +
                        sales_professional +
                        product +
                        n_service +
                        prior_revenue +
                        call_to_estimate - 1,
                    prior = normal(location = 0, scale = 1, autoscale = FALSE),
                    prior_intercept = cauchy(location = 0, scale = 2.5, autoscale = FALSE),
                    prior_aux = cauchy(location = 0, scale = 25, autoscale = FALSE),
                  data = dat,
                  family = binomial,
                  cores = 3,
                  chains = 4,
                  iter = 1000,
                  warmup = 150,
                  seed = 9, 
                  refresh = 50)
                  
'
code used below to compare to lme4 model
coef(m1_rstanarm)$`dsp:year`[[1]][1:5]
'