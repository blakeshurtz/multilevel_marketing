###mlm
mlm_sim <- stan_glmer(formula = close ~ (1|g_marketing) +
                        (1|g_year) +
                        (1|g_city) +
                        (1|g_salesperson) +
                        callcategory +
                        n_service_calls +
                        prior_revenue +
                        call_to_estimate - 1, 
                      data = dat,
                      family = binomial,
                      cores = 3,
                      chains = 4,
                      iter = 1000,
                      warmup = 150,
                      seed = 1, 
                      refresh = 50)


#check model
print(mlm_sim)
coef(mlm_sim)




mlm <- stan_glmer(formula = close ~  (1|year) + (1|city) + 
                                      (1|source_type/source) + 
                                      (1|dsp) + 
                                      callcategory + service_n +
                                      priorrevenue + call_to_estimate - 1, 
                          data = d,
                          # prior_PD = TRUE,
                  prior = normal(location = 0, scale = 1, autoscale = FALSE),
                  prior_intercept = normal(location = 0, scale = 1, autoscale = FALSE),
                  prior_aux = exponential(rate = 1, autoscale = FALSE),
                          family = binomial,
                          cores = 3,
                          chains = 4,
                          iter = 1000,
                          warmup = 150,
                          seed = 1, 
                          refresh = 50)


glm <- stan_glmer(formula = close ~ year + city + 
                   (1|source/source_type) + 
                   dsp + 
                   callcategory + service_n +
                   priorrevenue + call_to_estimate - 1, 
                  data = d,
                  # prior_PD = TRUE,
                  family = binomial,
                  prior = normal(location = 0, scale = 1, autoscale = FALSE),
                  prior_intercept = normal(location = 0, scale = 1, autoscale = FALSE),
                  prior_aux = exponential(rate = 1, autoscale = FALSE),
                  cores = 3,
                  chains = 2,
                  iter = 1000,
                  warmup = 200,
                  seed = 9, 
                  refresh = 50,
                  control = list(adapt_delta = 0.99))
                  
nested_mlm <- stan_glmer(formula = close ~ (1|year/dsp) + (1|city/dsp) + 
                           (1|source/source_type) + 
                           callcategory + service_n +
                           priorrevenue + call_to_estimate - 1, 
                    data = d,
                    # prior_PD = TRUE,
                    family = binomial,
                    prior = normal(location = 0, scale = 1, autoscale = FALSE),
                    prior_intercept = normal(location = 0, scale = 1, autoscale = FALSE),
                    prior_aux = exponential(rate = 1, autoscale = FALSE),
                    cores = 3,
                    chains = 4,
                    iter = 1000,
                    warmup = 200,
                    seed = 1, 
                    refresh = 50)

'
code used below to compare to lme4 model
coef(m1_rstanarm)$`dsp:year`[[1]][1:5]
'