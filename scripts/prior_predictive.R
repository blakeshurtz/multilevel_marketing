#priors mlm_sim with autoscale = TRUE
prior_summary(mlm_sim)

#construct generative model using priors from mlm_sim
mlm_sim_prior <- stan_glmer(formula = close ~ (1|lead_source) +
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
                               prior_PD = TRUE,
                               data = dat,
                               family = binomial,
                               cores = 3,
                               chains = 4,
                               iter = 1000,
                               warmup = 150,
                               seed = 1, 
                               refresh = 50)

#sample the response from model priors
#uses function posterior_predict, but model had prior_PD = TRUE, so data is not incorporated
y_rep <- posterior_predict(mlm_sim_prior)

#rows are iteractions, columns are samples. Why 1500?
plot(density(apply(y_rep, 2, mean)))

