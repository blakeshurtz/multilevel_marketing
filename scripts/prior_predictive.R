mlm_sim <- stan_glmer(formula = close ~  (1|year) + (1|city) + 
                        (1|source/source_type) + 
                        (1|dsp) + 
                        callcategory + service_n +
                        priorrevenue + call_to_estimate - 1, 
                      data = d,
                      prior_PD = TRUE,
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

prior_summary(mlm_sim)

mlm_sim_val <- posterior_predict(mlm_sim)

d_plot <- data.frame(real = d$close,
                   sim = mlm_sim_val[1,])


ggplot(d_plot, aes(real, sim)) +
  geom_point()
