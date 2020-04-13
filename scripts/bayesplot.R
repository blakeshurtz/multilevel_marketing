###NUTS diagnostics
source('scripts/stan_utility_rstanarm.R')
check_div(mlm_sim)
check_rhat(mlm_sim)
check_n_eff(mlm_sim) 
check_energy(mlm_sim)
check_treedepth(mlm_sim)

l_post <- log_posterior(mlm_sim)
nuts_p <- nuts_params(mlm_sim)
pars = names(mlm_sim$coefficients)

color_scheme_set("darkgray")
mcmc_parcoord(mlm_sim, np = nuts_p, pars = pars) + 
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
m_summary <- summary(mlm_sim) %>% 
  as.data.frame() %>% 
  mutate(variable = rownames(.)) %>% 
  select(variable, everything()) %>% 
  as_tibble()

m_summary %>% 
  ggplot(aes(Rhat)) + 
  geom_histogram() 

color_scheme_set("brightblue") # see help("color_scheme_set")
rhats <- rhat(mlm_sim, pars = pars) #works as a CDF too
mcmc_rhat(rhats) + yaxis_text(hjust = 1)

###Effective Sample Sie
ratios_cp <- neff_ratio(mlm_sim, pars = pars)
#print(ratios_cp)
mcmc_neff(ratios_cp, size = 2)

iterations = mlm_sim[["stanfit"]]@sim[["iter"]]
m_summary %>% 
  ggplot(aes(n_eff/iterations)) + 
  geom_histogram()

###autocorrelation
pars = names(mlm_sim$coefficients[1:3])
mcmc_acf(mlm_sim, pars = pars, lags = 10) 
ranef(mlm_sim)

###plotting draws
posterior <- as.matrix(mlm_sim)

exp(posterior)/(1+ exp(posterior))

colnames(posterior)[7:11] <- c("Internet", "Newspaper", "Radio", "TV", "Vehicle")

mcmc_intervals(
  exp(posterior)/(1 + exp(posterior)),
  pars = colnames(posterior)[7:11],
  prob = 0.8, # 80% intervals
  point_est = "mean"
) +
  labs(    title = "Vehicle & Internet marketing campaigns most successful",
           subtitle = "95% and 80% probability intervals, with mean point",
           caption = "x-axis transformed from logit to probability scale",
           x = "Value-Add (added probability of successful sale)",
           y = "") +
  theme(text = element_text(size=16))
  
ggsave(filename = "marketing_comparison.png", plot = last_plot(), device = "png", dpi = 300,
       width = 200, height = 150, units = "mm")

exp(-2)/(1+exp(-2))
exp(-2)/(1+exp(-2))

e
