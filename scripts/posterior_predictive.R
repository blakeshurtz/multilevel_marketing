#sample the response from model priors
#uses function posterior_predict, but model had prior_PD = TRUE, so data is not incorporated
y_rep <- posterior_predict(mlm_sim)

#rows are iteractions, columns are samples. Why 1500?
plot(density(apply(y_rep[1:50,], 2, mean)))

###posterior predictive check
pp_check(mlm_sim)

ggsave(filename = "ppc.png", plot = last_plot(), device = "png", dpi = 300,
       width = 150, height = 150, units = "mm")
