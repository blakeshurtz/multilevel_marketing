###waic
mlm_waic <- waic(mlm_sim, cores = 4)
glm_waic <- waic(glm_sim, cores = 4)

###WAIC comparison
loo_compare(mlm_waic, glm_waic)

###loo
tic()
mlm_loo <- loo(mlm_sim, cores = 1, save_psis = TRUE)
toc()
tic()
glm_loo <- loo(glm_sim, cores = 1, save_psis = TRUE)
toc()

###Loo comparison
loo_compare(mlm_loo, glm_loo)


###plot loo
plot(mlm_loo)
