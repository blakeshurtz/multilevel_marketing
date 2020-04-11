###waic
mlm_waic <- waic(mlm, cores = 4)
glm_waic <- waic(glm, cores = 4)
nested_mlm_waic <- waic(nested_mlm, cores = 4)

###WAIC comparison
loo_compare(mlm_waic, glm_waic, nested_mlm_waic)

###loo
tic()
mlm_loo <- loo(mlm, cores = 1, save_psis = TRUE)
toc()
tic()
lm_loo <- loo(glm, cores = 1, save_psis = TRUE)
toc()
tic()
nested_mlm_loo <- loo(nested_mlm, cores = 1, save_psis = TRUE)
toc()

###Loo comparison
loo_compare(mlm_loo, nested_mlm_loo)


###plot loo
plot(mlm_loo)
