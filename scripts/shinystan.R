y <- as.numeric(dat$close) #for posterior pred check
y_rep <- posterior_predict(mlm_sim) # for posterior pred check
launch_shinystan(mlm_sim) #launch it
###to save to shinyapps.io
sso <- as.shinystan(mlm_sim) #convert to shiny object
deploy_shinystan(sso, "Multi-Level-Marketing", account = 'blakeobeans') #deploy
