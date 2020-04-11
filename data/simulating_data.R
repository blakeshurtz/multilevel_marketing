library(rstanarm)
library(dplyr)

###simulating data
set.seed(849107) #for reproducability

###define multi-level vars
ng_marketing <- 5 #marketing sources
nj_marketing <- 300 #marketing cluster size (make unbalanced) 
n = ng_marketing * nj_marketing

ng_year <- 5 #years of data
nj_year <- 300 #cluster size (make unbalanced) 
n = ng_year * nj_year

ng_city <- 5 #citys of data
nj_city <- 300 #cluster size (make unbalanced) 
n = ng_city * nj_city

ng_salesperson <- 5 #citys of data
nj_salesperson <- 300 #cluster size (make unbalanced) 
n = ng_salesperson * nj_salesperson

###creating cluster variables, sample to randomise order

#marketing
g_marketing <- gl(ng_marketing, k = nj_marketing,
            labels = c("radio", "tv", "newspaper", 
                       "internet", "vehicle"
                       )
            )

g_marketing <- sample(g_marketing, n, replace = FALSE)

#year
g_year <- gl(ng_year, k = nj_year,
                  labels = c("2015", "2016", "2017", 
                             "2018", "2019"
                  )
)

g_year <- sample(g_year, n, replace = FALSE)

#city
g_city <- gl(ng_city, k = nj_city,
                  labels = c("davis", "vacaville", "fairfield", 
                             "woodland", "suisun"
                  )
)

g_city <- sample(g_city, n, replace = FALSE)

#salesperson
g_salesperson <- gl(ng_salesperson, k = nj_salesperson,
             labels = c("John", "Larry", "Peter", 
                        "Steven", "Mary"
             )
)

g_salesperson <- sample(g_salesperson, n, replace = FALSE)

###standard (single level) variables
#callcategory
callcategory <- factor(c(rep("HVAC", 750),
                         rep("DHW", 500),
                         rep("SOLAR", 250))
)


callcategory <- sample(callcategory, n, replace = FALSE)

#n_service_calls
n_service_calls <- scale(rpois(1500, 2))

#prior_revenue
prior_revenue <- c(rep(0, 1000), rep(1, 500))

prior_revenue <- sample(prior_revenue, n, replace = FALSE)

#call_to_estimate
call_to_estimate <- c(scale(rpois(1500, 2)))

#creating table showing factor label and numeric value
marketing_table = unique(data.frame(bind_cols(name = g_marketing,
                                               value= as.numeric(g_marketing)-3
                                          )
                                )
                    )

year_table = unique(data.frame(bind_cols(name = g_year,
                                          value= as.numeric(g_year)-3
                                         )
                                )
                    )

city_table = unique(data.frame(bind_cols(name = g_city,
                                              value= as.numeric(g_city)-3
                                        )
                              )
                    )

salesperson_table = unique(data.frame(bind_cols(name = g_salesperson,
                                         value= as.numeric(g_salesperson)-3
                                        )
                              )
                    )


callcategory_table = unique(data.frame(bind_cols(name = callcategory,
                                                value= as.numeric(callcategory)-2
                                                )
                                      )
                            )

#sd- in estimating coefficients, sigma has inverse relationship with n
sigma = .01

#simulate regression
close <- as.numeric(g_marketing) - 3 + 
         as.numeric(g_year) - 3 + 
         as.numeric(g_city) - 3 +
         as.numeric(g_salesperson) - 3 +
         as.numeric(callcategory) - 2 +
         n_service_calls +
         prior_revenue +
         call_to_estimate +
         rnorm(n, 0, sigma)

###convert to logistic
pr = 1/(1+exp(-close)) # pass through an inv-logit function
range(pr)
close = rbinom(n,1,pr)      # bernoulli response variable
table(close)

###create dataset
dat <- data.frame(close, g_marketing, g_year, g_city, g_salesperson,
                  callcategory, n_service_calls, prior_revenue, call_to_estimate)

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

#for reference
marketing_table
year_table
city_table
salesperson_table
callcategory_table

