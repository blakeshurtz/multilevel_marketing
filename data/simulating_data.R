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
lead_source <- gl(ng_marketing, k = nj_marketing,
            labels = c("radio", "tv", "newspaper", 
                       "internet", "vehicle"
                       )
            )

lead_source <- sample(lead_source, n, replace = FALSE)

#year
year <- gl(ng_year, k = nj_year,
                  labels = c("2015", "2016", "2017", 
                             "2018", "2019"
                  )
)

year <- sample(year, n, replace = FALSE)

#city
city <- gl(ng_city, k = nj_city,
                  labels = c("davis", "vacaville", "fairfield", 
                             "woodland", "suisun"
                  )
)

city <- sample(city, n, replace = FALSE)

#sales_professional
sales_professional <- gl(ng_salesperson, k = nj_salesperson,
             labels = c("John", "Susan", "Peter", 
                        "Steven", "Mary"
             )
)

sales_professional <- sample(sales_professional, n, replace = FALSE)

###standard (single level) variables
#product
product <- factor(c(rep("HVAC", 750),
                         rep("DHW", 500),
                         rep("SOLAR", 250))
)


product <- sample(product, n, replace = FALSE)

#n_service
n_service <- scale(rpois(1500, 2))

#prior_revenue
prior_revenue <- c(rep(0, 1000), rep(1, 500))

prior_revenue <- sample(prior_revenue, n, replace = FALSE)

#call_to_estimate
call_to_estimate <- c(scale(rpois(1500, 2)))

#creating table showing factor label and numeric value
marketing_table = unique(data.frame(bind_cols(name = lead_source,
                                               value= as.numeric(lead_source)-3
                                          )
                                )
                    )

year_table = unique(data.frame(bind_cols(name = year,
                                          value= as.numeric(year)-3
                                         )
                                )
                    )

city_table = unique(data.frame(bind_cols(name = city,
                                              value= as.numeric(city)-3
                                        )
                              )
                    )

salesperson_table = unique(data.frame(bind_cols(name = sales_professional,
                                         value= as.numeric(sales_professional)-3
                                        )
                              )
                    )


product_table = unique(data.frame(bind_cols(name = product,
                                                value= as.numeric(product)-2
                                                )
                                      )
                            )

#sd- in estimating coefficients, sigma has inverse relationship with n
sigma = .01

#simulate regression
close <- as.numeric(lead_source) - 3 + 
         as.numeric(year) - 3 + 
         as.numeric(city) - 3 +
         as.numeric(sales_professional) - 3 +
         as.numeric(product) - 2 +
         n_service +
         prior_revenue +
         call_to_estimate +
         rnorm(n, 0, sigma)

###convert to logistic
pr = 1/(1+exp(-close)) # pass through an inv-logit function
range(pr)
close = rbinom(n,1,pr)      # bernoulli response variable
table(close)

###create dataset
dat <- data.frame(close, lead_source, year, city, sales_professional,
                  product, n_service, prior_revenue, call_to_estimate)


write.csv(dat, "data/simulated_project_data.csv", row.names = FALSE)

#for reference
marketing_table
year_table
city_table
salesperson_table
product_table

