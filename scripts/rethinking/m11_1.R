library(rethinking)

data("chimpanzees")
d <- chimpanzees

#create index variable
d$treatment <- 1 + d$prosoc_left + 2*d$condition

#flat prior
m11.1 <- quap(
               alist(
                 pulled_left ~ dbinom( 1 , p ) ,
                 logit(p) <- a ,
                 a ~ dnorm( 0 , 10 )
               ) , data=d )


set.seed(1999)
prior <- extract.prior( m11.1 , n=1e4 )

p <- inv_logit( prior$a )
dens( p , adj=0.1 )


#sigma = 1.5 prior
m11.1 <- quap(
  alist(
    pulled_left ~ dbinom( 1 , p ) ,
    logit(p) <- a ,
    a ~ dnorm( 0 , 1.5 )
  ) , data=d )


set.seed(1999)
prior <- extract.prior( m11.1 , n=1e4 )

p <- inv_logit( prior$a )
dens( p , adj=0.1 )
