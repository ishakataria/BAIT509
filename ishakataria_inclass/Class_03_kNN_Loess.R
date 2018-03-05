library(tidyverse)
library(knitr)

## EXERCISE 1: MEAN AT X = 0
set.seed(87)
dat <- tibble(x = c(rnorm(100), rnorm(100)+5)-3,
              y = sin(x^2/5)/x + rnorm(200)/10 + exp(1))
head(dat)

# kNN
dat$x
dat$d <- abs(dat$x)
dat$d
arrange(dat, d)

# Subsetting the first 10 rows
dat.subset.kNN <- dat[1:10, ]
y.estimate.kNN <- mean(dat.subset$y)
y.estimate.kNN

#Loess
dat.subset.loess <- filter(dat, dat$d<1)
y.estimate.loess <- mean(dat.subset.loess$y)
y.estimate.loess


# EXERCISE 2: REGRESSION CURVE

library(tidyverse)
xgrid <- seq(-5, 4, length.out=1000)
kNN_estimates <- map_dbl(xgrid, function(x.input){
  dat$d <- abs(x.input - dat$x)
  arrange(dat, d)
  
  # Subsetting the first 10 rows
  dat.subset.kNN <- dat[1:5, ]
  yhat <- mean(dat.subset$y)
  return(yhat)
})

loess_estimates <- map_dbl(xgrid, function(x.input){
  dat$d <- abs(x.input - dat$x)
  arrange(dat, d)
  
  # Subsetting the first 10 rows
  dat.subset.loess <- filter(dat, dat$d<2)
  yhat <- mean(dat.subset.loess$y)
  return(yhat)
})

est <- tibble(x=xgrid, kNN=kNN_estimates, loess=loess_estimates) %>% 
  gather(key="method", value="estimate", kNN, loess)
ggplot() +
  geom_point(data=dat, mapping=aes(x,y), colour="red") +
  geom_line(data=est, 
            mapping=aes(x,estimate, group=method, colour=method)) +
  theme_bw()
