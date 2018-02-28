library(tidyverse)

# Oracle Regression
genreg <- function(n){
  x1 <- rnorm(n)
  x2 <- rnorm(n)
  eps <- rnorm(n)
  y <- 5-x1+2*x2+eps
  tibble(x1=x1, x2=x2, y=y)
}

dat <- genreg(1000)

dat <- mutate(dat,
              yhat = 0,
              yhat1 = 5 - x1,
              yhat2 = 5 + 2*x2,
              yhat12 = 5 - x1 + 2*x2)
#head(dat)

(mse <- mean((dat$yhat - dat$y)^2))
(mse1 <- mean((dat$yhat1 - dat$y)^2))
(mse2 <- mean((dat$yhat2 - dat$y)^2))
(mse12 <- mean((dat$yhat12 - dat$y)^2))

# Oracle Classification

# x1 represents x = 1
pA.x1 = 0.2
pB.x1 = 0.8/(1+exp(-1))
pC.x1 = 1 - (pA.x1 + pB.x1)

# x2 represents x = -2
pA.x2 = 0.2
pB.x2 = 0.8/(1+exp(2))
pC.x2 = 1 - (pA.x2 + pB.x2)

pA.x1
pB.x1
pC.x1

pA.x2
pB.x2
pC.x2

dat.x1 <- cbind(pA.x1, pB.x1, pC.x1)
barplot(dat.x1, main = "Probabilities of each category of y when x = 1")

dat.x2 <- cbind(pA.x2, pB.x2, pC.x2)
barplot(dat.x2, main = "Probabilities of each category of y when x = -2")


gencla <- function(n) {
  x <- rnorm(n) 
  pB <- 0.8/(1+exp(-x))
  y <- map_chr(pB, function(t) 
    sample(LETTERS[1:3], size=1, replace=TRUE,
           prob=c(0.2, t, 1-t-0.2)))
  tibble(x=x, y=y)
}

dat2 <- gencla(1000)
dat2 <- mutate(dat2,
               yhat = sapply(x, function(x_)
                 if(x_<0) "C" else "B"))

error.rate <- 1 - mean(dat2$yhat == dat2$y)

