# M7 Lab

# Lab
# 1. Simulations — generate data from different (pathological) distributions & sample sizes, and show that the t-test is still pretty robust. You can just have them look at p-values here. Make the simulation sizes at least 100,000 (we don’t want to worry about simulation error yet)

# (a) Start with Normal distribution, one sample—-demonstrate that the p- values are uniform (so that if we we’re interested in a particular level (T1E), we could get a rejection rate from that histogram)


library(ggplot2)

pval <- function(n){
  x <- rnorm(n, mean = 0, sd = 1)
  test <- t.test(x, mu = 0)
  test$p.value
}

pval()

p <- replicate(10000, t.test(rnorm(25, mean = 0, sd = 1), mu = 0)$p.value)

qplot(p, binwidth = 0.05) # Type 1 error calculator

# (b) Now simulate from 2 distributions with really different shapes but the same means, two small and different sample sizes—now show the histogram of p-values—won’t be so uniform. Gradually increase sample sizes–histogram of p-values starts to look more uniform.

# beta(0.5, 0.5) & Gamma(2,2)
x <- seq(from = 0, to = 1, by = 0.01)
y_b <- dbeta(x, 0.5, 0.5) # mean = 1/2 
y_u <- dunif(x, 0, 1) # mean = 1/2

ggplot() + geom_line(aes(x = x, y = y_b), color = "blue", size = 2) + 
  geom_line(aes(x = x, y = y_u), color = "red", size = 2) 

var(rbeta(10000, shape1 = 0.5, shape2 = 0.5))
var(runif(10000, 0, 1))

# 2-sample t-test
x <- rbeta(10, shape1 = 0.5, shape2 = 0.5)
y <- runif(10, 0, 1)
t.test(x, y, mu = 0, paired = FALSE, var.equal = FALSE)

pval2 <- function(n1, n2){
  x <- rbeta(n1, shape1 = 0.5, shape2 = 0.5)
# y <- rgamma(n2, shape = 2, rate = 4)
  y <- runif(n2, min = 0, max = 1)
  t.test(x, y, mu = 0, paired = FALSE, var.equal = FALSE)$p.value
}

var(runif(100000))

sim <- replicate(10000, pval2(50, 100))
qplot(sim)

# (c) We’re doing all of this with Welch’s t-test, so equal variance doesn’t matter.
