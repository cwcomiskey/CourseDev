# Permutation Tests

??coin
??perm
?exactRankTests() 

# Gamma vs. Normal Permutation test =============

# Gamma(2, 2) plot 
x <- seq(from = 0, to = 5, by = 0.01)
y <- dgamma(x, shape = 2, rate = 2) 

library("ggplot2")
ggplot() + geom_line(aes(x = x, y = y), color = "blue", size = 2)

set.seed(1930) # Permutation tests evolved from work of R.A. Fisher and E.J.G. Pitman in the 1930s
x <- rnorm(50, 0, 1) # mean = 0
y <- rgamma(50, 2, 2) # mean = 1

T <- mean(x) - mean(y) # "observed" test statistic

permtest <- function(){
  index <- sample(seq(from = 1, to = 100), size = 10, replace = FALSE) # Change "to = 100" for different sample sizes
  mean(c(x, y)[index]) - mean(c(x, y)[-index])
}

perms <- replicate(100, permtest())
mean(abs(perms) > abs(T))

qplot(perms, binwidth = 0.075) + 
  geom_vline(xintercept = c(-T,T), color = "blue", size = 1)

# O-rings Permutation Test ========================
# **Null hypothesis: two samples from same population

# library(Sleuth3)
# ?ex2223

# Data in below/above groups
b <- c(1,1,1,3) # below 18 degrees C
a <- c(rep(0, 17), 1, 1, 2) # above 18 degrees C

# Test Statistic
s_p2 <- (3*var(b) + 19*var(a))/22 # pooled variance estimate
T <- (mean(b) - mean(a))/sqrt(s_p2*(1/4 + 1/20)) 

# Create "all data" vector
data <- c(b, a) 

# Permutation function: (i) randomly reassign groups, (ii) calculate t-stat
permtest <- function(){
  index <- sample(seq(from = 1, to = 24), size = 4, replace = FALSE) 
  b <- data[index]
  a <- data[-index]
  s_p2 <- (3*var(b) + 19*var(a))/22 # pooled variance estimate
  (mean(b) - mean(a))/sqrt(s_p2*(1/4 + 1/20)) 
}

permtest()

perms <- replicate(10000, permtest())
mean(perms > T)

qplot(perms, binwidth = .5) + 
  geom_vline(xintercept = T, color = "blue", size = 1)

# O-rings Permutation Test - Coin Package ========================

b <- c(1,1,1,3) # below 18 degrees C
a <- c(rep(0, 17), 1, 1, 2) # above 18 degrees C

I <- c(b, a)
G <- c(rep(0,4), rep(1, 20))
str(G)

oneway_test(I ~ as.factor(G))

# O-rings Permutation Test - perm Package ========================

library(perm)
permTS(b, a, exact = TRUE, alternative = "greater") # duplicates exact answer from M8, Lecture 1, Slide 9



