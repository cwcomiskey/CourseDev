
library(Sleuth3)

data <- ex2223
?ex2223

# O-rings Example
b <- c(1,1,1,3) # below 18 degrees C
a <- c(rep(0, 17), 1, 1, 2) # above 18 degrees C
s_p2 <- (3*var(b) + 19*var(a))/22 # pooled variance estimate
# **Null hypothesis: two samples from same population
(mean(b) - mean(a))/sqrt(s_p2*(1/4 + 1/20)) # t-statistic!! --> Pooled variance estimate


