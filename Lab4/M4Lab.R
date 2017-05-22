install.packages("Sleuth3")
library("Sleuth3")

?ex0321
umpire <- ex0321 # pg 77 in Sleuth (2nd Ed.)

# or command line...
# ex0321 <- read.csv("~/Desktop/ex0321.csv")
# ex0321 <- read.csv("/Users/ABC/Desktop/ex0321.csv")
# ?read.csv

head(umpire) # Take a look at umpire
str(umpire) # R structural breakdown of umpire object

umpire <- subset(umpire, Censored == 0) # Remove umpires still alive
?subset

head(umpire)
# x <- umpire$Lifelength
x <- umpire$Expected
library("ggplot2")
qplot(x, binwidth = 1)

# pg 178, OpenIntro
Z <- (mean(x) - 69.5)/(sd(x)/sqrt(195))
pnorm(Z, mean = 0, sd = 1, lower.tail = TRUE)
?pnorm

x1 <- seq(from = -4, to = 4, by = 0.001)
y1 <- dnorm(x1, 0, 1)

x2 <- seq(from = -4, to = Z, by = 0.01)
y2 <- dnorm(x2, 0, 1)

ggplot() + geom_area(aes(x = x1, y = y1), size = 2) +
  labs(x = "", y = "") + 
  geom_area(aes(x = x2, y = y2), fill = "blue") +
  scale_x_continuous(breaks = c(-1.927, 0), 
                     labels = c("Z", "0"))

# M4 HW =======================================

qplot(x, binwidth = 1)
ggsave("M4HWQ1.pdf")

# pg 178, OpenIntro
Z <- (mean(x) - 68.75)/(sd(x)/sqrt(195)); Z
pnorm(Z, mean = 0, sd = 1, lower.tail = FALSE)
?pnorm

x1 <- seq(from = -4, to = 4, by = 0.001)
y1 <- dnorm(x1, 0, 1)

x2 <- seq(from = -4, to = Z, by = 0.01)
y2 <- dnorm(x2, 0, 1)

ggplot() + geom_area(aes(x = x1, y = y1), size = 2) +
  labs(x = "", y = "") + 
  geom_area(aes(x = x2, y = y2), fill = "blue") +
  scale_x_continuous(breaks = c(-1.927, 0), 
                     labels = c("Z", "0"))
 