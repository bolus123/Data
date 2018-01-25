######################################################################################
					#Probability Integral Transform
######################################################################################
# Set a seed to make this process repeatable
set.seed(12345) 

# Set the number of simulations
n <- 100000

# Simulate data from a standard normal distribution
X <- rnorm(n)

# Find out the coresponding quantiles
Y <- pnorm(X)

# Use histogram to graph the distribution
hist(Y, freq = F, ylim = c(0, 1.5))

######################################################################################
					#Normal distribution
######################################################################################
# 1. Direct
# Set a seed to make this process repeatable
set.seed(12345) 

# Set the number of simulations
n <- 100000

# Simulate data from a standard normal distribution
X <- rnorm(n)

# Use histogram to graph the distribution
hist(X, freq = F)

# 2. Indirect
# Set a seed to make this process repeatable
set.seed(12345) 

# Set the number of simulations
n <- 100000

# Simulate data from Uniform(0,1)
Y <- runif(n)

# By PIT, get a sample from a standard normal distribution
X <- qnorm(Y)

# Use histogram to graph the distribution
hist(X, freq = F)
######################################################################################
					#Contaminated Normal distribution
######################################################################################
# Set a seed to make this process repeatable
set.seed(12345) 

# Set the number of simulations
n <- 100000

# Build a function to simulate the "jumping" process
rctnorm <- function(n, eps = 0.5, mu = c(0, 0), 
sigma = c(1, 1)) {

# Define the "jumping" process
jump <- sample(c(1, 2), prob = c(1 - eps, eps), 
size = n, replace = TRUE)
# Simulate data with "jumps"
rnorm(n, mean = mu[jump], sd = sigma[jump])

}

# 3.4.26 part b, eps = 0.15, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 10
X1 <- rctnorm(n, 0.15, mu = c(0, 0), sigma = c(1, 10))
# Use histogram to graph the distribution
hist(X1, freq = F)

# 3.4.26 part c, eps = 0.15, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 20
X2 <- rctnorm(n, 0.15, mu = c(0, 0), sigma = c(1, 20))
# Use histogram to graph the distribution
hist(X2, freq = F)

# 3.4.26 part d, eps = 0.25, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 20
X3 <- rctnorm(n, 0.25, mu = c(0, 0), sigma = c(1, 20))
# Use histogram to graph the distribution
hist(X3, freq = F)

# An example when we have 2 normal distribution 
# with different locations. eps = 0.5, mu1 = 0, 
# mu2 = 2, sigma1 = sigma2 = 1
X4 <- rctnorm(n, 0.5, mu = c(0, 5), sigma = c(1, 1)) 
hist(X4, freq = F)

######################################################################################
					#gamma distribution
######################################################################################
# Set a seed to make this process repeatable
set.seed(12345) 

# Set the number of simulations
n <- 100000

# Simulate data from gamma with alpha = 2 and beta = 5
X <- rgamma(n, 2, 5)

# Use histogram to graph the distribution
hist(X, freq = F)