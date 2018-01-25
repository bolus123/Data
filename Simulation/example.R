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

# Build a function to simulate a contaminated normal distribution
rctnorm <- function(n, eps = 0.5, mu = c(0, 0), 
sigma = c(1, 1)) {

	# Simulate the indicators from bernoulli distribution p = eps
	I <- rbinom(n, 1, eps)
	# Simulate X
	X <- rnorm(n, mu[1], sigma[1])
	# Simulate Y
	Y <- rnorm(n, mu[2], sigma[2])

	W <- rep(NA, n)

	for (i in 1:n){

		W[i] <- ifelse(I[i] == 1, Y[i], X[i])

	}

	W

}

dctnorm <- function(x, eps = 0.5, mu = c(0, 0), 
sigma = c(1, 1)){

	dnorm(x, mu[1], sigma[1]) * (1 - eps) + dnorm(x, mu[2], sigma[2]) * eps

}

x <- seq(-100, 100, 0.01)

par(mfrow = c(1, 1))

# 3.4.26 part b, eps = 0.15, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 10
X1 <- rctnorm(n, 0.15, mu = c(0, 0), sigma = c(1, 10))
# Use histogram to graph the distribution
hist(X1, freq = F, main = 'eps = 0.15 and sigma.c = 10', ylim = c(0, 0.45))
points(x, dnorm(x, 0, 1), type = 'l', col = 'red')
points(x, dnorm(x, 0, 10), type = 'l', col = 'blue')
points(x, dctnorm(x, 0.15, c(0, 0), c(1, 10)), type = 'l', col = 'black')

# 3.4.26 part c, eps = 0.15, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 20
X2 <- rctnorm(n, 0.15, mu = c(0, 0), sigma = c(1, 20))
# Use histogram to graph the distribution
hist(X2, freq = F, main = 'eps = 0.15 and sigma.c = 20', ylim = c(0, 0.45))
points(x, dnorm(x, 0, 1), type = 'l', col = 'red')
points(x, dnorm(x, 0, 20), type = 'l', col = 'blue')
points(x, dctnorm(x, 0.15, c(0, 0), c(1, 20)), type = 'l', col = 'black')

# 3.4.26 part d, eps = 0.25, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 20
X3 <- rctnorm(n, 0.25, mu = c(0, 0), sigma = c(1, 20))
# Use histogram to graph the distribution
hist(X3, freq = F, main = 'eps = 0.25 and sigma.c = 20', ylim = c(0, 0.45))
points(x, dnorm(x, 0, 1), type = 'l', col = 'red')
points(x, dnorm(x, 0, 20), type = 'l', col = 'blue')
points(x, dctnorm(x, 0.25, c(0, 0), c(1, 20)), type = 'l', col = 'black')

# An example when we have 2 normal distribution 
# with different locations. eps = 0.5, mu1 = 0, 
# mu2 = 5, sigma1 = sigma2 = 1
X4 <- rctnorm(n, 0.5, mu = c(0, 5), sigma = c(1, 1)) 
hist(X4, freq = F, main = 'eps = 0.5 with different locations and same sigmas', ylim = c(0, 0.45))
points(x, dnorm(x, 0, 1), type = 'l', col = 'red')
points(x, dnorm(x, 5, 1), type = 'l', col = 'blue')
points(x, dctnorm(x, 0.5, c(0, 5), c(1, 1)), type = 'l', col = 'black')

# Comparing these histograms
# Define a 2 by 2 graph
par(mfrow = c(2, 2))
hist(X1, freq = F, main = 'eps = 0.15 and sigma.c = 10')
hist(X2, freq = F, main = 'eps = 0.15 and sigma.c = 20')
hist(X3, freq = F, main = 'eps = 0.25 and sigma.c = 20')
hist(X4, freq = F, main = 'eps = 0.5 with different locations and same sigmas')
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