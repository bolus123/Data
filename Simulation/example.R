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

# Simulate I
I <- rbinom(n, 1, 1 - eps)
# Simulate X
X <- rnorm(n, mu[1], sigma[1])
# Simulate Y
Y <- rnorm(n, mu[2], sigma[2])

# Simulate W
W <- rep(NA, n)
for (i in 1:n){
W[i] <- ifelse(I[i] == 1, X[i], Y[i])
}
W
}

# the p.d.f of contaminated normal distribution
dctnorm <- function(x, eps = 0.5, mu = c(0, 0), 
sigma = c(1, 1)){
dnorm(x, mu[1], sigma[1]) * (1 - eps) + 
dnorm(x, mu[2], sigma[2]) * eps
}

# get a sequence from -100 to 100 having step 0.01 
# as the points in x-aixs				
x <- seq(-100, 100, 0.01)

# 3.4.26 part b, eps = 0.15, mu1 = mu2 = 0, 
# sigma1 = 1, sigma2 = sigma.c = 10
X1 <- rctnorm(n, 0.15, mu = c(0, 0), sigma = c(1, 10))
# Use histogram to graph the distribution
hist(X1, freq = F, main = 'eps = 0.15 and sigma.c = 10'
, ylim = c(0, 0.45))

# graph the p.d.f. of standard normal distribution 
# in a red curve
points(x, dnorm(x, 0, 1), type = 'l', col = 'red')

# graph the p.d.f. of normal distribution with mu=0 
# and sigma=10 in a blue curve
points(x, dnorm(x, 0, 10), type = 'l', col = 'blue')

# graph the p.d.f. of contanminated normal distribution 
# with eps = 0.15, mu1 = mu2 = 0, sigma1 = 1, sigma 2 = 10 
# in a black curve
points(x, dctnorm(x, 0.15, c(0, 0), c(1, 10)), type = 'l'
, col = 'black')

# An example when we have 2 normal distribution 
# with different locations. eps = 0.5, mu1 = 0, 
# mu2 = 5, sigma1 = sigma2 = 1
X4 <- rctnorm(n, 0.5, mu = c(0, 5), sigma = c(1, 1)) 
hist(X4, freq = F, main = 'eps = 0.5 with different locations 
and same sigmas', ylim = c(0, 0.45))

# graph the p.d.f. of standard normal distribution 
# in a red curve
points(x, dnorm(x, 0, 1), type = 'l', col = 'red')

# graph the p.d.f. of normal distribution 
# with mu = 5 and sigma = 1 in a blue curve
points(x, dnorm(x, 5, 1), type = 'l', col = 'blue')

# graph the p.d.f. of contaminated normal distribution
# with eps = 0.5, mu1 = 0, mu2 = 5, sigma1 = 1 
# and sigma2 = 2 in a black curve
points(x, dctnorm(x, 0.5, c(0, 5), c(1, 1)), type = 'l'
, col = 'black')
15
