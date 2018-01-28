######################################################################################
					#Direct integration
######################################################################################
# define the p.d.f. of Y
Y.pdf <- function(y) 2 * dnorm(y)

# define the integrand of the expecation of Y
E.Y <- function(y) y * Y.pdf(y)

# Integrate the integrand
integrate(E.Y, lower = 0, upper = Inf)
# Result: 0.7978846

######################################################################################
					#MC with a Transformation
######################################################################################
# set seed to make this process repeatable
set.seed(12345)

# set the number of simulation
n <- 1000

# simulate a sample from the standard normal distribution
X.s <- rnorm(n)

# calculate the mean of the absoluate values
mean(abs(X.s))
# Result: 0.7944
######################################################################################
					#MC with another Transformation
######################################################################################
# set seed to make this process repeatable
set.seed(12345)

# set the number of simulation
n <- 1000

# define the p.d.f. of Y
Y.pdf <- function(y) 2 * dnorm(y)

# simulate a sample from the standard normal distribution
Y.s <- rexp(n)

# calculate the mean of the absoluate values
mean(Y.s * Y.pdf(Y.s) / dexp(Y.s))
# Result: 0.7729
