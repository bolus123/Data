################################################################################################
	# Loading data
################################################################################################

# set a seed
set.seed(12345)

# specify the address of the data
data.addr <- 'https://raw.githubusercontent.com/
	bolus123/R-handout/master/LikelihoodRatioTest/iris.csv'

# load the data
data <- read.csv(file = data.addr)

# name the columns
names(data) <- c( 
		'sl' #sepal length
   		,'sw' #sepal width
   		,'pl' #petal length
   		,'pw' #petal width
   		,'class' #class
)

# get the fraction of Virginica
data.virginica <- data[data$class == 'Iris-virginica', ]

# describe the Virginica data
mean(data.virginica$sw)
var(data.virginica$sw)

# build a histogram
hist(data.virginica$sw, freq = FALSE, xlab = 'Sepal Width', 
  main = 'Histogram of the Sepal Width of Virginica')
# add a normal distribution to see the fit
curve(dnorm(x, mean(data.virginica$sw), sd(data.virginica$sw)), add = TRUE)
# It looks good! We  can do more confirmatory analysis 
# of the fit but we don't do that here

################################################################################################
	# Exact Method
################################################################################################

# sample size of Virginica
n <- dim(data.virginica)[1]

# sigma2 is known
sigma2 <- 0.1040
sigma <- sqrt(sigma2)

# under the null hypothesis
mu.0 <- 3 

# under the alternative hypothesis
mu.1 <- mean(data.virginica$sw) 

# calculate the statistic
Lambda <- - n / 2 / sigma2 * (mu.1 - mu.0)^2
Lambda <- -2 * Lambda
Lambda

# critical values for alpha = 0.05 under the equal-tailed assumption
C <- qchisq(0.95, 1)
C

# calculate p-value
p <- pchisq(Lambda, 1)
p.value <- 1 - p
p.value

################################################################################################
	# Parametric Bootstrap
################################################################################################

# maximum number of simulations
sim <- 10000

# sample size of Virginica
n <- dim(data.virginica)[1]

# get the empirical distribution based on the parametric bootstrap
ref.par <- rep(NA, sim)

for (i in 1:sim){

	# simulate data under the null hypothesis
	X <- rnorm(n, mu.0, sigma)
  # mu.0 = 3 and sigma2 = 0.1040
	
	# calculate Lambda
	lnL.0 <- sum(log(dnorm(X, mu.0, sigma)))
	lnL.1 <- sum(log(dnorm(X, mean(X), sigma)))
	ref.par[i] <- -2 * (lnL.0 - lnL.1)

}

# describe the empirical distribution of Lambda based on the parametric bootstrap
hist(ref.par, freq = FALSE, main = 'Histogram of the empirical distribution 
	of -2Lambda based on the parametric bootstrap', xlab = '-2Lambda')
# point out the Lambda from the original data
abline(v = Lambda, lty = 2, col = 'red') 
text(Lambda, 0.4, as.character(round(Lambda, 4)), srt = 270, pos = 4, cex = 0.7) 

# critical values for alpha = 0.05 under the equal-tailed assumption
C <- quantile(ref.par, 0.95)
C

# calculate p-value
p <- mean(ref.par < Lambda)
p.value.par <- 1 - p
p.value.par

################################################################################################
	# Nonparametric Bootstrap
################################################################################################

# maximum number of simulations
sim <- 10000

# sample size of Virginica
n <- dim(data.virginica)[1]

# get the empirical distribution based on the nonparametric bootstrap
ref.nonpar <- rep(NA, sim)

for (i in 1:sim){

	# resample from the original data set
	X <- sample(data.virginica$sw, size = n, replace = TRUE)
	
	# calculate Lambda
	lnL.0 <- sum(log(dnorm(X, mu.0, sigma)))
	lnL.1 <- sum(log(dnorm(X, mean(X), sigma)))
	ref.nonpar[i] <- -2 * (lnL.0 - lnL.1)

}

# describe the empirical distribution of Lambda based on the nonparametric bootstrap
hist(ref.nonpar, freq = FALSE, main = 'Histogram of the empirical distribution 
     of -2Lambda based on the nonparametric bootstrap', xlab = '-2Lambda')
# point out the Lambda from the original data
abline(v = Lambda, lty = 2, col = 'red') 
text(Lambda, 0.4, as.character(round(Lambda, 4)), srt = 270, pos = 4, cex = 0.7) 

# critical values for alpha = 0.05 under the equal-tailed assumption
C <- quantile(ref.nonpar, 0.95)
C

# calculate p-value
p <- mean(ref.nonpar < Lambda)
p.value.nonpar <-  1 - p
p.value.nonpar


################################################################################################


data.versicolor <- data[data$class == 'Iris-versicolor', ]

par(mfrow = c(2, 2))

hist(data.versicolor$sl)
hist(data.versicolor$sw)
hist(data.versicolor$pl)
hist(data.versicolor$pw)


data.setosa <- data[data$class == 'Iris-setosa', ]

par(mfrow = c(2, 2))

hist(data.setosa$sl)
hist(data.setosa$sw)
hist(data.setosa$pl)
hist(data.setosa$pw)