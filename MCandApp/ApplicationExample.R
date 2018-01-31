#################################################
	# load the raw data, 
	# count crimes by month and crime type, 
	# and build our data set
	# you don't need to run this part
#################################################
## set the path storaging our data
#addr <- '/home/yuhuiyao/Documents/Github/R-handout/MCandApp/crime.csv'
## Load the data
#data <- read.csv(file = addr) 
#
## count them by month and crime type
#monthly.freq <- as.data.frame(
#	with(data, table(TYPE, YEAR, MONTH)))
#
## our analysis will be based on this data set
#BEC.monthly.freq <- monthly.freq[monthly.freq$TYPE == 
#	'Break and Enter Commercial', ]
#
## save our new data
#addr <- '/home/yuhuiyao/Documents/Github/R-handout/MCandApp/BEC.Rdata'
#save(BEC.monthly.freq, file = addr)
#################################################
	# describe BEC during the period
#################################################
# add a new column combining YEAR with MONTH
BEC.monthly.freq <- cbind(BEC.monthly.freq, 
	paste(BEC.monthly.freq$YEAR, 
			substr(
				as.character(
					as.numeric(
						BEC.monthly.freq$MONTH) + 
							100), 2, 3)
, sep = '-'))

# build a basic scatter frequency plot 
# during the period
plot(BEC.monthly.freq[, 5], BEC.monthly.freq[, 4], 
	xaxt="n", ylab = 'Frequency')
# define the tick of x-axis
labs <- sort(BEC.monthly.freq[, 5])[rep(c(T, F, F, 
	F, F, F, F, F, F, F, F, F), 15)]
for (i in 1:15){

	axis(1, at = (12 * (i - 1) + 1), 
		labels = labs[i], las = 2)

}

# specify special months
prepare.Winter.Olympic <- BEC.monthly.freq[, 5] == '2003-06'
Winter.Olympic <- BEC.monthly.freq[, 5] == '2010-02'
Stanley.Cup.riot <- BEC.monthly.freq[, 5] == '2011-06'

# show Preparation of Winter Olympic on the plot
abline(v = BEC.monthly.freq[prepare.Winter.Olympic, 5], 
	col = 'red', lty = 2)
text(BEC.monthly.freq[prepare.Winter.Olympic, 5], 5, 
	'Preparation of Winter Olympic', pos = 4, 
		srt = 90, cex = 0.8)

# show Winter Olympic on the plot 
abline(v = BEC.monthly.freq[Winter.Olympic, 5], 
	col = 'blue', lty = 2)
text(BEC.monthly.freq[Winter.Olympic, 5], 5, 
	'Winter Olympic', pos = 4, srt = 90, cex = 0.8)

# show Stanley Cup Riot on the plot 
abline(v = BEC.monthly.freq[Stanley.Cup.riot, 5], 
	col = 'green', lty = 2)
text(BEC.monthly.freq[Stanley.Cup.riot, 5], 5, 
	'Stanley Cup Riot', pos = 4, srt = 90, cex = 0.8)

# set the maximun date of the training data
x1.max.date <- which(
	BEC.monthly.freq[
		order(BEC.monthly.freq[, 5]), 5] == '2003-06')

# cut it off from the original data
x1 <- BEC.monthly.freq[
	order(BEC.monthly.freq[, 5]), 4][1:x1.max.date]

# fit a poisson model for the training data
lambda1 <- mean(x1)

# show the 2.5% and 97.5 quantiles on the plot
abline(h = qpois(0.975, lambda1), lty = 2)
abline(h = qpois(0.025, lambda1), lty = 2)

#################################################
	# find the gamma distribution for lambda 
	# by the nonparametric bootstrap
#################################################
set.seed(12345)

# the number of times for bootstrapping
n <- 100000
# set a vector to carry the means
xs.means <- rep(NA, n)

for (i in 1:n){

	# resample from the training data
	xs <- sample(x1, 5, replace = T)

	# calculate their means
	xs.means[i] <- mean(xs)

}

# calculate the grand mean
mu <- mean(xs.means)
# calculate the grand variance
sigma2 <- var(xs.means)

# fit a gamma model by the method of moment
alpha <- mu^2 / sigma2
beta <- sigma2 / mu

# check the gamma distribution
#x <- 1:1000 
#plot(x, dgamma(x, alpha, 1/beta), type = 'l')

#################################################
	# fit a new poisson model 
	# with parameter lambda 
	# which is a random variable
#################################################
# set a seed make this process repeatable
set.seed(12345)

# define a user-defined function
# p is P(X <= x)
# alpha is the alpha for gamma(alpha, beta)
# beta is the beta for gamma(alpha, beta)
# interval is the range of searching the quantile
# rnum is the number of simulations
X.quantile <- function(p, alpha, beta, 
	interval = c(100, 500), rnum = 10000){

	root.finding <- function(x, p, lambda){

		# The root searching form
		p - mean(ppois(x, lambda))

	}

	# simulate a sample from gamma distribution
	lambda <- rgamma(rnum, alpha, scale = beta)

	# search the root by the bisection method
	uniroot(root.finding, interval = interval, 
		p = p, lambda = lambda)$root


}

# calcualte the 2.5% quantile
q0025 <- X.quantile(p = 0.025, 
	alpha = alpha, beta = beta)
# calcualte the 97.5% quantile
q0975 <- X.quantile(p = 0.975, alpha = alpha, beta = beta)

# add horizontal lines on the plot
abline(h = q0025)
abline(h = q0975)