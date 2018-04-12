data <- read.csv(file = 'C:/Users/bolus/OneDrive - The University of Alabama/Document/Github/R-handout/LikelihoodRatioTest/iris.csv')

names(data) <- c( 
		'sl' #sepal lengt
   		,'sw' #sepal width
   		,'pl' #petal length
   		,'pw' #petal width
   		,'class' #class
)


data.virginica <- data[data$class == 'Iris-virginica', ]

#par(mfrow = c(2, 2))

#hist(data.virginica$sl)
hist(data.virginica$sw, freq = FALSE)
curve(dnorm(x, mean(data.virginica$sw), sd(data.virginica$sw)), add = TRUE)
#hist(data.virginica$pl)
#hist(data.virginica$pw)

################################################################################################

################################################################################################

sigma2 <- 0.1040
sigma <- sqrt(sigma2)

mu.0 <- 3
mu.1 <- mean(data.virginica$sw)

n <- dim(data.virginica)[1]

delta <- - n / 2 / sigma2 * (mu.1 - mu.0)^2

Delta <- -2 * delta

Delta

p <- pchisq(Delta, 1)
p.value <- ifelse(p > 0.5, 1 - (1 - p) * 2, p * 2 )

p.value

################################################################################################

################################################################################################

sim <- 10000

ref.par <- rep(NA, sim)

for (i in 1:sim){

	X <- rnorm(n, mu.0, sigma)

	lnL.0 <- sum(log(dnorm(X, mu.0, sigma)))
	lnL.1 <- sum(log(dnorm(X, mean(X), sigma)))

	ref.par[i] <- -2 * (lnL.0 - lnL.1)

}

p <- mean(ref.par < Delta)

p.value.par <- ifelse(p > 0.5, 1 - (1 - p) * 2, p * 2 ) 

p.value.par

################################################################################################

################################################################################################

sim <- 10000

ref.nonpar <- rep(NA, sim)

for (i in 1:sim){

	X <- sample(data.virginica$sw, size = n, replace = TRUE)

	lnL.0 <- sum(log(dnorm(X, mu.0, sigma)))
	lnL.1 <- sum(log(dnorm(X, mean(X), sigma)))

	ref.nonpar[i] <- -2 * (lnL.0 - lnL.1)

}

p <- mean(ref.nonpar < Delta)

p.value.nonpar <-  ifelse(p > 0.5, 1 - (1 - p) * 2, p * 2 ) 

p.value.nonpar