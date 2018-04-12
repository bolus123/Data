data <- read.csv(file = 'C:/Users/bolus/OneDrive - The University of Alabama/Document/Github/R-handout/LikelihoodRatioTest/iris.csv')

names(data) <- c( 
		'sl' #sepal lengt
   		,'sw' #sepal width
   		,'pl' #petal length
   		,'pw' #petal width
   		,'class' #class
)


data.virginica <- data[data$class == 'Iris-virginica', ]

par(mfrow = c(2, 2))

hist(data.virginica$sl)
hist(data.virginica$sw)
hist(data.virginica$pl)
hist(data.virginica$pw)

################################################################################################

sigma <- 0.1040

mu.0 <- 3
mu.1 <- mean(data.virginica$sw)

lnL.0 <- sum(log(dnorm(data.virginica$sw, mu.0, 0.1040)))
lnL.1 <- sum(log(dnorm(data.virginica$sw, mu.1, 0.1040)))

Delta <- -2 * (lnL.0 - lnL.1)

p.value <- 1 - (1 - pchisq(Delta, 1)) * 2

################################################################################################
