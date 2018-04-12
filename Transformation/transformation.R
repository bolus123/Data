#boxcox.f <- function(X, theta) {
	
#	if (theta == 0){
#
#		Y <- log(X)
#
#	} else {
#
#		Y <- (X ^ theta - 1) / theta
#
#	}
#
#	return(Y)

#}

back.transform.pdf <- function(X, T, pars) {

	a <- pars[1]
	b <- pars[2]
	c <- pars[3]
	sigma2 <- pars[4]

	Mu <- a * T ^ 2 + b * T + c

	res <- dnorm(X, Mu, sqrt(sigma2)) * X ^ (-1/2) * (1 / 2)

	return(res)

}

back.transform.quantile <- function(p, T, pars, interval = c(0, 100)) {

	root.finding <- function(q, p, T, pars) {

		p - integrate(back.transform.pdf, lower = 0, upper = q, T = T, pars = pars)$value

	}

	uniroot(root.finding, interval = interval, p = p, T = T, pars = pars)

}

pars <- c(15.42797, -10.62465, 13.83315, 1.194121^2)

#debug(back.transform.pdf)

back.transform.quantile(0.5, 0, pars, c(100, 700))

#logl.f <- function(X, T, pars) {
#	
#	a <- pars[1]
#	b <- pars[2]
#	c <- pars[3]
#	sigma2 <- pars[4]
#	#theta <- pars[5]
#	#lambda <- pars[6]
#
#	n <- length(X)
#
#	Mu <- a * T ^ 2 + b * T + c
#	Sigma <- diag(sigma2, n)
#
#	Y <- boxcox.f(X, theta)
#
#	ll <- - n / 2 * log(2 * pi) - 1 / 2 * log(det(Sigma)) - 
#	1 / 2 * t(Y - Mu) %*% solve(Sigma) %*% (Y - Mu)
#
#	return(-ll)
#
#}
#
#logl.pois.f <- function(X, T, pars) {
#	
#	a <- pars[1]
#	b <- pars[2]
#	c <- pars[3]
#	#lambda <- pars[4]
#
#	#n <- length(X)
#
#	lambda <- a * T ^ 2 + b * T + c
#
#	ll <- sum(log(dpois(X, lambda)))
#
#	return(-ll)
#
#}

###############################################################################################

data <- read.csv(file = '/home/yuhuiyao/Documents/Github/R-handout/Transformation/BEC.csv')

data <- data[data[, 3] < 2017, ]

min.year <- min(data[, 3])
min.month <- min(data[, 4])

X <- data[, 5]

T <- 12 * (data[, 3] - min.year) + data[, 4] - min.month

#pars <- c(1, 1, 1, 1, 1)


#num.res <- optim(pars, logl.f, hessian = TRUE, method = 'L-BFGS-B', 
#	lower = c(0, -Inf, -Inf, -Inf, -Inf), X = X, T = T)



#pars <- c(1, 1, 1, 1, 1)

model1 <- lm(X ~ poly(T, degrees = 2, raw = TRUE))

sum.model1 <- summary(model1)

X1 <- sqrt(X)

model2 <- lm(X1 ~ poly(T, degrees = 2, raw = TRUE))

sum.model2 <- summary(model2)

model3 <- glm(formula = X ~ poly(T, degrees = 2, raw = TRUE), family = poisson(link = "identity"))

sum.model3 <- summary(model3)


#pars.pois <- c(0, 0, 194.8452)

#debug(logl.pois.f)

#logl.pois.f(X, T, pars.pois)

#num.res.pois <- optim(pars.pois, logl.pois.f, hessian = TRUE, X = X, T = T)

## as same as #
#glm(formula = X ~ poly(T, degrees = 2, raw = TRUE), family = poisson(link = "identity"))
#
#Deviance Residuals: 
#    Min       1Q   Median       3Q      Max  
#-5.8613  -1.3822  -0.1294   1.2649  10.8291  
#
#Coefficients:
#                                    Estimate Std. Error z value Pr(>|z|)    
#(Intercept)                       316.106696   3.671168   86.11   <2e-16 ***
#poly(T, degrees = 2, raw = TRUE)1  -3.322372   0.094189  -35.27   <2e-16 ***
#poly(T, degrees = 2, raw = TRUE)2   0.016747   0.000533   31.42   <2e-16 ***
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1

TT <- cbind(1, poly(T, degrees = 2, raw = TRUE))

Mu1 <- TT %*% sum.model1$coefficient[, 1]

upper.bound1 <- qnorm(0.975, Mu1, sum.model1$sigma)
lower.bound1 <- qnorm(0.025, Mu1, sum.model1$sigma)

Mu2 <- TT %*% sum.model2$coefficient[, 1]

upper.bound2 <- sum.model2$sigma ^ 2 * qnorm(0.975, 1 + Mu2, sqrt(2 * (1 + 2 * Mu2)))
lower.bound2 <- sum.model2$sigma ^ 2 * qnorm(0.025, 1 + Mu2, sqrt(2 * (1 + 2 * Mu2)))

Mu3 <- TT %*% as.vector(sum.model3$coefficient[, 1])

upper.bound3 <- qpois(0.975, Mu3)
lower.bound3 <- qpois(0.025, Mu3)

###############################################################################################


T1 <- sort(T)

TT <- cbind(1, poly(T1, degrees = 2, raw = TRUE))

Mu1 <- TT %*% sum.model1$coefficient[, 1]

sigma <- sum.model1$sigma

upper.bound <- qnorm(0.975, Mu1, sqrt(sigma))

lower.bound <- qnorm(0.025, Mu1, sqrt(sigma))

plot(c(min(T), max(T)), c(min(X), max(X)), type = 'n')

points(T, X)
points(T1, upper.bound, type = 'l', lty = 1)
points(T1, lower.bound, type = 'l', lty = 1)

###############################################################################################





###############################################################################################


#Y <- boxcox.f(X, num.res$par[5])



T1 <- sort(T)
TT <- cbind(1, poly(T1, degrees = 2, raw = TRUE))

lambda <- TT %*% sum.model3$coefficient[, 1]

upper.bound <- qpois(0.975, lambda)

lower.bound <- qpois(0.025, lambda)

#plot(T, X)
points(T1, upper.bound, type = 'l', lty = 2)
points(T1, lower.bound, type = 'l', lty = 2)

###############################################################################################

###############################################################################################

###############################################################################################

ff <- function(theta) {

	1 - (3/4)^theta + (3/4)^theta * theta * (log(3) - log(4))

}