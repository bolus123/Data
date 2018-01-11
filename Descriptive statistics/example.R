# Load table from my Github
add <- 'https://raw.githubusercontent.com/bolus123/Data
/master/data1.csv'
data <- as.matrix(read.csv(file = add))
data

# Graph this data
hist(data, breaks = 20) #histogram for the whole data with 20 breaks
boxplot(as.vector(data)) #boxplot for the whole data
boxplot(data) #boxplot for each column

# Basic statistics
mean(data) #grand mean
colMeans(data) #means for each column
rowMeans(data) #means for each row

var(as.vector(data)) #grand variance
var(data) #covariance matrix

quantile(data, c(0.01, 0.05, 0.1, 0.25, 0.5, 0.75, 0.9, 0.95, 0.99)) #percentiles including 1%, 5%, 10%, 25%, 50%, 75%, 90%, 95% and 99%

# fit a model based on the univariate normal distribution for the whole data
mu <- mean(data) #grand mean
sigma <- sqrt(var(as.vector(data))) #standard deviation

hist(data, breaks = 20, freq = FALSE, ylim = c(0, 40)) #histogram for the whole data with 20 breaks
curve(dnorm(x, mean = mu, sd = sigma), add = T, col = 'blue')

# check the normality (Q-Q plot)
# we need to have the empirical quantile and the theoretical quantile based on the empirical probability

# 1. we need to know the whole sample size
n <- dim(data)[1] * dim(data)[2]

# 2. sort the data and this is our empirical quantile
e.q <- sort(data)

# 3. calculate the Empirical cdf
e.p <- 1:n / n

# 4. find out the theoretical quantile
t.q <- qnorm(e.p, mean = mu, sd = sigma)

# 5. draw a Q-Q plot
plot(e.q, t.q, xlab = 'Empirical', ylab = 'Theoretical', main = 'Q-Q plot')
points(c(0, 100), c(0, 100), type = 'l', col = 'blue') #reference line