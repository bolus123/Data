data <- read.csv(file = '/home/yuhuiyao/Documents/Github/R-handout/Estimation/crime.csv') 
monthly.freq <- as.data.frame(with(data, table(TYPE, YEAR, MONTH)))


monthly.freq$TYPE[monthly.freq$TYPE == ]

save(monthly.freq, file = '/home/yuhuiyao/Documents/Github/R-handout/Estimation/example.Rdata')


boxplot(Freq ~ TYPE, data = monthly.freq, las = 2, names = c('BEC', 'BER', 'HOM', 'MIS', 'OAP', 'OT', 'TFV', 'TOB', 'TOV', 'VPF', 'VPI'))


BEC.monthly.freq <- monthly.freq[monthly.freq$TYPE == 'Break and Enter Commercial', ]

boxplot(BEC.monthly.freq$Freq)


#suppose fit a poisson distribution
lambda <- mean(BEC.monthly.freq$Freq)

hist(BEC.monthly.freq$Freq, freq = F, breaks = 100, ylim = c(0, 0.03))
x <- seq(0, 1000, 1)
points(x, dpois(x, lambda), type = 'l')


BEC.monthly.freq <- cbind(BEC.monthly.freq, paste(BEC.monthly.freq$YEAR, substr(as.character(as.numeric(BEC.monthly.freq$MONTH) + 100), 2, 3), sep = '-'))

plot(BEC.monthly.freq[, 5], BEC.monthly.freq[, 4])

prepare.Winter.Olympic <- BEC.monthly.freq[, 5] == '2003-06'
Winter.Olympic <- BEC.monthly.freq[, 5] == '2010-02'
Stanley.Cup.riot <- BEC.monthly.freq[, 5] == '2011-06'

abline(v = BEC.monthly.freq[prepare.Winter.Olympic, 5], col = 'red', lty = 2)
text(BEC.monthly.freq[prepare.Winter.Olympic, 5], 5, 'Preparation of Winter Olympic', pos = 4, srt = 90, cex = 0.8)

abline(v = BEC.monthly.freq[Winter.Olympic, 5], col = 'blue', lty = 2)
text(BEC.monthly.freq[Winter.Olympic, 5], 5, 'Winter Olympic', pos = 4, srt = 90, cex = 0.8)

abline(v = BEC.monthly.freq[Stanley.Cup.riot, 5], col = 'green', lty = 2)
text(BEC.monthly.freq[Stanley.Cup.riot, 5], 5, 'Stanley Cup Riot', pos = 4, srt = 90, cex = 0.8)

x1.max.date <- which(BEC.monthly.freq[order(BEC.monthly.freq[, 5]), 5] == '2003-06')

x1 <- BEC.monthly.freq[order(BEC.monthly.freq[, 5]), 4][1:x1.max.date]

lambda1 <- mean(x1)

abline(h = lambda1 + 3 * sqrt(lambda1), lty = 2)
abline(h = lambda1 - 3 * sqrt(lambda1), lty = 2)