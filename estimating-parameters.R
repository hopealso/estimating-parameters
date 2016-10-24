set.seed(847337465)

# Simulate sample from binomial distribution of length K=500 with probability of success 
#   p=0.7 (prob=0.7) and number of Bernoulli trials Ntrials = 20 (size=20).
sample.length <- 500
sample.binom.n <- 20
sample.binom.p <- 0.7
sample.binom <-rbinom(sample.length, size = sample.binom.n, prob = sample.binom.p)
sample.binom

# Use method of moments to estimate p
(estimate.binom.p <- mean(sample.binom) / sample.binom.n)

(paste("Population p (provided to rbinom pseudom random sample generator):", sample.binom.p))
(paste("Method of Moments estimate of p:", estimate.binom.p))

suppressWarnings(library(fitdistrplus))
fit.binom <- fitdist(sample.binom, distr = "binom", fix.arg = list(size = sample.binom.n), 
                     start = list(prob = 0.7))
c(fit.binom$estimate, sd = fit.binom$sd)

left <- pbinom(sum(sample.binom), size = 20 * 500, prob = 0.7)
right <- pbinom(7000 + (7000 - sum(sample.binom)), size = 20 * 500, prob = 0.7, lower.tail = FALSE)
p.value <- c(left, right)
sum(p.value)

binom.test(sum(sample.binom), 500*20, p=0.7)
