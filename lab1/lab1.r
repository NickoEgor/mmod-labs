#!/usr/bin/env Rscript

library(ggplot2)
library(rjson)

source("brv.r")
source("weibull.r")
source("utils.r")

PREC <- 7

amount <- 10000
a <- 0
b <- 1.5

l <- 0.5
k <- 2

now <- round(as.numeric(Sys.time()))

brv0 <- generate_brv(alpha = now, n = amount)
brv <- a + (b-a)*brv0

prob_brv <- prob_weibull(brv, l, k)
distr_brv <- distr_weibull(brv0, l, k)

std <- seq(a, b, length.out = amount)
prob_std <- prob_weibull(std, l, k)
# distr_std <- distr_weibull(std, l, k)


# estimations
sink("raport.txt")

cat(paste("Mean (expected):", 1/2, "\n"))
cat(paste("Variance (expected):", 1/12, "\n"))
cat("\n")

cat(paste("Mean (point):", mean(brv0), "\n"))
cat(paste("Variance (point):", var(brv0), "\n"))
cat("\n")

cat("Mean (interval):\n")
cat(toJSON(get_mean_est(brv0), 2), "\n")
cat(toJSON(get_mean_est(brv0, 0.99), 2), "\n")
cat("\n")

# t.test(brv0)
# t.test(brv0, conf.level=0.99)
sink()
# ------------

pdf("raport.pdf")
hist(brv0, breaks = 20)


distr <- inv_weibull(brv0, l, k)
hist(distr, breaks = 20)
plot(std, prob_std)

dev.off()
