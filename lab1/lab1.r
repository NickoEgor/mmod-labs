#!/usr/bin/env Rscript

library(ggplot2)

source("brv.r")
source("weibull.r")

amount <- 1000
a <- 0
b <- 3

l <- 1
k <- 5

now <- round(as.numeric(Sys.time()))

brv <- generate_brv(alpha = now, n = amount, a = a, b = b)
prob_brv <- prob_weibull(brv, l, k)
distr_brv <- distr_weibull(brv, l, k)

std <- seq(a, b, length.out = amount)
prob_std <- prob_weibull(std, l, k)
distr_std <- distr_weibull(std, l, k)

pdf("raport.pdf")
hist(brv, breaks = 100)

plot(std, prob_std)
plot(brv, prob_brv)

hist(prob_std)
hist(prob_brv)

plot(std, distr_std)
plot(brv, distr_brv)

dev.off()
