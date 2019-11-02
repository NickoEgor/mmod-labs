#!/usr/bin/env Rscript

# распределение вейбулла

weibull.density <- function(x, l, k) {
    ifelse(x >= 0, k/l * (x/l)^(k-1) * exp(-(x/l)**k), 0)
}

weibull.distr <- function(x, l, k) {
    1 - exp(-((x/l)^k))
}

weibull.inv <- function(p, l, k) {
    l * (-log(1-p)) ^ (1/k)
}

weibull.mean <- function(l, k) {
    l*gamma(1 + 1/k)
}

weibull.var <- function(l, k) {
    l^2*gamma(1 + 2/k) - weibull.mean(l, k)^2
}

weibull.distr_func <- function(l, k) {
    function(x) 1 - exp(-((x/l)^k))
}
