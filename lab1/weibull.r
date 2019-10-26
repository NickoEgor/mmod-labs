#!/usr/bin/env Rscript

# распределение вейбулла

prob_weibull <- function(x, l, k) {
    if (k < 0) stop("error: k < 0")
    if (l < 0) stop("error: l < 0")
    ifelse(x >= 0, k/l * (x/l)^(k-1) * exp(-(x/l)**k), 0)
}

distr_weibull <- function(x, l, k) {
    1 - exp(-((x/l)^k))
}

inv_weibull <- function(p, l, k) {
    l * (-log(1-p)) ^ (1/k)
}
