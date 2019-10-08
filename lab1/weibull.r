#!/usr/bin/env Rscript

# распределение вейбулла

prob_weibull <- function(x, l = 1, k = 1) {
    if (k < 0) stop("error: k < 0")
    if (l < 0) stop("error: l < 0")
    res <- ifelse(x >= 0, k/l * (x/l)^(k-1) * exp(-(x/l)**k), 0)
}

distr_weibull <- function(x, l = 1, k = 1) {
    1 - exp(-(x/l)^k)
}
