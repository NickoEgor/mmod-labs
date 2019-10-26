#!/usr/bin/env Rscript

# базовая случайная величина
# basic random value

# мультипликативный конгруэнтный метод

source("is_whole.r")

residue <- function(num, mod) {
    num - mod * floor(num / mod)
}

generate_brv <- function(alpha = 65539, beta = 65539, M = 2**31, n = 100) {
    if (!is.wholenumber(alpha) || alpha < 1) {
        stop("alpha not natural")
    }

    if (!is.wholenumber(beta) || beta < 1) {
        stop("beta not natural")
    }

    if (!is.wholenumber(M) || M < 1) {
        stop("M not natural")
    }

    if (beta >= M) {
        stop("beta should be lower than module")
    }

    res <- c()
    for (i in 1:n) {
        alpha <- residue(beta * alpha, M)
        res <- c(res, alpha / M)
    }

    return(res)
}
