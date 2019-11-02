#!/usr/bin/env Rscript

# базовая случайная величина
# basic random variable

# мультипликативный конгруэнтный метод

source("is_whole.r")

residue <- function(num, mod) {
    num - mod * floor(num / mod)
}

generate_brv <- function(alpha = 65539, beta = 65539, M = 2**31, n = 100) {
    if (!is.wholenumber(alpha) || alpha < 1) { stop("alpha not natural") }
    if (!is.wholenumber(beta) || beta < 1) { stop("beta not natural") }
    if (!is.wholenumber(M) || M < 1) { stop("M not natural") }
    if (beta >= M) { stop("beta should be lower than module") }

    res <- c()
    for (i in 1:n) {
        alpha <- residue(beta * alpha, M)
        res <- c(res, alpha / M)
    }

    return(res)
}

check_brv_correlation <- function() {
    step <- 5
    now <- round(as.numeric(Sys.time()))

    for (i in 1:4) {
        n <- 10^i
        vals <- generate_brv(alpha = now, n = n)

        pairs <- c()
        for (i in 1:(n-step)) {
            pairs <- c(pairs, vals[i]*vals[i+step])
        }

        R <- 12 * (n - step)^(-1) * sum(pairs) - 3
        cat("n =", n, "R =", R, "\n")
    }
}
