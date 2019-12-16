#!/usr/bin/env Rscript

# линейный конгруэнтный метод

generate_brv <- function(a = 84589, x0 = 1, c = 45989, m = 217728, n = 100) {
    res <- c()
    for (i in 1:n) {
        x0 <- (a*x0 + c) %% m
        res <- c(res, x0/m)
    }
    return(res)
}
