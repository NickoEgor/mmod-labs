#!/usr/bin/env Rscript

library(ggplot2)
library(rjson)

source("brv.r")
source("weibull.r")
source("utils.r")
source("drv.r")

PREC <- 7

amount <- 10000
l <- 3
k <- 2

now <- round(as.numeric(Sys.time()))
brv0 <- generate_brv(alpha = now, n = amount)
distr <- weibull.inv(brv0, l, k)

describe_distr <- function(name, distr_func, values, mean_theory, var_theory,
                           is_discrete=FALSE, breaks=10) {
    printResult <- function(res) {
        cat(res$conf, ": (", res$left, ", ", res$right, ")", "\n", sep="")
    }

    cat(name, "\n")
    cat(paste("Mean (theory):", mean_theory, "\n"))
    cat(paste("Variance (theory):", var_theory, "\n"))
    cat("\n")
    cat(paste("Mean (point):", mean(values), "\n"))
    cat(paste("Variance (point):", var(values), "\n"))
    cat("\n")
    cat("Mean (interval):\n")
    printResult(get_mean_est(values))
    printResult(get_mean_est(values, 0.99))
    cat("\n")
    cat("Variance (interval):\n")
    printResult(get_variance_est(values))
    printResult(get_variance_est(values, 0.99))
    cat("\n")

    if (is_discrete) pearson <- check_pearson_discrete(distr_func, values)
    else pearson <- check_pearson(distr_func, values, breaks)
    cat("Pearson:\n")
    cat("theory:", pearson$theory, "\n")
    cat("empiric:", pearson$empiric, "\n")
    cat(ifelse(pearson$fit, "Successful hypothesys\n", "Invalid hypothesys\n"))
    cat("\n")

    pearson$fit
}

# ----------------------------

imit_amount <- 10000
imit_brv <- generate_brv(alpha = now, n = imit_amount)

imitation <- function(amount, brv, density_func) {
    imitator <- imitate_density(density_func)
    values <- sapply(brv, imitator)
    breaks <- seq(floor(min(values)), ceiling(max(values)))
    theory <- density_func(breaks)
    list("theory" = theory, "breaks" = breaks, "values" = values)
}

geom_p <- 0.3
geom <- imitation(imit_amount, imit_brv, geom.density(geom_p))

binom_n <- 20
binom_p <- 0.5
binom <- imitation(imit_amount, imit_brv, binom.density(binom_n, binom_p))

poisson_l <- 10
poisson <- imitation(imit_amount, imit_brv, poisson.density(poisson_l))

# ----------------------------

sink("raport.txt")
cat("//------------------------------------------\n")
pearson <- describe_distr("BASIC RANDOM VARIABLE",
                          function(x) x, brv0, 1/2, 1/12)
if (!pearson) {
    stop("Pearson failed. Please restart")
}
cat("Check independance\n")
check_brv_correlation()
cat("//------------------------------------------\n")
invisible(describe_distr("WEIBULL DISTRIBUTION",
                         weibull.distr_func(l, k), distr,
                         weibull.mean(l, k), weibull.var(l, k)))
cat("//------------------------------------------\n")
invisible(describe_distr("GEOMETRY DISTRIBUTION",
                         geom.density(geom_p), geom$values,
                         geom.mean(geom_p), geom.var(geom_p),
                         is_discrete=TRUE))
cat("//------------------------------------------\n")
invisible(describe_distr("BINOMIAL DISTRIBUTION",
                         binom.density(binom_n, binom_p), binom$values,
                         binom.mean(binom_n, binom_p),
                         binom.var(binom_n, binom_p),
                         is_discrete=TRUE))
cat("//------------------------------------------\n")
invisible(describe_distr("POISSON DISTRIBUTION",
                         poisson.density(poisson_l), poisson$values,
                         poisson.mean(poisson_l), poisson.var(poisson_l),
                         is_discrete=TRUE))
cat("//------------------------------------------\n")
sink()

# ----------------------------

pdf("raport.pdf")

hist(brv0, breaks=20, main="Basic random variable")

hist(distr, breaks=20, density=10, freq=FALSE, main="Weibull density")

std <- seq(floor(min(distr)), ceiling(max(distr)), length.out = amount)
theory_density <- weibull.density(std, l, k)
lines(std, theory_density, lwd = 3)

hist(geom$values, breaks=20, main="Geometry imitation", freq=FALSE)
points(geom$breaks, geom$theory, lwd = 3)

hist(binom$values, breaks = 20, main="Binomial imitation", freq=FALSE)
points(binom$breaks, binom$theory, lwd = 3)

hist(poisson$values, breaks = 20, main="Poisson imitation", freq=FALSE)
points(poisson$breaks, poisson$theory, lwd = 3)

dev.off()

