#!/usr/bin/env Rscript

library(ggplot2)
library(rjson)
library(BSDA, warn.conflicts=FALSE)

source("brv.r")

amount <- 10000

now <- round(as.numeric(Sys.time()))
brv_x <- generate_brv(x0 = now %% 2e6, n = amount)

now <- round(as.numeric(Sys.time())+1)
brv_y <- generate_brv(x0 = now %% 2e6, n = amount)

distr_func <- function(x, y) exp(-x-y)

distr_x <- function(x) exp(-x)
distr_y <- function(y) exp(-y)

dense_x <- function(x) log(1/(1-x))
dense_y <- function(y) log(1/(1-y))

x_range <- dense_x(brv_x)
y_range <- dense_y(brv_y)

pdf("raport.pdf")
hist(x_range, breaks=20, main="X", freq=FALSE)
hist(y_range, breaks=20, main="Y", freq=FALSE)
plot(x_range, y_range)
dev.off()

estim <- function(values, gamma = 0.95) {
    n <- length(values)
    m_ast <- mean(values)
    print(paste("m* =", m_ast))
    D_ast <- var(values)
    print(paste("D* =", D_ast))
    s <- sd(values)
    t2 <- pnorm(gamma/2) - pnorm(-gamma/2)
    left  <- m_ast - s*t2/sqrt(n)
    right <- m_ast + s*t2/sqrt(n)
    print(paste(left, "<= m_ast <", right))
    chi_left  <- qchisq((1+gamma)/2, df=n-1)
    chi_right <- qchisq((1-gamma)/2, df=n-1)
    print(paste(n * D_ast / chi_left, "<= D_ast <",
                n * D_ast / chi_right))
}

print("x_range")
estim(x_range)
print("")
print("y_range")
estim(y_range)

print("Z-test")

# int((int(x*exp(-x-y))dy from 0 to Inf))dx from 0 to Inf
print("x range")
x_z <- z.test(x_range, sigma.x=1, mu=1)
print(x_z)

print("y range")
y_z <- z.test(y_range, sigma.x=1, mu=1)
print(y_z)

print("x y ranges")
xy_z <- z.test(x_range, y_range, sigma.x=1, sigma.y=1, mu=1)
print(xy_z)

print("Covariance")
cov(x_range, y_range)

print("Correlation")
cor(x_range, y_range)