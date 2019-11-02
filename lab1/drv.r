imitate_density <- function(density_func) {
    function(x) {
        value <- 0
        right <- 0
        while (TRUE) {
            left <- right
            right <- right + density_func(value)
            if (left <= x && x < right) { break }
            value <- value + 1
        }
        value
    }
}

# geometry
geom.density <- function(p) { function(x) p*(1-p)^x }
geom.distr <- function(p) { function(x) 1-(1-p)^(x+1) }
geom.mean <- function(p) (1-p)/p
geom.var <- function(p) (1-p)/p^2

# binomial
binom.density <- function(n, p) { function(x) choose(n, x) * p^x * (1-p)^(n-x) }
binom.distr <- function(n, p) { function(x) pbinom(x, size=n, prob=p) }
binom.mean <- function(n, p) n*p
binom.var <- function(n, p) n*p*(1-p)

# poisson
poisson.density <- function(l) { function(x) l^x*exp(-l)/factorial(x) }
poisson.distr <- function(l) { function(x) gamma(x+1, l)/factorial(x) }
poisson.mean <- function(l) l
poisson.var <- function(l) l
