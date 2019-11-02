get_mean_est <- function(values, conf = 0.95, prec=7) {
    n <- length(values)
    deviation <- sd(values)  # sqrt(var(...))
    stud <- qt(conf + (1-conf)/2, df=n-1)
    delta <- deviation*stud/sqrt(n-1)
    m <- mean(values)
    list("mean" = round(m, prec),
         "delta" = round(delta, prec),
         "conf" = round(conf, prec),
         "left" = round(m-delta, prec),
         "right" = round(m+delta, prec))
}

# https://ru.wikipedia.org/wiki/Доверительный_интервал_для_дисперсии_нормальной_выборки
get_variance_est <- function(values, conf = 0.95, prec=7) {
    n <- length(values)
    variance <- var(values)  # sqrt(var(...))

    # chi_less <- qchisq(conf + (1-conf)/2, df=n-1)
    # chi_more <- qchisq((1-conf)/2, df=n-1)

    chi_less <- qchisq((1+conf)/2, df=n-1)
    chi_more <- qchisq((1-conf)/2, df=n-1)

    left <- (n-1) * variance / chi_less
    right <- (n-1) * variance / chi_more

    list("variance" = round(variance, prec),
         "conf" = round(conf, prec),
         "left" = round(left, prec),
         "right" = round(right, prec),
         "chi_less" = chi_less,
         "chi_more" = chi_more)
}

check_pearson <- function(distr_func, values, breaks, conf = 0.95) {
    n <- length(values)
    bounds = seq(min(values), max(values), length.out = breaks+1)
    h <- hist(values, breaks = bounds)
    density <- h$density

    probs <- c()
    for (i in 1:breaks) {
        p_i <- distr_func(bounds[i+1]) - distr_func(bounds[i])
        probs <- c(probs, p_i)
    }

    len <- (bounds[breaks+1] - bounds[1])/breaks
    chi2_emp <- n*sum((probs - len*density)^2/probs)
    chi2 <- qchisq(conf, df=breaks-1)

    list(fit = chi2_emp < chi2, theory = chi2, empiric = chi2_emp)
}

check_pearson_discrete <- function(density_func, values, conf = 0.95) {
    n <- length(values)
    len <- max(values)

    t <- table(values)
    vals <- sapply(c(1:len),
                   function(k) {
                       v <- t[toString(k)]
                       ifelse(is.na(v), 0, v)
                   })
    vals <- as.numeric(as.matrix(vals))

    probs <- density_func(c(1:len))
    chi2 <- qchisq(conf, df=as.integer(len))
    chi2_emp <- n*sum(((probs - vals/n)^2)/probs)

    list(fit = chi2_emp < chi2, theory = chi2, empiric = chi2_emp)
}
