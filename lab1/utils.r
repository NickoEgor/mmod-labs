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
