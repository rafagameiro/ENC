
u <- runif(10000)

expression <- function(u) {
  exp(u^2)
}

val <- expression(u)

mean(val)


sqrtPi <- function (u) {
  4 * sqrt(1-u^2)
}

pi <- sqrtPi(u)

mean(pi)
