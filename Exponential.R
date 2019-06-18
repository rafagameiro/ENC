u <- runif(1)

lambda <- 5

f_1 <- function (x) {
  (-1/lambda) * log10(1-x)
}

amostra <- c(NA, 100)

for ( i in 1: 100) {
  amostra[i] <- f_1(u)
  u <- runif(1)
}

