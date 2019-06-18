am30 <- c(138,93,61,179,48,37,29,23,30,2)
am50 <- c(143, 104, 69, 260, 75, 63, 50, 48, 111, 50)

x <- mean(am30)
y <- mean(am50)

theta_hat <- x/y

theta_hat
n <- 999

amBoots30 <- matrix(NA, nrow=n,ncol = 10)
amBoots50 <- matrix(NA, nrow=n,ncol = 10)
amTheta <- 1:n
for(r in 1:n) {
  amBoots30[r,] <- sample(am30, replace = TRUE)
  amBoots50[r,] <- sample(am50, replace = TRUE)
  
}

#calculo do viesamento
amVies <- 1:n
for(r in 1:n) {
  amTheta[r] <- mean(amBoots30[r,])/mean(amBoots50[r,])
  
  amVies[r] <- amTheta[r] - theta_hat
}
  
vies <- mean(amVies)

#calculo da variancia Bootstrap

mean_theta_hat <- mean(amTheta)
amVar <- 1:n

for(r in 1:n) {
  amVar[r] <- (amTheta[r] - mean_theta_hat)^2
}

varBoots <- sum(amVar) /(n-1)

varBoots

#calculo intervalo confiança

amIC <- 1:n+1
for(r in 1:r) {
  amIC[r] <- amVies[r]
}

amIC[n+1] <- 0

sort(amIC)

