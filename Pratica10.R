#Exemplo 1
#Distribuicao a priori: theta ~ Unif(0,1)
#Dados: x|theta ~ sin(n, theta)
#Observaçoes x=2 sucessos em n=10
#Distribuicao a posteriori: theta|x ~ Beta(x+1, n-x+1)

theta <- 0:1000/1000
#3 = 2+1, 9 =10-2+1
plot(theta,dbeta(theta, 3, 9), type="l", xlab="theta", ylab="densidade", col="red")
lines(theta, dunif(theta, 0, 1))
legend("topright", c("Distribuicao a priori", "Distribuicao a posteriori"), lty=c(1,1), col=c(1,2))

#nova distribuicao a priori Beta(2,3)
plot(theta,dbeta(theta, 4, 11), type="l", xlab="theta", ylab="densidade", col="red")
lines(theta, dbeta(theta, 2, 3))
legend("topright", c("Distribuicao a priori", "Distribuicao a posteriori"), lty=c(1,1), col=c(1,2))

#Usemos MC para estimar uma media a posteriori
#X | theta ~Normal(theta, 1)
#theta - Cauchy(0,1)

#Gerar dados (assumindo theta=2)

set.seed(111)
x <- rnorm(1,2,1)
x

med.post <- function(x, m=100000) {
  #Estima por MC a media a posteriori de theta
  #media de X[theta ~ Normal(theta,1)]
  #com dist. a priori de theta Cauchy(0,1)

  theta <- rnorm(m,x,1)
  
  g1 <- theta/(1+theta^2)
  g2 <- 1/(1+theta^2)
    
  med.post <- mean(g1)/mean(g2)
  return(med.post)
  
}

med.post(x)

#metodo de rejeicao

#gerar dados (!)
#Escolho um theta=111
#Gero uma amostra de dados de dimensao 100
#de uma Normal(111,1)

set.seed(111)
x100 <- rnorm(100, 4, 1)
hist(x100)

rej <- function(x, m=100000) {
  #funcao +ara gerar valores da distribuicao
  #a posteriori no caso em que
  #X | theta ~ Normal(theta, 1) e theta ~ Cauchy(0,1)
  
  thetaStar <- rcauchy(m,0,1)
  u <- runif(m, 0, 1)
  
  n <- length(x)
  prob.ac <- exp(-n*(mean(x)-thetaStar)^2/2)
  
  theta.ac <- thetaStar[u < prob.ac]
  
  acceptanceTax <- length(theta.ac)/m

  cat("Taxa de aceitacao: ", acceptanceTax, "\n")
  
  return(list(theta.ac=theta.ac, acceptanceTax=acceptanceTax))
}

ex1 <- rej(x100)

mean(ex1$theta.ac)
hist(ex1$theta.ac, prob=T)
lines(density(ex1$theta.ac),col=2)
