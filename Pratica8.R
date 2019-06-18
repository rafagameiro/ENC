#carregar a biblioteca stats4
library(stats4)

theta = 0.32

set.seed(100)

amostra.x <- rbinom(100,1,theta) #gera 100 amostras com probabilidade 0.32

mean(amostra.x)

#calculates the symmetric of the log-likelihood for the bernoulli data x
menos.Log.L <- function (x) {
    function(theta) {
      n <- length(x)
      - (sum(x)*log(theta)+(n-sum(x))*log(1-theta))
    }
    
}

mlogL.Ber <- menos.Log.L(amostra.x)

theta.a <- 0:1000/1000

plot(theta.a,mlogL.Ber(theta.a), type="l", col=2)

abline(v=0.32)

#otimizacao

mle(mlogL.Ber,start=list(theta=0.2))

#Determinacao numerica da funcao score

#funcao score de dados Bernoulli
score.BER <- function(x) {
  function(theta) {
    n <- length(x)
    sum(x)/theta - (n-sum(x))/(1-theta)
  }
}

SC <- score.BER(amostra.x)

uniroot(SC, lower=0, upper=1)



#Fisher Score method

phi0 <- 1

#obtermos 
met.score.BER <- function(phi0=0, tolerance=0.000001, x) {
  counter <- 1
  error <- 1
  
  phiK <- phi0
  while(error > tolerance) {
    phi <-phiK + (1 + exp(phiK))*(mean(x)/exp(phiK) + mean(x) - 1)
    
    error <- abs(phi - phiK)
    cat("Iteração ", counter, "\n")
    phiK <- phi
    counter <- counter + 1
  }
  
  return(phiK)
}

met.score.BER(x=amostra.x)

log(0.32/(1-0.32))


emp <- read.table("empresas.txt", header = F)
names(emp) <- c("nome", "n.socios", "c.social", "vmm", "n.emp")

plot(emp)
hist(emp$vmm)

#modelo de regressao linear multiplo

#calcula as estimativas de MQ no modelo de regressao linear
est.MRM <- function(x,y) {
  x <- as.matrix(x)
  X <- cbind(1,x)
  y <- as.matrix(y)
  
  betaHat <- solve(t(X)%*%X)%*%t(X)%*%y #multiplicacao matricial
  
  rownames(betaHat)[1] <- "intercept"
  
  for(k in 2:length(betaHat)) {
     rownames(betaHat)[k] <- paste("Beta", k-1, sep="")      
  }
          
   return(betaHat)   
}

est.MRM(x=data.frame(emp$n.socios, emp$c.social, emp$n.emp), y=emp$vmm)

#confirmacao dos resultados
lm(vmm ~ n.socios + c.social + n.emp, data = emp)

#estima a variancia do erro num modelo de regressao linear multipla
sigma.2 <- function(x,y) {
  x <- as.matrix(x)
  X <- cbind(1,x)
  y <- as.matrix(y)
  
  betaHat <- est.MRM(x,y)
  n <- length(y)
  k <- length(betaHat)-1
  
  sigma2 <- (t(y-X%*%betaHat)%*%(y-X%*%betaHat))/(n-k)
  
  return(sigma2)   
}

sigma.2(x=data.frame(emp$n.socios, emp$c.social, emp$n.emp), y=emp$vmm)

