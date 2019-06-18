set.seed(111)
x100 <- rnorm(100, 4, 1)

ream.pond <- function(x, m=100000) {
  #Gera valores da distribuicao a posteriori de theta
  #no caso em x|theta ~ Normal(theta, 1) e
  #theta ~ Cauchy(0,1), pelo metodo de
  #reamostragem ponderada
  
  theta <- rcauchy(m) #gerar m valores de theta da distribuicao proposta
  
  n <- length(x)
  peso <- exp(-n*(mean(x) - theta)^2/2)
  peso <- peso/sum(peso) #pesos de amostragem
  
  theta.rea <- sample(theta, prob = peso, replace = T) #valores de theta reamostrados de acordo com os pesos
  #como se fossem da distribuicao a posteriori
  
  return(list(theta.rea = theta.rea))
}

#dados: sao os que estao em x100
ex2 <- ream.pond(x100)
ex2

hist(ex2$theta.rea)
lines(density(ex2$theta.rea), col=2)
mean(ex2$theta.rea)
#estimativa da media a posteriori do theta
sd(ex2$theta.rea)
#estimativa do desvio padrao a posteriori do theta

#metodo MCMC
#Metropolis-Hasting
#Ex: Y ~ Multinomial(n, p1, p2, p3, p4), ver caderno
#Usar M-H para estimar theta, usando uma distribuicao
#proposta igual a distribuicao a priori theta
#unif(0,1) 

y <- c(125, 18, 20, 34)

MH <- function(y, theta0 = 0.99, burn.in=10000, m=100000) {
  #M-H para dados multinomial em que as probabilidades
  # dependem de theta....(e resto e igual ao que esta em cima)
  
  theta <- matrix(NA, nrow = burn.in+m)
  theta[1] <- theta0
  
  taxa <- 0
  
  for(i in 2:(burn.in+m)) {
    theta.linha <- runif(1)
    alfa.star <- ((2+theta.linha)/(2+theta[i-1]))^y[1]*((1-theta.linha)/(1-theta[i-1]))^(y[2]+y[3]) * (theta.linha/theta[i-1])^y[4]
    alfa <- min(1, alfa.star)
    
    u <- runif(1)
    
    if(u <= alfa) {
      theta[i] <- theta.linha
      taxa <- taxa + 1
    }else
      theta[i] <- theta[i-1]
  }
  
  taxa <- taxa/(burn.in+m)
  cat("Taxa de aceitacao: ", taxa, "\n")
  
  return(theta=theta[(burn.in+1): (burn.in+m)])
}

theta.MH <- MH(y=y)

plot.ts(theta.MH)

acf(theta.MH)
#funcao de autocorrelacao
#valores muito correlacionados, o que se
#justifica pela baixa probabilidade de aceitacao

hist(theta.MH, probability = T) #estimador de h(theta)
mean(theta.MH) #estimativa da media a posteriori do theta

sd(theta.MH) #estimativa do desvio padrao a posteriori do theta

#estimemos a P(theta>= 0.9)





#fazemos o thining(de 10 em 10)

theta.thin <- theta.MH

plot(theta.thin)
acf(theta.thin)

hist(theta.thin)
mean(theta.thin)