#Dados 
x <- c(1.0, 1.5, 1.5, 1.5, 2.5, 4.0, 5.0, 5.0, 7.0, 8.0, 8.5, 9.0, 9.5, 9.5, 10.0, 12.0, 12.0, 13.0, 13.0, 14.5, 15.5, 15.5, 16.5, 17.0, 22.5, 29.0, 31.5)
y <- c(1.80, 1.85, 1.87, 1.77, 2.02, 2.27, 2.15, 2.26, 2.47, 2.19, 2.26, 2.40, 2.39, 2.41, 2.50, 2.32, 2.32, 2.43, 2.47, 2.56, 2.65, 2.47, 2.64, 2.56, 2.70, 2.72, 2.57)

#Exercicio 2
#Algoritmo metropolis hastings aplicado a variavel gama com distribuicao a priori Uniforme(0,1)
metropolis_Hastings <- function(y, x, gama0 = 0.5, burn.in = 10000, m = 100000){
  gama <- matrix(NA, nrow = burn.in+m)
  gama[1] <- gama0
  
  taxa <- 0
  
  for(i in 2:(burn.in+m)){
    gama.linha <- runif(1)
    alpha.star <- (exp(sum(-(y-2.7+0.9*gama.linha^x)^2)/0.02))/(exp(sum(-(y-2.7+0.9*gama[i-1]^x)^2)/0.02))
    alpha <- min(1, alpha.star)

    u <- runif(1)
    
    if(u <= alpha){
      gama[i] <- gama.linha
      taxa <- taxa + 1
    }
    else
      gama[i] = gama[i-1]
    }
    taxa <- taxa/(burn.in+m)
    cat("Taxa de Aceitação: ", taxa, "\n")
    
    return(list(gama = gama[(burn.in+1):(burn.in+m)]))
  
}

gama.MH <- metropolis_Hastings(y=y,x=x)

plot.ts(gama.MH$gama) #trace

hist(gama.MH$gama,prob=T, main = "Histograma de Gama", xlab = "gama")  #estimador de h(theta|y)
lines(density(gama.MH$gama),col=2)

mean(gama.MH$gama) #estimativa da média 
#a posteriori do gama

sd(gama.MH$gama)

#Estimemos a P(gama>0.9)
sum(gama.MH$gama>0.9)/length(gama.MH$gama)

gama.thin<-gama.MH$gama[seq(1,length(gama.MH$gama),10)]
hist(gama.thin,prob=T, main = "Histograma de Gama", xlab = "gama")  #estimador de h(theta|y)
lines(density(gama.thin),col=2)

#Exercicio 3
#Algoritmo metropolis hastings aplicado a variavel gama com distribuicao a priori Beta(a,b)
metropolis_Hastings_Beta <- function(y, x, gama0 = 0.5, a, b, burn.in = 10000, m = 100000){
  gama <- matrix(NA, nrow = burn.in+m)
  gama[1] <- gama0
  
  taxa <- 0
  
  beta <- rbeta(1,a,b)
  
  for(i in 2:(burn.in+m)){
    gama.linha <- rbeta(1,a,b)
    alpha.star <- (exp(sum(-(y-2.7+0.9*gama.linha^x)^2)/0.02))/((exp(sum(-(y-2.7+0.9*gama[i-1]^x)^2)/0.02)))
    alpha <- min(1, alpha.star)
    
    u <- runif(1)
    
    if(u <= alpha){
      gama[i] <- gama.linha
      taxa <- taxa + 1
    }
    else
      gama[i] = gama[i-1]
  }
  taxa <- taxa/(burn.in+m)
  cat("Taxa de Aceitação: ", taxa, "\n")
  
  return(list(gama = gama[(burn.in+1):(burn.in+m)]))
  
}

# Valores de a e b com a ou b = 1 -> Gráficos para a análise de resultados de 3
gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=0.1, b=1)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=0.1 e b=1", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=0.3, b=1)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama,prob = T, main = "Histograma de gama com a=0.3 e b=1", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=0.5, b=1)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=0.5 e b=1", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=0.7, b=1)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama,prob = T, main = "Histograma de gama com a=0.7 e b=1", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=0.9, b=1)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=0.9 e b=1", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=1, b=0.1)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=1 e b=0.1", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=1, b=0.3)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama,  prob = T,main = "Histograma de gama com a=1 e b=0.3", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=1, b=0.5)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=1 e b=0.5", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=1, b=0.7)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=1 e b=0.7", xlab = "gama")
lines(density(gama.MH$gama),col=2)

gama.MHB <- metropolis_Hastings_Beta(y=y,x=x,a=1, b=0.9)
mean(gama.MHB$gama)
var(gama.MHB$gama)
hist(gama.MHB$gama, prob = T, main = "Histograma de gama com a=1 e b=0.9", xlab = "gama")
lines(density(gama.MH$gama),col=2)