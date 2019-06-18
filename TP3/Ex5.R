#Passar os dados dados para a variavel dados
dados <- read.table("Dados.T3.G2.1819.txt",header=T)

#analise prelimiar do dado temperatura

#media
plot(dados[,1],main = "Valores de Temperatura", xlab = "per?odo", ylab = "temperatura")

mediaTemp = mean(dados[,1])
mediaTemp

#variancia
varTemp = var(dados[,1])
varTemp

#desvio padrao
sdTemp = sd(dados[,1])
sdTemp

#coeficiente de varia??o
cvTemp = sdTemp/mediaTemp
cvTemp


#analise prelimiar do dado precipita??o m?dia
plot(dados[,2],main="Percipita??o M?dia", xlab = "per?do", ylab = "percipita??o m?dia")

#media
mediaPreci = mean(dados[,2])
mediaPreci

#variancia
varPreci = var(dados[,2])
varPreci

#desvio padrao
sdPreci = sd(dados[,2])
sdPreci

#coeficiente de varia??o
cvPreci = sdPreci/mediaPreci
cvPreci

#analise prelimiar do dado niveis de di?xido de carbono
plot(dados[,3], main = "Valores M?dios de Di?xido de Carbono", xlab = "per?odo", ylab = "CO2 m?dio")

#media
mediaCO2 = mean(dados[,3])
mediaCO2

#variancia
varMediaCO2 = var(dados[,3])
varMediaCO2

#desvio padrao
sdMediaCO2 = sd(dados[,3])
sdMediaCO2

#coeficiente de varia??o
cvMediaCO2 = sdMediaCO2/mediaCO2
cvMediaCO2

##analise prelimiar do dado n?veis m?dios de dioxido de carbono
plot(dados[,4], main = "Valores m?dios de Ozono", xlab = "per?odo", ylab = "N?veis m?dios de Ozono")

#media
mediaOzono = mean(dados[,4])
mediaOzono

#variancia
varMediaOzono = var(dados[,4])
varMediaOzono

#desvio padrao
sdMediaOzono = sd(dados[,4])
sdMediaOzono

#coeficiente de varia??o
cvMediaOzono = sdMediaOzono/mediaOzono
cvMediaOzono


#EXERCICIO 5

# Carregar a biblioteca stats4, para otimização numérica
library(stats4)

prevZ <-data.frame(dados$prec.b, dados$co2.b, dados$ozono.b)
Z <- cbind(1, as.matrix(prevZ))
ZT <- t(Z)
beta0 <- c(1,1,1,1)

calculateError <- function(x, y) {
  d1 <- (x[1] - y[1])^2
  d2 <- (x[2] - y[2])^2
  d3 <- (x[3] - y[3])^2
  d4 <- (x[4] - y[4])^2
  
  error <- sqrt(d1 + d2 + d3 + d4)
  return(error)
}

metScoresPoi <- function(beta0=c(1,1,1,1),tolerancia=0.0000001,x){
  
  contador=1
  erro <- 1
  W <- diag(x=20)
  diag(W) <- 1/(Z%*%beta0)
  u <- dados$temp.b
  beta.antes <- beta0
  n = length(x)
  
  beta.depois <- 1:4
  
  while(erro > tolerancia){
    beta.depois <- solve(ZT%*%W%*%Z)%*%(ZT%*%W%*%u)
    erro <- calculateError(beta.antes, beta.depois)
    beta.antes <- t(beta.depois)
    
    cat( "Iteration",contador, "\n")
    contador <- contador + 1
    diag(W) <- 1/(Z%*%beta.depois)
  }
  return(t(beta.depois))
}

betasFinais <- metScoresPoi(beta0=beta0, x=dados[,1])

glm(temp.b ~ prec.b + co2.b + ozono.b, family = poisson(link = "identity"), data = dados, start = beta0, maxit = 300)


#Calcular os residuos
residualTemp <- c(NA, length(dados[,1]))
residualPrec <- c(NA, length(dados[,1]))
residualCO2 <- c(NA, length(dados[,1]))
residualOzono <- c(NA, length(dados[,1]))

for (i in 1:length(dados[,1])){
  residualTemp[i] <- dados[i,1] - Z[i,1]*betasFinais[1]
  residualPrec[i] <- dados[i,1] - Z[i,2]*betasFinais[2]
  residualCO2[i] <- dados[i,1] - Z[i,3]*betasFinais[3]
  residualOzono[i] <- dados[i,1] - Z[i,4]*betasFinais[4]
}

plot(residualPrec, main = "Resíduos do modelo relativos à variável prec.b", xlab = "período", ylab = "resíduos", col="black")
par(new=TRUE)
plot(dados[,1], axes=FALSE, ann=FALSE, pch=16, col = 2)
axis(4)

plot(residualCO2, main = "Resíduos do modelo relativos à variável co2.b", xlab = "período", ylab = "resíduos")
par(new=TRUE)
plot(dados[,1], axes=FALSE, ann=FALSE, pch=16, col = 2)
axis(4)

plot(residualOzono, main = "Resíduos do modelo relativos à variável ozono.b", xlab = "período", ylab = "resíduos")
par(new=TRUE)
plot(dados[,1], axes=FALSE, ann=FALSE, pch=16, col = 2)
axis(4)

