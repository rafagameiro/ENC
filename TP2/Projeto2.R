#******************************************************************************************************************************


#EXERCICIO 1

x <- c(92,90,85,96,92,88,96,88)
y <- c(89,90,88,93,90,85,95,90)

alpha <- 0.025
n <- 999
p <- 8


#EXERCICIO 1 - ALINEA A

# Calculo de Pearson Coefficient
func <- function(x,y){
  sum((x-mean(x))*(y-mean(y)))/sqrt((sum((x-mean(x))^2))*(sum((y-mean(y))^2)))
}

pCoeficient = func(x,y)

plot(x,y)


#******************************************************************************************************************************


#EXERCICIO 1 - ALINEA B

# Criacao das matrizes xAux e yAux
# Ciclo for que preenche as matrizes xAux e yAux com os valores "teta_chapeu exceto o elemento na posicao j"
xAux <- matrix(nrow=8, ncol = 7)
yAux <- matrix(nrow=8, ncol = 7)

for(l in 1:8){
  tempX=x[-l]     # Remocao do elemento l do vetor x, com posterior criacao do vetor tempX ja sem esse elemento
  tempY=y[-l]     # Remocao do elemento l do vetor y, com posterior criacao do vetor tempY ja sem esse elemento
  
  for(r in 1:7){
    xAux[row=l,col=r]=tempX[r]
    yAux[row=l,col=r]=tempY[r]
  } 
}


# Calculo do lJack_j: (n-1) * (teta_chapeu - teta_chapeu_excetoJ)
# Seja n a dimensao das amostras, teta_chapeu o valor de pCoeficient calculado na alínea a)
# Consequente calculo do somatorio do lJack de todas as posicoes 
lJack = 0
somaBjack=0

for(i in 1:8){
  lJack = 7 * (pCoeficient - func(xAux[i, ], yAux[i, ]))   
  somaBjack = somaBjack + lJack
}


# Calculo do bJack: -(1/n) * somatorio(lJack_j)
bJack = -(1/8) * somaBjack


# Calculo do varJack: (1/(n(n-1))) * (somatorio(lJack^2 - n*bJack^2))
somaVarJack=0

for (i in 1:8){
  lJack = 7 * (pCoeficient - func(xAux[i, ], yAux[i, ]))   
  somaVarJack = somaVarJack + (lJack^2)
}

varJack = (1/(8*(8-1))) * (somaVarJack-(8*(bJack^2)))


#******************************************************************************************************************************


#EXERCICIO 1 - ALINEA C

# Geracao de amostras Bootstrap
amBootsX <- matrix(NA, nrow=n,ncol = 8)
amBootsY <- matrix(NA, nrow=n,ncol = 8)

for(r in 1:n) {
  amBootsX[r,] <- sample(x, replace = TRUE)
  amBootsY[r,] <- sample(y, replace = TRUE)
}


# Geracao de amostras auxiliares
amostra_Vies <- 1:n
amostra_Ro <- 1:n

for(r in 1:n) {
  amostra_Ro[r] <- func(amBootsX[r,], amBootsY[r,])
  amostra_Vies[r] <- amostra_Ro[r] - pCoeficient
}


# Calculo do Intervalo de Confiança
# Limite Inferior: teta_chapeu - a_1menosAlpha
# Limite Superior: teta_chapeu - a_alpha
# Seja o alpha = 0.025

# Passo 1: Gerar amostra usada para calcular o Intervalo de Confianca
# Seja amIC a amostra do Intervalo de Confianca
amIC <- 1:n+1
for(r in 1:r) {
  amIC[r] <- amostra_Vies[r]
}

amIC[n+1] <- 0
amIC <- sort(amIC)

# Passo 2: Calculo do a_alpha e do a_1menosAlpha
a_alpha <- amIC[(n+1)*alpha]
a_1menosAlpha <- amIC[(n+1)*(1-alpha)]

# Passo 3: Estabelecer limites inferior e superior
limite_inferior_Boots <- pCoeficient-a_1menosAlpha
limite_superior_Boots <- pCoeficient-a_alpha


#******************************************************************************************************************************


#EXERCICIO 1 - ALINEA D

# Calculo do theta da amostra
amRho <- 1:n


for(r in 1:n){
  amRho[r] <- func(amBootsX[r,], amBootsY[r,])
}

meanAmRho <- mean(amRho, na.rm = TRUE)


# Calculo do varBoots: (1/(r-1)) * (somatorio( (teta_chapeu_estrela_R - media_teta_chapeu_estrela)^2) )
# Seja r a dimensao n da amostra bootstrap
varboots <- (1/(n-1))*sum((amRho - meanAmRho)^2, na.rm = TRUE)


# Criacao de uma matriz com os elementos das amostras das variancias
# Calculo de todos os z's e suas variancias
ryr <- 1:8
rxr <- 1:8
z <- 1:(n+1)
amRhoStar <- 1:n

for(r in 1:n){
  for(i in 1:n){
    rxr <- sample(amBootsX[r, ], replace = TRUE)
    ryr <- sample(amBootsY[r, ], replace = TRUE)
    
    amRhoStar[i] <- func(rxr, ryr)
  }
  
  meanAmRhoStar <- mean(amRhoStar, na.rm = TRUE)
  
  # Calculo da variancia para o z atual
  varbootsStar <- (1/(n-1))*sum((amRhoStar - meanAmRhoStar)^2, na.rm = TRUE)
  z[r] <- (amRho[r] - pCoeficient)/sqrt(varbootsStar)
}


z[n+1] <- 0
amIC <- 1:(n+1)


# Calculo do Intervalo de Confiança
# Limite Inferior: teta_chapeu - z_estrela_((R+1)(1menosAlpha)) * raiz(variancia)
# Limite Superior: teta_chapeu - z_estrela_((R+1)*alpha) * raiz(variancia)
amIC <- sort(z)

limite_inferior_tBoots <- pCoeficient - (amIC[(n+1)*(1-alpha)]*sqrt(varboots))
limite_superior_tBoots <- pCoeficient - (amIC[(n+1)*alpha]*sqrt(varboots))




#******************************************************************************************************************************


#EXERCICIO 2


amostra <- c(1,3,3,1,1,3,2,2,3,0,2,4,2,6,4,2,4,3,2,4)
nObs <- c(1,3,6,5,4,1) # Numero de vezes que os valores 0, 1, 2, 3, 4 e 6 se repetem, respetivamente
k <- 6


# Funcao Auxiliar: Funcao Fatorial
fact <- function(x) {
  if(x == 0) {
    1
  }else {
    x * fact(x-1)
  }
}


# Calculo da Probabilidade da Distribuicao Binomial: probBinomial = (n!/(k!(n-k)!)) * (p^k) * ((1-p)^(n-k))
# Seja B(n,p) = B(8,0.3), temos n=8 e p=0.3. 
# Seja k cada valor de nObs
probBinomial <- 1:6
for(i in 1:k) {
  probBinomial[i] <- (fact(8)/(fact(nObs[i])*fact(8-nObs[i]))) * (0.3^nObs[i]) * ((0.7)^(8-nObs[i]))
}


# Calculo do T, usando a funcao de probabilidade dada: estaTest = somatorio( ((N_i-n*p_i)^2) / (n*p_i) )
# Seja N_i cada valor de nObs, n a dimensao da amostra e p_i cada valor da funcao de probabilidade
estaTestObs <- 0
for(i in 1:k) {
  estaTestObs <- sum( ((nObs[i]-20*probBinomial[i])^2) / (20*probBinomial[i]) )
}


# Calculo do p-value: (#{tj : tj>=tobs} + 1) / (m+1)
# Seja tj cada valor da amostra inicial, tobs a estatistica de teste e m a dimensao de nObs
soma=0
probAux <- 1:p
amAux <- matrix(nrow=(n+1), ncol=p)

#geracao de 1000 amostra com probabilidade binomial (8,0.3)
for(i in 1:(n+1)) {
  probAux <- rbinom(8,8,0.3)
  for(r in 1:p){
    amAux[i,r] <- probAux[r]
  }
}


#Calcular o numero de ocorrencias de cada valor na amostra gerada
nObsAux <- matrix(nrow=(n+1), ncol=9)
for(r in 1:(n+1)){
  for(c in 1:9){
    nObsAux[r,c] <- 0
  }
}


for(j in 1:(n+1)){
  for(i in 1:p){
    if(amAux[j,i] == 0){
      nObsAux[j, 1] = nObsAux[j,1]+1
    }
    else if(amAux[j,i] == 1){
      nObsAux[j, 2] = nObsAux[j,2]+1
    }
    else if(amAux[j,i] == 2){
      nObsAux[j, 3] = nObsAux[j,3]+1
    }
    else if(amAux[j,i] == 3){
      nObsAux[j, 4] = nObsAux[j,4]+1
    }
    else if(amAux[j,i] == 4){
      nObsAux[j, 5] = nObsAux[j,5]+1
    }
    else if(amAux[j,i] == 5){
      nObsAux[j, 6] = nObsAux[j,6]+1
    }
    else if(amAux[j,i] == 6){
      nObsAux[j, 7] = nObsAux[j,7]+1
    }
    else if(amAux[j,i] == 7){
      nObsAux[j, 8] = nObsAux[j,8]+1
    }
    else if(amAux[j,i] == 8){
      nObsAux[j, 9] = nObsAux[j,9]+1
    }
  }
}


estaTestAux <- 0
probBinomialAux <- 1:9


#faz o calculo de soma = #{tj : tj>=tobs}
for(i in 1:(n+1)){
  
  #calcula a probabilidade da ocorrencia de cada elemento gerado, assumindo que H0 e verdade
  for(j in 1:9){
    if(nObsAux[i,j] != 0){
      probBinomialAux[j] <- (fact(8)/(fact(nObsAux[i,j])*fact(8-nObsAux[i,j])))*(0.3^nObsAux[i,j])*((0.7)^(8-nObsAux[i,j]))
    }else{
      probBinomialAux[j] <- 0
    }
  }
  
  for(j in 1:9){
    if(probBinomialAux[j] == 0){
      estaTestAux <- estaTestAux + 0
    }else{
      estaTestAux <- sum(((nObsAux[i,j]-20*probBinomialAux[j])^2)/(20*probBinomialAux[j]))
    }
  }
  
  if(estaTestAux >= estaTestObs){
    soma = soma+1
  }
  
  estaTestAux <- 0
}


# calcula o pvalue = soma/m+1, com m = numero de elementos gerados
pvalue <- (soma+1)/(n+2)

