#Exercicio 1
#numero de amostras
n=100000

invertg <- function(x){
  sqrt((exp(1)^x)-3)/sqrt(2)                            
}

a <- invertg(1.099)                                     

b <- invertg(1.5)

func <- function(x){
  if (0 < x && x < 1){
    3*x^2                                               
  } else 0
}

u = runif(n, min =a, max = b)

amostra <- 1:n
for (i in 1:n){
  amostra[i] <- func(u[i])
}

prob <-(b-a)*mean(amostra)
sd <- sd(amostra)
prob

#Exercicio 2 - alinea a)
a <- 0                                                  #limite inferior
b <- 1                                                  #limite superior

n <- 1000                                               #numero de amostras

func <- function(x,y){                                  #funcao a analisar
  u = 1/(x+1)                                           #mudancas de variaveis
  v = 1/(y+1)
  
  auxX= (2/sqrt(2*pi))*exp(-(((1-u)/u)^2)/2) 
  auxY= (2/sqrt(2*pi))*exp(-(((1-v)/v)^2)/2) 
  
  exp(auxX+auxY) * -1/(u^2) * -1/(v^2)
}


resultArea <- 1:n
amostra <- 1:n
xRand = runif(min=a,max=b,n=n)   
yRand = runif(min=a,max=b,n=n)

for(j in 1:n){
  amostra[j] = func(xRand[j],yRand[j])
}


mean(amostra)                                            #valor esperado
sd(amostra)                                              #desvio padrão

#Exercicio 2 - alinea b)

a <- 0                                                   #limite inferior no caso da distribuicao normal e a media
b <- 1                                                   #limite superior no caso da distribuicao normal e a variancia

n <- 1000                                                #numero de amostras

#nao e preciso colocar a funcao pois variaveis geradas sobre essa funcao ja sofrem a funcao utilizada

amostra <- 1:n
xRand = runif(min=0, max=1, n)                           #amostra de 1000 elementos entre mean e sd 
yRand = runif(min=0, max=1, n)

func <- function(x){
  2*x
}

z1 = sqrt(-2*log(xRand))*cos(2*pi*yRand)
z2 = sqrt(-2*log(xRand))*sin(2*pi*yRand)

for(j in 1:n){
  amostra[j] = exp(func(z1[j]) + func(z2[j]))
}

mean(amostra)   #valor esperado
sd(amostra)     #desvio padrão
hist(amostra)   #histograma


#Exercicio 2 - alinea c)
a <- 0                                                   #limite inferior
b <- 1                                                   #limite superior

n <- 1000                                                #numero de amostras


func <- function(x,y){                                   #funcao a analisar
  u = 1/(x+1)                                            #mudanca de variavel
  v = 1/(y+1)                                            #mudança de variavel
  
  auxX= (2/sqrt(2*pi))*exp(-(((1-u)/u)^2)/2) 
  auxY= (2/sqrt(2*pi))*exp(-(((1-v)/v)^2)/2) 
  
  exp(auxX+auxY)  * -1/(u^2) * -1/(v^2)
}

generateVarControl <- function(){                        #funcao que gera a amostra de controlo 
  nctrl = 1000
  c = runif(min=0, max=1, nctrl)
  v = runif(min=0, max=1, nctrl)
  
  aux <- 1:nctrl
  for (i in 1:nctrl){
    aux[i] = c[i]*v[i]
  }
  aux
}


resultArea <- 1:n
amostra <- 1:n
xRand = runif(min=a,max=b,n=n)                           #amostra de 1000 elementos entre a e b 

for(j in 1:n){
  amostra[j] = func(xRand[j],yRand[j])
}

c = generateVarControl()                                #amostra de controlo
teta_chapeu = amostra                                   #valores tirado do exercio 2a)
beta = cov(x=teta_chapeu, y=c)/var(c)                   #beta minimo

finalC = 1:n
for(i in 1:n){                                          #geracao da amostra final usando a variavel de controlo
  finalC[i]=amostra[i] - (beta*(c[i]-mean(c)))
}

sd(amostra)                                             #sd inicial 
mean(amostra)                                           #estimador inicial

sd(finalC)                                              #sd final
mean(finalC)                                            #estimador final
