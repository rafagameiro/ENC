
varPiHat <- function( n, y ) {
  (32 - 3*(y^2))/(3*n)
}

piHatC <- function( y ) {
  y - beta()*(u - 1/2)
}

cov <- function( n, y) {
  (8 - 3*y)/(6*n)
}

varPiHatC <- function( n, y ) {
  varPiHat(n, y) + beta^2/(12*n) - 2*beta*cov(n, y)
}

amSize <- 10
resultC <- 1:7
result <- 1:7
y <- 1:7
for(i in 1:7 ){
  u <- runif(amSize)
  
  y[i] <-  mean(4 * sqrt(1 - u^2))
  
  beta <- 2*(8 - 3*y[i])
  
  result[i] <- varPiHat(amSize, y[i])
  resultC[i] <- varPiHatC(amSize, y[i])
  if(i %% 2==1){
    amSize <- amSize * 5
  }else {
    amSize <- amSize *2
  }
}

data.frame(y, result, resultC)

