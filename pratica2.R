 x <- c(1,2,3,4,5)
p <- c(0.2, 0.3, 0.1, 0.3, 0.1) 

u <- runif(1)


if( u < p[1]) {
  y <- x[1]
}else if(u < p[1] + p[2]) {
  y <- x[2]
}else if(u < p[1] + p[2] + p[3]) {
  y <- x[3]
}else if(u < p[1] + p[2] + p[3] + p[4]) {
  y <- x[4]
}else{
  y <- x[5]
}

amostra <- c(NA, 100)

for ( i in 1: 100) {
  if( u < p[1]) {
    amostra[i] <- x[1]
  }else if(u < p[1] + p[2]) {
    amostra[i] <- x[2]
  }else if(u < p[1] + p[2] + p[3]) {
    amostra[i] <- x[3]
  }else if(u < p[1] + p[2] + p[3] + p[4]) {
    amostra[i] <- x[4]
  }else{
    amostra[i] <- x[5]
  }
  u <- runif(1)
}

amostra

i
