spectral_clustering <- function(X, k, M) { 
  
  
  
  
}

Mnn <- function(X , M) { 
  n <- nrow(X)
  Z <- macierz_odl(X)
  Wynik_prawie <- matrix(0, n, n)
  for (i in 1:n){
    # to jest posortane inteksami
    t <- order(Z[i,])
    Wynik_prawie[i,] <-t 
  }
  # z macierzy zwracam od 2 do M+1 rzędu, bo bez 1 
  # wypełnionego samymi zerami
  Wynik <- Wynik_prawie[,2:(M+1)]
  return(Wynik)
}


macierz_odl <- function(X) { 
  # z jedna petla
  X <- as.matrix(X)
  n <- nrow(X)
  Z <- matrix(0, n ,n)
  for(i in 1:n) {
    xi <- rep(X[i,],n)
    xi <- matrix(xi, n, ncol(X), byrow = TRUE)
    e_dist <- sqrt(rowSums((X - xi)^2))
    Z[i,] <- e_dist
  }
return(Z)
}


Wyn <- Mnn(twodiamonds, 3)

x<- matrix(3,3,4)
y <-c(2,3,4,5,6)

sum((x-y)^2)



