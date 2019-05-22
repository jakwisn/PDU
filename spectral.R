install.packages("igraph")
library(igraph)


macierz_odl <- function(X) { 
  # z jedna petla
  X <- as.matrix(X)
  n <- nrow(X)
  Z <- matrix(0, n ,n)
  for(i in 1:n) {
    
    # na podstawie wiersza tworzymy 
    # nowy wektor zlozony z repliki wiersza i 
    xi <- rep(X[i,],n)
    xi <- matrix(xi, n, ncol(X), byrow = TRUE)
    # odejmujemy wektor xi od calej macierzy 
    e_dist <- sqrt(rowSums((X - xi)^2))
    Z[i,] <- e_dist
  }
  return(Z)
}


Mnn <- function(X , M) { 
  n <- nrow(X)
  Z <- macierz_odl(X)
  Wynik_prawie <- matrix(0, n, n)
  for (i in 1:n){
    # sortuje indeksami
    t <- order(Z[i,])
    Wynik_prawie[i,] <-t 
  }
  # z macierzy zwracam od 2 do M+1 rzędu, bo bez 1 
  # wypełnionego samymi zerami
  Wynik <- Wynik_prawie[,2:(M+1)]
  return(Wynik)
}



Mnn_graph <- function(S) { 
  n <- nrow(S)
  G <- matrix(0, n, n)
  for(i in 1:n){
    
    # przypisuje do temp pierwszy wiersz
    # czyli te wierzcholki z ktorymi i-ty jest polaczony
    temp <- S[i,]
    # w naszym grafie w i-ty wiersz i kolumny w temp
    G[i,temp] <- 1
  }
  # zamieniam na graf rozumiany przez paczke igraph
  g <- graph_from_adjacency_matrix(G,mode = "undirected")
  # jesli jest spojny zwracamy graf
  if(is_connected(g)){return(G)}
  else{
  # jesli nie niej spójny, patrzymy na ile podgrafow spojnych jest podzielony
  p <- components(g)$no
  # i ktore wierzcholki naleza do ktorego podgrafu
  h <- components(g)$membership
  i <- 1
  while (i < p){
    # tutaj doslownie lacze pierwsze wierzcholki w grupach
    # tworzac p-1 krawedzi
    temp1 <- match(h[h == i],h)[1]
    temp2 <- match(h[h == i+1], h)[1]
    G[temp1, temp2] <- 1
    G[temp2, temp1] <- 1
    i <- i+1
  }
  
  return(G)}
}


Laplacian_eigen <- function(G,k) {
  
  # zmieniam na graf w rozumieniu paczki igraph
  g <- graph_from_adjacency_matrix(G,mode = "undirected")
  
  # obliczam jego laplacian
  lp <- laplacian_matrix(g,sparse = FALSE)
  
  # a nastepnie jego wektory wlasne
  lp_eigen <- eigen(lp)
  
  # porzadkuje i biore od 2 do k+1 
  E <- lp_eigen$vectors[,order(lp_eigen$values)]
  return(E[,2:(k+1)])
  
}

# nasza wysjciowa funkcja 
spectral_clustering <- function(X, k, M){
  S <- Mnn(X,M)
  G <- Mnn_graph(S)
  E <- Laplacian_eigen(G,k)
  # funkcja k-means dziala dobrze przy wiekszej ilosci iteracji niz domyslna
  # ja wzialem 250
  
  K <- kmeans(E,k, iter.max = 250)$cluster
  return(K)
}

