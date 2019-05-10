install.packages("igraph")
library(igraph)

Mnn_graph <- function(S) { 
  n <- nrow(S)
  G <- matrix(0, n, n)
  for(i in 1:n){
      temp <- S[i,]
      G[i,temp] <- 1
  }
  g <- graph.adjacency(G,mode = "undirected")
  if(is_connected(g)){return(g)}
  # jesli nie niej spójny
  p <- components(g)$no
  h <- components(g)$membership
  i <- 1
  while (i < p){
    temp1 <- match(h[h == i],h)[1]
    temp2 <- match(h[h == i+1], h)[1]
    G[temp1, temp2] <- 1
    G[temp2, temp1] <- 1
  }
  return(G)
}

temp <- Wyn[1,]
G[1,temp] <- 1  