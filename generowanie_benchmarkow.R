install.packages("stringi")
install.packages("mclust")
install.packages("genie")
install.packages("fastcluster")
install.packages("cluster")
install.packages("clues")

library(cluster)
library(fastcluster)
library(genie)
library(mclust)
library(stringi)
library(clues)

# moje directory, w ktorym zapisalem i rozpakowalem zbiory benchmarkowe i wpakowalem 3 swoje
dir <- "/home/kuba/pdu/pd2/Goyle_Wisniewski_Jakub_298850_pd2/pd2-zbiory-benchmarkowe"
getwd()
github_pdu_repo_to_analyze <- function(dir){

  # usunalem plik .rmd i wczytuje katalogi
  katalogi_danych <- list.files(dir)
  
  OutputAR <- data.frame(0,0,0,0,0,0,0,0,0,0,0)
  
  colnames(OutputAR) <- c("Spectral_Clustering", "genie", "hclust-single ","hclust-complete","hclust-average", 
                          "hclust-mcquitty","hclust-ward.D","hclust-ward.D2","hclust-centorid","hclust-median" ,"PAM")
  
  OutputFM <- data.frame(0,0,0,0,0,0,0,0,0,0,0)
  
  colnames(OutputFM) <- c("Spectral_Clustering", "genie", "hclust-single ","hclust-complete","hclust-average", 
                          "hclust-mcquitty","hclust-ward.D","hclust-ward.D2","hclust-centorid","hclust-median" ,"PAM")
  
  
  i <- 2
  
  for(katalog in katalogi_danych){
    pliki <- list.files(paste(dir,katalog, sep="/"))
    for(plik in pliki){
      if(endsWith(plik, "data.gz")){
        tryCatch({
        
        
        dane <- read.table(paste(paste(dir, katalog, sep="/"),plik,sep = '/'))
        # na podstawie pliku znajduje plik labels0
        # korzystajc z popularnej biblioteki do obrobki tekstu 
        plik_labels <- stri_replace(plik, replacement = ".labels0.gz", regex = ".data.gz")
        labels0 <- read.table(paste(paste(dir, katalog, sep="/"), plik_labels,sep = '/'))
      
        # sprawdzam na ile grup podzielic zbior
        k <- max(labels0)
        
        # do Genie
        genie <- hclust2(objects = as.matrix(dane), thresholdGini = 0.3)
        genieToAdd <- cutree(genie, k)
        
        # do PAM z pakiety CRAN 
        p <- pam(dane, k)
        
        # Skorygowane indeksy Randa
        OutputAR <- rbind(OutputAR, round(c(adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,10)), 
                                    adjustedRandIndex(unlist(labels0), genieToAdd),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "single"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "complete"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "average"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "mcquitty"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D2"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "centroid"),k)  ),
                                    adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "median"),k)  ),
                                    adjustedRandIndex(unlist(labels0), p$clustering)),3))
        
        
        # Teraz indeksy FM
        OutputFM <- rbind(OutputFM, round(c(adjustedRand(unlist(labels0), spectral_clustering(dane,k,10), randMethod = "FM" ), 
                                      adjustedRand(unlist(labels0), genieToAdd, randMethod = "FM"),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "single"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "complete"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "average"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "mcquitty"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D2"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "centroid"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "median"),k),randMethod = "FM"  ),
                                      adjustedRand(unlist(labels0), p$clustering)),3))
        
        
        rownames(OutputAR)[i] <- plik
        rownames(OutputFM)[i] <- plik
        i <- i+1
        })}
    }}
  write.csv(OutputAR, file = "all_AR" )
  write.csv(OutputFM, file = "FM_all")
}

otherMs <- function(dir){
  
  # usunalem plik .rmd i wczytuje katalogi
  katalogi_danych <- list.files(dir)
  
  OutputAR <- data.frame(0,0,0)
  
  colnames(OutputAR) <- c("M=2","M=5","M=15")
  
  OutputFM <- data.frame(0,0,0)
  
  colnames(OutputFM) <- c("M=2","M=5","M=15")
  
  i <- 2
  
  for(katalog in katalogi_danych){
    pliki <- list.files(paste(dir,katalog, sep="/"))
    for(plik in pliki){
      if(endsWith(plik, "data.gz")){
        tryCatch({
          
          
          dane <- read.table(paste(paste(dir, katalog, sep="/"),plik,sep = '/'))
          # na podstawie pliku znajduje plik labels0
          # korzystajc z popularnej biblioteki do obrobki tekstu 
          plik_labels <- stri_replace(plik, replacement = ".labels0.gz", regex = ".data.gz")
          labels0 <- read.table(paste(paste(dir, katalog, sep="/"), plik_labels,sep = '/'))
          
          # sprawdzam na ile grup podzielic zbior
          k <- max(labels0)
          
          # do Genie
          genie <- hclust2(objects = as.matrix(dane), thresholdGini = 0.3)
          genieToAdd <- cutree(genie, k)
          
          # do PAM z pakiety CRAN 
          p <- pam(dane, k)
          
          # Skorygowane indeksy Randa
          OutputAR <- rbind(OutputAR, round(c(adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,2)), 
                                        adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,5)),
                                        adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,15))
                                        ),3))
          
          
          # Teraz indeksy FM
          OutputFM <- rbind(OutputFM, round(c(adjustedRand(unlist(labels0), spectral_clustering(dane,k,2), randMethod = "FM" ),
                                              adjustedRand(unlist(labels0), spectral_clustering(dane,k,5), randMethod = "FM" ),
                                              adjustedRand(unlist(labels0), spectral_clustering(dane,k,15), randMethod = "FM" )
                                              ),3))
          
          
          rownames(OutputAR)[i] <- plik
          rownames(OutputFM)[i] <- plik
          i <- i+1
        })}
    }}
  write.csv(OutputAR, file = "AR_rozneM" )
  write.csv(OutputFM, file = "FM_rozneM")
}

standaryzuj <- function(X){
  c <- ncol(X)
  for(i in 1:c){
    X[,i] <- (X[,i] - mean(X[,i]))/sd(X[,i])
  }
  return(X)
}


github_pdu_repo_standaryzowane_zmienne <- function(dir){
  
  # usunalem plik .rmd i wczytuje katalogi
  katalogi_danych <- list.files(dir)
  
  OutputAR <- data.frame(0,0,0,0,0,0,0,0,0,0,0)
  
  colnames(OutputAR) <- c("Spectral_Clustering", "genie", "hclust-single ","hclust-complete","hclust-average", 
                          "hclust-mcquitty","hclust-ward.D","hclust-ward.D2","hclust-centorid","hclust-median" ,"PAM")
  
  OutputFM <- data.frame(0,0,0,0,0,0,0,0,0,0,0)
  
  colnames(OutputFM) <- c("Spectral_Clustering", "genie", "hclust-single ","hclust-complete","hclust-average", 
                          "hclust-mcquitty","hclust-ward.D","hclust-ward.D2","hclust-centorid","hclust-median" ,"PAM")
  
  
  i <- 2
  
  for(katalog in katalogi_danych){
    pliki <- list.files(paste(dir,katalog, sep="/"))
    for(plik in pliki){
      if(endsWith(plik, "data.gz")){
        tryCatch({
          
          
          dane <- read.table(paste(paste(dir, katalog, sep="/"),plik,sep = '/'))
          
          # teraz dane poddaje standaryzacji 
          dane <- standaryzuj(dane)
          
          
          # na podstawie pliku znajduje plik labels0
          # korzystajc z popularnej biblioteki do obrobki tekstu 
          plik_labels <- stri_replace(plik, replacement = ".labels0.gz", regex = ".data.gz")
          labels0 <- read.table(paste(paste(dir, katalog, sep="/"), plik_labels,sep = '/'))
          
          # sprawdzam na ile grup podzielic zbior
          k <- max(labels0)
          
          # do Genie
          genie <- hclust2(objects = as.matrix(dane), thresholdGini = 0.3)
          genieToAdd <- cutree(genie, k)
          
          # do PAM z pakiety CRAN 
          p <- pam(dane, k)
          
          # Skorygowane indeksy Randa
          OutputAR <- rbind(OutputAR, round(c(adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,10)), 
                                        adjustedRandIndex(unlist(labels0), genieToAdd),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "single"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "complete"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "average"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "mcquitty"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D2"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "centroid"),k)  ),
                                        adjustedRandIndex(unlist(labels0), cutree(hclust(dist(dane), method = "median"),k)  ),
                                        adjustedRandIndex(unlist(labels0), p$clustering)),3))
          
          
          # Teraz indeksy FM
          OutputFM <- rbind(OutputFM, round(c(adjustedRand(unlist(labels0), spectral_clustering(dane,k,10), randMethod = "FM" ), 
                                              adjustedRand(unlist(labels0), genieToAdd, randMethod = "FM"),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "single"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "complete"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "average"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "mcquitty"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "ward.D2"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "centroid"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), cutree(hclust(dist(dane), method = "median"),k),randMethod = "FM"  ),
                                              adjustedRand(unlist(labels0), p$clustering)),3))
          
          
          rownames(OutputAR)[i] <- plik
          rownames(OutputFM)[i] <- plik
          i <- i+1
        })}
    }}
  write.csv(OutputAR, file = "AR_standaryzowane" )
  write.csv(OutputFM, file = "FM_standaryzowane")
}


github_pdu_repo_to_analyze(dir)
github_pdu_repo_standaryzowane_zmienne(dir)
otherMs(dir)