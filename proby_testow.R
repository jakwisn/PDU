install.packages("stringi")
install.packages("mclust")
library(mclust)
library(stringi)

# moje directory, w ktorym zapisalem i rozpakowalem cale repo z gita
dir <- "/home/kuba/pdu/pd2/pdu1819-master/prace_domowe/pd2-zbiory-benchmarkowe"

# usunalem plik .rmd i wczytuje katalogi
katalogi_danych <- list.files(dir)
katalogi <- c("graves", "other" , "sipu"  , "wut"  )

i <- 1
Output <- data.frame(0,0,0,0)
colnames(Output) <- c("Spectral_Clustering", "genie", "cos", "costam")

for(katalog in katalogi_danych){
  pliki <- list.files(paste(dir,katalog, sep="/"))
  
  for(plik in pliki){
    tryCatch({
      if(endsWith(plik, "data.gz")){
        
      dane <- read.table(paste(paste(dir, katalog, sep="/"),plik,sep = '/'))
      
      # na podstawie pliku znajduje plik labels0
      plik_labels <- stri_replace(plik, replacement = ".labels0.gz", regex = ".data.gz")
      labels0 <- read.table(paste(paste(dir, katalog, sep="/"), plik_labels,sep = '/'))
      
      # sprawdzam na ile grup podzielic zbior
      k <- max(labels0)
      
      Output <- rbind(Output, c(adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,5)), 0,0,0))
      rownames(Output)[i] <- plik
      i <- i+1
      }
      })
  }
}

adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,5))
spectral_clustering(dane,4,3)

plot(dane)