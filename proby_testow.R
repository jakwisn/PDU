install.packages("stringi")
install.packages("mclust")
install.packages("genie")
install.packages("hclust")


library(genie)
library(mclust)
library(stringi)

# moje directory, w ktorym zapisalem i rozpakowalem cale repo z gita
dir <- "/home/kuba/pdu/pd2/pdu1819-master/prace_domowe/pd2-zbiory-benchmarkowe"

# usunalem plik .rmd i wczytuje katalogi
katalogi_danych <- list.files(dir)


i <- 2
Output1 <- data.frame(0,0,0,0)
colnames(Output1) <- c("Spectral_Clustering", "genie", "cos", "costam")
set.seed(123)


github_pdu_repo_to_analyze <- function(dir){

  # usunalem plik .rmd i wczytuje katalogi
  katalogi_danych <- list.files(dir)
  
  Output1 <- data.frame(0,0,0,0)
  colnames(Output1) <- c("Spectral_Clustering", "genie", "cos", "costam")
  set.seed(123)
  
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
        genie <- hclust2(objects = as.matrix(dane), thresholdGini = 0.3)
        genieToAdd <- cutree(genie, k)
        Output1 <- rbind(Output1, c(adjustedRandIndex(unlist(labels0), spectral_clustering(dane,k,15)), 
                                    adjustedRandIndex(unlist(labels0), genieToAdd),0,0))
        rownames(Output1)[i] <- plik
        i <- i+1
        }
        })
    }
  }
  write.csv(Output1, file = "OutputPDU" )
}




github_pdu_repo_to_analyze(dir)

moj <- OutputPDU[,2]
geniee <- OutputPDU[,3]

mean(moj)
mean(geniee)

genie <- hclust2(objects = as.matrix(danee), thresholdGini = 0.3)
genieToAdd <- cutree(genie, k)





out <- hclust2(objects = as.matrix(danee), thresholdGini = 0.3)
out1 <- cutree(out,2)
max(ring)



danee <- a1.data
labels <- a1.labels0
k <- max(labels)
set.seed(23)
danee$col <- spectral_clustering(danee,k,15)
adjustedRandIndex(unlist(labels), genieToAdd)

plot(danee[,1:3], col = danee$col)

boxplot(OutputPDU[,2], OutputPDU[,3], horizontal = TRUE)

h <- hclust2(objects=as.matrix(iris[,2:3]), thresholdGini=0.2)
plot(iris[,2], iris[,3], col=cutree(h, 3), pch=as.integer(iris[,5]))

plot(dane)