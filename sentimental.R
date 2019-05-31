library(dplyr)
library(ggplot2)
library(stringi)

# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
# Pytania - jakich zwierzat jest najwiecej 
# jakie zwierzeta sa najbardziej problematyczne 
# 

negative <- read.csv("pets/negative", sep = "")
negative <- as.array(negative$X2.faced)
# test "bad" %in% negative
positive <- read.csv("pets/positive", sep="")
positive <- as.array(positive$a.)



naive_sentimental_analysis <- function(sentance){

  # Algorytm do naiwnej analizy sentymentalnej 
    list_of_words <- strsplit(sentance, split = " ")
  list_of_words <- list_of_words[[1]]
  output <- rep(0, length(list_of_words))
  i <- 1
  for(word in list_of_words){
    ifelse(word %in% positive, output[i] <- 1, 
           ifelse(word %in% negative, output[i] <- -1, output[i] <- 0))
    i <- i+1
  }
  return(output)
}

check_all <- function(tytuly){
  
  #  funkcja wykonuje naive sentimental analysis na kazdym tytule
  output <- rep(0, length(tytuly))
  i <- 1
  # usuwam znaki zapytania i wykrzykniki aby ostatnie slowa tez byly łapane
  tytuly <- stri_replace_all_coll(tytuly,"?","", vecorize_all= TRUE)
  tytuly <- stri_replace_all_coll(tytuly,"!","", vecorize_all= TRUE)
  tytuly <- tolower(tytuly)
  # na każdym tytule wywołuje naive sentimental alalysis
  # i przypisuje 1 jesli slowo jest pozytywne, o jesli jest neutralne i -1 dla negatywnych
  for(tytul in tytuly){
    x <- naive_sentimental_analysis(tytul) %>% sum()
    ifelse(x >= 1 , output[i] <- "positive", ifelse(x <= -1, output[i] <- "negative", output[i] <- "neutral" ))
    i <- i +1
  }
  return(output)
}



Tagi <- Posts$X_Tags
typeof(Tagi)
animals <- c("cats","dogs","fish", "rabbits","birds","turtles","snakes", "horses", "fleas","hamsters", "lizards")

# tworze z tagow tableke tagow np <dog><food><love> ---> ["dog","food","love"]
strip_tagi <- function(Tagi){
tagi <- stri_replace_all_coll(Tagi,"<","", vecorize_all= TRUE)
tagi <- stri_replace_all_coll(tagi,">"," ", vecorize_all= TRUE)
tagi <- strsplit(tagi, split = " ")
return(tagi)
}

Tagi<- X[1:2,]
strip_tagi(X$Tags)

# tutaj jezeli zwierze jest w tabelce tagow, to przypisuje je do animal
which_animal <- function(Tags){
  i <- 1
  animal <- matrix(0, length(Tags), 1)
  for(i in 1:length(Tags)){
  a <- animals[which(animals %in% Tags[[i]])]
  ifelse( is.character(a) & length(a) == 1 , 
          animal[i,1] <- a , 
          animal[i,1] <- "other")
  }
  return(animal)
}


