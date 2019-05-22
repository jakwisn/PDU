library(dplyr)
library(ggplot2)
library(stringi)
library(forcats)

# https://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html#lexicon
# Pytania - jakich zwierzat jest najwiecej 
# jakie zwierzeta sa najbardziej problematyczne 
# 

negative <- read.csv("pets/negative", sep = "")
negative <- as.array(negative$X2.faced)
# test "bad" %in% negative
positive <- read.csv("pets/positive", sep="")
positive <- as.array(positive$a.)
Posts <- read.csv("pets/Posts.csv")

Titles <- as.character(Posts$X_Title[Posts$X_Title != "" ])
Posts <- Posts[Posts$X_Title != "",]

tytul <- Titles[3]

naive_sentimental_analysis <- function(sentance){
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
  X <- matrix(0, length(tytuly),2)
  X[,1] <- tytuly
  X[,2] <- output
  X <- as.data.frame(X)
  return(X)
}




df <-check_all(Titles)
Posts$sentiment <-df$sentiment
colnames(df) <- c("Title", "sentiment")

tagi <- stri_replace_all_coll(Posts$X_Tags,"<","", vecorize_all= TRUE)
tagi <- stri_replace_all_coll(tagi,">"," ", vecorize_all= TRUE)
tagi <- strsplit(tagi, split = " ")
tabelkatagow <- unlist(tagi)

animals <- c("cats","dogs","fish", "rabbits","birds","turtles","snakes", "horses", "fleas","parrots","hamsters", "lizards")


for(i in 1:length(df$Title)){
  a <- animals[which(animals %in% tagi[[i]])]
  ifelse( is.character(a) & length(a) != 0, 
          df$pet[i] <- a,
          df$pet[i] <- "other")
}

table(df$sentiment)

df$sentiment <- as.factor(df$sentiment)

ggplot(df, aes(x = sentiment , fill = sentiment)) + geom_bar() + theme_bw()
ggplot(df, aes(x = pet, fill= pet) )+  geom_bar() + theme_bw() + coord_flip()
ggplot(df, aes(x = pet, fill = sentiment)) + geom_bar(position = "fill") +
        guides(fill = guide_legend(reverse = TRUE)) + scale_y_reverse() 

which(animals %in% tagi[[953]])















