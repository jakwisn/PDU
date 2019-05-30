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
Posts <- read.csv("pets/Posts.csv")


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





animals <- c("cats","dogs","fish", "rabbits","birds","turtles","snakes", "horses", "fleas","hamsters", "lizards")

# tworze z tagow tableke tagow np <dog><food><love> ---> ["dog","food","love"]
strip_tagi <- function(Tagi){
tagi <- stri_replace_all_coll(Tagi,"<","", vecorize_all= TRUE)
tagi <- stri_replace_all_coll(tagi,">"," ", vecorize_all= TRUE)
tagi <- strsplit(tagi, split = " ")
return(tagi)
}

Tags <- posty$Tags %>% strip_tagi()
i <- 1
# tutaj jezeli zwierze jest w tabelce tagow, to przypisuje je do animal
which_animal <- function(Tags){
  animal <- matrix(0, length(Tags), 1)
  for(i in 1:length(Tags)){
  a <- animals[which(animals %in% Tags[[i]])]
  ifelse( is.character(a) & length(a) == 1 , 
          animal[i,1] <- a , 
          animal[i,1] <- "other")
  }
  return(animal)
}

posty <-  Posts %>% filter(X_Tags != "") %>% filter(X_Title != "" ) %>% 
          select(X_OwnerUserId, X_Title, X_Score, X_ViewCount, X_AnswerCount, X_Tags )
colnames(posty) <- c("OwnerUserId", "Title", "Score", "ViewCount", "AnswerCount","Tags")
posty$Sentiment <- check_all(posty$Title) %>% as.factor()
posty$animal <- posty$Tags %>% strip_tagi() %>% which_animal()



# 3 rysunki dla petsow

# ilość pozytywnych i negatywnych
ggplot(posty, aes(x = Sentiment , fill = Sentiment)) + geom_bar() + theme_bw()
# jakie zwierzęta najczęściej się pojawiają
ggplot(posty, aes(x = animal, fill= animal) )+  geom_bar() + theme_bw() + coord_flip()
# ilość pozytywnych i negatywnych w stosunku do zwierząt
ggplot(posty, aes(x = animal, fill = Sentiment)) + geom_bar(position = "fill") +
        guides(fill = guide_legend(reverse = TRUE)) + scale_y_reverse() 


# Users 

Users <- read.csv("./pets/Users.csv")

X <-  Posts %>% inner_join(Users , by= c("X_OwnerUserId" = "X_Id")) %>%
      select(X_Id, X_DisplayName, X_Title, X_Score, X_ViewCount,X_UpVotes, X_Location, X_Tags)
X$Sentiment <- X$X_Title %>% check_all()
colnames(X) <- c("Id", "Name","Title", "Score", "ViewCount", "Upvotes", "Location", "Tags", "Sentiment")

X <-  X %>% filter(Title != "") %>% arrange(desc(ViewCount)) 
X$Animal <- X$Tags %>% strip_tagi() %>% which_animal() %>% as.factor() 

top_users <-  X %>% group_by(Name) %>% summarise(TotalUpvotes = sum(Upvotes)) %>% arrange(desc(TotalUpvotes)) %>% 
              slice(1:5) %>% left_join(X, by = "Name") 


ggplot(top_users, aes(x = Name, fill = Animal)) + geom_bar() + theme_bw()
ggplot(top_users, aes(x = Name, fill = Sentiment)) + geom_bar() + theme_bw()







