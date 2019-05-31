Posts <- read.csv("pets/Posts.csv")

posty <-  Posts %>% filter(X_Tags != "") %>% filter(X_Title != "" ) %>% 
  select(X_OwnerUserId, X_Title, X_Score, X_ViewCount, X_AnswerCount, X_Tags )
colnames(posty) <- c("OwnerUserId", "Title", "Score", "ViewCount", "AnswerCount","Tags")
posty$Sentiment <- check_all(posty$Title) %>% as.factor()
posty$animal <- posty$Tags %>% strip_tagi() %>% which_animal()


# 3 rysunki dla petsow

X <- posty %>%  group_by(animal) %>% summarise(Count = table(animal)) %>% left_join(posty, by= "animal") %>% arrange(desc(Count.x))
Y <- posty %>% group_by(animal) %>% summarise(part = (table(Sentiment[Sentiment == "negative"])[1])/sum(table(Sentiment))) %>% 
      left_join(posty, by = "animal")
 
posty <- within(posty, Position <- factor(animal, levels= names(sort(table(animal), decreasing = TRUE)) ))
# ilość pozytywnych i negatywnych
ggplot(posty, aes(x = Sentiment , fill = Sentiment)) + geom_bar() + theme_bw()
# jakie zwierzęta najczęściej się pojawiają
ggplot(X, aes(x = reorder(animal, Count.x), fill= animal) )+  geom_bar() + theme_bw() + coord_flip()
# ilość pozytywnych i negatywnych w stosunku do zwierząt
ggplot(Y, aes(x = reorder(animal, part) , fill = Sentiment)) + geom_bar(position = "fill") +
  guides(fill = guide_legend(reverse = TRUE)) + scale_y_reverse() 




# Users 

Users <- read.csv("./pets/Users.csv")

X <-  Posts %>% inner_join(Users , by= c("X_OwnerUserId" = "X_Id")) %>%
  select(X_Id, X_DisplayName, X_Title, X_Score, X_ViewCount,X_UpVotes, X_DownVotes,  X_Location, X_Tags)
X$Sentiment <- X$X_Title %>% check_all()
colnames(X) <- c("Id", "Name","Title", "Score", "ViewCount", "UpVotes","DownVotes" ,"Location", "Tags", "Sentiment")

X <-  X %>% filter(Title != "") %>% arrange(desc(ViewCount)) 
X$Animal <- X$Tags %>% strip_tagi() %>% which_animal() %>% as.factor() 

top_users <-  X %>% group_by(Name) %>% summarise(TotalUpvotes = sum(UpVotes)-sum(DownVotes)) %>% arrange(desc(TotalUpvotes)) %>% 
  slice(1:5) %>% left_join(X, by = "Name") 

# maja najwiekszy stosunek downvotes do upvotes
worst_users <-X %>% group_by(Name) %>% summarise(DownUpDifference = sum(DownVotes)-sum(UpVotes)) %>% arrange(desc(DownUpDifference)) %>% 
  slice(1:5) %>% left_join(X, by = "Name")


ggplot(top_users, aes(x = Name, fill = Animal)) + geom_bar() + theme_bw()
ggplot(worst_users, aes(x = Name, fill = Animal)) + geom_bar() + theme_bw()


