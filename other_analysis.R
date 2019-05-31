Posts <- read.csv("pets/Posts.csv")

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


