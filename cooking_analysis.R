library(countrycode)
library(data.table)
library(XML)
library(dplyr)

xmlToList("./cooking/Posts.xml") %>% lapply(as.list) %>% rbindlist(fill=TRUE) %>% as.data.frame() -> posts
xmlToList("./cooking/Tags.xml") %>% lapply(as.list) %>% rbindlist(fill=TRUE) %>% as.data.frame() -> tagi
xmlToList("./cooking/Users.xml") %>% lapply(as.list) %>% rbindlist(fill=TRUE) %>% as.data.frame() -> users

#zakladanie kont w czasie 
users$year <- users$CreationDate %>% substring(1,4)
X <- users$year[users$year != "2019"] %>% as.integer()  
X <-  users %>% group_by(year) %>% summarise(Count = table(year)) %>%
      left_join(users, by= "year") %>% filter(year != "2019") 
colnames()      
tbl <- as.data.frame(table(X$year))
ggplot(tbl,  aes(x = Var1, y=Freq )) + geom_line() + theme_bw()  

X <- posts$Tags %>% na.omit() %>% strip_tagi()
X <- tagi %>% arrange(desc(as.integer(Count)))

posts %>% select(Title) %>% filter(Title != "")

## Kontynenty 
users %>% na.omit 
location <- users$Location %>% countrycode("country.name", "continent")
users$continent <- location 
loc_usr <- users %>% na.omit()

ggplot(loc_usr, aes(x = continent, fill= continent)) + geom_bar()

## Kontynenty / liczbę osób żyjących w kontynentach 

tabelka_kontynentów <- table(loc_usr$continent)
ilosc_postow_do_populacji <- tabelka_kontynentów/c(1216, 1002, 4463, 741,43)
X <- as.data.frame(ilosc_postow_do_populacji)
colnames(X) <- c("Kontynenty", "ilość_postów/populacja(mln)" )
ggplot(X, aes(x = Kontynenty, y = X[,2], fill = Kontynenty)) + 
  geom_bar(stat = "identity") + theme_bw() + ggtitle("Ilość postów w stosunku do populacju kontynentów w milionach") +
  theme(plot.title = element_text(hjust = 0.5, size = 20, face = "bold")) + ylab("ilość postów / populacja w mln") 

posts %>% arrange(desc(ViewCount)) %>% filter(Title != "" ) %>% slice(1:10)

