require(sqldf)
require(RSQLite)
require(tm)
require(dplyr)
require(Rstem)
#require(Snowball)
library(wordnet)
library(wordcloud)
library(RODBCext)

db1 <- dbConnect(SQLite(), dbname="NewBase/BeerDB.sqlite")
db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")

BeerStyles <- dbReadTable(db1, "Styles")
Beers <- dbReadTable(db1, "Beers")
GeneralInfo <- dbReadTable(db1, "GeneralInfo")
BeerReviews <- dbReadTable(db1, "BeerReviewsNew")







SQLQueryStyles <- "SELECT BeerStyle, Type, style FROM BeerStyles"

SQLQueryDescription <- "SELECT style, Name, Beers.BeerLink, Description FROM Beers JOIN GeneralInfo
                ON (Beers.BeerLink = GeneralInfo.BeerLink)"
                
SQLQueryReviews <- "SELECT style, Name, Beers.BeerLink, V2 FROM Beers JOIN BeerReviewsNew
                ON (Beers.BeerLink = BeerReviewsNew.V1)"


#WHERE Beers.BeerLink = 71"

myQuery <- dbSendQuery(db1, SQLQueryReviews)
Beerdata <- dbFetch(myQuery, n = -1)
dbDisconnect(db)

temp <- filter(Beerdata, style == "71") %>% select(Description)

tm_map(temp, asPlain)
temp_txt <- paste(temp$Description, collapse = " ")
temp.vec <- VectorSource(temp_txt)
temp.cor <- Corpus(temp.vec)
#summary(temp.cor)
#inspect(temp.cor)

temp.cor <- tm_map(temp.cor, content_transformer(tolower))
temp.cor <- tm_map(temp.cor, removePunctuation)
temp.cor <- tm_map(temp.cor, removeNumbers)
#temp.cor <- tm_map(temp.cor, removeNumbers)
Beerstopwords <- c(stopwords("english"), "beer", "beers", "bottle", "conditioned", "ale", "abbey", "brewed")
temp.cor <- tm_map(temp.cor, removeWords, Beerstopwords)
temp.cor <- tm_map(temp.cor, stripWhitespace)
DTM <- DocumentTermMatrix(temp.cor)
#TDM <- TermDocumentMatrix(temp.cor)
#inspect(TDM)
inspect(DTM)
DTM_Mat <- as.matrix(DTM)
frequency <- colSums(as.matrix(DTM))
frequency <- sort(frequency, decreasing = TRUE)
head(frequency)
frequency[which(frequency>=5)]

DTM_Mat <- as.matrix(DTM)
DTM_v <- sort(colSums(DTM_Mat),decreasing=TRUE)
DTM_d <- data.frame(word = names(DTM_v),freq=DTM_v)
table(DTM_d$freq)
pal2 <- brewer.pal(8,"Dark2")
png("wordcloud_Beers_Rew.png", width=480,height=300)
wordcloud(DTM_d$word,DTM_d$freq, scale=c(8,.5),min.freq=2,
          max.words=Inf, random.order=FALSE, rot.per=.30, colors=pal2)
dev.off()








