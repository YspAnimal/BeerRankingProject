require(shiny)
require(sqldf)
require(RSQLite)
require(tm)
require(dplyr)
library(wordcloud)
library(memoise)
require(RColorBrewer)
#library(RODBCext)
#require(Rstem)
#require(Snowball)
#library(wordnet)

db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
SQLQueryTypesStyles <- "SELECT BeerStyle, Type, style FROM Styles"
myQuery <- dbSendQuery(db, SQLQueryTypesStyles)
TypesFrame <- dbFetch(myQuery, n = -1)
Types <- unique(TypesFrame$Type)
dbDisconnect(db)

GetStyles <- function(type){
    #db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
    #SQLQueryTypesStyles <- paste0("SELECT BeerStyle FROM Styles WHERE Type = '", type, "'")
    #myQuery <- dbSendQuery(db, SQLQueryTypesStyles)
    #styles <- dbFetch(myQuery, n = -1)
    unique(filter(TypesFrame, Type == type)$BeerStyle)
}

getTermMatrix <- memoise(function(style){
    styleN <- TypesFrame[which(TypesFrame$BeerStyle == style), ]$style
    db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
    SQLQueryBeers <- paste0("SELECT BeerLink FROM Beers WHERE Style = '", styleN, "'")
    myQuery <- dbSendQuery(db, SQLQueryBeers)
    Style <- dbFetch(myQuery, n = -1)
    dbDisconnect(db)
    
    db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
    data <- lapply(Style, function(y){
        dbGetQuery(db, paste0("SELECT V1 FROM BeerReviewsNew WHERE V2='", y, "'"))
    })
    dbDisconnect(db)
    data <- data$BeerLink
    #tm_map(data, asPlain)
    #temp_txt <- paste(temp$Description, collapse = " ")
    temp.vec <- VectorSource(data)
    temp.cor <- Corpus(temp.vec)
    #summary(temp.cor)
    #inspect(temp.cor)
    temp.cor <- tm_map(temp.cor, content_transformer(tolower))
    temp.cor <- tm_map(temp.cor, removePunctuation)
    temp.cor <- tm_map(temp.cor, removeNumbers)
    #temp.cor <- tm_map(temp.cor, removeNumbers)
    Beerstopwords <- c(stopwords("english"),"aroma", "appearance","taste", 
                       "palate","overall","beer", "beers", "bottle", "conditioned", 
                       "ale", "abbey", "brewed", "beer", "ale", "brewered", "abbey", 
                       "bottle", "aroma", "flavour", "gagooglefillslotbeerpage")
    temp.cor <- tm_map(temp.cor, removeWords, Beerstopwords)
    temp.cor <- tm_map(temp.cor, stripWhitespace)
    #DTM <- TermDocumentMatrix(temp.cor)
    DTM <- DocumentTermMatrix(temp.cor)
    DTM_Mat <- as.matrix(DTM)
    DTM_Mat <- sort(colSums(DTM_Mat),decreasing=TRUE)
    DTM_Mat <- data.frame(word = names(DTM_Mat),freq=DTM_Mat)
    #DTM_v <- sort(colSums(DTM_Mat),decreasing=TRUE)
    #DTM_d <- data.frame(word = names(DTM_v),freq=DTM_v)
    #table(DTM_d$freq)
    #pal2 <- brewer.pal(8,"Dark2")
    #png("wordcloud_Beers_Rew.png", width=480,height=300)
    #wordcloud(DTM_d$word,DTM_d$freq, scale=c(8,.5),min.freq=2,
    #       max.words=Inf, random.order=FALSE, rot.per=.30, colors=pal2)
    #dev.off()
    #return(data)
    #sort(rowSums(DTM_Mat), decreasing = TRUE)
})


    




# db1 <- dbConnect(SQLite(), dbname="NewBase/BeerDB.sqlite")
# db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
# 
# BeerStyles <- dbReadTable(db1, "Styles")
# Beers <- dbReadTable(db1, "Beers")
# GeneralInfo <- dbReadTable(db1, "GeneralInfo")
# BeerReviews <- dbReadTable(db1, "BeerReviewsNew")
# 
# 
# 
# 
# 
# 
# 
# SQLQueryStyles <- "SELECT BeerStyle, Type, style FROM BeerStyles"
# 
# SQLQueryDescription <- "SELECT style, Name, Beers.BeerLink, Description FROM Beers JOIN GeneralInfo
#                         ON (Beers.BeerLink = GeneralInfo.BeerLink)"
# 
# SQLQueryReviews <- "SELECT style, Name, Beers.BeerLink, V2 FROM Beers JOIN BeerReviewsNew
#                     ON (Beers.BeerLink = BeerReviewsNew.V1)"
# 
# 
# #WHERE Beers.BeerLink = 71"
# 
# myQuery <- dbSendQuery(db1, SQLQueryReviews)
# Beerdata <- dbFetch(myQuery, n = -1)
# dbDisconnect(db)
# 
# temp <- filter(Beerdata, style == "71") %>% select(Description)
# 
# tm_map(temp, asPlain)
# temp_txt <- paste(temp$Description, collapse = " ")
# temp.vec <- VectorSource(temp_txt)
# temp.cor <- Corpus(temp.vec)
# #summary(temp.cor)
# #inspect(temp.cor)
# 
# temp.cor <- tm_map(temp.cor, content_transformer(tolower))
# temp.cor <- tm_map(temp.cor, removePunctuation)
# temp.cor <- tm_map(temp.cor, removeNumbers)
# #temp.cor <- tm_map(temp.cor, removeNumbers)
# Beerstopwords <- c(stopwords("english"), "beer", "beers", "bottle", "conditioned", "ale", "abbey", "brewed", "beer", "ale", "brewered", "abbey", "bottle", "aroma", "flavour")
# temp.cor <- tm_map(temp.cor, removeWords, Beerstopwords)
# temp.cor <- tm_map(temp.cor, stripWhitespace)
# DTM <- DocumentTermMatrix(temp.cor)
# #TDM <- TermDocumentMatrix(temp.cor)
# #inspect(TDM)
# inspect(DTM)
# DTM_Mat <- as.matrix(DTM)
# frequency <- colSums(as.matrix(DTM))
# frequency <- sort(frequency, decreasing = TRUE)
# head(frequency)
# frequency[which(frequency>=5)]
# 
# DTM_Mat <- as.matrix(DTM)
# DTM_v <- sort(colSums(DTM_Mat),decreasing=TRUE)
# DTM_d <- data.frame(word = names(DTM_v),freq=DTM_v)
# table(DTM_d$freq)
# pal2 <- brewer.pal(8,"Dark2")
# png("wordcloud_Beers_Rew.png", width=480,height=300)
# wordcloud(DTM_d$word,DTM_d$freq, scale=c(8,.5),min.freq=2,
#           max.words=Inf, random.order=FALSE, rot.per=.30, colors=pal2)
# dev.off()
