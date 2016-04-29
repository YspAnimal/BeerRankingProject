require(sqldf)
require(RSQLite)
require(tm)
require(dplyr)


db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
BeerStyles <- dbReadTable(db, "Styles")
Beers <- dbReadTable(db, "Beers")
GeneralInfo <- dbReadTable(db, "GeneralInfo")

SQLQuery <- "SELECT style, Name, Beers.BeerLink, Description FROM Beers JOIN GeneralInfo
                ON (Beers.BeerLink = GeneralInfo.BeerLink)"
                

#WHERE Beers.BeerLink = 71"

myQuery <- dbSendQuery(db, SQLQuery)
Beerdata <- dbFetch(myQuery, n = -1)
dbDisconnect(db)

temp <- filter(Beerdata, style == "71") %>% select(Description)