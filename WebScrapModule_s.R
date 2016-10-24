require(rvest)
require(reshape2)
require(stringr)
require(plyr)
require(sqldf)
require(data.table)
options(stringsAsFactors = FALSE)

##Get Beer styles table
GetBeerStylesDataframe <- function (link) {
        beer_page <-
                read_html(link) # Set WEB-page of beer ranking
        beer_Groups <-
                html_text(html_nodes(beer_page, ".groupname")) # Get Global beer groups
        beer_stylesHTML <-
                html_nodes(beer_page, ".styleGroup") # Get beer styles nodes
        beer_stylesHTML <-
                lapply(beer_stylesHTML, function(x) {
                        html_nodes(x, "li")
                }) # Prepare to transform to list
        beer_styles <-
                lapply(beer_stylesHTML, html_text) # Get list of beer styles
        names(beer_styles) <- beer_Groups # Rename beer styles list
        beer_styles <- melt(beer_styles) # melt list to data frame
        beer_linksHTML <-
                lapply(beer_stylesHTML, function(x) {
                        html_nodes(x, "a")
                }) # extract information about links
        beer_links <-
                lapply(beer_linksHTML, function(x) {
                        html_attr(x, "href")
                }) # Get beer styles links to own web page
        beer_links <- unlist(beer_links) # Unlist it to vector
        beer_styles <-
                cbind(beer_styles, beer_links) # Create one data set, thet contains information about all of characteristic of beer
        return(beer_styles)
}


####Try to aggregate full information about beer styles in one dataFrame

##Get beers list from style link
stylesFrame <- GetBeerStylesDataframe("http://www.ratebeer.com/beerstyles")
parlist <- list()
for (i in 1:nrow(stylesFrame)) {
        styleLink <- as.vector(stylesFrame$beer_links[[i]])
        StylePage <- paste0("http://www.ratebeer.com", styleLink)
        ScriptTXT <- read_html(StylePage) %>% html_nodes("script")
        ScriptTXT <- html_text(ScriptTXT[9]) %>% strsplit("[\r\n\t]") %>% unlist
        needPar <- ScriptTXT[c(6,9,12,15,18,21,24,27)]
        needParVal <- gsub('([[:punct:]])([[:alpha:]]*)([[:blank:]]*)', "", needPar)
        parlist[[i]] <- needParVal
}
parlist <- do.call(rbind, parlist)
stylesFrame <- cbind(stylesFrame, parlist, stringsAsFactors = FALSE)
needParVal <- gsub('([[:punct:]])([[:alpha:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('([[:punct:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('[[:digit:]]*', "", needParNames)
names(stylesFrame) <- c("BeerStyle", "Type", "Link", needParNames) # Rename columns in dataset


####Try to scrap beer information table.
###Add a new column "JSLink" to be used for create beers table
stylesFrame <- mutate(stylesFrame, JSlink = paste0("http://www.ratebeer.com", "/ajax/top-beer-by-style.asp?", "style=", style,
                                    "&sort=", sort, "&order=", order, "&min=", min, "&max=", max,
                                    "&retired=", retired, "&new=", new, "&mine=", mine))
makeBeerDF <- function(LinkList){
        d <- lapply(LinkList, function(i){
                table <- readHTMLTable(i, as.data.frame = TRUE)[[1]]
                links <- read_html(i) %>% html_nodes("a") %>% html_attr("href")
                table[, 1] <- str_extract(i, "\\d+")
                links <- paste0("http://www.ratebeer.com", links)
                table <- cbind(table, links, stringsAsFactors = FALSE)
        } )
        rbindlist(d)
        #do.call(rbind, d)
        #as.data.frame(d)
        #names(d) <- c("style", "Name", "Count", "ABV", "Score", "BeerLink")
        #return(d)
}
beerTable <- makeBeerDF(stylesFrame$JSlink)
names(beerTable) <- c("style", "Name", "Count", "ABV", "Score", "BeerLink")


####Try to scrap General information and description about beers.
makeBeerGeneralInformationDF <- function(BeerLink) {
        d <- lapply(BeerLink, function(i){
                URL <- paste0("http://www.ratebeer.com", i)
                #Info <- read_html(URL)
                Info <- read_html(URL) %>%
                        html_nodes("#container table+ div:nth-child(2) , #_brand4 span , #_aggregateRating6 span , #_description3") %>%
                        html_text()
                Info <- append(Info[c(2,4,5)], URL)
        })
        do.call(rbind, d)
        as.data.frame(d)
        names(d) <- c("Overall", "Brewed", "Description", "BeerLink")
        return(d)
}
BeerGeneralInformation <- makeBeerGeneralInformationDF(beerTable$BeerLink)


##Try to scrap beer reviews
makeBeerReviewsDF <- function(BeerLink, conn) {
        d <- lapply(BeerLink, function(Link){
                PageNumber <- read_html(Link) %>% html_nodes(".ballno") %>% html_text() #Scrap max page namber for selected beer
                PageNumber <- tail(PageNumber, n=1)
                InfoV <- c()
                if (!is.null(PageNumber)) {
                  if (identical(PageNumber, character(0))) {
                    InfoV <- c("Proceed to the aliased beer...")
                    
                  } else {
                    if (!is.na(PageNumber)) {
                      if (as.numeric(PageNumber) >= 2) {
                        for (i in c(1:as.numeric(PageNumber))) {
                          URL <- paste0(Link, "1/", i, "/")
                          Info <-
                            read_html(URL) %>% html_nodes("#container div br+ div") %>% html_text() #Scrap all comment at page
                          #Info <- Info[1:(length(Info)-2)]
                          InfoV <- append(InfoV, Info)
                          #dbWriteTable(conn = conn, name = "BeerReviews", value = as.data.frame(c(Info, Link)), row.names = FALSE, append=TRUE)
                          #print(c(Link, PageNumber, i))
                        }
                      }
                    }
                  }
                  
                  
                  
                }
                #as.data.frame(c(Info, Link))
                  #dbWriteTable(conn = db, name = "BeerReviews", value = as.data.frame(df), row.names = FALSE, append=TRUE)
                  if (!is.null(InfoV)) {
                    df <- do.call(cbind, list(InfoV, Link))
                    dbWriteTable(
                      conn = db,
                      name = "BeerReviewsNew",
                      value = as.data.frame(df),
                      row.names = FALSE,
                      append = TRUE
                    )
                    print(Link)
                  }
                }
        )
        #do.call(rbind, d)
        #as.data.frame(d)
        #names(d) <- c("Overall", "Brewed", "Description", "BeerLink")
        #do.call(rbind, lapply(d, unlist))
        #do.call(rbind, lapply(d, as.data.frame))
        #return(d)
}

db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
BeerReviews <- dbReadTable(db, "BeerReviewsNew")
BeerReviewsDone <- unique(dbReadTable(db, "BeerReviewsNew")$V2)

toScrap <- BeerGeneralInformation$BeerLink[!(BeerGeneralInformation$BeerLink %in% BeerReviewsDone)]

BeerReviews <- makeBeerReviewsDF(toScrap, db)

BeerGeneralInformation$BeerLink[!(BeerGeneralInformation$BeerLink %in% BeerReviewsDone)]

dbDisconnect(db)




###Read dataframe fro SQLite database to enviroment
db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
stylesFrame <- dbReadTable(db, "Styles")
beerTable <- dbReadTable(db, "Beers")
BeerGeneralInformation <- dbReadTable(db, "GeneralInfo")
BeerReviewsDone <- unique(dbReadTable(db, "BeerReviewsNew")$V2)


dbDisconnect(db)




###Write dataframes to SQLite database
db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")
dbWriteTable(conn = db, name = "Styles", value = stylesFrame, row.names = FALSE, overwrite = TRUE)
dbWriteTable(conn = db, name = "Beers", value = beerTable, row.names = FALSE, overwrite = TRUE)
dbWriteTable(conn = db, name = "GeneralInfo", value = as.data.frame(BeerGeneralInformation), row.names = FALSE, overwrite = TRUE)


dbReadTable(db, "Styles")
str(dbReadTable(db, "Beers"))
BeerGeneralInformation <- dbReadTable(db, "GeneralInfo")

dbDisconnect(db)











Test <- makeBeerReviewsDF("http://www.ratebeer.com/beer/langelus-biere-de-froment/6756/")
t <- do.call(rbind, lapply(BeerReviews, as.data.frame))
#"http://www.ratebeer.com/beer/amager-forarsbryg/99916/"
PageNumber <-read_html("http://www.ratebeer.com/beer/bourgogne-des-flandres-biere-brune/107135/") %>% html_nodes(".ballno") %>% html_text() #Scrap max page namber for selected beer
PageNumber <-read_html("http://www.ratebeer.com/beer/amager-forarsbryg/99916/") %>% html_nodes(".ballno") %>% html_text() #Scrap max page namber for selected beer
InfoV <- c()
if ((!is.na(PageNumber))&(PageNumber>2)) {
        for (i in c(2:PageNumber)) {
                URL <- paste0("http://www.ratebeer.com/beer/la-choulette-de-noel/23326/", "1/", i,"/")
                Info <- read_html(URL) %>% html_nodes("#container div br+ div") %>% html_text() #Scrap all comment at page
                #Info <- Info[1:(length(Info)-2)]
                InfoV <- append(InfoV, Info)
                print(c("http://www.ratebeer.com/beer/la-choulette-de-noel/23326/", PageNumber, i))
        }        
}
#dbRemoveTable(db, "BeerReviewsNew")





"http://www.ratebeer.com/beer/amager-forarsbryg/99916/"


#test scrap comments!!!!!
# BeerGeneralInformation$BeerLink[1]
#         URL <- BeerGeneralInformation$BeerLink[1]
#         PageNumber <-read_html(URL) %>% html_nodes("b.ballno+ .ballno") %>% html_text() #Scrap max page namber for selected beer
#         for (i in c(2:3)) {
#                 URLf <- paste0(URL, "1/", i,"/")
#                 Info <- read_html(URLf) %>% html_nodes("#container div br+ div") %>% html_text() #Scrap all comment at page
#                 Info <- Info[1:(length(Info)-2)]
#                 Info <- append(Info, Info)
#         }        
# t <- cbind(Info, URL)
#         
#         
#         
#         Infot <- read_html(URL) %>%
#                 html_nodes("#container div br+ div") %>%
#                 html_text()     




# d <- lapply(BeerGeneralInformation$BeerLink[1:2], function(Link){
#         PageNumber <-read_html(Link) %>% html_nodes("b.ballno+ .ballno") %>% html_text() #Scrap max page namber for selected beer
#         InfoV <- c()
#         for (i in c(2:PageNumber)) {
#                 URL <- paste0(Link, "1/", i,"/")
#                 Info <- read_html(URL) %>% html_nodes("#container div br+ div") %>% html_text() #Scrap all comment at page
#                 Info <- Info[1:(length(Info)-2)]
#                 InfoV <- append(InfoV, Info)
#                 #do.call(cbind, list(Info))
#         }        
#         #Info <- append(Info[c(2,4,5)], URL)
#         do.call(cbind, list(InfoV, Link))
# })
# eee <- do.call(rbind, lapply(d, unlist))
# 
# 
# eee <- bindlist(d)
# eee <- as.data.frame(d)
# do.call(rbind, d)
# 






####Try to scrap user comments and rating scores.
# ResultURL <- paste0("http://www.ratebeer.com", beerTable[1, ]$BeerLink)
# beerGeneralInfo <- read_html(ResultURL) %>% html_nodes("#container div table:nth-child(8)") %>% html_text()
# beerGeneralInfo[, 1] <- stylesFrame[1, ]$style
# beerGeneralInfo <- cbind(beerGeneralInfo[, 1:5], beerLinks)
# 
# html_attr(beerGeneralInfo, "id")
# test <- html_text(beerGeneralInfo)
# 


#BeerGeneralInformation <- as.data.frame(BeerGeneralInformation)
#names(BeerGeneralInformation) <- c("Overall", "Brewed", "Description", "BeerLink")


## #container div table:nth-child(8)


# ResultURL <- paste0("http://www.ratebeer.com", "/ajax/top-beer-by-style.asp?", "style=", stylesFrame[1, ]$style,
#                     "&sort=", stylesFrame[1, ]$sort, "&order=", stylesFrame[1, ]$order, "&min=", stylesFrame[1, ]$min, "&max=", stylesFrame[1, ]$max,
#                     "&retired=", stylesFrame[1, ]$retired, "&new=", stylesFrame[1, ]$new, "&mine=", stylesFrame[1, ]$mine)
# 
# 
# 
# styleLink <- as.vector(stylesFrame$Link[[1]])
# StylePage <- paste0("http://www.ratebeer.com", styleLink)
# 
# #GetBeersTable
# ScriptTXT <- read_html(StylePage) %>% html_nodes("script")
# ScriptTXT <- html_text(ScriptTXT[9]) %>% strsplit("[\r\n\t]") %>% unlist
# needPar <- ScriptTXT[c(6,9,12,15,18,21,24,27)]
# needParVal <- gsub('([[:punct:]])([[:alpha:]]*)([[:blank:]]*)', "", needPar)
# needParNames <- gsub('([[:punct:]]*)([[:blank:]]*)', "", needPar)
# needParNames <- gsub('[[:digit:]]*', "", needParNames)
# 
# ParDataFrame <- as.data.frame(rbind(needParVal))
# names(ParDataFrame) <- needParNames
# 
# 
# 
# 
# #needPar <- cbind(needParNames, needParVal)
# 
# 
# 
# 
# library(XML)
# beerTable <- readHTMLTable(ResultURL, as.data.frame = TRUE)[[1]]
# beerLinks <- read_html(ResultURL) %>% html_nodes("a") %>% html_attr("href")
# beerTable[, 1] <- stylesFrame[1, ]$style
# beerTable <- cbind(beerTable[, 1:5], beerLinks)





##Test PhantomJS
# write out a script phantomjs can process
# library(RSelenium)
# pJS <- phantom()
# remDr <- remoteDriver(browserName = "chrome")
# remDr$open()
# remDr$navigate(StylePage)# process it with phantomjs
# result <- remDr$phantomExecute("var page = http://www.ratebeer.com/beerstyles/abbey-dubbel/71/;
#                                var fs = require('fs');
#                                page.onLoadFinished = function(status) {
#                                var file = fs.open('output.htm', \"w\");
#                                file.write(page.content);
#                                file.close();
#                                phantom.exit();
#                                };")
# 




# str(beer_styles)
# 
# html_children(beer_stylesHTML[[1]])
# 
#beer_stylesMatrix <- data.frame(sapply(beer_styles, '[', seq(max(sapply(beer_styles, length)))))
###!!!Test scrapping
# beer_stylespage <- read_html("http://www.ratebeer.com/beerstyles")
# beer_stylesUP <- html_nodes(beer_stylespage, ".groupname")
# beer_stylesCustomL <- html_nodes(beer_stylespage, ".styleGroup")
# beer_stylesCustom <- html_nodes(beer_stylesCustomL, "a")
# 
# beer_stylesTable <- html_nodes(beer_stylespage, ".col-lg-12 table")
# 
# table
# v <- html_text(beer_stylesCustom)
# 
# html_text(beer_stylesCustom, trim = TRUE)
# html_attrs(beer_stylesCustom)[[1]]
# 
# 
# Ta <- html_table(beer_stylesTable, fill = TRUE)
# html_table(beer_stylesCustom)
# str