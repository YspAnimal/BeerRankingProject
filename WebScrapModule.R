require(rvest)
require(reshape2)

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
ParDataFrame <- data.frame(style=character(),
                           sort=character(),
                           order=character(),
                           min=character(),
                           max=character(),
                           retired=character(),
                           new=character(),
                           mine=character(),
                           stringsAsFactors=FALSE)

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
stylesFrame <- cbind(stylesFrame, parlist)
needParVal <- gsub('([[:punct:]])([[:alpha:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('([[:punct:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('[[:digit:]]*', "", needParNames)
names(stylesFrame) <- c("BeerStyle", "Type", "Link", needParNames) # Rename columns in dataset


####Try to scrap beer information table.
ResultURL <- paste0("http://www.ratebeer.com", "/ajax/top-beer-by-style.asp?", "style=", stylesFrame[1, ]$style,
                    "&sort=", stylesFrame[1, ]$sort, "&order=", stylesFrame[1, ]$order, "&min=", stylesFrame[1, ]$min, "&max=", stylesFrame[1, ]$max,
                    "&retired=", stylesFrame[1, ]$retired, "&new=", stylesFrame[1, ]$new, "&mine=", stylesFrame[1, ]$mine)

library(XML)
beerTable <- readHTMLTable(ResultURL, as.data.frame = TRUE)[[1]]
beerLinks <- read_html(ResultURL) %>% html_nodes("a") %>% html_attr("href")
beerTable[, 1] <- stylesFrame[1, ]$style
beerTable <- cbind(beerTable[, 1:5], beerLinks)
names(beerTable) <- c("style", "Name", "Count", "ABV", "Score", "BeerLink")

###Write dataframes to SQLite database

library("sqldf")
db <- dbConnect(SQLite(), dbname="BeerDB.sqlite")

dbWriteTable(conn = db, name = "Styles", value = stylesFrame, row.names = FALSE)
dbWriteTable(conn = db, name = "Beers", value = beerTable, row.names = FALSE)
#dbWriteTable(conn = db, name = “Votes”, value = School, row.names = FALSE)
dbDisconnect(db)






#/ajax/top-beer-by-style.asp?style=2&sort=-1&order=0&min=10&max=9999&retired=0&new=0&mine=0&
#    function loadList(vl) {
#          var strURL = '/ajax/top-beer-by-style.asp?';
#             for (var src in vl) {
#                         strURL += src + '=' + vl[src] + '&';
#                 }
#                 console.log(strURL);
#                 $('#styleList').load(strURL);
#         }















styleLink <- as.vector(stylesFrame$Link[[1]])
StylePage <- paste0("http://www.ratebeer.com", styleLink)

#GetBeersTable
ScriptTXT <- read_html(StylePage) %>% html_nodes("script")
ScriptTXT <- html_text(ScriptTXT[9]) %>% strsplit("[\r\n\t]") %>% unlist
needPar <- ScriptTXT[c(6,9,12,15,18,21,24,27)]
needParVal <- gsub('([[:punct:]])([[:alpha:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('([[:punct:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('[[:digit:]]*', "", needParNames)

ParDataFrame <- as.data.frame(rbind(needParVal))
names(ParDataFrame) <- needParNames




#needPar <- cbind(needParNames, needParVal)









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