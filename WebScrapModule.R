library(rvest)
library(reshape2)

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
        names(beer_styles) <-
                c("Style", "Type", "Link") # Rename columns in dataset
        return(beer_styles)
}

##Get beers list from style link
stylesFrame <- GetBeerStylesDataframe("http://www.ratebeer.com/beerstyles")
styleLink <- as.vector(stylesFrame$Link[[4]])
StylePage <- paste0("http://www.ratebeer.com", styleLink)

#GetBeersTable
ScriptTXT <- read_html(StylePage) %>% html_nodes("script")
ScriptTXT <- html_text(ScriptTXT[9]) %>% strsplit("[\r\n\t]") %>% unlist
needPar <- ScriptTXT[c(6,9,12,15,18,21,24,27)]
needParVal <- gsub('([[:punct:]])([[:alpha:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('([[:punct:]]*)([[:blank:]]*)', "", needPar)
needParNames <- gsub('[[:digit:]]*', "", needParNames)
needPar <- cbind(needParNames, needParVal)







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