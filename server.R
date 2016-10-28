

function(input, output, session) {
    # Define a reactive expression for the document term matrix
    # Drop-down selection box for which data set
    output$choose_Type <- renderUI({
        selectInput("Type", "Type", as.list(Types))
    })
    
    output$choose_Style <- renderUI({
        if(is.null(input$Type))
            return()
        Styles <- GetStyles(input$Type)
        selectInput("Style", "Styles", as.list(Styles))
    })
    
    terms <- reactive({
        if(is.null(input$Style))
            return()
        # Change when the "update" button is pressed...
        input$update
        # ...but not for anything else
        isolate({
            withProgress({
                setProgress(message = "Processing corpus...")
                getTermMatrix(input$Style)
            })
        })
    })
    
    # Make the wordcloud drawing predictable during a session
    wordcloud_rep <- repeatable(wordcloud)
    
    output$plotReviews <- renderPlot({
        if(is.null(input$Style))
            return()
        v <- terms()
        wordcloud_rep(v$word, v$freq, scale=c(4,0.5),
                      min.freq = input$freq, max.words=input$max, random.order=FALSE,
                      colors=brewer.pal(8, "Dark2"), vfont=c("gothic english", "plain"), rot.per=.45)
    })
}#colors=brewer.pal(8, "Dark2")