fluidPage(
    # Application title
    titlePanel("Beer reviews Cloud"),
    
    sidebarLayout(
        # Sidebar with a slider and selection inputs
        sidebarPanel(
            uiOutput("choose_Type"),
            
            uiOutput("choose_Style"),
            
            sliderInput("freq",
                         "Minimum Frequency:",
                         min = 10,  max = 200, value = 100),
            sliderInput("max",
                         "Maximum Number of Words:",
                         min = 30,  max = 500,  value = 100),
            hr(),
            actionButton("update", "Change")
            ),
        
        # Show Word Cloud
        mainPanel(
            #plotOutput("plotDescriptions"),
            tabsetPanel(type = "tabs", 
                tabPanel("As product is"),
                tabPanel("As people think", plotOutput("plotReviews"))
            )
        )
    )
)