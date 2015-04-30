library(shiny)
# Define UI for application
shinyUI(fluidPage(
  # Header or Title Panel 
  titlePanel(title = h4("Data Arena", align="center")),
  sidebarLayout(
    # Sidebar panel
    sidebarPanel(
      shiny::fileInput("fb",label="Upload your fieldbook"),
      br(),
      selectInput("var", "1. Select the variable from the iris dataset", choices =c("PPH", "NMTP", "MTWP"), selected = 1),
      br(),
      sliderInput("bins", "2. Select the number of BINs for histogram", min=5, max = 25, value=15),
      br(), 
      radioButtons("color", "3. Select the colour of histogram", choices=c("Green", "Red", "Yellow"), selected ="Green")
      
    ),
    
    # Main Panel
    mainPanel(
      tabsetPanel(type="tab", 
                  tabPanel("Data", tableOutput("data")),
                  tabPanel("Summary",tableOutput("sum")),
                  tabPanel("Structure", verbatimTextOutput("str")),
                  tabPanel("Plot", plotOutput("myhist")),
                  tabPanel("Summary",tableOutput("sbsum"))
      )
      
    )
    
  )
)  
)