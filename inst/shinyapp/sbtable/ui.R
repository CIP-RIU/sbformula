library(shiny)
shinyUI(fluidPage(
  #header of title panel
  titlePanel(h4("Uso de los TabSets en shiny",align="center")),
  
  sidebarLayout(
    sidebarPanel("SidebarPanel para tabsets",
                 br(),
                 
                 shiny::fileInput(inputId = "fb",label = "Upload your fieldbook"),
                 selectInput(inputId = "var",label = "Select the variable from iris dataset",
                             choices = c("PPH",  "NMTP",	"MTWP",	"MTWPL"),selected="TNTP")
                 
                 #                                   
                 #                  sliderInput(inputId = "bins",label = "2. Select the BINs for histogram",min = 5,max=25,value = 5),
                 #                  br(),
                 #                  
                 #                  radioButtons(inputId = "color",label="3. Select the color",choices = c("Green","Red","Blue"),selected = "Green")
                 #                                                 
    ),
    mainPanel(
      tabsetPanel(type = "tab",
                  tabPanel(title = "Data",tableOutput("data")),
                  tabPanel(title = "Summary",verbatimTextOutput("summary")),
                  tabPanel(title = "Structure",verbatimTextOutput("str")),
                  tabPanel(title = "Variable Selected", tableOutput("varcol")),
                  tabPanel(title = "Summary by variable", tableOutput("sbsum"))
                  #tabPanel(title = "Plot",plotOutput("myhist"))
                  
      )
      
      #plotOutput("myhist")
      
    )
  )
)
)
