# library(shiny)
# shinyUI(fluidPage(
#   #header of title panel
#   titlePanel(h4("HIDAP-Data Quality",align="center")),
#   
#   sidebarLayout(
#     sidebarPanel("Data Quality on Potato Traits",
#                  br(),
#                  
#                  shiny::fileInput(inputId = "fb",label = "Upload your fieldbook"),
#                  shiny::fileInput(inputId = "dict",label = "Upload your data dictionary"),
#                  selectInput(inputId = "var",label = "Select the variable from iris dataset",
#                              choices = c( "NTP","NPH","PPH","TNTP","TNTPL","TTWP","TTWPL","TTYNA","TTYA","ATW","FWTS1","DWTS1","DM1","AVDM"),selected="TNTP")
#                                  
#                  #                                   
#                  #                  sliderInput(inputId = "bins",label = "2. Select the BINs for histogram",min = 5,max=25,value = 5),
#                  #                  br(),
#                  #                  
#                  #                  radioButtons(inputId = "color",label="3. Select the color",choices = c("Green","Red","Blue"),selected = "Green")
#                  #                                                 
#     ),
#     mainPanel(
#       tabsetPanel(type = "tab",
#                   tabPanel(title = "Data",tableOutput("data")),
#                   tabPanel(title = "Summary",verbatimTextOutput("summary")),
#                   tabPanel(title = "Structure",verbatimTextOutput("str")),
#                   tabPanel(title = "Variable Selected", tableOutput("varcol")),
#                   tabPanel(title = "Summary by variable", tableOutput("sbsum")),
#                   tabPanel(title = "Out of Range Values",  tableOutput("oofr"))
#                   #tabPanel(title = "Plot",plotOutput("myhist"))
#                   
#       )
#       
#       #plotOutput("myhist")
#       
#     )
#   )
# )
# )
