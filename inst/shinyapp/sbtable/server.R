library(shiny)
shinyServer(
  function(input,output){
    
    datos <- reactive({
      file1 <- input$fb
      if(is.null(file1)){return()}
      datos <- xlsx::read.xlsx(file = file1$datapath,"Fieldbook",stringsAsFactors=FALSE)
      
    })
    
    output$data <- renderTable({
      #colm <- as.numeric(input$var)
      #datos()[colm]
      #head(iris[colm])
      datos()
    })
      
    
    output$summary <- renderPrint({
      summary(datos())
      
    })
    
    output$str <- renderPrint({
      colm <- as.character(input$var) 
      print(colm)
      str(datos()[colm])
#         str(datos())
      
    })
    
    output$varcol <- renderTable({
      col <- as.character(input$var) 
      datos()[col]
    })
    
    output$sbsum <- renderTable({
      col <- as.character(input$var) 
      sbformula::sbsummary(datos(),col,groupfactors = "INSTN",na.rm = TRUE)
    })



#     output$myhist <- renderPlot({
#         
#       colm <- as.numeric(input$var)
#       expr = hist(iris[,colm],breaks = seq(0,max(iris[,colm]),l=input$bins+1),
#                     xlim = c(0, max(iris[,colm])), main = "RenderPlot",xlab = names(iris[colm]),
#                     col=input$color)
#     })
#   
  }
  )