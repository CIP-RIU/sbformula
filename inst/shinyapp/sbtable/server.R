library(shiny)
shinyServer(
  function(input,output){
    
    datos <- reactive({
      file1 <- input$fb
      if(is.null(file1)){return()}
      datos <- xlsx::read.xlsx(file = file1$datapath,"Fieldbook",stringsAsFactors=FALSE)
      
    })
    
    ddict <- reactive({
      file2 <- input$dict
      if(is.null(file2)){return()}
      dict <- xlsx::read.xlsx(file=file2$datapath,sheetIndex = 1,startRow = 6,stringsAsFactors=FALSE)
      
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
      p <- "FACTOR" %in% names(datos())
      print(names(datos()))
      print(p)
      print(head(ddict()))
#         str(datos())
      
    })
    
    output$varcol <- renderTable({
      col <- as.character(input$var) 
      datos()[col]
    })
    
    output$sbsum <- renderTable({
      
      col <- as.character(input$var)
#       if("FACTOR" %in% names(data())){
      p <- "FACTOR" %in% names(datos())
      
      if(p){
        sbformula::sbsummary(datos(),col,groupfactors = c("INSTN","FACTOR"),na.rm = TRUE)
      #}
       }else{
        sbformula::sbsummary(datos(),col,groupfactors = "INSTN",na.rm = TRUE)
       }
      
    })
 
   output$oofr <- renderTable({
    
     col <- as.character(input$var)
     a <- sbformula::sb_qualityvar(data = datos(),genotipes = "INSTN",trait = col,datadict = ddict())
     out_of_range <- as.data.frame(a)
   })
 
 
# 
#     output$plot <- renderPlot({
#       boxplot(datos()[col] ~ FACTOR,data=datos(),main=paste("Boxplot of"),sub=NULL,ylab=NULL)
#       #plot(cars)
#     })
    
     
     

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