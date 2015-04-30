library(shiny)





shinyServer(
  
  function(input, output) {
  
    datos<-reactive({
      file1 <- input$fb
      if(is.null(file1)){return()} 
      datos<-xlsx::read.xlsx(file1$datapath,"Fieldbook",stringsAsFactors=FALSE)
      datos
    })
    
    
    output$sum <- renderTable({
      colm <- as.character(input$var)
      datos()[colm]
      
    })
    
    output$str <- renderPrint({
      str(datos())
      
    })
    
    output$data <- renderTable({
#       colm <- as.numeric(input$var)
#       iris[colm]
#       # head(iris)
      datos()
      
    })
    
    
    output$sbsum <- renderTable({
            colm <- as.character(input$var)
            A<-as.data.frame(datos())
            a<-sbformula::sbsummary(data=A,idx=colm,groupfactors=NULL,na.rm=FALSE)
            a
            # head(iris)
#       datos()
#       
    })
    
    
    
    output$myhist <- renderPlot({
      colm <- as.numeric(input$var)
      hist(datos[,colm], breaks=seq(0, max(datos[,colm]), l=input$bins+1), col=input$color, main="Histogram of iris dataset", xlab=names(datos[colm]), xlim=c(0,max(datos[,colm])))
      
    })
    
  }
)
