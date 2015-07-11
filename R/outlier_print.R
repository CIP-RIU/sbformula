#' Function to to highlight outliers with colours in Fieldbook files (MSExcel)
#'
#'@param file filename of the fieldbook
#'@param trait Name of the trait column
#'@param sheetname Sheet name of the fieldbook
#'@param ll lower limit
#'@param ul upper limit
#'@export
#'
outlier_colors<- function(file="mydata.xlsx",trait,sheetname="mysheet",ll,ul){
  
  file <- as.character(file)  
  ll <- as.numeric(ll)
  ul <- as.numeric(ul)
  trait <- as.character(trait)
  sheetname <- as.character(sheetname)
  filename <- as.character(file)
  data <- xlsx::read.xlsx(file=file,sheetName = sheetname,stringsAsFactors=FALSE)
  d <- data  
  pos <- which(names(d)==trait)
  cols<-length(d[1,])
  
  wb <- xlsx::loadWorkbook(file)              # load workbook
  fo1 <- xlsx::Fill(foregroundColor="blue")   # create fill object # 1
  #fo1 <- Fill(foregroundColor="lightblue", backgroundColor="lightblue",pattern="SOLID_FOREGROUND")
  cs1 <- xlsx::CellStyle(wb, fill=fo1)        # create cell style # 1
  fo2 <- xlsx::Fill(foregroundColor="red")    # create fill object # 2
  #fo2 <- Fill(foregroundColor="tomato", backgroundColor="tomato",pattern="SOLID_FOREGROUND")
  cs2 <- xlsx::CellStyle(wb, fill=fo2)        # create cell style # 2 
  sheets <- xlsx::getSheets(wb)               # get all sheets
  sheet <- sheets[[sheetname]]          # get specific sheet
  rows <- xlsx::getRows(sheet, rowIndex=2:(nrow(d)+1))     # get rows
  # 1st row is headers
  #cells <- getCells(rows, colIndex = 4:cols)         # get cells
  cells <- xlsx::getCells(rows, colIndex = pos)         # get cells
  values <- lapply(cells, xlsx::getCellValue) # extract the cell values
  
  highlightblue <- NULL
  for (i in names(values)) {
    x <- as.numeric(values[i])
    
    if(!is.na(x)){
      if (x>=ll & x<=ul) {
        highlightblue <- c(highlightblue, i)
      }
    }
    
  }
  
  # find cells meeting conditional criteria < 5
  highlightred <- NULL
  for (i in names(values)) {
    x <- as.numeric(values[i])
    
    if(!is.na(x)){
      if (x<ll || x>ul){
        highlightred <- c(highlightred, i)
      }    
    }
  }
  #Finally, apply the formatting and save the workbook.
  
  lapply(names(cells[highlightblue]),
         function(ii)xlsx::setCellStyle(cells[[ii]],cs1))
  
  lapply(names(cells[highlightred]),
         function(ii)xlsx::setCellStyle(cells[[ii]],cs2))
  
  xlsx::saveWorkbook(wb, file)
  #shell.exec(file)
}

# library(xlsx)
# fp <- "C:\\Users\\USER\\Desktop\\PTYL200211_CHIARA.xls"
# file=fp
# sheetname <- "Fieldbook"
# trait <- "NPH"
# ll <- 4
# ul <- 7
# sbformula::outlier_colors(file=fp,trait="NPH",sheetname="Fieldbook",ll=4,ul=7)