#'Quality control for fieldbook traits 
#'
#'@param data The data frame of the fieldbook
#'@param genotipes The genotipes column name
#'@param trait The trait column name
#'@param lfactor The factor column name
#'@param datadict The data frame of the data dictionary
#'@return Return the values which are out of range using a data diccionary.
#'@details This function return the values which are out of range. It use a data dicctionary to define the lower limit
#'and the upper limit for a quality control.
#'@author Omar Benites
#'@export
sb_qualityvar <- function(data,genotipes,trait,lfactor=NULL,datadict){
  
  datos<-data
  xvar <- trait
  lfactor <- lfactor
  genotipes <- genotipes 
  datos[,genotipes] <- as.factor(datos[,genotipes])
  
  if(!is.null(lfactor)){
    datos[,lfactor] <-as.factor(datos[,lfactor]) 
  }
  
  if(is.null(lfactor)){
    select_fb <- dplyr::select_(datos,genotipes,xvar)
  }
  
  if(!is.null(lfactor)){
    select_fb <- dplyr::select_(datos,genotipes,xvar,lfactor) 
  }
  
  abbr <- datadict$ABBR
  if(!(xvar %in% abbr)){
    cat("We can not find the trait in the data dictionary")
  }
  
  lowerl <- as.numeric(datadict[datadict$ABBR==xvar,c("LOWER")])
  upperl <- as.numeric(datadict[datadict$ABBR==xvar,c("UPPER")])
  #   
  #   lowerl <- 30
  #   upperl <- 35
  
  out_rule <- datos[,xvar]>lowerl & datos[,xvar]<upperl #the good rule, so we denied with !
  if(!any(out_rule)){ #If any value of 'out_rule' is true (ie. has outliers)
    
    out_pos <- rownames(datos[!out_rule,])
    out_values <- datos[out_pos,c(genotipes,xvar)]
    #out_genotipes <- datos[out_pos,"INSTN"]
    
    out_print <- paste("Please validate the column '", xvar, " 'the row '",out_pos, " 'the '", genotipes, " is " ,out_values[,genotipes],
                       "' the value' ", round(out_values[,xvar],3), " .The range is from ",lowerl, " to ", upperl ,sep="")
    
    # lowerl <- 30
    # upperl <- 35
    
    #  oor_low <- dplyr::filter(select_fb, datos[,xvar] < lowerl) #values out of range - lower
    #  oor_up  <- dplyr::filter(select_fb, datos[,xvar] > upperl) #values out of range - upper  
    
    return(out_print) 
  }
  
  #out_print <- "There is no values out of range"
  #return(out_print)
}


#############################################################################################################
# fp <- "D:\\sbformula\\data\\Potato_Yield_traitTemplate_ver_1_EN.xlsx"
# fp2 <- "D:\\sbformula\\data\\PTYL200211_CHIARA.xlsx"

#datos <- xlsx::read.xlsx(fp2,"Fieldbook")
#datadict <- openxlsx::read.xlsx(fp,2,startRow=7,skipEmptyRows=TRUE,stringsAsFactors=F)

# remove_na_cols <-function(datos){
#   datos<-datos[ ! apply( datos ,1 , function(x) all(is.na(x)) ), ]
#   return(datos)
# }  
# remove_na_rows <- function(datos){
#   datos<-datos[ , ! apply( datos ,2 , function(x) all(is.na(x)) ) ] 
#   return(datos)
# }
# 
# datadict <- xlsx::read.xlsx(fp,2,startRow = 7,stringsAsFactors=F)
# datadict <- remove_na_cols(datadict)
# datadict <- remove_na_rows(datadict)
# 
# datos  <- xlsx::read.xlsx(file = fp2,sheetName = "Fieldbook", stringsAsFactors=F)
# datos <- remove_na_cols(datos)
# datos <- remove_na_rows(datos)

#look for a function to read just columns with information!

# trait <- "MTYA"
# #xplot <- "PLOT"
# lfactor <- NULL
# genotipes <- "INSTN"



