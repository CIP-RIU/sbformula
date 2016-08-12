has.data <- function(acol){
  if(length(acol)<1) return(FALSE)
  res = length(acol)!=length(which(is.na(acol)))
  res
}

remove_na_cols <-function(datos){
  datos<-datos[ ! apply( datos ,1 , function(x) all(is.na(x)) ), ]
  return(datos)
}  

remove_na_rows <- function(datos){
  datos<-datos[ , ! apply( datos ,2 , function(x) all(is.na(x)) ) ] 
  return(datos)
}

#'This function allows sum columns and avoid NA values at the same time
#'
#'@param x vector of measurements
#'@return A vector contains sum of values that avoid NA's
#'@keywords sum, NA's
#'@export
#'
sbsum <- function(x){
  z <- x[!is.na(x)]; 
  ifelse(length(z), sum(z), NA)
} 

themode <-function(x){
  tv=table(x)
  paste(names(tv[tv==max(tv)]),collapse=", ")
}

#' Logical function to detect if a group of elements are part of the set of header
#' @description Check if a header(s) correspond to the fieldbook names. It returns a TRUE if all values are contained in the set and FALSE
#' if just few or neither elements are contained into the set.
#' 
is_contained <- function(..., set){
  
  logic_value <- all(is.element(c(...), set))
  logic_value

}

#all(is.element(c("MTWP", "NMTP"),names(fb)))



# get.data.dict = function(terms="all",sheetName="any"){
#   #fp = file.path(getwd(),"res","data_dictionary_yield.xls")
#   dic=getResourceData("dictionary","Variables")
#   nco=26
#   if(length(terms)==1){
#     if(terms=="all"){
#       return(dic[,1:nco])
#     }else{
#       return(dic[dic$ABBR %in% terms,1:nco])
#     }
#     
#   }else {
#     return(dic[dic$ABBR %in% terms,1:nco])	
#   }
#   
# } 

# getResourceData <- function(type, sheetName){
#   fp = getResourcePath(type)
#   getResourceSheet(fp, sheetName)
# }

#'This function print cipnumber which are not well-written
#'
#'@param data The fiedlbook data frame
#'@param cip_name The name of the column which contains the cipnumbers 
#'@return Return all wrong cip numbers
#'@keywords cipnumber,quality
#'@export
#'

sb_cipnumberquality <-function(data,cip_name){

  cipnumber_checks <- sbformula::cip_number_check(data[,cip_name])
  if(length(cipnumber_checks$cipnumber_wrong)>0){
   cipnumber_checks <- paste("This cipnumber does not exist: '", cipnumber_checks$cipnumber_wrong,"'",sep = "")  
   #cipnumber_checks <- as.data.frame(cipnumber_checks$cipnumber_wrong)
   cipnumber_checks <- as.data.frame(cipnumber_checks)
   names(cipnumber_checks) <- "Checks"
   cipnumber_checks
  }
}
