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