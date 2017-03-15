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


# Drougth Indexes ---------------------------------------------------------

#' Resumen of variables to calculate the mean 
#' @description Summarization of variables according to institutional number and factors (treatments)
#' if just few or neither elements are contained into the set.
#' @param datos the fieldbook data
#' @param idx the genotypes
#' @param groupvars Extra treatments
#' @param na.rm Logical. TRUE to remove NA values. FALSE is by default
#' @author Omar Benites
#' @export

sumbymean <- function(datos=NULL,idx, groupvars=NULL, na.rm=FALSE) {
  #require(doBy)
  
  # New version of length which can handle NA's: if na.rm==T, don't count them
  vvv=datos[,idx]
  lbl=names(datos[idx])
  measurevar =lbl
  
  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupvars, collapse=" + "), sep=" ~ "))
  datac <- summaryBy(formula, data=datos, FUN=mean, na.rm=na.rm)
  
  # Rename columns
  #names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- paste(measurevar,"_Mean",sep="")  
  
  return(datac)
}


#' Yield index
#' @description Summarization of variables according to institutional number and factors (treatments)
#' if just few or neither elements are contained into the set.
#' @param treatYield the yield column
#' @author Omar Benites
#' @export

yield.index <- function (treatYield) {
  
  instn=as.character(treatYield$INSTN)
  Yc=rep(NA,length(instn))
  for(i in 1:length(instn))
  {
    Yc[i]=treatYield[i,3]  
  }
  #Mc=mean(condicion[,3],na.rm = TRUE)
  Yc
  #Mc
}

#' Yield index based on the means
#' @description Summarization of variables based on the means by treatment
#' if just few or neither elements are contained into the set.
#' @param treatYield the yield column
#' @author Omar Benites
#' @export
#' 

yield.mean.treat<- function(treatYield){
  my=mean(treatYield[,3],na.rm = TRUE) #mean yield
  my
}

#' Drought Indexes based on means and summary sheet, both previously calculated by HIDAP
#' @description Calcutation of drought indexes using two levels in treatments
#' if just few or neither elements are contained into the set.
#' @param fb the fieldbook data
#' @param f_lvl1 factor's first level 
#' @param f_lvl2 factor's second level
#' @author Omar Benites
#' @export

drought_index <- function(fb, f_lvl1, f_lvl2)
{
  fb <- as.data.frame(fb)
  instn <- data.frame(INSTN = as.factor(sort(unique(fb$INSTN))))
  
  is_ttwp <- is.element("TTWP",set =  names(fb))
  
  if(is_ttwp){
  
  genomean <- sumbymean(fb,"TTWP",groupvars=c("INSTN","FACTOR"),na.rm=TRUE)
  
  #wy <- subset(genomean,FACTOR=="Irrigated"| FACTOR=="irrigation"| FACTOR=="wellwater"|FACTOR=="irrigacion"|FACTOR=="NI")
  wy <- subset(genomean, FACTOR == f_lvl1)
  #dy <- subset(genomean,FACTOR=="drought"| FACTOR=="Drought"| FACTOR=="drydown"|FACTOR=="sequia"|FACTOR=="DR")
  dy <- subset(genomean, FACTOR == f_lvl2)
  
  Yc <- yield.index(wy)
  Ys <- yield.index(dy)
  Mc <- yield.mean.treat(wy)
  Ms <- yield.mean.treat(dy)
  
  DSI <-  data.frame("DSI"=(1-(Ys/Yc))/(1-(Ms/Mc))) #Drought Susceptibility Index
  DTI <-  data.frame("DTI"=(Ys*Yc)/((Mc)^2))  #Drought Tolerance Index
  TOL <- data.frame("TOL"=(Yc-Ys)) #Tolerance
  MP <- data.frame("MP"=(Yc+Ys)/2) #mean productivity 
  GM <- data.frame("GM"=sqrt(Yc*Ys))  #geometric mean
  TDWS <- data.frame("TDWS"=(Ys/Yc)*100) #Tolerant to decrease to water supply
  
  instn <- data.frame(INSTN=as.factor(sort(unique(fb$INSTN))))
  dParameters <- data.frame(INSTN=instn, DSI=DSI, DTI=DTI, TOL=TOL ,MP=MP, GM=GM, TDWS=TDWS)
  #        
  dParameters[,-1] <- round(dParameters[,-1],2)
  indexTable <- dParameters
  
  indexTable
  }
  
  else{
    indexTable <- NULL
  }
  
  indexTable
}
