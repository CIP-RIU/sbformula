#'Function to calculate summary statistics for breeding data
#'
#'@param data A data frame
#'@param idx The name or position of the measured variable that will be summarized.
#'@param groupfactors A vector containing names of columns that contain grouping variables
#'@param na.rm A boolean that indicates whether to ignore NA's
#'@return A data frame with the count, mean and standard desviation
#'@author Omar Benites
#'@details This function returns the count, mean and standard desviation.
#'Furthermore, this function use the doBy package.
#'@references Some code for this function were inspired by Cookbook for R. 
#'@keywords stats, summary
#'@family stats,summary
#'@export 
#' 
sbsummary<- function(data=NULL,idx, groupfactors=NULL, na.rm=FALSE) {
  #require(doBy)
  # New version of length which can handle NA's: if na.rm==T, don't count them
  if(missing(data)){
    stop("Please enter your data")
  }
  if(missing(idx)){
    stop("Please enter the name or position of the measured variable")
  }
  if(missing(groupfactors)){
    stop("Please enter the name of columns that contain grouping variables")
  }
  length2 <- function (x, na.rm=FALSE) {
    if (na.rm) sum(!is.na(x))
    else length(x)
  }
    
  datos <- data
  vvv <- datos[,idx]
  lbl <- names(datos[idx])
  measurevar <- lbl

  # Collapse the data
  formula <- as.formula(paste(measurevar, paste(groupfactors, collapse=" + "), sep=" ~ "))
  datac <- doBy::summaryBy(formula, data=data, FUN=c(length2,mean,sd), na.rm=na.rm)
  
  # Rename columns
  #names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- measurevar
  names(datac)[ names(datac) == paste(measurevar, ".length2", sep="") ] <- paste(measurevar,"_n",sep="")
  names(datac)[ names(datac) == paste(measurevar, ".mean",    sep="") ] <- paste(measurevar,"_Mean",sep="")  
  names(datac)[ names(datac) == paste(measurevar, ".sd",      sep="") ] <- paste(measurevar,"_sd",sep = "")
  
  return(datac)
}
