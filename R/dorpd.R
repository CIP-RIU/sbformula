#'Strings to days
#'
#'@param adate Data in format yyyy-mm-dd
#'@return Return the Julian date
#'@details This function converts a date string (yyyy-mm-dd) to julian date (ddMonthNameyyyy)
#'@note  str2days("2011-09-07") to 7Sep2011
#'@author Reinhard Simon
#'
str2days <- function(adate=""){
  res = NA
  try({
    x = as.integer(stringr::str_split(adate,"-")[[1]])
    res = date::mdy.date(x[2],x[3],x[1])
  })
  res
}

#'Function for calculating the Number Date of dehaulming to Date of sprouting (DORPD)
#'
#'@param datesp Date of sprouting (format: yyyy-mm-dd)
#'@param ddehaulm Date of dehaulming (format: yyyy-mm-dd)
#'@return Return the 
#'@details This function returns 
#'@description Counted as days from haulm_cutting to sprouting of 80% of the tubers with at least one sprout longer than 2 mm. 
#'Monitoring of sprout initiation and growth should be carried out at 10 day-intervals in order to record an accurate dormancy period 
#'@author Omar Benites
#'@references Procedures for standard evaluation and data management of advanced potato clones: Assessment of Dormancy and Sprouting
#'Behavior of Elite and Advanced Clones.International Potato Center (CIP).
#'@family Sprout-dormancy, evaluation, potato
#'@export

dorpd <- function(datesp,ddehaulm){
  
  if(missing(datesp)){
    stop("Please enter the date of sprouting 'datesp'")
    return(FALSE)
  }
  
  if(missing(ddehaulm)){
    stop("Please etner the date of dehaulm 'date.dehaulm'")
    return(FALSE)
  }
  
  if((is.na(ddehaulm))){
    stop('Please enter dates in specified format (yyyy-mm-dd)')
    return(FALSE)
  }
  
  date.dehaulm = str2days(ddehaulm)
    
  dorpd <- apply(cbind(datesp),1,str2days) - date.dehaulm
  return(dorpd)
}
  