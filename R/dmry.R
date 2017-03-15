#'Formula for calculating the Dry matter root  yield (DMRY)
#'
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@param pls Net plot size
#'@param dmf Fresh weight of roots for dry matter assessment
#'@param dmd Dry weight of dmf samples
#'@return Return the Dry matter root yield 'dmry' 
#'@author Omar Benites
#'@details Formula for calculating dry matter root yield
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'  
dmry <- function(crw,ncrw,pls,dmd,dmf){
  
  if(missing(pls)){
  #stop("Please enter the net plot size 'pls'")
    dmry <- NA
  }
  if(missing(dmd)){
  #stop("Please enter the dry matter root yield 'dmd")
    dmry <- NA
  }
  if(missing(dmf)){
  #stop("Please enter the Fresh weight of roots for dry matter assessment 'dmf")
    dmry <- NA
  }                  
  if(!missing(crw) && missing(ncrw))  {dmry <- (crw*10)/pls*dmd/dmf  }  
  if(missing(crw)  && !missing(ncrw)) {dmry <- (ncrw*10)/pls*dmd/dmf }       
  if(!missing(crw) && !missing(ncrw)) {dmry <- apply(cbind(crw, ncrw), 1, sbsum)*10/pls*dmd/dmf }
  return(dmry)
}
