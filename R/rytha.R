#'Formula for calculating the Total root yield (RYTHA)
#'
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@param pls Net plot size
#'@return Return the Total root yield 'rytha'
#'@author Omar Benites
#'@details Formula for calculating total root yield
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield,
#'@family sweetpotato,quality,harvest
#'@export 
#'
rytha <- function(crw,ncrw,pls){
  if(missing(pls)){
    stop("Please enter the net plot size 'pls")
  }
  if(!missing(crw) && missing(ncrw) ) {rytha <- (crw*10)/pls}  
  if(missing(crw)  && !missing(ncrw)) {rytha <- (ncrw*10)/pls}        
  if(!missing(crw) && !missing(ncrw)) {rytha <- apply(cbind(crw,ncrw),1,sbsum)*10/pls}        
  return(rytha)
}
