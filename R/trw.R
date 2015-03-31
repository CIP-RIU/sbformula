#'Formula for calculating the Total Root Weight (TRW)
#'
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@return Return the Total root weight 'trw'
#'@author Omar Benites
#'@details Formula for calculating the total root weight
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield,
#'@family sweetpotato,quality,harvest
#'@export 
#'
trw <- function(crw,ncrw){
  if(!missing(crw) && missing(ncrw))  {trw <- crw}  
  if(missing(crw)  && !missing(ncrw)) {trw <- ncrw}        
  if(!missing(crw) && !missing(ncrw)) {trw <- apply(cbind(crw,ncrw),1,sbsum)/2}        
  return(trw)
}
