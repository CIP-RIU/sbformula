#'Formula for calculating the Biomass Yield (BIOM)
#'
#'@param vw Vine weight
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@param pls New plot size
#'@return Return the biommas yield 'biom'
#'@author Omar Benites
#'@details Formula for calculating the biomass yield
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
#BIOM: Falta multiplicar por 10 para convertir en tons/ha.
#https://github.com/omarbenites/sbformula/issues/1
biom<- function(vw,crw,ncrw,pls){
  if(missing(pls)){
    stop("Please enter the new plot size 'pls'")
  }
  if(missing(vw)  && !missing(crw) && !missing(ncrw)) {biom <- apply(cbind(crw,ncrw),1,sbsum)/pls}  
  if(!missing(vw) && missing(crw)  && !missing(ncrw)) {biom <- apply(cbind(vw,ncrw),1,sbsum)/pls}
  if(!missing(vw) && !missing(crw) &&  missing(ncrw)) {biom <- apply(cbind(vw,crw),1,sbsum)/pls} 
  if(!missing(vw) && !missing(crw) && !missing(ncrw)) {biom <- apply(cbind(vw,crw,ncrw),1,sbsum)*10/pls}
  return(biom)
}
