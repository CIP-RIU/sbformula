#'Formula for calculating the Harvest index (HI)
#'
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@param vw Vine weight
#'@return Return the harvest index 'hi'
#'@author Omar Benites
#'@details Formula for calculating harvest index 
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
hi<- function(crw,ncrw,vw){
        if(missing(crw) && missing(ncrw) && missing(vw)){
          hi <- NA
        } else {
          hi <- apply(cbind(crw, ncrw), 1, sbsum)*100/(apply(cbind(vw, crw, ncrw), 1, sbsum))
        }
        return(hi)
}
