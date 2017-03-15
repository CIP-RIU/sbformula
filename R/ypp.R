#'Formula for calculating the Yield Per Plant (YPP)
#'
#'@param crw Commercial root weight
#'@param ncrw Non commercial root weight
#'@param noph Number of plants harvested
#'@return Return the yield per plant 'ypp'
#'@author Omar Benites, Raul Eyzaguirre
#'@details Formula for calculating the yield per plant
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
# YPP: Tener cuidado cuando NOPH = 0. En esos casos YPP debe ser NA y no Inf. github issue#1
#https://github.com/omarbenites/sbformula/issues/1

ypp <- function(crw,ncrw,noph){
#         if(missing(noph)){
#           stop("Please enter Number of plants harvested 'noph'")
#       }
        if(sum(noph, na.rm = TRUE)==0){ 
          
          ypp <- NA 
        
        } else { 
          
        if(!missing(crw) && missing(ncrw))  { ypp <- crw/noph}  
        if(missing(crw)  && !missing(ncrw)) { ypp <- ncrw/noph}  
        if(!missing(crw) && !missing(ncrw)) { ypp <- apply(cbind(crw,ncrw), 1, sbsum)/noph}
          
        }
        
        return(ypp)
}
