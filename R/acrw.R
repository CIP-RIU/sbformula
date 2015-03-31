#'Formula for calculating the Average commercial root weight (ACRW)
#'
#'@param crw Commercial root weight
#'@param nocr Number of commercial roots
#'@return Return the average commercial root weight  'acrw'
#'@author Omar Benites
#'@details Formula for calculating the average commercial root weight
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#' 
acrw <- function(crw,nocr){
        
        if(missing(crw)){
          stop("Please enter the commercial root weight 'crw'")
        }
        if(missing(nocr)){
          stop("Please enter the number of commercial roots 'nocr'")
        }
        acrw <- crw/nocr
        return(acrw)
}
