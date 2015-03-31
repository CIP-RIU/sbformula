#'Formula for calculating the Foliage total 'FYTHA'
#'
#'@param vw Vine weight
#'@param pls Net plot size
#'@return Return of the foliage total 'fytha' 
#'@author Omar Benites
#'@details Formula for calculating the foliage total
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield
#'@family sweetpotato,quality,harvest
#'@export 
#'
fytha <- function(vw,pls){
            if(missing(pls)){
              stop("Please enter 'net plot size'")
            }
            if(missing(fytha)){
              stop("Please enter 'vine weight'")
            }
            fytha  <-  vw*10/pls
            return(fytha)
}
