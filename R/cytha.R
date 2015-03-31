#'Formula for calculating the Commercial Root Yield (CYTHA)
#'
#'@param crw Commercial root weight
#'@param pls Net plot size
#'@return Return the commercial root yield 'cytha'
#'@author Omar Benites
#'@details Formula for calculating commercial root yield
#'@keywords sweetpotato, agronomy,harvest,quantitative-continuous,yield,
#'@family sweetpotato,quality,harvest
#'@export 
#'

cytha <- function(crw,pls){
  
           if (missing(crw)){
              stop("Please enter the commercial root weight 'crw'")
           }
           if (missing(pls)){
             stop("Please enter the net plot size 'pls'")
           }
           cytha <-  (crw*10)/pls
        }
