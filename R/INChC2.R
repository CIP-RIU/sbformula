#'Function for calculating the Increase Chlorophyll Content 2 - Second difference (INChC2)
#'  
#' @param ChC2 Chlorophyll Content sample 2
#' @param ChC3 Chlorophyll Content sample 3
#' @return Return the Increase Chlorophyll Content 2
#' @author Omar Benites
#' @details This function calculates the Increase Chlorophyll Content 2 (Second difference)
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#

INChC2 <- function(ChC3,ChC2){
  
  if(missing(ChC2)){
    stop("Please enter the Chlorophyll Content sample 2 'ChC2'")
  }
  
  if(missing(ChC3)){
    stop("Please enter the Chlorophyll Content sample 3 'ChC3'")
  }

  INChC2 <- (apply(cbind(ChC3,-ChC2),1,sbsum)/ChC2)*100

return(INChC2)
} 
