#'Function for calculating the Increase Chlorophyll Content 3 - Third difference (INChC3)
#'  
#' @param ChC3 Chlorophyll Content sample 3
#' @param ChC4 Chlorophyll Content sample 4
#' @return Return the Increase Chlorophyll Content 3
#' @author Omar Benites
#' @details This function calculates the Increase Chlorophyll Content 3 (Third difference)
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#

INChC3 <- function(ChC4,ChC3){
  
   if(missing(ChC3)){
    stop("Please enter the Chlorophyll Content sample 3 'ChC3'")
  }
  
  if(missing(ChC4)){
    stop("Please enter the Chlorophyll Content sample 3 'ChC4'")
  }

INChC3 <- (apply(cbind(ChC4,-ChC3),1,sbsum)/ChC3)*100
return(INChC3)

} 