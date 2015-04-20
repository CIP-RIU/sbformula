#'Function for calculating the Increase Chlorophyll Content 1 - First difference (INChC1)
#'  
#' @param ChC1 Chlorophyll Content sample 1
#' @param ChC2 Chlorophyll Content sample 2
#' @return Return the Increase Chlorophyll Content 1
#' @return Return the Increase Chlorophyll Content 2
#' @return Return the Increase Chlorophyll Content 3
#' @return Return the Increase Chlorophyll Content 4
#' @author Omar Benites
#' @details This function calculates the Increase Chlorophyll Content 1 (First difference)
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#

INChC1 <- function(ChC2,ChC1){
  
  if(missing(ChC1)){
    stop("Please enter the Chlorophyll Content sample 1 'ChC1'")
  }
  if(missing(ChC2)){
    stop("Please enter the Chlorophyll Content sample 2 'ChC2'")
  }
  
  INChC1 <- (apply(cbind(ChC2,-ChC1),1,sbsum)/ChC1)*100
  return(INChC1)

} 

