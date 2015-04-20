#'Function for calculating the Increase Chlorophyll Content 4 - Fourth difference (INChC4)
#'  
#' @param ChC4 Chlorophyll Content sample 4
#' @param ChC5 Chlorophyll Content sample 5
#' @return Return the Increase Chlorophyll Content 4
#' @author Omar Benites
#' @details This function calculates the Increase Chlorophyll Content 4 (Fourth difference)    
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#

INChC4 <- function(ChC5,ChC4){
  
  if(missing(ChC4)){
    stop("Please enter the Chlorophyll Content sample 4 'ChC4'")
  }
  
  if(missing(ChC5)){
    stop("Please enter the Chlorophyll Content sample 5 'ChC5'")
  }
  
  INChC4 <- (apply(cbind(ChC5,-ChC4),1,sbsum)/ChC4)*100
 
  return(INChC4)

}