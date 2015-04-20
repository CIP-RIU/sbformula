#'Function for calculating the Increase Stem diameter 1 - First difference (INSD1)
#' @param sd_ev1 Stem diameter first Evaluation DAP
#' @param sd_ev2 Stem diameter second Evaluation DAP
#' @return insd1 Return the Increase Stem diameter 1 - First difference
#' @author Omar Benites
#' @details This function calculates the Increase Stem diameter 1 - First difference
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export
#'  

insd1 <- function(sd_ev1,sd_ev2){
  
  if(missing(sd_ev1)){
    stop("Please enter the Stem diameter first Evaluation DAP 'sd_ev1'")
  }
  
  if(missing(sd_ev2)){
    stop("Please enter the Stem diameter second Evaluation DAP 'sd_ev2'")
  }
    
  insd1 <- (apply(cbind(sd_ev2,-sd_ev1),1,sbsum)/sd_ev1)*100
  return(insd1)  
}