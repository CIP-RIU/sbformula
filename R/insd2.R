#'Function for calculating the Increase Stem diameter 2 - Second difference (INSD2)
#' @param sd_ev2 Stem diameter second Evaluation DAP
#' @param sd_ev3 Stem diameter third Evaluation DAP
#' @return insd2 Return the Increase Stem diameter 2 - Second difference
#' @author Omar Benites
#' @details This function calculates the Increase Stem diameter 2 - First difference
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export
#'  

insd2 <- function(sd_ev2,sd_ev3){
  
  if(missing(sd_ev2)){
    stop("Please enter the Stem diameter second Evaluation DAP 'sd_ev2'")
  }
  
  if(missing(sd_ev3)){
    stop("Please enter the Stem diameter third Evaluation DAP 'sd_ev3'")
  }
  
  insd2 <- (apply(cbind(sd_ev3,-sd_ev2),1,sbsum)/sd_ev2)*100
  return(insd2)
}