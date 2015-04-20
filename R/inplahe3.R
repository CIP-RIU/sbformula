#'Function for calculating the Increase Plant height 3 (INPLAHE3) 
#' 
#' @param plahe_ev3 Plant height 3 Evaluation Days After Planting (DAP)
#' @param plahe_ev4 Plant height 4 Evaluation Days After Planting (DAP)
#' @return inplahe3 Return the increase plant height 3 (Third difference)  
#' @author Omar Benites
#' @details This function calculates the increase plant height 3 (Third difference) 
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 

inplahe3 <- function(plahe_ev3,plahe_ev4){
  
  if(missing(plahe_ev3)){
    stop("Please enter the Plant height 3 Evaluation DAP plahe_ev3")
  }
  
  if(missing(plahe_ev4)){
    stop("Please enter the Plant height 4 Evaluation DAP plahe_ev4")
  }
  
  inplahe3 <- (apply(cbind(plahe_ev4,-plahe_ev3),1,sbsum)/plahe_ev3)*100
  return(inplahe3)
  
}