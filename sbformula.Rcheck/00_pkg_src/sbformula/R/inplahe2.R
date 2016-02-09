#'Function for calculating the Increase Plant height 2 (INPLAHE2)
#' 
#' @param plahe_ev2 Plant height 2 Evaluation Days After Planting (DAP)
#' @param plahe_ev3 Plant height 3 Evaluation Days After Planting (DAP)
#' @return inplahe2 Return the increase plant height 2 (Second difference) 
#' @author Omar Benites
#' @details This function calculate the increase plant height 2 (Second difference) 
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 

inplahe2 <- function(plahe_ev2,plahe_ev3){
  
  if(missing(plahe_ev2)){
    stop("Please enter the Plant height 2 Evaluation DAP plahe_ev2")
  }
  
  if(missing(plahe_ev3)){
    stop("Please enter the Plant height 3 Evaluation DAP plahe_ev3")
  }
  
  inplahe2 <- (apply(cbind(plahe_ev3,-plahe_ev2),1,sbsum)/plahe_ev2)*100
  return(inplahe2)
  
}