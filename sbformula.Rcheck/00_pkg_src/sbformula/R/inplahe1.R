#'Function for calculating the Increase Plant height 1 (INPLAHE1)
#'  
#' @param plahe_ev1 Plant height 1 Evaluation Days After Planting (DAP)
#' @param plahe_ev2 Plant height 2 Evaluation Days After Planting (DAP)
#' @return inplahe1 Return the increase plant height 1 (First difference) 
#' @author Omar Benites
#' @details This function returns the increase plant height 1 (First difference) 
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 

inplahe1 <- function(plahe_ev1,plahe_ev2){
  
  if(missing(plahe_ev1)){
    stop("Please enter the plant height 1 evaluation DAP 'plahe_ev1'")
  }
  
  if(missing(plahe_ev2)){
    stop("Please enter the plant height 2 evaluation DAP 'plahe_ev2'")
  }
    
  inplahe1 <- (apply(cbind(plahe_ev2,-plahe_ev1),1,sbsum)/plahe_ev1)*100
  return(inplahe1)
  
}
