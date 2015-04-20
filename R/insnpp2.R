#'Function for calculating the Increase Stem number/plant 2 (INSNPP2)
#'  
#' @param snpp_ev2 Stem number/plant 2 Evaluation DAP
#' @param snpp_ev3 Stem number/plant 3 Evaluation DAP
#' @return insnpp2 Return the Increase Stem number/plant 2
#' @author Omar Benites
#' @details This function calculates the Increase Stem number/plant 2  
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 

insnpp2 <- function(snpp_ev2,snpp_ev3){
  
  if(missing(snpp_ev2)){
    stop("Please enter the Stem number/plant 2 Evaluation DAP 'snpp_ev2'")
  }
  
  if(missing(snpp_ev3)){
    stop("Please enter the Stem number/plant 3 Evaluation DAP 'snpp_ev3'")
  }
  
  insnpp2  <- (apply(cbind(snpp_ev3,-snpp_ev2),1,sbsum)/snpp_ev2)*100
  
  return(insnpp2)
  
}