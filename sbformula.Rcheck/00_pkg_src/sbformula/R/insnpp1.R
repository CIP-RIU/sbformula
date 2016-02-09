#'Function for calculating the Increase Stem number/plant 1 (INSNPP1)
#'  
#' @param snpp_ev1 Stem number/plant 1 Evaluation DAP
#' @param snpp_ev2 Stem number/plant 2 Evaluation DAP
#' @return insnpp1 Return the Increase Stem number/plant 1
#' @author Omar Benites
#' @details This function calculates the Increase Stem number/plant 1
#' @references Assessing Potato Clone for Drought Tolerance under Field Condition
#' @family drought, evaluation, potato
#' @export 
#' 

insnpp1 <- function(snpp_ev1,snpp_ev2){

  if(missing(snpp_ev1)){
    stop("Please enter the Stem number/plant 1 Evaluation DAP 'snpp_ev1'")
  }
  
  if(missing(snpp_ev2)){
    stop("Please enter the Stem number/plant 2 Evaluation DAP 'snpp_ev2'")
  }
  
  insnpp1  <- (apply(cbind(snpp_ev2,-snpp_ev1),1,sbsum)/snpp_ev1)*100
  
  return(insnpp1)

}